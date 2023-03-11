package riscv.plugins.scheduling.dynamic

import riscv._
import riscv.plugins.mimicry.MimicryRegisterType
import riscv.plugins.mimicry.MimicryRegisterType.MIMIC_GPR
import spinal.core._
import spinal.lib.{Counter, Flow}

// Each ROB entry is tagged with the following metadata:
// shadowingActivating: the ID of the activating branch this instruction is mimic-dependent on
// previousWaw: the ID of the previous instruction that writes to the same target as this instruction (only if this instruction writes to a register)
// pendingExitAddress: if the instruction is activating and the corresponding exit has not been reached yet, we store the corresponding exit address here
case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]], indexBits: BitCount)(
    implicit config: Config
) extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle
  val rdbUpdated = Bool()
  val cdbUpdated = Bool()
  val mimicDependency = Flow(UInt(indexBits))
  val previousWaw = Flow(UInt(indexBits))
  val pendingExit = Flow(UInt(config.xlen bits))

  // temporary
  val mmac = UInt(config.xlen bits)
  val mmen = UInt(config.xlen bits)
  val mmex = UInt(config.xlen bits)

  override def clone(): RobEntry = {
    RobEntry(retirementRegisters, indexBits)
  }
}

case class RsData(indexBits: BitCount)(implicit config: Config) extends Bundle {
  val updatingInstructionFound = Bool()
  val updatingInstructionFinished = Bool()
  val updatingInstructionMimicked = Bool()
  val updatingInstructionIndex = UInt(indexBits)
  val updatingInstructionValue = UInt(config.xlen bits)
  val previousValid = Flow(UInt(config.xlen bits))
  val previousWaw = Flow(UInt(indexBits))
}

case class EntryMetadata(indexBits: BitCount)(implicit config: Config) extends Bundle {
  val rs1Data = Flow(RsData(indexBits))
  val rs2Data = Flow(RsData(indexBits))

  val mmac = UInt(config.xlen bits)
  val mmen = UInt(config.xlen bits)
  val mmex = UInt(config.xlen bits)

  val previousWaw = Flow(UInt(indexBits))

  override def clone(): EntryMetadata = {
    EntryMetadata(indexBits)
  }
}

/** Terminology:
  *   - absolute index: the actual index of an entry in the circular buffer
  *   - relative index: an index that shows the order of instructions inserted into the ROB, 0 being
  *     the oldest
  *
  * Relative indices are internal to the ROB, outside components only see the absolute index of an
  * entry
  */
class ReorderBuffer(
    pipeline: DynamicPipeline,
    robCapacity: Int,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area
    with CdbListener {
  private def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  private val robEntries =
    Vec.fill(capacity)(RegInit(RobEntry(retirementRegisters, indexBits).getZero))
  private val oldestIndex = Counter(capacity)
  private val newestIndex = Counter(capacity)
  private val isFullNext = Bool()
  private val isFull = RegNext(isFullNext).init(False)
  private val willRetire = False
  val isAvailable: Bool = !isFull || willRetire

  private val pushInCycle = Bool()
  pushInCycle := False
  private val pushedEntry = RobEntry(retirementRegisters, indexBits)
  pushedEntry := RobEntry(retirementRegisters, indexBits).getZero

  // save some work by keeping a copy of CSR values and not looking them up every cycle
  private val MMAC = Reg(UInt(config.xlen bits)).init(0)
  private val MMEN =
    Reg(UInt(config.xlen bits)).init(pipeline.service[MimicryService].CSR_MMADDR_NONE)
  private val MMEX =
    Reg(UInt(config.xlen bits)).init(pipeline.service[MimicryService].CSR_MMADDR_NONE)

  case class MimicryStackEntry() extends Bundle {
    val robId = UInt(indexBits)
    val exit = UInt(config.xlen bits)
  }

  private val mimicryStack = Vec.fill(capacity)(RegInit(MimicryStackEntry().getZero))
  private val stackOldestIndex = Counter(capacity).init(1)
  private val stackNewestIndex = Counter(capacity)

  private val stackBottomRemoveNow = Bool()
  stackBottomRemoveNow := False
  private val stackPushNow = Bool()
  stackPushNow := False
  private val stackPopNow = Bool()
  stackPopNow := False
  private val pushedStackEntry = MimicryStackEntry()
  pushedStackEntry.assignDontCare()

  // stack management
  private val singleElement = stackOldestIndex === stackNewestIndex

  private def nextIndex(index: UInt) = {
    val result = U(0)
    when(index =/= capacity - 1) {
      result := index + 1
    }
    result
  }

  private def stackEmpty(): Bool = {
    stackOldestIndex === nextIndex(stackNewestIndex)
  }

  private def pushToStack(id: UInt, exit: UInt) = {
    stackNewestIndex.increment()
    mimicryStack(stackNewestIndex.valueNext).robId := id
    mimicryStack(stackNewestIndex.valueNext).exit := exit
  }

  private def stackTop(): Flow[MimicryStackEntry] = {
    val result = Flow(MimicryStackEntry())
    result.setIdle()
    when(!stackEmpty()) {
      result.push(mimicryStack(stackNewestIndex))
    }
    result
  }

  private def stackUnderTop(): Flow[MimicryStackEntry] = {
    val result = Flow(MimicryStackEntry())
    result.setIdle()
    val previous = stackNewestIndex - 1
    when(stackNewestIndex === 0) {
      previous := capacity - 1
    }
    when(!singleElement) {
      result.push(mimicryStack(previous))
    }
    result
  }

  private def stackPop(): Unit = {
    when(stackNewestIndex === 0) {
      stackNewestIndex := capacity - 1
    } otherwise {
      stackNewestIndex := stackNewestIndex - 1
    }
  }

  private def stackBottom(): Flow[MimicryStackEntry] = {
    val result = Flow(MimicryStackEntry())
    result.setIdle()
    when(!stackEmpty()) {
      result.push(mimicryStack(stackOldestIndex))
    }
    result
  }

  private def removeBottom() = {
    stackOldestIndex.increment()
    stackBottomRemoveNow := True
  }

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    isFull := False

    stackOldestIndex := 1
    stackNewestIndex.clear()
  }

  private def isValidAbsoluteIndex(index: UInt): Bool = {
    val ret = Bool()

    val oldest = UInt(indexBits)
    val newest = UInt(indexBits)
    oldest := oldestIndex.value
    newest := newestIndex.value

    when(isFull) {
      ret := True
    } elsewhen (oldest === newest && !isFull) { // empty
      ret := False
    } elsewhen (newest > oldest) { // normal order
      ret := index >= oldest && index < newest
    } otherwise { // wrapping
      ret := index >= oldest || index < newest
    }
    ret
  }

  private def relativeIndexForAbsolute(absolute: UInt): UInt = {
    val adjustedIndex = UInt(32 bits)
    when(absolute >= oldestIndex.value) {
      adjustedIndex := (absolute.resized - oldestIndex.value).resized
    } otherwise {
      val remainder = capacity - oldestIndex.value
      adjustedIndex := (absolute + remainder).resized
    }
    adjustedIndex
  }

  private def absoluteIndexForRelative(relative: UInt): UInt = {
    val absolute = UInt(32 bits)
    val adjusted = UInt(32 bits)
    val oldestResized = UInt(32 bits)
    oldestResized := oldestIndex.value.resized
    absolute := oldestResized + relative
    when(absolute >= capacity) {
      adjusted := absolute - capacity
    } otherwise {
      adjusted := absolute
    }
    adjusted
  }

  val pendingActivating = Flow(UInt(indexBits))
  pendingActivating.setIdle()

  val metaUpdateNeeded = Bool()
  metaUpdateNeeded := False

  val secondP = Bool()
  secondP := False

  def pushEntry(): (UInt, EntryMetadata, Flow[UInt], (UInt, UInt, UInt)) = {
    val issueStage = pipeline.issuePipeline.stages.last

    pushInCycle := True
    pushedEntry.rdbUpdated := False
    pushedEntry.cdbUpdated := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := issueStage
      .output(pipeline.data.PC)
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := issueStage
      .output(pipeline.data.RD)
    pushedEntry.registerMap.element(
      pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
    ) := issueStage.output(pipeline.data.RD_TYPE)
    pipeline.service[LsuService].operationOfBundle(pushedEntry.registerMap) := pipeline
      .service[LsuService]
      .operationOutput(issueStage)
    pipeline.service[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False

    val rs1 = Flow(UInt(5 bits))
    val rs2 = Flow(UInt(5 bits))
    val rd = Flow(UInt(5 bits))

    rs1.valid := issueStage.output(pipeline.data.RS1_TYPE) === RegisterType.GPR
    rs1.payload := issueStage.output(pipeline.data.RS1)

    rs2.valid := issueStage.output(pipeline.data.RS2_TYPE) === RegisterType.GPR
    rs2.payload := issueStage.output(pipeline.data.RS2)

    rd.valid := issueStage.output(pipeline.data.RD_TYPE) === RegisterType.GPR || issueStage.output(
      pipeline.data.RD_TYPE
    ) === MimicryRegisterType.MIMIC_GPR
    rd.payload := issueStage.output(pipeline.data.RD)

    val entryMeta = bookkeeping(rs1, rs2, rd)

    val pc = issueStage.output(pipeline.data.PC)

    def localUpdate(mmac: UInt, mmen: UInt, mmex: UInt): (UInt, UInt, UInt) = {
      val outac = UInt(config.xlen bits)
      outac := mmac
      val outen = UInt(config.xlen bits)
      outen := mmen
      val outex = UInt(config.xlen bits)
      outex := mmex

      when(pc === mmen && mmac > 0) {
        outac := mmac + 1
      }
      when(pc === mmex && mmac > 0) {
        when(mmac === 1) {
          outen := 0
          outex := 0
        }
        outac := mmac - 1
      }
      (outac, outen, outex)
    }

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      {
        val outac = entryMeta.mmac
        val outen = entryMeta.mmen
        val outex = entryMeta.mmex

        val found = Bool()
        found := False

        def handleExit(absolute: UInt): Unit = {
          val entry = robEntries(absolute)
          pushedEntry.mimicDependency.push(absolute)
          when(entry.cdbUpdated /*|| entry.rdbUpdated*/ ) {
            outac := entry.mmac
            outen := entry.mmen
            outex := entry.mmex
          } otherwise {
            metaUpdateNeeded := True
          }
        }

        val noPush = False

        // find first prior instruction with pendingExit
        val priorPending = stackTop()
        when(priorPending.valid) {
          when(priorPending.exit === pc) {
            stackPopNow := True
//            when(mimicry.isActivating(issueStage)) {
//              noPush := True
//            } otherwise {
//              stackPop()
//            }
            val secondPending = stackUnderTop()
            // if pc == pendingExit, remove this pendingExit, look for the next one, then (if valid, copy MM, if not valid, depend on it)
            when(secondPending.valid) {
              secondP := True
              handleExit(secondPending.robId)
              found := True
            }
          } otherwise {
            // if pc != pendingExit, (if valid, copy MM, if not valid, depend on it)
            handleExit(priorPending.robId)
            found := True
          }
        }

        // if activating, set pendingExit
        when(mimicry.isActivating(issueStage)) {
          val nextPc = UInt()
          nextPc := issueStage.output(pipeline.data.NEXT_PC)
          when(mimicry.isABranch(issueStage)) {
            nextPc := pc + issueStage.output(pipeline.data.IMM)
          }
          pushedEntry.pendingExit.push(nextPc)
          pushedStackEntry.robId := newestIndex
          pushedStackEntry.exit := nextPc
          stackPushNow := True
        }

        when(stackPopNow) {
          // implies >= 1 entries
          when(singleElement) {
            // single entry in stack
            when(stackBottomRemoveNow) {
              // don't remove from two sides
              when(stackPushNow) {
                pushToStack(pushedStackEntry.robId, pushedStackEntry.exit)
              }
            } otherwise {
              when(stackPushNow) {
                mimicryStack(stackNewestIndex).robId := pushedStackEntry.robId
                mimicryStack(stackNewestIndex).exit := pushedStackEntry.exit
              } otherwise {
                stackPop()
              }
            }
          } otherwise {
            // multiple entries in stack
            when(stackPushNow) {
              mimicryStack(stackNewestIndex).robId := pushedStackEntry.robId
              mimicryStack(stackNewestIndex).exit := pushedStackEntry.exit
            } otherwise {
              stackPop()
            }
          }
        } otherwise {
          when(stackPushNow) {
            pushToStack(pushedStackEntry.robId, pushedStackEntry.exit)
          }
        }

        val (mmac, mmen, mmex) = localUpdate(outac, outen, outex)
        pushedEntry.mmac := mmac
        pushedEntry.mmen := mmen
        pushedEntry.mmex := mmex

        when(metaUpdateNeeded) {
          pendingActivating := pushedEntry.mimicDependency
        }
      }
    }

    (
      newestIndex.value,
      entryMeta,
      pendingActivating,
      (pushedEntry.mmac, pushedEntry.mmen, pushedEntry.mmex)
    )
  }

  private def bookkeeping(rs1Id: Flow[UInt], rs2Id: Flow[UInt], rdId: Flow[UInt]): EntryMetadata = {
    val meta = EntryMetadata(indexBits)
    meta.rs1Data.payload.assignDontCare()
    meta.rs2Data.payload.assignDontCare()

    meta.rs1Data.valid := rs1Id.valid
    meta.rs2Data.valid := rs2Id.valid

    meta.mmac := MMAC
    meta.mmen := MMEN
    meta.mmex := MMEX

    meta.previousWaw.setIdle()

    def rsUpdate(rsId: Flow[UInt], index: UInt, entry: RobEntry, rsMeta: RsData): Unit = {
      val registerType = entry.registerMap.element(
        pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
      )
      when(
        rsId.valid
          && rsId.payload =/= 0
          && entry.registerMap.element(
            pipeline.data.RD.asInstanceOf[PipelineData[Data]]
          ) === rsId.payload
          && (registerType === RegisterType.GPR || registerType === MIMIC_GPR)
      ) {
        val value =
          entry.registerMap.elementAs[UInt](pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]])

        when((entry.cdbUpdated /*|| entry.rdbUpdated*/ ) && registerType === RegisterType.GPR) {
          rsMeta.previousValid.push(value)
        }

        rsMeta.previousWaw := entry.previousWaw
        rsMeta.updatingInstructionMimicked := registerType === MIMIC_GPR

        rsMeta.updatingInstructionFound := True
        rsMeta.updatingInstructionFinished := (entry.cdbUpdated || entry.rdbUpdated)
        rsMeta.updatingInstructionIndex := index
        rsMeta.updatingInstructionValue := value
      }
    }

    // loop through valid values and return the freshest if present
    for (relative <- 0 until capacity) {
      val absolute = absoluteIndexForRelative(relative).resized
      val entry = robEntries(absolute)

      when(isValidAbsoluteIndex(absolute)) {
        rsUpdate(rs1Id, absolute, entry, meta.rs1Data.payload)
        rsUpdate(rs2Id, absolute, entry, meta.rs2Data.payload)

        when(!entry.mimicDependency.valid) {
          meta.mmac := entry.mmac
          meta.mmen := entry.mmen
          meta.mmex := entry.mmex
        }

        val sameTarget = entry.registerMap.elementAs[UInt](
          pipeline.data.RD.asInstanceOf[PipelineData[Data]]
        ) === rdId.payload

        val registerType =
          entry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]])

        when(
          rdId.valid
            && sameTarget
            && !(entry.cdbUpdated /* || entry.ready*/ )
            && (registerType === RegisterType.GPR || registerType === MIMIC_GPR)
        ) {
          meta.previousWaw.push(absolute)
        }
      }
    }
    meta
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).cdbUpdated := True
    robEntries(cdbMessage.robIndex).registerMap
      .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue

    // When instructions are updated in the ROB after execution (onCdbMessage?):
    // Update MM** registers of the given instruction if it had a shadowing activating (even if not?)
    robEntries(cdbMessage.robIndex).mmac := cdbMessage.mmac
    robEntries(cdbMessage.robIndex).mmen := cdbMessage.mmen
    robEntries(cdbMessage.robIndex).mmex := cdbMessage.mmex
    robEntries(cdbMessage.robIndex).previousWaw := cdbMessage.previousWaw

    when(cdbMessage.realUpdate) {
      robEntries(cdbMessage.robIndex).registerMap
        .element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := RegisterType.GPR
    } otherwise {
      robEntries(cdbMessage.robIndex).registerMap
        .element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := MIMIC_GPR
    }
  }

  def hasPendingStoreForEntry(robIndex: UInt, address: UInt): Bool = {
    val found = Bool()
    found := False

    val wordAddress = config.dbusConfig.byte2WordAddress(address)

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val lsuService = pipeline.service[LsuService]
      val entryIsStore = lsuService.operationOfBundle(entry.registerMap) === LsuOperationType.STORE
      val entryAddressValid = lsuService.addressValidOfBundle(entry.registerMap)
      val entryAddress = lsuService.addressOfBundle(entry.registerMap)
      val entryWordAddress = config.dbusConfig.byte2WordAddress(entryAddress)
      val addressesMatch = entryWordAddress === wordAddress
      val isOlder = relativeIndexForAbsolute(index) < relativeIndexForAbsolute(robIndex)

      when(
        isValidAbsoluteIndex(nth)
          && isOlder
          && entryIsStore
          && ((entryAddressValid && addressesMatch) || !entryAddressValid)
      ) {
        found := True
      }
    }
    found
  }

  def onRdbMessage(rdbMessage: RdbMessage): Unit = {
    robEntries(rdbMessage.robIndex).registerMap := rdbMessage.registerMap

    // to make sure CSR values are propagated correctly, flush the pipeline after CSR operations
    when(pipeline.service[CsrService].isCsrInstruction(rdbMessage.registerMap)) {
      pipeline
        .service[JumpService]
        .jumpOfBundle(robEntries(rdbMessage.robIndex).registerMap) := True
    }

    // TODO: update mm** registers

    robEntries(rdbMessage.robIndex).rdbUpdated := True
  }

  def build(): Unit = {
    isFullNext := isFull
    val oldestEntry = robEntries(oldestIndex.value)
    val updatedOldestIndex = UInt(indexBits)
    updatedOldestIndex := oldestIndex.value
    val isEmpty = oldestIndex.value === newestIndex.value && !isFull

    val ret = pipeline.retirementStage
    ret.arbitration.isValid := False
    ret.arbitration.isStalled := False

    for (register <- retirementRegisters.keys) {
      ret.input(register) := oldestEntry.registerMap.element(register)
    }

    // FIXME this doesn't seem the correct place to do this...
    ret.connectOutputDefaults()
    ret.connectLastValues()

    when(
      !isEmpty && oldestEntry.rdbUpdated && (oldestEntry.cdbUpdated || (!oldestEntry.registerMap
        .elementAs[Bool](pipeline.data.RD_DATA_VALID.asInstanceOf[PipelineData[Data]]) &&
        pipeline
          .service[LsuService]
          .operationOfBundle(oldestEntry.registerMap) =/= LsuOperationType.LOAD))
    ) {
      ret.arbitration.isValid := True

      pipeline.serviceOption[MimicryService].foreach { mimicry =>
        MMAC := oldestEntry.mmac
        MMEN := oldestEntry.mmen
        MMEX := oldestEntry.mmex
        mimicry.inputMeta(ret, oldestEntry.mmac, oldestEntry.mmen, oldestEntry.mmex)
      }

      when(ret.arbitration.isDone) {
        val bottom = stackBottom()
        when(bottom.valid && bottom.robId === oldestIndex) {
          removeBottom()
        }
        // removing the oldest entry
        updatedOldestIndex := oldestIndex.valueNext
        oldestIndex.increment()
        willRetire := True
        isFullNext := False
      }
    }

    when(pushInCycle) {
      robEntries(newestIndex.value) := pushedEntry
      val updatedNewest = newestIndex.valueNext
      newestIndex.increment()
      when(updatedOldestIndex === updatedNewest) {
        isFullNext := True
      }
    }
  }
}
