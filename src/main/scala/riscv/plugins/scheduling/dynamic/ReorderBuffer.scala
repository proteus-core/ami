package riscv.plugins.scheduling.dynamic

import riscv._
import riscv.plugins.mimicry.MimicryRegisterType
import riscv.plugins.mimicry.MimicryRegisterType.MIMIC_GPR
import spinal.core._
import spinal.lib.{Counter, Flow}

// Each ROB entry is tagged with the following metadata:
// shadowingActivating: the ID of the activating branch this instruction is mimic-dependent on
// previousWaw: the ID of the previous instruction that writes to the same target as this instruction (only if this instruction writes to a register)
case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]], indexBits: BitCount)(
    implicit config: Config
) extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle // TODO: this still contains the mimicry registers, we can save further by removing them somehow
  val rdbUpdated = Bool()
  val cdbUpdated = Bool()
  val mimicDependency = Flow(UInt(indexBits))
  val previousWaw = Flow(UInt(indexBits))

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

  // TODO: not like this
  private val CSR_MMAC = 0x7ff
  private val CSR_MMENTRY = 0x7df // CSR identifier
  private val CSR_MMEXIT = 0x7ef // CSR identifier

  private val acCsr = readOnlyCsr(CSR_MMAC)
  private val enCsr = readOnlyCsr(CSR_MMENTRY)
  private val exCsr = readOnlyCsr(CSR_MMEXIT)

  val committedMMEX = exCsr.read()

  val currentMMAC = Reg(UInt(config.xlen bits)).init(0)

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
    currentMMAC := acCsr.read()
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

  def pushEntry(): (UInt, EntryMetadata, Flow[UInt], Bool) = {
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

    val pendingActivating = Flow(UInt(indexBits))
    pendingActivating.setIdle()

    val metaUpdateNeeded = Bool()
    metaUpdateNeeded := False

    val mimicryMode = Bool()
    mimicryMode := currentMMAC > 0

    val mimicry = pipeline.service[MimicryService]

    def handleExit(absolute: UInt): Unit = {
      val entry = robEntries(absolute)
      pushedEntry.mimicDependency.push(absolute)
      when(entry.cdbUpdated /*|| entry.rdbUpdated*/ ) {
        // TODO: we're stretching the meaning of this JUMP_REQUESTED quite a lot here
        mimicryMode := pipeline.service[JumpService].jumpOfBundle(entry.registerMap)
      } otherwise {
        metaUpdateNeeded := True
      }
    }

    val priorPending = stackTop()
    when(priorPending.valid) {
      when(priorPending.exit === pc) {
        stackPopNow := True
        val secondPending = stackUnderTop()
        // if pc == pendingExit, remove this pendingExit, look for the next one, then (if valid, copy MM, if not valid, depend on it)
        when(secondPending.valid) {
          handleExit(secondPending.robId)
        }
      } otherwise {
        // if pc != pendingExit, (if valid, copy MM, if not valid, depend on it)
        handleExit(priorPending.robId)
      }
    }

    // if activating, set pendingExit
    when(mimicry.isActivating(issueStage)) {
      val nextPc = UInt()
      nextPc := issueStage.output(pipeline.data.NEXT_PC)
      when(mimicry.isABranch(issueStage)) {
        nextPc := pc + issueStage.output(pipeline.data.IMM)
      }
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

    when(!pushedEntry.mimicDependency.valid && currentMMAC > 0 && pc === exCsr.read()) {
      currentMMAC := currentMMAC - 1
      when(currentMMAC === 1) {
        mimicryMode := False
      }
    }

    when(metaUpdateNeeded) {
      pendingActivating := pushedEntry.mimicDependency
    }

    (
      newestIndex.value,
      entryMeta,
      pendingActivating,
      mimicryMode
    )
  }

  private def bookkeeping(rs1Id: Flow[UInt], rs2Id: Flow[UInt], rdId: Flow[UInt]): EntryMetadata = {
    val meta = EntryMetadata(indexBits)
    meta.rs1Data.payload.assignDontCare()
    meta.rs2Data.payload.assignDontCare()

    meta.rs1Data.valid := rs1Id.valid
    meta.rs2Data.valid := rs2Id.valid

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
    robEntries(cdbMessage.robIndex).previousWaw := cdbMessage.previousWaw

    pipeline
      .service[JumpService]
      .jumpOfBundle(robEntries(cdbMessage.robIndex).registerMap) := cdbMessage.activatingTaken

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

    robEntries(rdbMessage.robIndex).rdbUpdated := True
  }

  def readOnlyCsr(csrId: Int): CsrIo = {
    val csrService = pipeline.service[CsrService]
    val csr = csrService.getCsr(csrId)
    csr.write := False
    csr.wdata.assignDontCare()
    csr
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

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      mimicry.inputMeta(ret, 0, 0, 0)
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
        mimicry.inputMeta(
          ret,
          acCsr.read(),
          enCsr.read(),
          exCsr.read()
        )
      }

      when(ret.arbitration.isDone) {
        val bottom = stackBottom()
        when(bottom.valid && bottom.robId === oldestIndex) {
          removeBottom()
          when(!singleElement || !stackPopNow) {
            // TODO: automate this somehow based on the output of the retirement stage?
            when(pipeline.service[JumpService].jumpOfBundle(oldestEntry.registerMap)) {
              when(
                currentMMAC === 0 || oldestEntry.registerMap.element(
                  pipeline.data.PC.asInstanceOf[PipelineData[Data]]
                ) === enCsr.read()
              ) {
                currentMMAC := currentMMAC + 1
              }
            }
          }
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
