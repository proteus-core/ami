package riscv.plugins.scheduling.dynamic

import riscv._
import riscv.plugins.mimicry.MimicryRegisterType
import riscv.plugins.mimicry.MimicryRegisterType.MIMIC_GPR
import spinal.core._
import spinal.lib.{Counter, Flow}

case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]], indexBits: BitCount)(implicit config: Config)
    extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle
  val ready = Bool()
  val hasValue = Bool()
  val mimicDependency = Flow(UInt(indexBits))

  override def clone(): RobEntry = {
    RobEntry(retirementRegisters, indexBits)
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
  def capacity: Int = robCapacity
  def indexBits: BitCount = log2Up(capacity) bits

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry(retirementRegisters, indexBits).getZero))
  val oldestIndex = Counter(capacity)
  val newestIndex = Counter(capacity)
  private val isFullNext = Bool()
  private val isFull = RegNext(isFullNext).init(False)
  private val willRetire = False
  val isAvailable = !isFull || willRetire

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry(retirementRegisters, indexBits)
  pushedEntry := RobEntry(retirementRegisters, indexBits).getZero

  val activatingShadow = RegInit(Flow(UInt(indexBits)).setIdle())  // TODO: assign
  val pendingActivating = RegInit(Flow(UInt(indexBits)).setIdle())
  val dummyPendingActivating = RegInit(Flow(UInt(indexBits)).setIdle()) // To keep track of activating instructions that arrive during AC > 0
  val internalMMAC = RegInit(UInt(config.xlen bits).getZero)
  val internalMMEN = RegInit(UInt(config.xlen bits).getZero)
  val internalMMEX = RegInit(UInt(config.xlen bits).getZero)

  def readOnlyCsr(csrId: Int): CsrIo = {
    val csrService = pipeline.service[CsrService]
    val csr = csrService.getCsr(csrId)
    csr.write := False
    csr.wdata.assignDontCare()
    csr
  }

  // TODO: not like this
  private val CSR_MMAC = 0x7ff
  private val CSR_MMENTRY = 0x7df // CSR identifier
  private val CSR_MMEXIT = 0x7ef // CSR identifier

  val acCsr = readOnlyCsr(CSR_MMAC)
  val enCsr = readOnlyCsr(CSR_MMENTRY)
  val exCsr = readOnlyCsr(CSR_MMEXIT)

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    pendingActivating.setIdle()
    dummyPendingActivating.setIdle()
    isFull := False
    when(!pendingActivating.valid || pendingActivating.payload =/= oldestIndex) {
      when(
        robEntries(oldestIndex).ready && pipeline
          .service[JumpService]
          .jumpOfBundle(robEntries(oldestIndex).registerMap)
      ) {
        pipeline.serviceOption[MimicryService].foreach { mimicry =>
          // could also take it from stage output i guess
          internalMMAC := mimicry.acOfBundle(robEntries(oldestIndex).registerMap)
          internalMMEN := mimicry.enOfBundle(robEntries(oldestIndex).registerMap)
          internalMMEX := mimicry.exOfBundle(robEntries(oldestIndex).registerMap)
        }
      } otherwise {
        internalMMAC := acCsr.read()
        internalMMEN := enCsr.read()
        internalMMEX := exCsr.read()
      }
    }
  }

  def waitingForActivating(): Bool = {
    pendingActivating.valid || dummyPendingActivating.valid
  }

  def isValidAbsoluteIndex(index: UInt): Bool = {
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

  def relativeIndexForAbsolute(absolute: UInt): UInt = {
    val adjustedIndex = UInt(32 bits)
    when(absolute >= oldestIndex.value) {
      adjustedIndex := (absolute.resized - oldestIndex.value).resized
    } otherwise {
      val remainder = capacity - oldestIndex.value
      adjustedIndex := (absolute + remainder).resized
    }
    adjustedIndex
  }

  def absoluteIndexForRelative(relative: UInt): UInt = {
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

  def pushEntry(
      rd: UInt,
      rdType: SpinalEnumCraft[RegisterType.type],
      lsuOperationType: SpinalEnumCraft[LsuOperationType.type],
      pc: UInt,
      nextPc: UInt
  ): (UInt, Flow[UInt], (UInt, UInt, UInt)) = {
    pushInCycle := True
    pushedEntry.ready := False
    pushedEntry.hasValue := False
    pushedEntry.registerMap.element(pipeline.data.PC.asInstanceOf[PipelineData[Data]]) := pc
    pushedEntry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) := rd
    pushedEntry.registerMap.element(
      pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
    ) := rdType
    pipeline.service[LsuService].operationOfBundle(pushedEntry.registerMap) := lsuOperationType
    pipeline.service[LsuService].addressValidOfBundle(pushedEntry.registerMap) := False

    val mimicDependency = Flow(UInt(indexBits))
    mimicDependency.setIdle()
    val newinternalMMAC = UInt(config.xlen bits)
    val newinternalMMEN = UInt(config.xlen bits)
    val newinternalMMEX = UInt(config.xlen bits)

    newinternalMMAC := internalMMAC
    newinternalMMEN := internalMMEN
    newinternalMMEX := internalMMEX

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      val reactivation = Bool()
      reactivation := False

      val isExit = (internalMMAC === 1) && (pc === internalMMEX)

      val issueStage = pipeline.issuePipeline.stages.last

      when(mimicry.isAJump(issueStage) || mimicry.isABranch(issueStage)) {
        when((internalMMAC === 0) || isExit) {
          pendingActivating.push(newestIndex.value)
          reactivation := True

          newinternalMMEN := pc
          newinternalMMEX := nextPc // this is pc + 4
          newinternalMMAC := 1
        } otherwise {
          dummyPendingActivating.push(newestIndex.value)
        }
      }

      // 2) Is the current program counter registered as the entry address?
      when(pc === internalMMEN && internalMMAC > 0) {
        newinternalMMAC := internalMMAC + 1
      }

      // 3) Is the current program counter registered as the exit address?
      when(!reactivation) {
        when(pc === internalMMEX && internalMMAC > 0) {
          newinternalMMAC := internalMMAC - 1
        }
      }

      // 4) Do we need to mimic the execution?
      when(mimicry.isMimic(issueStage)) {
        mimicDependency.push(activatingShadow.payload)
      }

      when(mimicry.isGhost(issueStage)) {
        when((internalMMAC === 0) || isExit) {
          mimicDependency.push(activatingShadow.payload)
        }
      } elsewhen (!mimicry.isPersistent(issueStage)) {
        when((internalMMAC > 0) && (!isExit)) {
          mimicDependency.push(activatingShadow.payload)
        }
      }

      internalMMAC := newinternalMMAC
      internalMMEN := newinternalMMEN
      internalMMEX := newinternalMMEX
      pushedEntry.mimicDependency := mimicDependency
    }

    (newestIndex.value, mimicDependency, (newinternalMMAC, newinternalMMEN, newinternalMMEX))
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).hasValue := True
    robEntries(cdbMessage.robIndex).registerMap
      .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
  }

  def findRegisterValue(regId: UInt): (Bool, Flow[CdbMessage], Flow[CdbMessage]) = {
    val found = Bool()
    found := False

    val target = Flow(CdbMessage(metaRegisters, indexBits))
    target.valid := False
    target.payload.robIndex := 0
    target.payload.writeValue := 0

    val mimicTarget = Flow(CdbMessage(metaRegisters, indexBits))
    mimicTarget.valid := False
    mimicTarget.payload.robIndex := 0
    mimicTarget.payload.writeValue := 0

    // loop through valid values and return the freshest if present
    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized
      val entry = robEntries(absolute)
      val registerType =
        entry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]])

      // last condition: prevent dependencies on x0
      when(
        isValidAbsoluteIndex(absolute)
          && entry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) === regId
          && regId =/= U(0)
          && (registerType === RegisterType.GPR || registerType === MIMIC_GPR)
      ) {
        when(robEntries(absolute).mimicDependency.valid) {
          mimicTarget.valid := !entry.hasValue // We want to track if it is hasn't finished executing yet
          mimicTarget.robIndex := absolute
          mimicTarget.writeValue := entry.registerMap.elementAs[UInt](
            pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]
          )
        } otherwise {
          found := True
          target.valid := entry.hasValue
          target.robIndex := absolute
          target.writeValue := entry.registerMap.elementAs[UInt](
            pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]
          )
        }
      }
    }
    (found, target, mimicTarget)
  }

  def hasMimicryDependency(regId: UInt, dependency: Flow[UInt]): Flow[UInt] = {
    val result = Flow(UInt(indexBits))
    result.setIdle()

    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized

      val entry = robEntries(absolute)

      val sameTarget = entry.registerMap.elementAs[UInt](
        pipeline.data.RD.asInstanceOf[PipelineData[Data]]
      ) === regId
      val differentMimicryContext = (entry.mimicDependency.valid =/= dependency.valid) || (entry.mimicDependency.valid && dependency.valid && entry.mimicDependency.payload =/= dependency.payload)
      val isInProgress = !entry.ready

      val registerType =
        entry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]])

      when(
        isValidAbsoluteIndex(absolute)
          && sameTarget
          && differentMimicryContext
          && isInProgress
          && (registerType === RegisterType.GPR || registerType === MIMIC_GPR)
      ) {
        result.push(absolute)
      }
    }
    result
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

    robEntries(rdbMessage.robIndex).ready := True
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

    when(!isEmpty && oldestEntry.ready) {
      ret.arbitration.isValid := True

      when(pendingActivating.valid && pendingActivating.payload === oldestIndex) {
        when(pipeline.service[JumpService].jumpOfBundle(oldestEntry.registerMap)) {
          // mimicry mode activated
        } otherwise {
          internalMMAC := 0 // acCsr.read()
          internalMMEN := enCsr.read()
          internalMMEX := exCsr.read()
          pipeline.serviceOption[MimicryService].foreach { mimicry =>
            mimicry.inputMeta(ret, 0, enCsr.read(), exCsr.read())
          }
        }
        pendingActivating.setIdle()
//        }
      }

      when(dummyPendingActivating.valid && dummyPendingActivating.payload === oldestIndex) {
        dummyPendingActivating.setIdle()
      }

      pipeline.serviceOption[MimicryService].foreach { mimicry =>
        when(mimicry.isCtBranchOfBundle(oldestEntry.registerMap)) {
          pipeline.service[FetchService].flushCache(ret)
        }
      }
    }

    when(!isEmpty && oldestEntry.ready && ret.arbitration.isDone) {
      // removing the oldest entry
      updatedOldestIndex := oldestIndex.valueNext
      oldestIndex.increment()
      willRetire := True
      isFullNext := False
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
