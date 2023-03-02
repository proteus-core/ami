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
  val ready = Bool()
  val hasValue = Bool()
  val shadowActivating = Flow(UInt(indexBits))
  val previousWaw = Flow(UInt(indexBits))
  val pendingExitAddress = Flow(UInt(config.xlen bits))

  // temporary
  val mmac = UInt(config.xlen bits)
  val mmen = UInt(config.xlen bits)
  val mmex = UInt(config.xlen bits)

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

  private val acCsr = readOnlyCsr(CSR_MMAC)
  private val enCsr = readOnlyCsr(CSR_MMENTRY)
  private val exCsr = readOnlyCsr(CSR_MMEXIT)

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    isFull := False
    when(
      robEntries(oldestIndex).ready && pipeline
        .service[JumpService]
        .jumpOfBundle(robEntries(oldestIndex).registerMap)
    ) {
      pipeline.serviceOption[MimicryService].foreach { mimicry =>
        // could also take it from stage output i guess
//        internalMMAC := mimicry.acOfBundle(robEntries(oldestIndex).registerMap)
//        internalMMEN := mimicry.enOfBundle(robEntries(oldestIndex).registerMap)
//        internalMMEX := mimicry.exOfBundle(robEntries(oldestIndex).registerMap)
      }
    } otherwise {
//      internalMMAC := acCsr.read()
//      internalMMEN := enCsr.read()
//      internalMMEX := exCsr.read()
    }
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

  def currentMMAC(): UInt = {
    val mmac = UInt(config.xlen bits)
    when(isValidAbsoluteIndex(newestIndex)) {
      mmac := robEntries(newestIndex).mmac
    } otherwise {
      mmac := acCsr.read()
    }
    mmac
  }

  def currentMMEN(): UInt = {
    val mmen = UInt(config.xlen bits)
    when(isValidAbsoluteIndex(newestIndex)) {
      mmen := robEntries(newestIndex).mmen
    } otherwise {
      mmen := acCsr.read()
    }
    mmen
  }

  def currentMMEX(): UInt = {
    val mmex = UInt(config.xlen bits)
    mmex := acCsr.read()

    when(isValidAbsoluteIndex(newestIndex)) {
      val previous = robEntries(newestIndex)
      when(previous.pendingExitAddress.valid) {
        mmex := previous.pendingExitAddress.payload
      } otherwise {
        when(previous.shadowActivating.valid) {
          robEntries(previous.shadowActivating.payload).pendingExitAddress
        }
      }
    }
    mmex
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

    val issueStage = pipeline.issuePipeline.stages.last

    val mimicDependency = Flow(UInt(indexBits))
    mimicDependency.setIdle()

    val cMMAC: UInt = currentMMAC()
    val cMMEN: UInt = currentMMEN()
    val cMMEX: UInt = currentMMEX()

    pushedEntry.mmac := cMMAC
    pushedEntry.mmen := cMMEN
    pushedEntry.mmex := cMMEX

    // When instructions are inserted into the ROB (pushEntry):

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      {

        // If the instruction matches the exit address of the shadowActivating of the last valid instruction (or if it doesn't exist, csrEX),
        //   look for the previous activating branch and set that as shadowActivating

        val previousShadow = Flow(UInt(indexBits))
        previousShadow.setIdle()

        when(isValidAbsoluteIndex(newestIndex)) {
          // Copy the previous shadowActivating
          previousShadow := robEntries(newestIndex).shadowActivating
        }

        when(pc === cMMEX) {
          when(previousShadow.valid) {
            pushedEntry.shadowActivating := findPreviousActiveActivating(previousShadow.payload)
          }
        } otherwise {
          // Copy the previous shadowActivating
          pushedEntry.shadowActivating := previousShadow
        }

        when(mimicry.isActivating(issueStage)) {
          // If the instruction is activating, set pendingExitAddress to its nextPc (and set shadowActivating to itself?)
          pushedEntry.pendingExitAddress.push(nextPc)
        }
      }
    }

    (newestIndex.value, mimicDependency, (cMMAC, cMMEN, cMMEX))
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).hasValue := True
    robEntries(cdbMessage.robIndex).registerMap
      .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue

    // When instructions are updated in the ROB after execution (onCdbMessage?):
    // Update MM** registers of the given instruction if it had a shadowing activating (even if not?)
    robEntries(cdbMessage.robIndex).mmac := cdbMessage.mmac
    robEntries(cdbMessage.robIndex).mmen := cdbMessage.mmen
    robEntries(cdbMessage.robIndex).mmex := cdbMessage.mmex
    robEntries(cdbMessage.robIndex).previousWaw := cdbMessage.previousWaw

    // TODO: what about branches?
    when(cdbMessage.realUpdate) {
      robEntries(cdbMessage.robIndex).registerMap
        .element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := RegisterType.GPR
    } otherwise {
      robEntries(cdbMessage.robIndex).registerMap
        .element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]) := MIMIC_GPR
    }
  }

  private def findPreviousActiveActivating(previousShadow: UInt): Flow[UInt] = {
    val result = Flow(UInt(indexBits))
    result.setIdle()

    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized

      val entry = robEntries(absolute)

      when(
        isValidAbsoluteIndex(
          absolute
        ) && entry.pendingExitAddress.valid && entry.pendingExitAddress.payload =/= previousShadow
      ) {
        result.push(absolute)
      }
    }
    result
  }

  def findRegisterValue(regId: UInt): (Bool, Flow[CdbMessage]) = {
    val found = Bool()
    found := False

    val target = Flow(CdbMessage(metaRegisters, indexBits)) // TODO: refactor this to not use cdb?
    target.valid := False
    target.realUpdate := False
    target.payload.robIndex := 0
    target.payload.writeValue := 0
    target.previousWaw.setIdle()

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
        found := True
        target.valid := entry.hasValue
        target.robIndex := absolute
        target.previousWaw := entry.previousWaw
        target.writeValue := entry.registerMap.elementAs[UInt](
          pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]
        )
        target.realUpdate := registerType === RegisterType.GPR
      }
    }
    (found, target)
  }

  // TODO: wrapper function for these lookups? for (all) { condition(target, iter) ... }

  def findPreviousWaw(regId: UInt): Flow[UInt] = {
    val result = Flow(UInt(indexBits))
    result.setIdle()

    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized

      val entry = robEntries(absolute)

      val sameTarget = entry.registerMap.elementAs[UInt](
        pipeline.data.RD.asInstanceOf[PipelineData[Data]]
      ) === regId
      val isInProgress = !entry.ready

      val registerType =
        entry.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]])

      when(
        isValidAbsoluteIndex(absolute)
          && sameTarget
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

      when(pipeline.service[JumpService].jumpOfBundle(oldestEntry.registerMap)) {
        // mimicry mode activated
      } otherwise {
        pipeline.serviceOption[MimicryService].foreach { mimicry =>
          mimicry.inputMeta(ret, 0, enCsr.read(), exCsr.read())
        }
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
