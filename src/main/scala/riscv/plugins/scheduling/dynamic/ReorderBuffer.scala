package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.{Counter, Flow}

case class RobEntry(retirementRegisters: DynBundle[PipelineData[Data]])(implicit config: Config)
    extends Bundle {
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle
  val ready = Bool()
  val hasValue = Bool()
  val mimicryExit = Flow(UInt(config.xlen bits))

  override def clone(): RobEntry = {
    RobEntry(retirementRegisters)
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

  val robEntries = Vec.fill(capacity)(RegInit(RobEntry(retirementRegisters).getZero))
  val oldestIndex = Counter(capacity)
  val newestIndex = Counter(capacity)
  private val isFullNext = Bool()
  private val isFull = RegNext(isFullNext).init(False)
  private val willRetire = False
  val isAvailable = !isFull || willRetire

  val pushInCycle = Bool()
  pushInCycle := False
  val pushedEntry = RobEntry(retirementRegisters)
  pushedEntry := RobEntry(retirementRegisters).getZero

  val pendingActivating = RegInit(Flow(UInt(indexBits)).setIdle())
  val internalMMAC = RegInit(UInt(config.xlen bits).getZero)
  val internalMMEN = RegInit(UInt(config.xlen bits).getZero)
  val internalMMEX = RegInit(UInt(config.xlen bits).getZero)

  def reset(): Unit = {
    oldestIndex.clear()
    newestIndex.clear()
    isFull := False
  }

  def newActivating(index: UInt, exit: UInt): Unit = {
    when(internalMMAC === 0) {
      pendingActivating.push(index)
      internalMMAC := 1
      internalMMEN := robEntries(index).registerMap
        .elementAs[UInt](pipeline.data.PC.asInstanceOf[PipelineData[Data]])
      internalMMEX := exit
      robEntries(index).mimicryExit.push(exit)
    }
  }

  def waitingForActivating(): Bool = {
    pendingActivating.valid
  }

  def isValidAbsoluteIndex(index: UInt): Bool = {
    val ret = Bool()

    val oldest = UInt(indexBits)
    val newest = UInt(indexBits)
    oldest := oldestIndex.value
    newest := newestIndex.value

    when(index >= capacity) {
      ret := False
    } elsewhen (isFull) {
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

  def previousIndexFor(index: UInt): UInt = {
    val previous = UInt()
    when(index === 0) {
      previous := capacity - 1
    } otherwise {
      previous := index - 1
    }
    previous
  }

  def pushEntry(
      rd: UInt,
      rdType: SpinalEnumCraft[RegisterType.type],
      lsuOperationType: SpinalEnumCraft[LsuOperationType.type],
      pc: UInt
  ): UInt = {
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

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      when(internalMMAC =/= 0) {
        when(pc === internalMMEN) {
          internalMMAC := internalMMAC + 1
        }
        when(pc === internalMMEX) {
          internalMMAC := internalMMAC - 1
        } otherwise {
          pushedEntry.mimicryExit.push(internalMMEX)
        }
      }
    }

    newestIndex.value
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    robEntries(cdbMessage.robIndex).hasValue := True
    robEntries(cdbMessage.robIndex).registerMap
      .element(pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]) := cdbMessage.writeValue
  }

  def findRegisterValue(regId: UInt): (Bool, Flow[CdbMessage]) = {
    val found = Bool()
    val target = Flow(CdbMessage(metaRegisters, indexBits))
    target.valid := False
    target.payload.robIndex := 0
    target.payload.writeValue := 0
    found := False

    // loop through valid values and return the freshest if present
    for (relative <- 0 until capacity) {
      val absolute = UInt(indexBits)
      absolute := absoluteIndexForRelative(relative).resized
      val entry = robEntries(absolute)

      // last condition: prevent dependencies on x0
      when(
        isValidAbsoluteIndex(absolute)
          && entry.registerMap.element(pipeline.data.RD.asInstanceOf[PipelineData[Data]]) === regId
          && regId =/= 0
          && entry.registerMap.element(
            pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
          ) === RegisterType.GPR
      ) {
        found := True
        target.valid := entry.hasValue
        target.robIndex := absolute
        target.writeValue := entry.registerMap.elementAs[UInt](
          pipeline.data.RD_DATA.asInstanceOf[PipelineData[Data]]
        )
      }
    }
    (found, target)
  }

  def hasMimicryDependency(robIndex: UInt, regId: UInt): Flow[UInt] = {
    val result = Flow(UInt(indexBits))
    result.setIdle()

    for (nth <- 0 until capacity) {
      val entry = robEntries(nth)
      val index = UInt(indexBits)
      index := nth

      val sameTarget = entry.registerMap.elementAs[UInt](
        pipeline.data.RD.asInstanceOf[PipelineData[Data]]
      ) === robEntries(robIndex).registerMap
        .elementAs[UInt](pipeline.data.RD.asInstanceOf[PipelineData[Data]])
      val differentMimicryContext = (entry.mimicryExit.valid || robEntries(
        robIndex
      ).mimicryExit.valid) && (entry.mimicryExit.payload =/= robEntries(
        robIndex
      ).mimicryExit.payload)
      val isOlder = relativeIndexForAbsolute(index) < relativeIndexForAbsolute(robIndex)
      val isInProgress = !entry.ready

      when(
        isValidAbsoluteIndex(nth)
          && isOlder
          && sameTarget
          && differentMimicryContext
          && isInProgress
      ) {
        result.push(nth)
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

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      when(pendingActivating.valid && pendingActivating.payload === rdbMessage.robIndex) {
        when(pipeline.service[JumpService].jumpOfBundle(rdbMessage.registerMap)) {
          // mimicry mode activated
        } otherwise {
          robEntries(rdbMessage.robIndex).mimicryExit.setIdle()
          internalMMAC := 0
        }
        pendingActivating.setIdle()
      }
    }

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
