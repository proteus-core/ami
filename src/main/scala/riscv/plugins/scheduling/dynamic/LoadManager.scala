package riscv.plugins.scheduling.dynamic

import riscv._
import riscv.plugins.mimicry.MimicryRegisterType
import spinal.core._
import spinal.lib.{Flow, Stream}

class LoadManager(
    pipeline: Pipeline,
    loadStage: Stage,
    rob: ReorderBuffer,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area
    with Resettable
    with CdbListener {
  val storedMessage: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)
  val outputCache: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)
  val rdbStream: Stream[RdbMessage] = Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))
  )
  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(metaRegisters, rob.indexBits)))
  private val resultCdbMessage = RegInit(CdbMessage(metaRegisters, rob.indexBits).getZero)
  val rdbWaitingNext, cdbWaitingNext = Bool()
  val rdbWaiting: Bool = RegNext(rdbWaitingNext).init(False)
  val cdbWaiting: Bool = RegNext(cdbWaitingNext).init(False)

  private val activeFlush = Bool()

  val previousWaw: RegisterSource = RegisterSource(rob.indexBits)

  val wawBufferNext = Flow(UInt(config.xlen bits))
  val wawBuffer = RegNext(wawBufferNext).init(wawBufferNext.getZero)

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_STORE, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)
  val isAvailable: Bool = Bool()

  val cdbReceive = Bool()
  cdbReceive := False

  val cdbUpdate = Bool()
  cdbUpdate := False

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    cdbReceive := True
    val currentWaw = Flow(UInt(rob.indexBits))
    // we need to keep track of WAW updates here as well
    when(state === State.IDLE || stateNext === State.IDLE) {
      currentWaw := previousWaw.priorInstructionNext
    } otherwise {
      currentWaw := previousWaw.priorInstruction
    }

    when(currentWaw.valid && cdbMessage.robIndex === currentWaw.payload) {
      cdbUpdate := True
      when(cdbMessage.realUpdate && !wawBufferNext.valid) {
        wawBuffer.push(cdbMessage.writeValue)
      }
      when(cdbMessage.previousWaw.valid) {
        previousWaw.priorInstruction.push(cdbMessage.previousWaw.payload)
      } otherwise {
        previousWaw.priorInstruction.valid := False
      }
    }
  }

  def receiveMessage(rdbMessage: RdbMessage): Bool = {
    val ret = Bool()
    ret := False
    when(isAvailable) {
      ret := True
      storedMessage := rdbMessage
      previousWaw.priorInstructionNext := rdbMessage.previousWaw
      wawBufferNext := rdbMessage.wawBuffer
      val address = pipeline.service[LsuService].addressOfBundle(rdbMessage.registerMap)
      when(!rob.hasPendingStoreForEntry(rdbMessage.robIndex, address)) {
        stateNext := State.EXECUTING
      } otherwise {
        stateNext := State.WAITING_FOR_STORE
      }
    }
    ret
  }

  def build(): Unit = {
    stateNext := state
    isAvailable := False
    activeFlush := False

    previousWaw.build()
    wawBufferNext := wawBuffer

    when(state === State.IDLE) {
      isAvailable := !activeFlush
    }

    cdbWaitingNext := cdbWaiting
    rdbWaitingNext := rdbWaiting

    loadStage.arbitration.isStalled := state === State.WAITING_FOR_STORE
    loadStage.arbitration.isValid := state === State.EXECUTING

    // execution was invalidated while running
    when(activeFlush) {
      stateNext := State.IDLE
    }

//    val regType =
//      storedMessage.registerMap.element(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]])
//    when(regType === RegisterType.GPR || regType === MimicryRegisterType.MIMIC_GPR) {
//      resultCdbMessage.previousWaw := rob.findPreviousWawForEntry(storedMessage.robIndex)
//    }

    cdbStream.valid := False
    cdbStream.payload := resultCdbMessage

    for (register <- retirementRegisters.keys) {
      loadStage.input(register) := storedMessage.registerMap.element(register)
    }

    rdbStream.valid := False
    rdbStream.payload := outputCache

    when(state === State.WAITING_FOR_STORE && !activeFlush) {
      val address = pipeline.service[LsuService].addressOfBundle(storedMessage.registerMap)
      when(!rob.hasPendingStoreForEntry(storedMessage.robIndex, address)) {
        state := State.EXECUTING
      }
    }

    when(state === State.EXECUTING && loadStage.arbitration.isDone && !activeFlush) {
      rdbStream.valid := True
      rdbStream.payload.robIndex := storedMessage.robIndex
      for (register <- retirementRegisters.keys) {
        rdbStream.payload.registerMap.element(register) := loadStage.output(register)
      }
      outputCache := rdbStream.payload

      cdbStream.valid := True
      cdbStream.payload.realUpdate := loadStage.output(
        pipeline.data.RD_TYPE
      ) =/= MimicryRegisterType.MIMIC_GPR
      cdbStream.payload.activatingTaken := False
      cdbStream.payload.writeValue := loadStage.output(pipeline.data.RD_DATA)
      cdbStream.payload.robIndex := storedMessage.robIndex

      when(loadStage.output(pipeline.data.RD_TYPE) === MimicryRegisterType.MIMIC_GPR) {
        cdbStream.payload.writeValue := wawBuffer.payload
        cdbStream.payload.realUpdate := wawBuffer.valid
      }

      cdbStream.previousWaw := previousWaw.priorInstruction

      resultCdbMessage := cdbStream.payload

      rdbWaitingNext := !rdbStream.ready
      cdbWaitingNext := !cdbStream.ready

      when(!rdbStream.ready || !cdbStream.ready) {
        stateNext := State.BROADCASTING_RESULT
      } otherwise {
        stateNext := State.IDLE
        isAvailable := True
      }
    }

    when(state === State.BROADCASTING_RESULT && !activeFlush) {
      rdbStream.valid := rdbWaiting
      cdbStream.valid := cdbWaiting

      when(cdbWaiting) {
        cdbStream.payload.previousWaw := previousWaw.priorInstruction
        when(!resultCdbMessage.realUpdate) {
          resultCdbMessage.realUpdate := wawBuffer.valid
          resultCdbMessage.writeValue := wawBuffer.payload

        }
      }

      when(rdbStream.ready) {
        rdbWaitingNext := False
      }
      when(cdbStream.ready) {
        cdbWaitingNext := False
      }
      when((rdbStream.ready || !rdbWaiting) && (cdbStream.ready || !cdbWaiting)) {
        stateNext := State.IDLE
        isAvailable := True
        previousWaw.reset()
      }
    }

    // FIXME this doesn't seem the correct place to do this...
    loadStage.connectOutputDefaults()
    loadStage.connectLastValues()
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
