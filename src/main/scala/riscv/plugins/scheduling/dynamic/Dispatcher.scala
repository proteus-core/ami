package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib.Stream

class Dispatcher(
    pipeline: DynamicPipeline,
    rob: ReorderBuffer,
    loadManagers: Seq[LoadManager],
    retirementRegisters: DynBundle[PipelineData[Data]]
) extends Area
    with Resettable {
  val rdbStream: Stream[RdbMessage] = Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))
  )
  val storedMessage: RdbMessage = RegInit(RdbMessage(retirementRegisters, rob.indexBits).getZero)

  private object State extends SpinalEnum {
    val IDLE, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)

  private val activeFlush = Bool()

  def processMessage(rdbMessage: RdbMessage): Bool = {
    val ret = Bool()
    ret := False

    val lsu = pipeline.service[LsuService]

    when(lsu.operationOfBundle(rdbMessage.registerMap) === LsuOperationType.LOAD) {
      var context = when(False) {}
      for (lm <- loadManagers) {
        context = context.elsewhen(lm.isAvailable) {
          ret := lm.receiveMessage(rdbMessage)
        }
      }
      context.otherwise {
        ret := False
      }
    } elsewhen (!activeFlush) {
      when(state === State.BROADCASTING_RESULT) {
        ret := False
      } otherwise {
        storedMessage := rdbMessage
        rdbStream.payload := rdbMessage
        rdbStream.valid := True
        when(!rdbStream.ready) {
          stateNext := State.BROADCASTING_RESULT
        }
        ret := True
      }
    } otherwise {
      ret := False
    }
    ret
  }

  def build(): Unit = {
    rdbStream.payload := storedMessage
    rdbStream.valid := False

    stateNext := state

    activeFlush := False

    when(activeFlush) {
      stateNext := State.IDLE
    }

    when(state === State.BROADCASTING_RESULT && !activeFlush) {
      rdbStream.valid := True
      when(rdbStream.ready) {
        stateNext := State.IDLE
      }
    }
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
