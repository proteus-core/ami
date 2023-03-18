package riscv.plugins.scheduling.dynamic

import riscv._
import riscv.plugins.mimicry.MimicryRegisterType
import spinal.core._
import spinal.lib.{Flow, Stream}

case class RegisterSource(indexBits: BitCount) extends Bundle {
  val priorInstructionNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorInstruction: Flow[UInt] =
    RegNext(priorInstructionNext).init(priorInstructionNext.getZero)

  val waitingForRealNext: Bool = Bool()
  val waitingForReal: Bool = RegNext(waitingForRealNext).init(False)

  def build(): Unit = {
    priorInstructionNext := priorInstruction
    waitingForRealNext := waitingForReal
  }

  def reset(): Unit = {
    priorInstructionNext.setIdle()
    waitingForRealNext := False
  }
}

case class InstructionDependencies(indexBits: BitCount)(implicit config: Config) extends Bundle {
  val rs1: RegisterSource = RegisterSource(indexBits)
  val rs2: RegisterSource = RegisterSource(indexBits)
  val pendingActivating: RegisterSource = RegisterSource(indexBits)
  val previousWaw: RegisterSource = RegisterSource(indexBits)

  val wawBufferValidNext = Bool()
  val wawBufferValid = RegNext(wawBufferValidNext).init(False)

  val wawBuffer = Reg(UInt(config.xlen bits)).init(0)

  val mimicryModeNext = Bool()
  val mimicryMode = RegNext(mimicryModeNext).init(False)

  def build(): Unit = {
    rs1.build()
    rs2.build()
    pendingActivating.build()
    previousWaw.build()

    wawBufferValidNext := wawBufferValid
    mimicryModeNext := mimicryMode
  }

  def reset(): Unit = {
    rs1.reset()
    rs2.reset()
    pendingActivating.reset()
    previousWaw.reset()

    wawBufferValidNext := False
    mimicryModeNext := False
  }
}

class ReservationStation(
    exeStage: Stage,
    rob: ReorderBuffer,
    pipeline: DynamicPipeline,
    retirementRegisters: DynBundle[PipelineData[Data]],
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area
    with CdbListener
    with Resettable {
  setPartialName(s"RS_${exeStage.stageName}")

  private val meta = InstructionDependencies(rob.indexBits)

  private val robEntryIndex = Reg(UInt(rob.indexBits)).init(0)

  private object State extends SpinalEnum {
    val IDLE, WAITING_FOR_ARGS, EXECUTING, BROADCASTING_RESULT = newElement()
  }

  private val stateNext = State()
  private val state = RegNext(stateNext).init(State.IDLE)

  private val cdbWaitingNext, dispatchWaitingNext = Bool()
  private val cdbWaiting = RegNext(cdbWaitingNext).init(False)
  private val dispatchWaiting = RegNext(dispatchWaitingNext).init(False)

  private val resultCdbMessage = RegInit(CdbMessage(metaRegisters, rob.indexBits).getZero)
  private val resultDispatchMessage = RegInit(
    RdbMessage(retirementRegisters, rob.indexBits).getZero
  )

  val cdbStream: Stream[CdbMessage] = Stream(HardType(CdbMessage(metaRegisters, rob.indexBits)))
  val dispatchStream: Stream[RdbMessage] = Stream(
    HardType(RdbMessage(retirementRegisters, rob.indexBits))
  )

  private val regs = pipeline.pipelineRegs(exeStage)

  val isAvailable: Bool = Bool()

  val activeFlush: Bool = Bool()

  val rs1CdbUpdate = Bool()
  rs1CdbUpdate := False

  val rs2CdbUpdate = Bool()
  rs2CdbUpdate := False

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
    meta.reset()
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Prior, currentRs2Prior, currentPendingActivating, currentWaw = Flow(
      UInt(rob.indexBits)
    )

    val currentRs1Waiting, currentRs2Waiting = Bool()

    // Wait with execution until RS1, RS2 chains and dependent activating are done (onCdbMessage)
    // Update values on the first real update, wait for the rest while throwing away the output
    // Wait until dependent activating instruction is done, update AC, EN, EX from it
    // Don’t wait for previous WAW, but keep track of it resolving/changing and propagate it on CDB

    when(state === State.WAITING_FOR_ARGS) {
      currentRs1Prior := meta.rs1.priorInstruction
      currentRs1Waiting := meta.rs1.waitingForReal
      currentRs2Prior := meta.rs2.priorInstruction
      currentRs2Waiting := meta.rs2.waitingForReal
      currentPendingActivating := meta.pendingActivating.priorInstruction
      currentWaw := meta.previousWaw.priorInstruction
    } otherwise {
      currentRs1Prior := meta.rs1.priorInstructionNext
      currentRs1Waiting := meta.rs1.waitingForRealNext
      currentRs2Prior := meta.rs2.priorInstructionNext
      currentRs2Waiting := meta.rs2.waitingForRealNext
      currentPendingActivating := meta.pendingActivating.priorInstructionNext
      currentWaw := meta.previousWaw.priorInstructionNext
    }

    when(currentWaw.valid && cdbMessage.robIndex === currentWaw.payload) {
      when(cdbMessage.realUpdate && !meta.wawBufferValidNext) {
        meta.wawBuffer := cdbMessage.writeValue
        meta.wawBufferValid := True
      }
      when(cdbMessage.previousWaw.valid) {
        meta.previousWaw.priorInstruction.push(cdbMessage.previousWaw.payload)
      } otherwise {
        meta.previousWaw.priorInstruction.valid := False
      }
    }

    when(state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Prior.valid
      val r2w = Bool()
      r2w := currentRs2Prior.valid
      val paw = Bool()
      paw := currentPendingActivating.valid

      when(currentRs1Prior.valid && cdbMessage.robIndex === currentRs1Prior.payload) {
        when(currentRs1Waiting && cdbMessage.realUpdate) {
          // if the cdb contains the true register value
          regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
          rs1CdbUpdate := True
          meta.rs1.waitingForReal := False
        }
        when(cdbMessage.previousWaw.valid) {
          // if we still have to wait for the true value
          meta.rs1.priorInstruction.push(cdbMessage.previousWaw.payload)
        } otherwise {
          meta.rs1.priorInstruction.valid := False
          r1w := False
        }
      }

      when(currentRs2Prior.valid && cdbMessage.robIndex === currentRs2Prior.payload) {
        when(currentRs2Waiting && cdbMessage.realUpdate) {
          // if the cdb contains the true register value
          regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
          rs2CdbUpdate := True
          meta.rs2.waitingForReal := False
        }
        when(cdbMessage.previousWaw.valid) {
          // if we still have to wait for the true value
          meta.rs2.priorInstruction.push(cdbMessage.previousWaw.payload)
        } otherwise {
          meta.rs2.priorInstruction.valid := False
          r2w := False
        }
      }

      when(
        currentPendingActivating.valid && cdbMessage.robIndex === currentPendingActivating.payload
      ) {
        meta.pendingActivating.priorInstruction.valid := False
        paw := False
        meta.mimicryMode := cdbMessage.activatingTaken
      }

      when(!r1w && !r2w && !paw) { // we don't have to wait for all waws to resolve here
        // This is the only place where state is written directly (instead of
        // via stateNext). This ensures that we have priority over whatever
        // execute() writes to it which means that the order of calling
        // execute() and processUpdate() doesn't matter. We need this priority
        // to ensure we start executing when execute() is called in the same
        // cycle as (one of) its arguments arrive(s) via processUpdate().
        state := State.EXECUTING
      }
    }
  }

  def build(): Unit = {
    meta.build()

    cdbWaitingNext := cdbWaiting
    dispatchWaitingNext := dispatchWaiting

    stateNext := state

    dispatchStream.valid := False
    dispatchStream.payload := resultDispatchMessage

    cdbStream.valid := False
    cdbStream.payload := resultCdbMessage

    regs.shift := False

    exeStage.arbitration.isStalled := state === State.WAITING_FOR_ARGS
    exeStage.arbitration.isValid :=
      (state === State.WAITING_FOR_ARGS) || (state === State.EXECUTING)

    isAvailable := False

    when(state === State.IDLE) {
      isAvailable := !activeFlush
    }

    activeFlush := False

    // execution was invalidated while running
    when(activeFlush) {
      reset()
    }

    // when waiting for the result, and it is ready, put in on the bus
    when(state === State.EXECUTING && exeStage.arbitration.isDone && !activeFlush) {
      val realUpdate = Bool()

      pipeline.serviceOption[MimicryService].foreach { mimicry =>
        {
          realUpdate :=
            !(meta.mimicryMode ^ mimicry.isGhost(exeStage)) | mimicry.isPersistent(exeStage)

          when(!realUpdate) {
            cdbStream.payload.writeValue := meta.wawBuffer
            cdbStream.payload.realUpdate := meta.wawBufferValid
          } otherwise {
            cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)
            cdbStream.payload.realUpdate := exeStage.output(pipeline.data.RD_DATA_VALID)
          }
        }
      }

      // Based on AC (either directly obtained or through a CDB update), set output register to MIMIC_GPR before it is propagated on CDB or RDB (build?)

      cdbStream.payload.activatingTaken := False
      cdbStream.payload.previousWaw := meta.previousWaw.priorInstruction
      dispatchStream.payload.previousWaw := meta.previousWaw.priorInstruction
      dispatchStream.payload.wawBuffer.valid := meta.wawBufferValid
      dispatchStream.payload.wawBuffer.payload := meta.wawBuffer

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex

      for (register <- retirementRegisters.keys) {
        dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
      }

      when(exeStage.output(pipeline.data.RD_DATA_VALID)) {
        cdbStream.valid := True
      }

      pipeline.serviceOption[MimicryService].foreach { mimicry =>
        {
          when(mimicry.isActivating(exeStage)) {
            val taken = pipeline.service[JumpService].jumpRequested(exeStage) | meta.mimicryMode
            cdbStream.valid := True
            cdbStream.payload.activatingTaken := taken
            pipeline.service[JumpService].jumpOfBundle(dispatchStream.payload.registerMap) := taken
          }
          when(!realUpdate && exeStage.output(pipeline.data.RD_TYPE) === RegisterType.GPR) {
            dispatchStream.payload.registerMap.element(
              pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]]
            ) := MimicryRegisterType.MIMIC_GPR
          }
        }
      }

      dispatchStream.valid := True

      // Override the assignment of resultCdbMessage to make sure data can be sent in later cycles
      // in case of contention in this cycle
      resultCdbMessage := cdbStream.payload
      resultDispatchMessage := dispatchStream.payload

      cdbWaitingNext := (!cdbStream.ready && cdbStream.valid)
      dispatchWaitingNext := !dispatchStream.ready

      when((cdbStream.ready || !cdbStream.valid) && dispatchStream.ready) {
        reset()
      } otherwise {
        stateNext := State.BROADCASTING_RESULT
      }
    }

    // if the result is on the buses and it has been acknowledged, make the RS
    // available again
    when(state === State.BROADCASTING_RESULT && !activeFlush) {
      // keep WAW up to date
      when(cdbWaiting) {
        cdbStream.payload.previousWaw := meta.previousWaw.priorInstruction
        when(!resultCdbMessage.realUpdate) {
          resultCdbMessage.realUpdate := meta.wawBufferValid
          resultCdbMessage.writeValue := meta.wawBuffer

        }
      }

      when(dispatchWaiting) {
        dispatchStream.payload.previousWaw := meta.previousWaw.priorInstruction
        dispatchStream.payload.wawBuffer.valid := meta.wawBufferValid
        dispatchStream.payload.wawBuffer.payload := meta.wawBuffer
      }

      cdbStream.valid := cdbWaiting
      dispatchStream.valid := dispatchWaiting

      when(cdbStream.ready && cdbWaiting) {
        cdbWaitingNext := False
      }

      when(dispatchStream.ready && dispatchWaiting) {
        dispatchWaitingNext := False
      }

      when((cdbStream.ready || !cdbWaiting) && (dispatchStream.ready || !dispatchWaiting)) {
        reset()
      }
    }
  }

  def execute(): Unit = {
    val (robIndex, entryMeta, mimicDependency, mmMode) = rob.pushEntry()

    meta.mimicryModeNext := mmMode

    robEntryIndex := robIndex

    stateNext := State.EXECUTING
    regs.shift := True

    // TODO: possible optimization to enable this if, but need to be careful with MM** updates
//    when(!pipeline.service[MimicryService].isPersistent(issueStage)) { // could also be determined in the ROB, not sure which one is better
    meta.pendingActivating.priorInstructionNext := mimicDependency
    when(mimicDependency.valid) {
      stateNext := State.WAITING_FOR_ARGS
    }
//    }

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      meta.previousWaw.priorInstructionNext := entryMeta.previousWaw
      when(entryMeta.previousWaw.valid) {
        stateNext := State.WAITING_FOR_ARGS
      }
    }

    def dependencySetup(
        metaRs: RegisterSource,
        rsCdbUpdate: Bool,
        rsData: Flow[RsData],
        regData: PipelineData[UInt]
    ): Unit = {
      when(rsData.valid) {
        when(rsData.payload.updatingInstructionFound) {
          when(rsData.payload.updatingInstructionFinished) {
            when(!rsData.payload.updatingInstructionMimicked) {
              regs.setReg(regData, rsData.payload.updatingInstructionValue)
            } otherwise {
              metaRs.waitingForRealNext := True
              // TODO: move this out?
              when(rsData.payload.previousValid.valid && !rsCdbUpdate) {
                regs.setReg(regData, rsData.payload.previousValid.payload)
              }
            }
            when(rsData.payload.previousWaw.valid) {
              metaRs.priorInstructionNext.push(rsData.payload.previousWaw.payload)
            }
          } otherwise {
            metaRs.priorInstructionNext.push(rsData.payload.updatingInstructionIndex)
            metaRs.waitingForRealNext := True
            when(rsData.payload.previousValid.valid && !rsCdbUpdate) {
              regs.setReg(regData, rsData.payload.previousValid.payload)
            }
          }
        }

        when(
          rsData.payload.updatingInstructionFound && !rsData.payload.updatingInstructionFinished
        ) {
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    dependencySetup(meta.rs1, rs1CdbUpdate, entryMeta.rs1Data, pipeline.data.RS1_DATA)
    dependencySetup(meta.rs2, rs2CdbUpdate, entryMeta.rs2Data, pipeline.data.RS2_DATA)
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
