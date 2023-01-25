package riscv.plugins.scheduling.dynamic

import riscv._
import riscv.plugins.mimicry.MimicryRegisterType
import spinal.core._
import spinal.lib.{Flow, Stream}

case class RegisterSource(indexBits: BitCount) extends Bundle {
  val priorInstructionNext: Flow[UInt] = Flow(UInt(indexBits))
  val priorInstruction: Flow[UInt] =
    RegNext(priorInstructionNext).init(priorInstructionNext.getZero)

  def build(): Unit = {
    priorInstructionNext := priorInstruction
  }

  def reset(): Unit = {
    priorInstructionNext.setIdle()
  }
}

case class InstructionDependencies(indexBits: BitCount) extends Bundle {
  val rs1: RegisterSource = RegisterSource(indexBits)
  val rs2: RegisterSource = RegisterSource(indexBits)
  val mdep_rs1: RegisterSource = RegisterSource(indexBits)
  val mdep_rs2: RegisterSource = RegisterSource(indexBits)
  val mdep_write: RegisterSource = RegisterSource(indexBits)

  def build(): Unit = {
    rs1.build()
    rs2.build()
    mdep_rs1.build()
    mdep_rs2.build()
    mdep_write.build()
  }

  def reset(): Unit = {
    rs1.reset()
    rs2.reset()
    mdep_rs1.reset()
    mdep_rs2.reset()
    mdep_write.reset()
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

  private val regs = pipeline.pipelineRegs(exeStage) // TODO: do we need this?

  val isAvailable: Bool = Bool()

  val activeFlush: Bool = Bool()

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
    meta.reset()
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Prior, currentRs2Prior, currentM1Prior, currentM2Prior, currentCryPrior = Flow(
      UInt(rob.indexBits)
    )

    when(state === State.WAITING_FOR_ARGS) {
      currentRs1Prior := meta.rs1.priorInstruction
      currentRs2Prior := meta.rs2.priorInstruction
      currentM1Prior := meta.mdep_rs1.priorInstruction
      currentM2Prior := meta.mdep_rs2.priorInstruction
      currentCryPrior := meta.mdep_write.priorInstruction
    } otherwise {
      currentRs1Prior := meta.rs1.priorInstructionNext
      currentRs2Prior := meta.rs2.priorInstructionNext
      currentM1Prior := meta.mdep_rs1.priorInstructionNext
      currentM2Prior := meta.mdep_rs2.priorInstructionNext
      currentCryPrior := meta.mdep_write.priorInstructionNext
    }

    when(state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Prior.valid
      val r2w = Bool()
      r2w := currentRs2Prior.valid
      val m1w = Bool()
      m1w := currentM1Prior.valid
      val m2w = Bool()
      m2w := currentM2Prior.valid
      val cyw = Bool()
      cyw := currentCryPrior.valid

      when(currentRs1Prior.valid && cdbMessage.robIndex === currentRs1Prior.payload) {
        meta.rs1.priorInstruction.valid := False
        r1w := False
        regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
      }

      when(currentRs2Prior.valid && cdbMessage.robIndex === currentRs2Prior.payload) {
        meta.rs2.priorInstruction.valid := False
        r2w := False
        regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
      }

      when(currentM1Prior.valid && cdbMessage.robIndex === currentM1Prior.payload) {
        meta.mdep_rs1.priorInstruction.valid := False
        m1w := False
      }

      when(currentM2Prior.valid && cdbMessage.robIndex === currentM2Prior.payload) {
        meta.mdep_rs2.priorInstruction.valid := False
        m1w := False
      }

      when(currentCryPrior.valid && cdbMessage.robIndex === currentCryPrior.payload) {
        meta.mdep_write.priorInstruction.valid := False
        cyw := False
      }

      when(!r1w && !r2w && !m1w && !m2w && !cyw) {
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
      cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)

      cdbStream.payload.robIndex := robEntryIndex
      dispatchStream.payload.robIndex := robEntryIndex

      for (register <- retirementRegisters.keys) {
        dispatchStream.payload.registerMap.element(register) := exeStage.output(register)
      }

      when(exeStage.output(pipeline.data.RD_DATA_VALID)) {
        cdbStream.valid := True
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
    val issueStage = pipeline.issuePipeline.stages.last

    val nextPc = UInt()
    nextPc := issueStage.output(pipeline.data.NEXT_PC)

    // calculate next pc for activating jumps here
    pipeline.serviceOption[MimicryService] foreach { mimicry =>
      when(mimicry.isABranch(issueStage)) {
        nextPc := issueStage.output(pipeline.data.PC) + issueStage.output(pipeline.data.IMM)
      }
    }

    val (robIndex, mimicked, (mmac, mmen, mmex)) = rob.pushEntry(
      issueStage.output(pipeline.data.RD),
      issueStage.output(pipeline.data.RD_TYPE),
      pipeline.service[LsuService].operationOutput(issueStage),
      issueStage.output(pipeline.data.PC),
      nextPc
    )

    pipeline.serviceOption[MimicryService] foreach { mimicry =>
      mimicry.inputMeta(exeStage, mmac, mmen, mmex)
      when(mimicked) {
        regs.setReg(pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]], MimicryRegisterType.MIMIC_GPR.craft())
      }
//      when(mimicry.isActivating(issueStage)) {
//        rob.newActivating(robIndex, nextPc)
//      }
    }

    robEntryIndex := robIndex

    stateNext := State.EXECUTING
    regs.shift := True

    meta.reset()

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      when(issueStage.output(pipeline.data.RD_TYPE) === RegisterType.GPR) {
        val mimicryDep = rob.hasMimicryDependency(robIndex)
        when(mimicryDep.valid) {
          meta.mdep_write.priorInstructionNext.push(mimicryDep.payload)
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    def dependencySetup(
        metaRs: RegisterSource,
        mimicRs: RegisterSource,
        reg: PipelineData[UInt],
        regData: PipelineData[UInt],
        regType: PipelineData[SpinalEnumCraft[RegisterType.type]]
    ): Unit = {
      val rsUsed = issueStage.output(regType) === RegisterType.GPR

      when(rsUsed) {
        val rsReg = issueStage.output(reg)
        val (rsInRob, rsValue, mimicTarget) = rob.findRegisterValue(rsReg)

        when(mimicTarget.valid) {
          mimicRs.priorInstructionNext.push(mimicTarget.payload.robIndex)
        }

        when(rsInRob) {
          when(rsValue.valid) {
            regs.setReg(regData, rsValue.payload.writeValue)
          } otherwise {
            metaRs.priorInstructionNext.push(rsValue.payload.robIndex)
          }
        } otherwise {
          regs.setReg(regData, issueStage.output(regData))
        }

        when((rsInRob && !rsValue.valid) || mimicTarget.valid) {
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    dependencySetup(
      meta.rs1,
      meta.mdep_rs1,
      pipeline.data.RS1,
      pipeline.data.RS1_DATA,
      pipeline.data.RS1_TYPE
    )
    dependencySetup(
      meta.rs2,
      meta.mdep_rs2,
      pipeline.data.RS2,
      pipeline.data.RS2_DATA,
      pipeline.data.RS2_TYPE
    )
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
