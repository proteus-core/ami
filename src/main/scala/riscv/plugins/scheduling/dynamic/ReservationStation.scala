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
  val rs1Waiting: Bool = RegInit(False)
  val rs2Waiting: Bool = RegInit(False)
  val pendingActivating: RegisterSource = RegisterSource(indexBits)
  val previousWaw: RegisterSource = RegisterSource(indexBits)

  def build(): Unit = {
    rs1.build()
    rs2.build()
    pendingActivating.build()
    previousWaw.build()
  }

  def reset(): Unit = {
    rs1.reset()
    rs2.reset()
    rs1Waiting := False
    rs2Waiting := False
    pendingActivating.reset()
    previousWaw.reset()
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

  private val wawBuffer: Flow[UInt] = Reg(Flow(UInt(config.xlen bits)))

  private val mimicked = RegInit(False)

  def reset(): Unit = {
    isAvailable := !activeFlush
    stateNext := State.IDLE
    meta.reset()
    wawBuffer.setIdle()
  }

  override def onCdbMessage(cdbMessage: CdbMessage): Unit = {
    val currentRs1Prior, currentRs2Prior, currentPendingActivating, currentWaw = Flow(
      UInt(rob.indexBits)
    )

    when(state === State.WAITING_FOR_ARGS) {
      currentRs1Prior := meta.rs1.priorInstruction
      currentRs2Prior := meta.rs2.priorInstruction
      currentPendingActivating := meta.pendingActivating.priorInstruction
      currentWaw := meta.previousWaw.priorInstruction
    } otherwise {
      currentRs1Prior := meta.rs1.priorInstructionNext
      currentRs2Prior := meta.rs2.priorInstructionNext
      currentPendingActivating := meta.pendingActivating.priorInstructionNext
      currentWaw := meta.previousWaw.priorInstructionNext
    }

    when(state === State.WAITING_FOR_ARGS || stateNext === State.WAITING_FOR_ARGS) {
      val r1w = Bool()
      r1w := currentRs1Prior.valid
      val r2w = Bool()
      r2w := currentRs2Prior.valid
      val paw = Bool()
      paw := currentPendingActivating.valid
      val waw = Bool()
      waw := currentWaw.valid

      when(currentRs1Prior.valid && cdbMessage.robIndex === currentRs1Prior.payload) {
        when(meta.rs1Waiting && cdbMessage.realUpdate) {
          // if the cdb contains the true register value
          regs.setReg(pipeline.data.RS1_DATA, cdbMessage.writeValue)
          meta.rs1Waiting := False
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
        when(meta.rs2Waiting && cdbMessage.realUpdate) {
          // if the cdb contains the true register value
          regs.setReg(pipeline.data.RS2_DATA, cdbMessage.writeValue)
          meta.rs2Waiting := False
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
        // TODO: check for previousWaw here? assign only for dependent activating branches? even then... forwarded ac somehow? could just do an or, but what if exit inbetween? rob also can't know
        // maybe: get id of oldest activating that could apply, then check the chain for the youngest, and do oldest < youngest?
        when(cdbMessage.activatingTaken) {
          mimicked := !mimicked
        }
        paw := False
      }

      when(currentWaw.valid && cdbMessage.robIndex === currentWaw.payload) {
        when(cdbMessage.realUpdate && !wawBuffer.valid) {
          wawBuffer.push(cdbMessage.writeValue)
        }
        when(cdbMessage.previousWaw.valid) {
          meta.previousWaw.priorInstruction.push(cdbMessage.previousWaw.payload)
        } otherwise {
          meta.previousWaw.priorInstruction.valid := False
          waw := False
        }
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
      when(mimicked) { // TODO: this might have to be a register-combinatorial dual
        regs.setReg(
          pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]],
          MimicryRegisterType.MIMIC_GPR.craft()
        )
      }

      when(mimicked) {
        cdbStream.payload.writeValue := wawBuffer.payload
        cdbStream.payload.realUpdate := wawBuffer.valid
      } otherwise {
        cdbStream.payload.writeValue := exeStage.output(pipeline.data.RD_DATA)
        cdbStream.payload.realUpdate := exeStage.output(pipeline.data.RD_DATA_VALID)
      }

      cdbStream.payload.previousWaw := meta.previousWaw.priorInstruction

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
          when(mimicry.isABranch(exeStage) || mimicry.isAJump(exeStage)) {
            cdbStream.valid := True
            cdbStream.payload.activatingTaken := pipeline
              .service[JumpService]
              .jumpRequested(exeStage)
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
        nextPc := issueStage.output(pipeline.data.PC) + issueStage.output(
          pipeline.data.IMM
        ) // TODO: we still need this, right?
      }
    }

    mimicked := pipeline.service[MimicryService].isGhost(issueStage)

    val (robIndex, mimicDependency, (mmac, mmen, mmex)) = rob.pushEntry(
      issueStage.output(pipeline.data.RD),
      issueStage.output(pipeline.data.RD_TYPE),
      pipeline.service[LsuService].operationOutput(issueStage),
      issueStage.output(pipeline.data.PC),
      nextPc
    )

    // TODO: this whole thing is unnecessary?
//    pipeline.serviceOption[MimicryService] foreach { mimicry =>
//      mimicry.inputMeta(regs, mmac, mmen, mmex)
//      when(mimicDependency.valid) { // TODO: incorrect
//        regs.setReg(
//          pipeline.data.RD_TYPE.asInstanceOf[PipelineData[Data]],
//          MimicryRegisterType.MIMIC_GPR.craft()
//        )
//      }
////      when(mimicry.isActivating(issueStage)) {
////        rob.newActivating(robIndex, nextPc)
////      }
//    }

    robEntryIndex := robIndex

    stateNext := State.EXECUTING
    regs.shift := True

    meta.reset() // TODO: is this needed? reset at the end of the previous anyway?

    when(!pipeline.service[MimicryService].isPersistent(issueStage)) { // TODO: could also be determined in the ROB, not sure which one is better
      meta.pendingActivating.priorInstructionNext.push(0)
    }

    pipeline.serviceOption[MimicryService].foreach { mimicry =>
      when(
        issueStage.output(pipeline.data.RD_TYPE) === RegisterType.GPR || issueStage
          .output(pipeline.data.RD_TYPE) === MimicryRegisterType.MIMIC_GPR
      ) {
        // TODO: how about activating jumps?
        meta.previousWaw.priorInstructionNext := rob
          .findPreviousWaw(issueStage.output(pipeline.data.RD))
        meta.pendingActivating.priorInstructionNext := mimicDependency
//        when(mimicryDep.valid) {
//          meta.mdep_write.priorInstructionNext.push(mimicryDep.payload)
//          stateNext := State.WAITING_FOR_ARGS
//        }
      }
    }

    def dependencySetup(
        metaRs: RegisterSource,
        rsWaiting: Bool,
//        mimicRs: RegisterSource,
        reg: PipelineData[UInt],
        regData: PipelineData[UInt],
        regType: PipelineData[SpinalEnumCraft[RegisterType.type]]
    ): Unit = {
      val rsUsed = issueStage.output(regType) === RegisterType.GPR

      when(rsUsed) {
        val rsReg = issueStage.output(reg)
        val (rsInRob, rsValue) = rob.findRegisterValue(rsReg)

//        when(mimicTarget.valid) {
//          mimicRs.priorInstructionNext.push(mimicTarget.payload.robIndex)
//        }

        when(rsInRob) {
          when(rsValue.valid) {
            regs.setReg(regData, rsValue.payload.writeValue)
            when(rsValue.previousWaw.valid) {
              metaRs.priorInstructionNext.push(rsValue.previousWaw.payload)
            }
          } otherwise {
            metaRs.priorInstructionNext.push(rsValue.payload.robIndex)
            rsWaiting := True // TODO: will this change too late? reg/comb
          }
        } otherwise {
          regs.setReg(regData, issueStage.output(regData))
        }

        when(rsInRob && !rsValue.valid) {
          stateNext := State.WAITING_FOR_ARGS
        }
      }
    }

    dependencySetup(
      meta.rs1,
      meta.rs1Waiting,
//      meta.mdep_rs1,
      pipeline.data.RS1,
      pipeline.data.RS1_DATA,
      pipeline.data.RS1_TYPE
    )
    dependencySetup(
      meta.rs2,
      meta.rs2Waiting,
//      meta.mdep_rs2,
      pipeline.data.RS2,
      pipeline.data.RS2_DATA,
      pipeline.data.RS2_TYPE
    )
  }

  override def pipelineReset(): Unit = {
    activeFlush := True
  }
}
