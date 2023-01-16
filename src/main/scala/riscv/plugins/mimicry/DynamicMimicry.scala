package riscv.plugins.mimicry

import riscv._
import spinal.core._
import spinal.lib.slave

//object MimicryRegisterType {
//  val MIMIC_GPR = RegisterType.newElement("MIMIC_GPR")
//}

class DynamicMimicry(exeStages: Seq[Stage]) extends Plugin[Pipeline] with MimicryService {

  override def getImplementedExtensions = Seq('X')

  // The mimicry state is stored in CSRs (activation counter, entry address, and
  //  exit address) with the following idendifiers:
  private val CSR_MMAC = 0x7ff
  private val CSR_MMENTRY = 0x7df // CSR identifier
  private val CSR_MMEXIT = 0x7ef // CSR identifier

  private val CSR_MMADDR_NONE = 0x7fffffff // TODO: which value?

  def isJump(ir: UInt): Bool = {
    (ir === Opcodes.JAL) ||
      (ir === Opcodes.JALR)
  }

  def isConditional(ir: UInt): Bool = {
    (ir === Opcodes.BEQ) ||
      (ir === Opcodes.BNE) ||
      (ir === Opcodes.BLT) ||
      (ir === Opcodes.BGE) ||
      (ir === Opcodes.BLTU) ||
      (ir === Opcodes.BGEU)
  }

  // TODO: Prefix with MM
  private object Data {
    // Mimic execution
    object GHOST extends PipelineData(Bool()) // Ghost instruction
    object MIMIC extends PipelineData(Bool()) // Always mimic
    object PERSISTENT extends PipelineData(Bool()) // Always execute

    // Activating control-flow
    object AJUMP extends PipelineData(Bool()) // Activating jump
    object ABRANCH extends PipelineData(Bool()) // Activating branch
    object OUTCOME extends PipelineData(Bool()) // Branch outcome

    // Constant-timeness
    object CTBRANCH extends PipelineData(Bool()) // CT conditional branch

    // CSR data
    object MMEXIT extends PipelineData(UInt(32 bits)) // Mimicry exit address
    object MMENTRY extends PipelineData(UInt(32 bits)) // Mimicry entry address
    object MMAC extends PipelineData(UInt(32 bits)) // Activation counter
    object MM_WRITE_AC extends PipelineData(Bool()) // Update AC?
    object MM_WRITE_BOUNDS extends PipelineData(Bool()) // Update entry/exit?
  }

  // Mimicry mode entry
  private class MmEntry(implicit config: Config) extends Csr {

    val entry = Reg(UInt(config.xlen bits)).init(CSR_MMADDR_NONE)

    override def read(): UInt = entry

    override def write(addr: UInt): Unit = entry := addr
  }

  // Mimicry mode exit
  private class MmExit(implicit config: Config) extends Csr {

    val exit = Reg(UInt(config.xlen bits)).init(CSR_MMADDR_NONE)

    override def read(): UInt = exit

    override def write(addr: UInt): Unit = exit := addr
  }

  // Mimicry mode activation counter (AC)
  private class MmAC(implicit config: Config) extends Csr {
    val AC = Reg(UInt(config.xlen bits)).init(0)

    override def read(): UInt = AC

    override def write(value: UInt): Unit = AC := value
  }

  override def setup(): Unit = {

    val csrService = pipeline.service[CsrService]
    csrService.registerCsr(CSR_MMAC, new MmAC)
    csrService.registerCsr(CSR_MMENTRY, new MmEntry)
    csrService.registerCsr(CSR_MMEXIT, new MmExit)

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          // Mimic execution
          Data.GHOST -> False,
          Data.MIMIC -> False,
          Data.PERSISTENT -> False,

          // Activating control-flow
          Data.AJUMP -> False,
          Data.ABRANCH -> False,
          Data.OUTCOME -> False,

          // Constant-timeness
          Data.CTBRANCH -> False,

          // CSR updates
          Data.MM_WRITE_AC -> False,
          Data.MM_WRITE_BOUNDS -> False
        )
      )

      val stage = pipeline.retirementStage

      config.setIrMapper((stage, ir) => {
        val result = ir | 3
        val conditional = isConditional(result)
        val jump = isJump(result)

        switch(ir(1 downto 0)) {
          is(0) {
            when(conditional) {
              stage.output(Data.CTBRANCH) := True
            } otherwise {
              stage.output(Data.GHOST) := True
            }
          }
          is(1) {
            when(jump) {
              stage.output(Data.AJUMP) := True
            } elsewhen (conditional) {
              stage.output(Data.ABRANCH) := True
            } otherwise {
              stage.output(Data.MIMIC) := True
            }
          }
          is(2) {
            stage.output(Data.PERSISTENT) := True
          }
          is(3) {
            when(jump) {
              // Jumps are persistent by default, i.e., rd is always written
              stage.output(Data.PERSISTENT) := True
            }
          }
        }

        result
      })
    }

    pipeline.service[JumpService].onJump { (stage, _, _, jumpType) =>
      if (jumpType == JumpType.Normal) {
        when(stage.value(Data.ABRANCH)) {
          pipeline.service[JumpService].disableJump(stage)
          // TODO: Get rid of Data.OUTCOME, Data.ABRANCH
          //         (redundant with Data.MMEXIT)
          stage.output(Data.OUTCOME) := True
          stage.output(Data.MMEXIT) := stage.value(pipeline.data.NEXT_PC)
        } otherwise {
          pipeline.service[FetchService].flushCache(stage)
        }
      }
    }

    pipeline.service[BranchService].onBranch { (stage, _, taken) =>
      when(!taken && stage.value(Data.CTBRANCH)) {
        stage.arbitration.jumpRequested := True
        pipeline.service[FetchService].flushCache(stage)
      }
    }

    // Since the CSR forwarding logic only accesses these WB output in the finish() phase, they are
    // not automatically routed (because the routing uses the information available *before*
    // finish()). Therefore, we have to manually inform the router that these outputs will be
    // needed.
    pipeline.retirementStage.output(Data.MM_WRITE_BOUNDS)
    pipeline.retirementStage.output(Data.MM_WRITE_AC)
    pipeline.retirementStage.output(Data.MMAC)
    pipeline.retirementStage.output(Data.MMENTRY)
    pipeline.retirementStage.output(Data.MMEXIT)

    if (config.debug) {
      // Needed for the tests
      pipeline.retirementStage.output(Data.AJUMP)
      pipeline.retirementStage.output(Data.ABRANCH)
      pipeline.retirementStage.output(Data.OUTCOME)
      pipeline.retirementStage.output(Data.GHOST)
      pipeline.retirementStage.output(Data.MIMIC)
      pipeline.retirementStage.output(Data.PERSISTENT)
    }
  }

  override def isActivating(stage: Stage): Bool = {
    stage.output(Data.AJUMP) || stage.output(Data.ABRANCH)
  }

  override def build(): Unit = {
    // TODO: implement
    for (exeStage <- exeStages) {
      exeStage plug new Area {
        import exeStage._

        when(arbitration.isRunning) {
          val AC = input(Data.MMAC)
          val mmexit = input(Data.MMEXIT)
          val mmentry = input(Data.MMENTRY)
          val pc = input(pipeline.data.PC)
          val reactivation = False

          val isExit = (AC === 1) && (pc === mmexit)

          // 1) Is mimicry mode disabled?
          when((AC === 0) || isExit) {

            // 1.1) Are we dealing with an activating jump?
            when(value(Data.AJUMP)) {
              reactivation := True

              output(Data.MMENTRY) := pc
              output(Data.MMEXIT) := input(pipeline.data.NEXT_PC)
              output(Data.MM_WRITE_BOUNDS) := True

              output(Data.MMAC) := 1
              output(Data.MM_WRITE_AC) := True
            }

            // 1.2) Are we dealing with an activating branch?
            when(value(Data.ABRANCH) && value(Data.OUTCOME)) {
              reactivation := True

              output(Data.MMENTRY) := pc
              // mmexit written in onJump
              output(Data.MM_WRITE_BOUNDS) := True

              output(Data.MMAC) := 1
              output(Data.MM_WRITE_AC) := True
            }
          }

          // 2) Is the current program counter registered as the entry address?
          when(pc === mmentry) {
            // TODO: assert AC > 0
            output(Data.MMAC) := AC + 1
            output(Data.MM_WRITE_AC) := True
          }

          // 3) Is the current program counter registered as the exit address?
          when(!reactivation) {
            when(pc === mmexit) {
              when(AC === 1) {
                // We are exiting mimicry mode
                output(Data.MMENTRY) := CSR_MMADDR_NONE
                output(Data.MMEXIT) := CSR_MMADDR_NONE
                output(Data.MM_WRITE_BOUNDS) := True
              }

              // TODO: assert AC > 0
              output(Data.MMAC) := AC - 1
              output(Data.MM_WRITE_AC) := True
            }
          }

          // 4) Do we need to mimic the execution?
          when(value(Data.MIMIC)) {
            output(pipeline.data.RD_TYPE) := MimicryRegisterType.MIMIC_GPR
          }

          when(value(Data.GHOST)) {
            when((AC === 0) || isExit) {
              output(pipeline.data.RD_TYPE) := MimicryRegisterType.MIMIC_GPR
            }
          } elsewhen (!value(Data.PERSISTENT)) {
            when((AC > 0) && (!isExit)) {
              output(pipeline.data.RD_TYPE) := MimicryRegisterType.MIMIC_GPR
            }
          }
        }
      }
    }

    val wbStage = pipeline.retirementStage

    val csrArea = wbStage plug new Area {
      import wbStage._

      val mmAC = slave(new CsrIo)
      val mmentry = slave(new CsrIo)
      val mmexit = slave(new CsrIo)

      when(arbitration.isRunning) {
        when(value(Data.MM_WRITE_BOUNDS)) {
          mmentry.write(value(Data.MMENTRY))
          mmexit.write(value(Data.MMEXIT))
        }

        when(value(Data.MM_WRITE_AC)) {
          mmAC.write(value(Data.MMAC))
        }
      }
    }

    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      csrArea.mmAC <> csrService.getCsr(CSR_MMAC)
      csrArea.mmentry <> csrService.getCsr(CSR_MMENTRY)
      csrArea.mmexit <> csrService.getCsr(CSR_MMEXIT)
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]

      def readOnlyCsr(csrId: Int): CsrIo = {
        val csr = csrService.getCsr(csrId)
        csr.write := False
        csr.wdata.assignDontCare()
        csr
      }

      val mmAC = readOnlyCsr(CSR_MMAC)
      val mmentry = readOnlyCsr(CSR_MMENTRY)
      val mmexit = readOnlyCsr(CSR_MMEXIT)

      // Connect current CSR values to the inputs of exeStage. If there are updated values later in
      // the pipeline, they will be forwarded below.
      //      exeStage.input(Data.MMAC) := mmAC.read()
      //      exeStage.input(Data.MMENTRY) := mmentry.read()
      //      exeStage.input(Data.MMEXIT) := mmexit.read()
    }

    //    pipeline
    //      .service[DataHazardService]
    //      .resolveHazard((stage, nextStages) => {
    //        // Forward values written to the CSRs from later stages to the exeStage. Since only exeStage
    //        // reads those values, we don't have to forward to other stages.
    //        if (stage == exeStage) {
    //          for (laterStage <- nextStages.reverse) {
    //            when(laterStage.output(Data.MM_WRITE_AC)) {
    //              stage.input(Data.MMAC) := laterStage.output(Data.MMAC)
    //            }
    //
    //            when(laterStage.output(Data.MM_WRITE_BOUNDS)) {
    //              stage.input(Data.MMENTRY) := laterStage.output(Data.MMENTRY)
    //              stage.input(Data.MMEXIT) := laterStage.output(Data.MMEXIT)
    //            }
    //          }
    //        }
    //
    //        // We never need to stall.
    //        False
    //      })
    //
    //    pipeline
    //      .service[DataHazardService]
    //      .resolveHazard((stage, nextStages) => {
    //        val stall = False
    //
    //        def mimicStall(
    //                        rsNeeded: Bool,
    //                        rs: PipelineData[UInt],
    //                        rsType: PipelineData[SpinalEnumCraft[RegisterType.type]]
    //                      ): Unit = {
    //          if (stage.hasInput(rs)) {
    //            val readRs = stage.input(rs)
    //
    //            when(rsNeeded && readRs =/= 0) {
    //              val readRegType = stage.input(rsType)
    //              val readMimicGpr = readRegType === MimicryRegisterType.MIMIC_GPR
    //
    //              for (laterStage <- nextStages.reverse) {
    //                val writeRd = laterStage.input(pipeline.data.RD)
    //                val writeRegType = laterStage.input(pipeline.data.RD_TYPE)
    //                val writeNone = writeRegType === RegisterType.NONE
    //                val writeMimicGpr = writeRegType === MimicryRegisterType.MIMIC_GPR
    //
    //                // We only need to check for additional stalls if either the register read or the
    //                // register write is mimicked (or both). Otherwise, we either dealing with normal
    //                // GPR-GPR dependencies which are handled by the data hazard resolver.
    //                val checkStall =
    //                (readRs === writeRd) && !writeNone && (writeMimicGpr || readMimicGpr)
    //
    //                when(laterStage.arbitration.isValid && checkStall) {
    //                  stall := !laterStage.input(pipeline.data.RD_DATA_VALID)
    //                }
    //              }
    //            }
    //          }
    //        }
    //
    //        mimicStall(stage.arbitration.rs1Needed, pipeline.data.RS1, pipeline.data.RS1_TYPE)
    //        mimicStall(stage.arbitration.rs2Needed, pipeline.data.RS2, pipeline.data.RS2_TYPE)
    //
    //        stall
    //      })
  }

}
