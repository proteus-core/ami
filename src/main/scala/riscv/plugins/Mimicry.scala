package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave

class Mimicry(exeStage: Stage) extends Plugin[Pipeline] {

  override def getImplementedExtensions = Seq('X')

  private val CSR_MMSTAT        = 0x7FF          // CSR identifier
  private val CSR_MMSTAT_DEPTH  = (15 downto 0)  // activation nesting level
  private val CSR_MMSTAT_PDEPTH = (31 downto 16) // previous nesting level

  private val CSR_MMENTRY      = 0x7DF         // CSR identifier
  private val CSR_MMEXIT       = 0x7EF         // CSR identifier
  private val CSR_MMADDR_NONE  = 0x7FFFFFFF    // TODO: which value?

  def isJump(ir: UInt): Bool = {
    (ir === Opcodes.JAL)  ||
    (ir === Opcodes.JALR)
  }

  def isConditional(ir: UInt): Bool = {
    (ir === Opcodes.BEQ)  ||
    (ir === Opcodes.BNE)  ||
    (ir === Opcodes.BLT)  ||
    (ir === Opcodes.BGE)  ||
    (ir === Opcodes.BLTU) ||
    (ir === Opcodes.BGEU)
  }

  // TODO: Prefix with MM
  private object Data {
    // Mimic execution
    object GHOST extends PipelineData(Bool())      // Ghost instruction
    object MIMIC extends PipelineData(Bool())      // Always mimic
    object PERSISTENT extends PipelineData(Bool()) // Always execute

    // Activating control-flow
    object AJUMP extends PipelineData(Bool())         // Activating jump
    object ABRANCH extends PipelineData(Bool())       // Activating branch
    object OUTCOME extends PipelineData(Bool())       // Branch outcome

    // CSR data
    object MMEXIT extends PipelineData(UInt(32 bits)) // Mimicry exit address
    object MMENTRY extends PipelineData(UInt(32 bits)) // Mimicry entry address
    object MMSTAT_DEPTH extends PipelineData(UInt(16 bits))
    object MM_WRITE_DEPTH extends PipelineData(Bool()) // Update depth?
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

  // Mimicry mode status information
  private class MmStat(implicit config: Config) extends Csr {
    val depth  = Reg(UInt(CSR_MMSTAT_DEPTH.length bits)).init(0)
    val pdepth = Reg(UInt(CSR_MMSTAT_DEPTH.length bits)).init(0)

    val mmstat = pdepth ## depth

    override def read(): UInt = mmstat.asUInt

    override def write(value: UInt): Unit = {
      depth := value(CSR_MMSTAT_DEPTH)
      pdepth := value(CSR_MMSTAT_PDEPTH)
    }
  }

  override def setup(): Unit = {

    val csrService = pipeline.service[CsrService]
    csrService.registerCsr(CSR_MMSTAT, new MmStat)
    csrService.registerCsr(CSR_MMENTRY, new MmEntry)
    csrService.registerCsr(CSR_MMEXIT , new MmExit)

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(Map(
        // Mimic execution
        Data.GHOST      -> False,
        Data.MIMIC      -> False,
        Data.PERSISTENT -> False,

        // Activating control-flow
        Data.AJUMP   -> False,
        Data.ABRANCH -> False,
        Data.OUTCOME -> False,

        // CSR updates
        Data.MM_WRITE_DEPTH  -> False,
        Data.MM_WRITE_BOUNDS -> False
      ))

      val stage = pipeline.retirementStage

      config.setIrMapper((stage, ir) => {
        var result = ir | 3

        when (ir =/= 0) {
          switch (ir(1 downto 0)) {
            is(0) {
              stage.output(Data.GHOST) := True
            }
            is(1) {
              when (isJump(result)) {
                stage.output(Data.AJUMP) := True
              } elsewhen (isConditional(result)) {
                stage.output(Data.ABRANCH) := True
              } otherwise {
                stage.output(Data.MIMIC) := True
              }
            }
            is(2) {
              stage.output(Data.PERSISTENT) := True
            }
            is(3) {
              when (isJump(result)) {
                // Jumps are persistent by default, i.e., rd is always written
                stage.output(Data.PERSISTENT) := True
              }
            }
          }
        }

        result
      })
    }

    pipeline.service[JumpService].onJump { (stage, _, _, jumpType) =>

      val mmstat = Utils.outsideConditionScope(slave(new CsrIo))
      val mmstatCur = mmstat.read()
      val mmstatNew = UInt(config.xlen bits)
      mmstatNew := mmstatCur

      jumpType match {
        case JumpType.Trap =>
          mmstatNew(CSR_MMSTAT_DEPTH) := U(0)
          mmstatNew(CSR_MMSTAT_PDEPTH) := mmstatCur(CSR_MMSTAT_DEPTH)
          mmstat.write(mmstatNew)
        case JumpType.TrapReturn =>
          mmstatNew(CSR_MMSTAT_DEPTH) := mmstatCur(CSR_MMSTAT_PDEPTH)
          mmstatNew(CSR_MMSTAT_PDEPTH) := U(0)
          mmstat.write(mmstatNew)
        case JumpType.Normal =>
          when (stage.value(Data.ABRANCH)) {
            pipeline.service[JumpService].disableJump(stage)
            // TODO: Get rid of Data.OUTCOME, Data.ABRANCH
            //         (redundant with Data.MMEXIT)
            stage.output(Data.OUTCOME) := True
            stage.output(Data.MMEXIT) := stage.value(pipeline.data.NEXT_PC)
          }
      }

      pipeline plug new Area {
        val csrService = pipeline.service[CsrService]
        mmstat <> csrService.getCsr(CSR_MMSTAT)
      }
    }

    // Since the CSR forwarding logic only accesses these WB output in the finish() phase, they are
    // not automatically routed (because the routing uses the information available *before*
    // finish()). Therefore, we have to manually inform the router that these outputs will be
    // needed.
    pipeline.retirementStage.output(Data.MM_WRITE_BOUNDS)
    pipeline.retirementStage.output(Data.MM_WRITE_DEPTH)
    pipeline.retirementStage.output(Data.MMSTAT_DEPTH)
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

  override def build(): Unit = {
    exeStage plug new Area {
      import exeStage._

      when (arbitration.isRunning) {
        val depth = input(Data.MMSTAT_DEPTH)
        val mmexit = input(Data.MMEXIT)
        val mmentry = input(Data.MMENTRY)
        val pc = input(pipeline.data.PC)
        val reactivation = False

        val isExit = (depth === 1) && (pc === mmexit)

        // 1) Is mimicry mode disabled?
        when ((depth === 0) || isExit) {

          // 1.1) Are we dealing with an activating jump?
          when (value(Data.AJUMP)) {
            reactivation := True

            output(Data.MMENTRY) := pc
            output(Data.MMEXIT) := pc + 4
            output(Data.MM_WRITE_BOUNDS) := True

            output(Data.MMSTAT_DEPTH) := 1
            output(Data.MM_WRITE_DEPTH) := True
          }

          // 1.2) Are we dealing with an activating branch?
          when (value(Data.ABRANCH) && value(Data.OUTCOME)) {
            reactivation := True

            output(Data.MMENTRY) := pc
            // mmexit written in onJump
            output(Data.MM_WRITE_BOUNDS) := True

            output(Data.MMSTAT_DEPTH) := 1
            output(Data.MM_WRITE_DEPTH) := True
          }
        }

        // 2) Is the current program counter registered as the entry address?
        when (pc === mmentry) {
          // TODO: assert depth > 0
          output(Data.MMSTAT_DEPTH) := depth + 1 // Update recursion depth
          output(Data.MM_WRITE_DEPTH) := True
        }

        // 3) Is the current program counter registered as the exit address?
        when (! reactivation) {
          when (pc === mmexit) {
            when (depth === 1) {
              // We are exiting mimicry mode
              output(Data.MMENTRY) := CSR_MMADDR_NONE
              output(Data.MMEXIT) := CSR_MMADDR_NONE
              output(Data.MM_WRITE_BOUNDS) := True
            }

            // TODO: assert depth > 0
            output(Data.MMSTAT_DEPTH) := depth - 1 // Update recursion depth
            output(Data.MM_WRITE_DEPTH) := True
          }
        }

        // 4) Do we need to mimic the execution?
        when (value(Data.MIMIC)) {
          output(pipeline.data.RD_TYPE) := RegisterType.NONE
        }

        when (value(Data.GHOST)) {
          when ((depth === 0) || isExit) {
            output(pipeline.data.RD_TYPE) := RegisterType.NONE
          }
        } elsewhen (!value(Data.PERSISTENT)) {
          when ((depth > 0) && (! isExit)) {
            output(pipeline.data.RD_TYPE) := RegisterType.NONE
          }
        }
      }
    }

    val wbStage = pipeline.retirementStage

    val csrArea = wbStage plug new Area {
      import wbStage._

      val mmstat  = slave(new CsrIo)
      val mmentry = slave(new CsrIo)
      val mmexit  = slave(new CsrIo)

      when (arbitration.isRunning) {
        when (value(Data.MM_WRITE_BOUNDS)) {
          mmentry.write(value(Data.MMENTRY))
          mmexit.write(value(Data.MMEXIT))
        }

        when (value(Data.MM_WRITE_DEPTH)) {
          val mstatNew = UInt(32 bits)
          mstatNew := mmstat.read()
          mstatNew(CSR_MMSTAT_DEPTH) := value(Data.MMSTAT_DEPTH)
          mmstat.write(mstatNew)
        }
      }
    }

    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      csrArea.mmstat  <> csrService.getCsr(CSR_MMSTAT)
      csrArea.mmentry <> csrService.getCsr(CSR_MMENTRY)
      csrArea.mmexit  <> csrService.getCsr(CSR_MMEXIT)
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

      val mmstat = readOnlyCsr(CSR_MMSTAT)
      val mmentry = readOnlyCsr(CSR_MMENTRY)
      val mmexit = readOnlyCsr(CSR_MMEXIT)

      // Connect current CSR values to the inputs of exeStage. If there are updated values later in
      // the pipeline, they will be forwarded below.
      exeStage.input(Data.MMSTAT_DEPTH) := mmstat.read()(CSR_MMSTAT_DEPTH)
      exeStage.input(Data.MMENTRY) := mmentry.read()
      exeStage.input(Data.MMEXIT) := mmexit.read()
    }

    pipeline.service[DataHazardService].resolveHazard((stage, nextStages) => {
      // Forward values written to the CSRs from later stages to the exeStage. Since only exeStage
      // reads those values, we don't have to forward to other stages.
      if (stage == exeStage) {
        for (laterStage <- nextStages.reverse) {
          when (laterStage.output(Data.MM_WRITE_DEPTH)) {
            stage.input(Data.MMSTAT_DEPTH) := laterStage.output(Data.MMSTAT_DEPTH)
          }

          when (laterStage.output(Data.MM_WRITE_BOUNDS)) {
            stage.input(Data.MMENTRY) := laterStage.output(Data.MMENTRY)
            stage.input(Data.MMEXIT) := laterStage.output(Data.MMEXIT)
          }
        }
      }

      // We never need to stall.
      False
    })
  }
}
