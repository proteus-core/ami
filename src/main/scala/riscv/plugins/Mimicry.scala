package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave

class Mimicry() extends Plugin[Pipeline] {

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
    object MMEXIT extends PipelineData(UInt(32 bits)) // Mimicry exit address
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

    val csrService = pipeline.getService[CsrService]
    csrService.registerCsr(CSR_MMSTAT, new MmStat)
    csrService.registerCsr(CSR_MMENTRY, new MmEntry)
    csrService.registerCsr(CSR_MMEXIT , new MmExit)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        // Mimic execution
        Data.GHOST      -> False,
        Data.MIMIC      -> False,
        Data.PERSISTENT -> False,

        // Activating control-flow
        Data.AJUMP   -> False,
        Data.ABRANCH -> False,
        Data.OUTCOME -> False
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
          }
        }

        result
      })
    }

    pipeline.getService[JumpService].onJump { (stage, _, _, jumpType) =>

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
            pipeline.getService[JumpService].disableJump(stage)
            // TODO: Get rid of Data.OUTCOME (redundant with Data.MMEXIT)
            stage.output(Data.OUTCOME) := True
            stage.output(Data.MMEXIT) := stage.value(pipeline.data.NEXT_PC)
          }
      }

      pipeline plug new Area {
        val csrService = pipeline.getService[CsrService]
        mmstat <> csrService.getCsr(CSR_MMSTAT)
      }
    }
  }

  override def build(): Unit = {

    val stage = pipeline.retirementStage

    val mimicryArea = stage plug new Area {
      import stage._

      val mmstat  = slave(new CsrIo)
      val mmentry = slave(new CsrIo)
      val mmexit  = slave(new CsrIo)

      // TODO: check more arbitration logic ?
      when (arbitration.isValid) {

        val mmstatCur = mmstat.read()
        val mmstatNew = UInt(config.xlen bits)
        val depth     = mmstatCur(CSR_MMSTAT_DEPTH)
        val PC        = value(pipeline.data.PC)
        mmstatNew := mmstatCur

        // 1) Is mimicry mode disabled?
        when (depth === 0) {

          // 1.1) Are we dealing with an activating jump?
          when (value(Data.AJUMP)) {
            mmentry.write(PC)
            mmexit.write(PC+4)
            mmstatNew(CSR_MMSTAT_DEPTH) := 1
          }

          // 1.1) Are we dealing with an activating branch?
          when (value(Data.ABRANCH) && value(Data.OUTCOME)) {
            mmentry.write(PC)
            mmexit.write(value(Data.MMEXIT))
            mmstatNew(CSR_MMSTAT_DEPTH) := 1
          }
        }

        // 2) Is the current program counter registered as the entry address?
        when (PC === mmentry.read()) {
          // TODO: assert depth > 0
          mmstatNew(CSR_MMSTAT_DEPTH) := depth + 1 // Update recursion depth
        }

        val isExit = (depth === 1) && (PC === mmexit.read())

        // 3) Is the current program counter registered as the exit address?
        when (PC === mmexit.read()) {
          when (depth === 1) {
            // We are exiting mimicry mode
            mmentry.write(CSR_MMADDR_NONE)
            mmexit.write(CSR_MMADDR_NONE)
          }
          // TODO: assert depth > 0
          mmstatNew(CSR_MMSTAT_DEPTH) := depth - 1 // Update recursion depth
        }

        // 4) Do we need to mimic execution?
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

        mmstat.write(mmstatNew)
      }
    }

    pipeline plug new Area {
      val csrService = pipeline.getService[CsrService]
      mimicryArea.mmstat  <> csrService.getCsr(CSR_MMSTAT)
      mimicryArea.mmentry <> csrService.getCsr(CSR_MMENTRY)
      mimicryArea.mmexit  <> csrService.getCsr(CSR_MMEXIT)
    }
  }
}
