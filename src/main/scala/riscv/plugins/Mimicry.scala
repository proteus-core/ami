package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave

class Mimicry() extends Plugin[Pipeline] {

  override def getImplementedExtensions = Seq('X')

  private val CSR_MMIMSTAT       = 0x7FF          // mmimstat CSR identifier
  private val CSR_MIMSTAT_MIME   = 0              // mimicry mode enabled (mime)
  private val CSR_MIMSTAT_PMIME  = 1              // previous mime (pmime)
  private val CSR_MIMSTAT_DEPTH  = (31 downto 2)  // activation nesting level

  private val CSR_MMIMEXIT       = 0x7EF      // mmimexit CSR identifier
  private val CSR_MIMEXIT_NONE   = 0x7FFFFFFF // TODO: which value?

  private object Data {
    object ISJUMP extends PipelineData(Bool())
    object OUTCOME extends PipelineData(Bool()) // Outcome of branch predicate

    // Mimic execution
    object GHOST extends PipelineData(Bool())   // Ghost instruction
    object MIMIC extends PipelineData(Bool())   // Always mimic
    object EXECUTE extends PipelineData(Bool()) // Always execute

    // Conditional mimicry (CM)
    object ACTIVATE extends PipelineData(Bool())   // Activate conditionaly
    object INVERT extends PipelineData(Bool())     // Activate or deactivate
    object DEACTIVATE extends PipelineData(Bool()) // Deactivate conditionally
  }

  // mmimstat CSR: machine mimicry mode status
  private class Mmimstat(implicit config: Config) extends Csr {
    val mime   = Reg(Bool).init(False)
    val pmime  = Reg(Bool).init(False)
    val depth  = Reg(UInt(CSR_MIMSTAT_DEPTH.length bits)).init(0)

    val mmimstat = depth ## pmime ## mime

    override def read(): UInt = mmimstat.asUInt

    override def write(value: UInt): Unit = {
      mime := value(CSR_MIMSTAT_MIME)
      pmime := value(CSR_MIMSTAT_PMIME)
      depth := value(CSR_MIMSTAT_DEPTH)
    }

    override def swWrite(value: UInt): Unit = {
      when (value(CSR_MIMSTAT_MIME)) {
        mime := True
        depth := depth + 1
      } otherwise {
        when (depth > 0) {
          when (depth === 1) {
            mime := False
          }
          depth := depth - 1  // TODO: assert depth > 0
        }
      }
    }
  }

  // mmimexit CSR: machine mimicry mode exit address
  private class Mmimexit(implicit config: Config) extends Csr {

    val exit = Reg(UInt(config.xlen bits)).init(CSR_MIMEXIT_NONE)

    override def read(): UInt = exit

    override def write(value: UInt): Unit = {
      exit := value
    }
  }

  def inMimicryExecutionMode(stage: Stage): Bool = {
    stage.value(Data.GHOST)   ||
    stage.value(Data.MIMIC)   ||
    stage.value(Data.EXECUTE)
  }

  def inConditionalMimicry(stage: Stage): Bool = {
    stage.value(Data.ACTIVATE)   ||
    stage.value(Data.INVERT)     ||
    stage.value(Data.DEACTIVATE)
  }

  def isConditional(ir: UInt): Bool = {
    (ir === Opcodes.BEQ)  ||
    (ir === Opcodes.BNE)  ||
    (ir === Opcodes.BLT)  ||
    (ir === Opcodes.BGE)  ||
    (ir === Opcodes.BLTU) ||
    (ir === Opcodes.BGEU)
  }

  def isJump(ir: UInt): Bool = {
    (ir === Opcodes.JAL)  ||
    (ir === Opcodes.JALR)
  }

  override def setup(): Unit = {
    val csrService = pipeline.getService[CsrService]
    csrService.registerCsr(CSR_MMIMSTAT, new Mmimstat)
    csrService.registerCsr(CSR_MMIMEXIT, new Mmimexit)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ISJUMP  -> False,
        Data.OUTCOME -> False,

        // Mimic execution
        Data.GHOST   -> False,
        Data.MIMIC   -> False,
        Data.EXECUTE -> False,

        // Conditional mimicry (CM)
        Data.ACTIVATE   -> False,
        Data.INVERT     -> False,
        Data.DEACTIVATE -> False
      ))

      val stage = pipeline.retirementStage

      config.setIrMapper((stage, ir) => {
        var result = ir | 3

        when (ir =/= 0) {
          switch (ir(1 downto 0)) {
            is(0) { 
              when (isConditional(result)) {
                stage.output(Data.INVERT) := True
              } otherwise {
                stage.output(Data.GHOST) := True 
              }
            }
            is(1) { 
              when (isConditional(result)) {
                stage.output(Data.ACTIVATE) := True
              } otherwise {
                stage.output(Data.MIMIC) := True 
              }
            }
            is(2) { 
              when (isConditional(result)) {
                stage.output(Data.DEACTIVATE) := True
              } otherwise {
                stage.output(Data.EXECUTE) := True 
              }
            }
          }
        }

        stage.output(Data.ISJUMP) := isJump(result)

        result
      })
    }

    pipeline.getService[JumpService].onJump { (stage, _, _, jumpType) =>

      val mimstat = Utils.outsideConditionScope(slave(new CsrIo))
      val mimstatCurrent = mimstat.read()
      val mimstatNew = UInt(config.xlen bits)
      mimstatNew := mimstatCurrent

      val mimexit = Utils.outsideConditionScope(slave(new CsrIo))

      jumpType match {
        case JumpType.Trap =>
          mimstatNew(CSR_MIMSTAT_MIME) := False
          mimstatNew(CSR_MIMSTAT_PMIME) := mimstatCurrent(CSR_MIMSTAT_MIME)
        case JumpType.TrapReturn =>
          mimstatNew(CSR_MIMSTAT_MIME) := mimstatCurrent(CSR_MIMSTAT_PMIME)
          mimstatNew(CSR_MIMSTAT_PMIME) := False
        case JumpType.Normal =>
          when (inConditionalMimicry(stage)) {
            pipeline.getService[JumpService].disableJump(stage)
            stage.output(Data.OUTCOME) := True
          }
      }

      mimstat.write(mimstatNew)
      
      pipeline plug new Area {
        val csrService = pipeline.getService[CsrService]
        mimstat <> csrService.getCsr(CSR_MMIMSTAT)
        mimexit <> csrService.getCsr(CSR_MMIMEXIT)
      }
    }
  }

  override def build(): Unit = {

    val stage = pipeline.retirementStage
    val mimicryArea = stage plug new Area {
      import stage._

      val mimstat = slave(new CsrIo)
      val mimstatCurrent = mimstat.read()
      val mimstatNew = UInt(config.xlen bits)
      mimstatNew := mimstatCurrent

      val mimexit = slave(new CsrIo)

      // TODO: check arbitration logic ?

      // Deactivate mimicry mode when
      //   - we are executing in an outermost region
      //   - and the progam counter equals the exit address
      when (   (mimstatCurrent(CSR_MIMSTAT_DEPTH) === 1)
            && (value(pipeline.data.PC) === mimexit.read()) ) {
        // TODO: How to avoid duplication with mimstat.swWrite ?
        mimstatNew(CSR_MIMSTAT_MIME) := False
        mimstatNew(CSR_MIMSTAT_DEPTH) := 0
        mimstat.write(mimstatNew)
        mimexit.write(CSR_MIMEXIT_NONE)

        // TODO: - Code below should read from mimstatNew?
        //       - Or always read from mimstatCurrent and update that one too?
      }

      // Case 1: Conditional mimicry
      when (inConditionalMimicry(stage)) {
        val depth = mimstatCurrent(CSR_MIMSTAT_DEPTH)

        // TODO: How to avoid duplication with mimstat.swWrite ?
        when (value(Data.ACTIVATE)) {
          when (value(Data.OUTCOME)) {
            mimstatNew(CSR_MIMSTAT_MIME) := True
            mimstatNew(CSR_MIMSTAT_DEPTH) := depth + 1
          }
        } elsewhen (value(Data.INVERT)) {
          when (value(Data.OUTCOME)) {
            mimstatNew(CSR_MIMSTAT_MIME) := True
            mimstatNew(CSR_MIMSTAT_DEPTH) := depth + 1
          } otherwise {
            mimstatNew(CSR_MIMSTAT_MIME) := False
            mimstatNew(CSR_MIMSTAT_DEPTH) := depth - 1
          }
        } otherwise {
          // TODO: assert value(Data.DEACTIVATE)
          when (!value(Data.OUTCOME)) {
            mimstatNew(CSR_MIMSTAT_MIME) := False
            mimstatNew(CSR_MIMSTAT_DEPTH) := depth - 1 // TODO: assert depth > 0
          }
        }

        mimstat.write(mimstatNew)

      // Case 2: Mimic jump
      } elsewhen (value(Data.MIMIC) && value(Data.ISJUMP)) {
        // If this is an outermost region, activate mimicry mode and regsiter
        // the address of the next instruction as the exit address
        when (mimstatCurrent(CSR_MIMSTAT_DEPTH) === 0) {
          mimstatNew(CSR_MIMSTAT_MIME) := True
          mimstatNew(CSR_MIMSTAT_DEPTH) := 1

          mimstat.write(mimstatNew)
          mimexit.write(value(pipeline.data.PC)+4)
        }

      // Case 3: Mimic exeuction 
      } elsewhen (!value(Data.EXECUTE)) {
        val mime = mimstat.read()(CSR_MIMSTAT_MIME)
        when (   value(Data.MIMIC)
              || (mime && (!value(Data.GHOST)))
              || (!mime && (value(Data.GHOST)))) {
          output(pipeline.data.RD_TYPE) := RegisterType.NONE
        }
      }
    }

    pipeline plug new Area {
      val csrService = pipeline.getService[CsrService]
      mimicryArea.mimstat <> csrService.getCsr(CSR_MMIMSTAT)
      mimicryArea.mimexit <> csrService.getCsr(CSR_MMIMEXIT)
    }
  }
}
