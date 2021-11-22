package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave

class Mimicry() extends Plugin[Pipeline] {

  override def getImplementedExtensions = Seq('X')

  private val CSR_MMIMSTAT       = 0x7FF // mmimstat identifier
  private val CSR_MIMSTAT_MIME   = 0     // mimicry mode enabled (mime)
  private val CSR_MIMSTAT_PMIME  = 1     // previous mime (pmime)
  private val CSR_MIMSTAT_DEPTH  = (31 downto 2)  // activation nesting level

  private object Data {
    object GHOST extends PipelineData(Bool())   // Ghost instruction
    object MIMIC extends PipelineData(Bool())   // Always mimic
    object EXECUTE extends PipelineData(Bool()) // Always execute

    object ACTIVATE extends PipelineData(Bool())   // Conditional mimciry (CM)
    object INVERT extends PipelineData(Bool())     // Conditional mimicry (CM)
    object DEACTIVATE extends PipelineData(Bool()) // Conditional mimicry (CM)

    object OUTCOME extends PipelineData(Bool())   // CM predicate outcome
  }

  // machine mimicry mode status (mmimstat) CSR
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

  override def setup(): Unit = {
    pipeline.getService[CsrService].registerCsr(CSR_MMIMSTAT, new Mmimstat)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ISJUMP  -> False,

        // Mimic execution
        Data.GHOST   -> False,
        Data.MIMIC   -> False,
        Data.EXECUTE -> False,

        // Conditional mimicry
        Data.OUTCOME    -> False,
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

        result
      })

      config.addDecoding(Opcodes.JAL, InstructionType.J, Map(
        Data.ISJUMP -> True,
      ))

      config.addDecoding(Opcodes.JALR, InstructionType.I, Map(
        Data.ISJUMP -> True,
      ))
    }

    pipeline.getService[JumpService].onJump { (stage, _, _, jumpType) =>

      val mimstat = Utils.outsideConditionScope(slave(new CsrIo))
      val mimstatCurrent = mimstat.read()
      val mimstatNew = UInt(config.xlen bits)
      mimstatNew := mimstatCurrent

      jumpType match {
        case JumpType.Trap =>
          mimstatNew(CSR_MIMSTAT_MIME) := False
          mimstatNew(CSR_MIMSTAT_PMIME) := mimstatCurrent(CSR_MIMSTAT_MIME)
        case JumpType.TrapReturn =>
          mimstatNew(CSR_MIMSTAT_MIME) := mimstatCurrent(CSR_MIMSTAT_PMIME)
          mimstatNew(CSR_MIMSTAT_PMIME) := False
        case JumpType.Normal =>
          when (inConditionalMimicry(stage)) {
            stage.output(Data.OUTCOME) := True
            pipeline.getService[JumpService].disableJump(stage)
          }
      }

      mimstat.write(mimstatNew)
      
      pipeline plug new Area {
        mimstat <> pipeline.getService[CsrService].getCsr(CSR_MMIMSTAT)
      }
    }
  }

  override def build(): Unit = {

    val stage = pipeline.retirementStage
    val mimicryArea = stage plug new Area {
      import stage._

      val mimstat = slave(new CsrIo)

      // TODO: check arbitration logic ?

      when (inConditionalMimicry(stage)) {
        val mimstatCurrent = mimstat.read()
        val mimstatNew = UInt(config.xlen bits)
        val depth = mimstatCurrent(CSR_MIMSTAT_DEPTH)
        mimstatNew := mimstatCurrent

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
      } elsewhen (value(Data.ISJUMP) && value(Data.MIMIC)) {
        // TODO: When this is an other region
        //         - Activate mimicry mode
        //         - Register return address
        //         - in onjump observer, deactivate mimicry mode when
        //                - instruction address == registered address
        //                - activation counter is zero
         
      }
      elsewhen (!value(Data.EXECUTE)) {
        val mime = mimstat.read()(CSR_MIMSTAT_MIME)
        when (   value(Data.MIMIC)
              || (mime && (!value(Data.GHOST)))
              || (!mime && (value(Data.GHOST)))) {
          output(pipeline.data.RD_TYPE) := RegisterType.NONE
        }
      }
    }

    pipeline plug new Area {
      mimicryArea.mimstat <> pipeline.getService[CsrService].getCsr(CSR_MMIMSTAT)
    }
  }
}
