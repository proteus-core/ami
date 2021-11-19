package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave

class Mimicry() extends Plugin[Pipeline] {

  override def getImplementedExtensions = Seq('X')

  private val CSR_MMIMSTAT = 0x7FF   // mmimstat identifier
  private val CSR_MIMSTAT_MIME = 0   // mimicry mode enabled (mime)
  private val CSR_MIMSTAT_PMIME = 1  // previous mime (pmime)
  private val CSR_MIMSTAT_DEPTH = (31 downto 2)  // activation nesting level

  private object Data {
    object GHOST extends PipelineData(Bool())   // Ghost instruction
    object MIMIC extends PipelineData(Bool())   // Always mimic
    object EXECUTE extends PipelineData(Bool()) // Always execute
  }

  // machine mimicry mode status (mmimstat) CSR
  private class Mmimstat(implicit config: Config) extends Csr {
    val mmime = Reg(Bool).init(False)
    val mpmime = Reg(Bool).init(False)
    val mdepth = Reg(UInt(CSR_MIMSTAT_DEPTH.length bits)).init(0)

    val mmimstat = mdepth ## mpmime ## mmime

    override def read(): UInt = mmimstat.asUInt

    override def write(value: UInt): Unit = {
      mmime := value(CSR_MIMSTAT_MIME)
      mpmime := value(CSR_MIMSTAT_PMIME)
      mdepth := value(CSR_MIMSTAT_DEPTH)
    }

    override def swWrite(value: UInt): Unit = {
      when (value.lsb) {
        mmime := True
        mdepth := mdepth + 1
      } otherwise {
        when (mdepth === 1) {
          mmime := False
        }
        mdepth := mdepth - 1   // TODO: assert mdepth > 0
      }
    }
  }

  override def setup(): Unit = {
    pipeline.getService[CsrService].registerCsr(CSR_MMIMSTAT, new Mmimstat)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.GHOST -> False,
        Data.MIMIC -> False,
        Data.EXECUTE -> False
      ))

      val stage = pipeline.retirementStage

      config.setIrMapper((stage, ir) => {
        var result = ir
        when (ir =/= 0) {
          switch (ir(1 downto 0)) {
            is(0) { stage.output(Data.GHOST)   := True }
            is(1) { stage.output(Data.MIMIC)   := True }
            is(2) { stage.output(Data.EXECUTE) := True }
          }
          result = ir | 3
        }
        result
      })
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
          when (stage.value(Data.MIMIC)) {
            // Register branch outcome and fall through
            // TODO: mimstatNew(CSR_MIMSTAT_XXX) := branch-outcome
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
      val mime = mimstat.read()(CSR_MIMSTAT_MIME)

      when (!value(Data.EXECUTE)) {
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
