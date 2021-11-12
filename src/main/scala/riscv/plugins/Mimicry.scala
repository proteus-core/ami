package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave
import scala.collection.mutable

class Mimicry() extends Plugin[Pipeline] {

  val CSR_MMIMSTAT = 0x7FF   // mmimstat identifier
  val CSR_MIMSTAT_MIME = 0   // mimicry mode enabled (mime)
  val CSR_MIMSTAT_PMIME = 1  // previous mime (pmime)

  val GHOST = 0
  val MIMIC = 1
  val EXECUTE = 2

  object Data {
    object GHOST extends PipelineData(Bool())   // Ghost instruction
    object MIMIC extends PipelineData(Bool())   // Always mimic
    object EXECUTE extends PipelineData(Bool()) // Always execute
  }

  // machine mimicry mode status (mmimstat) CSR
  private class Mmimstat(implicit config: Config) extends Csr {
    val mmime = Reg(Bool).init(False)
    val mpmime = Reg(Bool).init(False)

    val mmimstat = B(0, config.xlen - 2 bits) ## mpmime ## mmime

    override def read(): UInt = mmimstat.asUInt

    override def write(value: UInt): Unit = {
      mmime := value(CSR_MIMSTAT_MIME)
      mpmime := value(CSR_MIMSTAT_PMIME)
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
            is(GHOST)   { stage.output(Data.GHOST)   := True }
            is(MIMIC)   { stage.output(Data.MIMIC)   := True }
            is(EXECUTE) { stage.output(Data.EXECUTE) := True }
          }
          result = ir | 3
        }
        result
      })
    }

    pipeline.getService[JumpService].onJump { (_, _, _, jumpType) =>

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
        case _ => {}
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
