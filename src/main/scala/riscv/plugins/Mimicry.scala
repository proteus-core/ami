package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave
import scala.collection.mutable

class Mimicry(stage: Stage) extends Plugin[Pipeline] {

  val CSR_MIMSTAT = 0x7FF
  val CSR_MIMSTAT_MIME = 0
  val CSR_MIMSTAT_PMIME = 1

  object Data {
    object ENABLE_MIMICRY_ONCE extends PipelineData(Bool()) // Enable mimicry mode for the current instruction only
    object IGNORE_MIMICRY_ONCE extends PipelineData(Bool()) // Ignore mimicry mode for the current instruction only
  }

  // mimicry mode status (mimstat) CSR
  private class Mmimstat(implicit config: Config) extends Csr {
    val mmime = Reg(Bool).init(False)   // mimicry mode enabled (mime)
    val mpmime = Reg(Bool).init(False)  // previous mime (pmime)

    val mmimstat = B(0, config.xlen - 2 bits) ## mpmime ## mmime

    override def read(): UInt = mmimstat.asUInt

    override def write(value: UInt): Unit = {
      mmime := value(CSR_MIMSTAT_MIME)
      mpmime := value(CSR_MIMSTAT_PMIME)
    }
  }

  override def setup(): Unit = {
    pipeline.getService[CsrService].registerCsr(CSR_MIMSTAT, new Mmimstat)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ENABLE_MIMICRY_ONCE -> False,
        Data.IGNORE_MIMICRY_ONCE -> False
      ))
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
        mimstat <> pipeline.getService[CsrService].getCsr(CSR_MIMSTAT)
      }
    }
  }

  override def build(): Unit = {
    val mimicryArea = stage plug new Area {
      import stage._

      val mimstat = slave(new CsrIo)
      val mime = mimstat.read()(CSR_MIMSTAT_MIME)

      when (   (mime && !(value(Data.IGNORE_MIMICRY_ONCE)))
            || value(Data.ENABLE_MIMICRY_ONCE)) {
        output(pipeline.data.RD_TYPE) := RegisterType.NONE
      }
    }

    pipeline plug new Area {
      mimicryArea.mimstat <> pipeline.getService[CsrService].getCsr(CSR_MIMSTAT)
    }
  }
}
