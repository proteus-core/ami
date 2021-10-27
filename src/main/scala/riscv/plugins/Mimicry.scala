package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave
import scala.collection.mutable

class Mimicry(stage: Stage) extends Plugin[Pipeline] {

  object Data {
    object ENABLE_MIMICRY_ONCE extends PipelineData(Bool()) // Enable mimicry mode for the current instruction only
    object IGNORE_MIMICRY_ONCE extends PipelineData(Bool()) // Ignore mimicry mode for the current instruction only
  }

  // mimicry mode status (mimstat) CSR
  private class Mmimstat(implicit config: Config) extends Csr {
    val mmime = Reg(Bool).init(False)   // mimicry mode enabled (mime)
    val mpmime = Reg(Bool).init(False)  // prev mime (pmime)

    val mmimstat = B(0, config.xlen - 2 bits) ## mpmime ## mmime

    override def read(): UInt = mmimstat.asUInt

    override def write(value: UInt): Unit = {
      mmime := value(0)
      mpmime := value(1)
    }
  }

  override def setup(): Unit = {
    pipeline.getService[CsrService].registerCsr(0x7FF, new Mmimstat)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ENABLE_MIMICRY_ONCE -> False,
        Data.IGNORE_MIMICRY_ONCE -> False
      ))
    }

    pipeline.getService[JumpService].onJump { (_, _, _, jumpType) =>

      jumpType match {
        case JumpType.Trap =>
          val mimstat = slave(new CsrIo)
          val mimstatCurrent = mimstat.read()
          val mimstatNew = UInt(config.xlen bits)
          mimstatNew := mimstatCurrent
          mimstatNew(0) := False             // mime = 0
          mimstatNew(1) := mimstatCurrent(0) // pmime = mime
          //mimstat.write(mimstatNew)
          pipeline plug new Area {
            mimstat <> pipeline.getService[CsrService].getCsr(0x7FF)
          }
        case JumpType.TrapReturn =>
        case _ =>
      }
    }
  }

  override def build(): Unit = {
    val mimicryArea = stage plug new Area {
      import stage._

      val mimstat = slave(new CsrIo)
      val mime = mimstat.read()(0) // mime bit

      when (   (mime && !(value(Data.IGNORE_MIMICRY_ONCE)))
            || value(Data.ENABLE_MIMICRY_ONCE)) {
        output(pipeline.data.RD_TYPE) := RegisterType.NONE
      }
    }

    pipeline plug new Area {
      mimicryArea.mimstat <> pipeline.getService[CsrService].getCsr(0x7FF)
    }
  }
}
