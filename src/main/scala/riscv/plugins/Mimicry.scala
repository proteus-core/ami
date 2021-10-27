package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib.slave

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
    val csr = pipeline.getService[CsrService].registerCsr(0x7FF, new Mmimstat)

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ENABLE_MIMICRY_ONCE -> False,
        Data.IGNORE_MIMICRY_ONCE -> False
      ))
    }

    pipeline.getService[JumpService].onJump { (_, prevPc, nextPc, jumpType) =>
      jumpType match {
        case JumpType.Trap =>
        case JumpType.TrapReturn =>
        case _ =>
      }
    }
  }

  override def build(): Unit = {
    val mimicryArea = stage plug new Area {
      import stage._

      val mstatus = slave(new CsrIo)
      val mime = mstatus.read()(0) // mime bit

      when (   (mime && !(value(Data.IGNORE_MIMICRY_ONCE)))
            || value(Data.ENABLE_MIMICRY_ONCE)) {
        output(pipeline.data.RD_TYPE) := RegisterType.MIMIC
      }
    }

    pipeline plug new Area {
      mimicryArea.mstatus <> pipeline.getService[CsrService].getCsr(0x7FF)
    }
  }
}
