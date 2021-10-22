package riscv.plugins

import riscv._
import spinal.core._

class Mimicry(stage: Stage) extends Plugin[Pipeline] {

  object Opcodes {
    val EMM = M"00000000000000000000000000001011" // Enable Mimicry Mode
    val DMM = M"00000000000100000000000000001011" // Disable Mimicry Mode
  }

  object Data {
    object ENABLE_MIMICRY_ONCE  extends PipelineData(Bool()) // Mimic behavior for the current instruction only

    object ENABLE_MIMICRY_MODE  extends PipelineData(Bool()) // Enable mimicry mode
    object DISABLE_MIMICRY_MODE extends PipelineData(Bool()) // Disable mimicry mode
    object IGNORE_MIMICRY_MODE  extends PipelineData(Bool()) // Ignore mimicry mode for the current instruction only
  }

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ENABLE_MIMICRY_ONCE  -> False,
        Data.ENABLE_MIMICRY_MODE  -> False,
        Data.DISABLE_MIMICRY_MODE -> False,
        Data.IGNORE_MIMICRY_MODE  -> False
      ))

      config.addDecoding(Opcodes.EMM, InstructionType.I, Map(
        Data.ENABLE_MIMICRY_MODE -> True
      ))

      config.addDecoding(Opcodes.DMM, InstructionType.I, Map(
        Data.DISABLE_MIMICRY_MODE -> True
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
    stage plug new Area {
      import stage._

      private val inMimicryMode = Reg(Bool).init(False)

      when (arbitration.isValid) {
        when (value(Data.ENABLE_MIMICRY_MODE)) {
          inMimicryMode := True
        }

        when (value(Data.DISABLE_MIMICRY_MODE)) {
          inMimicryMode := False
        }
      }

      when (   (inMimicryMode && !(value(Data.IGNORE_MIMICRY_MODE)))
            || value(Data.ENABLE_MIMICRY_ONCE)) {
        output(pipeline.data.RD_TYPE) := RegisterType.MIMIC
      }
    }
  }
}
