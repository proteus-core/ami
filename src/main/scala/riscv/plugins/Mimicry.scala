package riscv.plugins

import riscv._
import spinal.core._

class Mimicry(decodeStage: Stage) extends Plugin[Pipeline] {

  object Opcodes {
    val EMM = M"00000000000000000000000000001011" // Enable Mimicry Mode
    val DMM = M"00000000000100000000000000001011" // Disable Mimicry Mode
  }

  object Data {
    object MIMICRY_IS_ENABLED extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    decodeStage plug new Area {
      private val inMimicryMode = Reg(Bool).init(False)
    }

    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.MIMICRY_IS_ENABLED -> False
      ))

      config.addDecoding(Opcodes.EMM, InstructionType.I, Map(
        Data.MIMICRY_IS_ENABLED -> True
      ))

      config.addDecoding(Opcodes.DMM, InstructionType.I, Map(
        Data.MIMICRY_IS_ENABLED -> False
      ))
    }
  }

  override def build(): Unit = {
    decodeStage plug new Area {
    }
  }
}
