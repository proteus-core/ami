package riscv.plugins

import riscv._
import spinal.core._

class Mimicry(decodeStage: Stage) extends Plugin[Pipeline] {

  object Opcodes {
    val EMM = M"00000000000000000000000000001011" // Enable Mimicry Mode
    val DMM = M"00000000000100000000000000001011" // Disable Mimicry Mode
  }

  object Data {
    object ENABLE_MIMICRY_ONCE  extends PipelineData(Bool()) // Mimic behavior for the current instruction only
    object ENABLE_MIMICRY_MODE  extends PipelineData(Bool()) // Enable mimicry mode
    object DISABLE_MIMICRY_MODE extends PipelineData(Bool()) // Disable mimicry mode
    object IGNORE_MIMICRY_MODE  extends PipelineData(Bool()) // Ignore mimicry mode for the current instruction only

    object MIMIC                extends PipelineData(Bool()) // Propagates to further pipeline stages if the
                                                             // the current instruction must be mimicked
  }

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure { config =>
      config.addDefault(Map(
        Data.ENABLE_MIMICRY_ONCE  -> False,
        Data.ENABLE_MIMICRY_MODE  -> False,
        Data.DISABLE_MIMICRY_MODE -> False,
        Data.IGNORE_MIMICRY_MODE  -> False,
        Data.MIMIC                -> False
      ))

      config.addDecoding(Opcodes.EMM, InstructionType.I, Map(
        Data.ENABLE_MIMICRY_MODE -> True
      ))

      config.addDecoding(Opcodes.DMM, InstructionType.I, Map(
        Data.DISABLE_MIMICRY_MODE -> False
      ))
    }
  }

  override def build(): Unit = {
    decodeStage plug new Area {
      import decodeStage._

      private val inMimicryMode = Reg(Bool).init(False)

      when (value(Data.ENABLE_MIMICRY_MODE)) {
        inMimicryMode  := True
      }

      when (value(Data.DISABLE_MIMICRY_MODE)) {
        inMimicryMode  := False
      }

      output(Data.MIMIC) := inMimicryMode
    }
  }
}
