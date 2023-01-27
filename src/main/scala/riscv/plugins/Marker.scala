package riscv.plugins

import riscv._
import spinal.core._

class Marker() extends Plugin[Pipeline] {

  object Opcodes {
    val MARK = M"-------------------------0001011"
  }

  object Data {
    object MARK extends PipelineData(Bool())
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          Data.MARK -> False
        )
      )

      // TODO: discuss creating a new instruction type
      config.addDecoding(
        Opcodes.MARK,
        InstructionType.I,
        Map(
          Data.MARK -> True,
          pipeline.data.RD_TYPE -> RegisterType.NONE
        )
      )
    }
  }

  override def build(): Unit = {
    for (stage <- pipeline.stages) {
      if (stage != pipeline.fetchStage) {
        stage plug new Area {
          stage.output(Data.MARK)
        }
      }
    }
  }
}
