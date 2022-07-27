package riscv.plugins

import riscv._
import spinal.core._

class ConstantTime extends Plugin[Pipeline] with ConstantTimeService {
  object Data {
    object CT_IGNORE extends PipelineData(Bool())
  }

  override def ignore(stage: Stage): Unit = {
    stage.output(Data.CT_IGNORE) := True
  }

  private def shouldIgnore(stage: Stage): Bool = {
    stage.value(Data.CT_IGNORE)
  }

  override def setup(): Unit = {
    pipeline.service[DecoderService].configure {config =>
      config.addDefault(Data.CT_IGNORE, False)
    }

    pipeline.service[JumpService].onJump { (stage, _, _, jumpType) =>
      if (jumpType == JumpType.Normal) {
        when (!shouldIgnore(stage)) {
          pipeline.service[FetchService].flushCache(stage)
        }
      }
    }

    pipeline.service[BranchService].onBranch { (stage, _, taken) =>
      when (!taken && !shouldIgnore(stage)) {
        stage.arbitration.jumpRequested := True
        pipeline.service[FetchService].flushCache(stage)
      }
    }
  }
}
