package riscv.plugins

import riscv._

import spinal.core._
import spinal.lib._

import collection.mutable

class Fetcher(fetchStage: Stage, ibusLatency: Int = 2) extends Plugin[Pipeline] with FetchService {
  private var addressTranslator = new FetchAddressTranslator {
    override def translate(stage: Stage, address: UInt): UInt = {
      address
    }
  }

  private var addressTranslatorChanged = false
  private var flushCacheIn: Bool = _
  private val flushCacheStages = mutable.Map[Stage, Bool]()

  override def setup(): Unit = {
    val fetchArea = fetchStage plug new Area {
      val flushCache = in Bool()
      flushCacheIn = flushCache
    }

    pipeline plug new Area {
      fetchArea.flushCache := False
    }
  }

  override def build(): Unit = {
    fetchStage plug new Area {
      import fetchStage._

      val ibus = pipeline.service[MemoryService].createInternalIBus(fetchStage)
      val ibusCtrl = new IBusControl(ibus, ibusLatency, ibusLatency)

      arbitration.isReady := False

      val pc = input(pipeline.data.PC)
      val nextPc = pc + 4

      when (flushCacheIn) {
        ibusCtrl.flushCache()
      }

      when (arbitration.isRunning) {
        val fetchAddress = addressTranslator.translate(fetchStage, pc)
        val (valid, rdata) = ibusCtrl.read(fetchAddress)

        when (valid) {
          arbitration.isReady := True

          output(pipeline.data.NEXT_PC) := nextPc
          output(pipeline.data.IR) := rdata
        }
      }
    }
  }

  override def finish(): Unit = {
    pipeline plug new Area {
      for ((_, flush) <- flushCacheStages) {
        when (flush) {
          flushCache()
        }
      }
    }
  }

  override def setAddressTranslator(translator: FetchAddressTranslator): Unit = {
    assert(!addressTranslatorChanged, "FetchAddressTranslator can only be set once")

    addressTranslator = translator
    addressTranslatorChanged = true
  }

  override def flushCache(): Unit = {
    flushCacheIn := True
  }

  override def flushCache(stage: Stage): Unit = {
    val flush = flushCacheStages.getOrElseUpdate(stage, {
      Utils.outsideConditionScope {
        val flush = out(Bool())
        flush := False
        flush
      }
    })

    flush := True
  }
}
