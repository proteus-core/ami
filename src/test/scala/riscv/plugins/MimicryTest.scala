package riscv.plugins

import org.scalatest.funsuite.AnyFunSuite
import riscv._
import riscv.soc._
import spinal.core._
import spinal.core.sim._
import spinal.lib.master

class TIBus(implicit config: Config) extends IBusControl {
  def read(address: UInt): (Bool, UInt) = {
    (True, 0x02)
  }
}

class TestMemoryBackbone(implicit config: Config) extends Plugin with MemoryService {
  private var externalIBus: MemBus = null
  private var externalDBus: MemBus = null
  private var internalIBus: MemBus = null
  private var internalDBus: MemBus = null
  private var internalDBusStage: Stage = null
  private var dbusFilter: Option[MemBusFilter] = None

  override def finish(): Unit = {
    pipeline plug new Area {
      externalIBus = master(new MemBus(config.ibusConfig)).setName("ibus")
      externalIBus <> internalIBus
    }

    if (dbusFilter.isEmpty) {
      dbusFilter = Some((_, idbus, edbus) => {
        idbus <> edbus
      })
    }

    pipeline plug new Area {
      externalDBus = master(new MemBus(config.dbusConfig)).setName("dbus")
      dbusFilter.get (internalDBusStage, internalDBus, externalDBus)
    }
  }

  override def createInternalIBus(stage: Stage, ibusLatency: Int): IBusControl = {
    stage plug new Area {
      internalIBus = master(new MemBus(config.ibusConfig))
    }

    new riscv.IBus(internalIBus, ibusLatency)
    new TIBus
  }

  override def createInternalDBus(stage: Stage): MemBus = {
    assert(internalDBus == null)

    stage plug new Area {
      internalDBus = master(new MemBus(config.dbusConfig))
    }

    internalDBusStage = stage
    internalDBus
  }

  override def getExternalIBus: MemBus = externalIBus

  override def getExternalDBus: MemBus = externalDBus

  override def getDBusStages: Seq[Stage] = Seq()

  override def filterDBus(filter: MemBusFilter): Unit = Unit

  override def observeDBus(observer: MemBusObserver): Unit = Unit
}

class MimicryTest extends AnyFunSuite {
  test("mimicry-enable-disable") {
    val hexFile = "src/test/resources/test-mimicry-enable-disable.ihex"
    val ramType = RamType.OnChipRam(10 MiB, Some(hexFile))
    implicit val config = new Config(BaseIsa.RV32I) // TODO
    //val m = new TestMemoryBackbone
    val m = new MemoryBackbone

    SimConfig.withWave.compile(new SoC(ramType, config => createStaticPipeline(memory = m)(config))).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      SimTimeout(10*1000)
      var done = false
      while (! done) {
        dut.clockDomain.waitSampling()
        if (dut.io.charOut.valid.toBoolean) {
          val c = dut.io.charOut.payload.toInt.toChar
          done = c == 4 // ASCII EOT (0x04) marks end of simulation

          if (c == 2) { // ASCII STX (0x02) marks start of test

          }
        }
      }
    }
  }
}