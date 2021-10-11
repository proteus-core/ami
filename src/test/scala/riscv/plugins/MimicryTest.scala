package riscv.plugins

import org.scalatest.funsuite.AnyFunSuite
import riscv.SoC
import riscv.soc.RamType
import spinal.core.IntToBuilder
import spinal.core.sim.{SimClockDomainHandlePimper, SimConfig}

class MimicryTest extends AnyFunSuite {
  test("Mimicry") {
    val hexFile = "src/test/resources/hex/mimicry.ihex"
    SimConfig.withWave.compile(SoC.static(RamType.OnChipRam(10 MiB, Some(hexFile)))).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
    }
  }
}