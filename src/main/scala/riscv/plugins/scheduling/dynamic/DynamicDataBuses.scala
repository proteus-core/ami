package riscv.plugins.scheduling.dynamic

import riscv._
import spinal.core._
import spinal.lib._

case class CdbMessage(metaRegisters: DynBundle[PipelineData[Data]], robIndexBits: BitCount)(implicit
    config: Config
) extends Bundle {
  val metadata: Bundle with DynBundleAccess[PipelineData[Data]] = metaRegisters.createBundle

  val robIndex: UInt = UInt(robIndexBits)
  val writeValue: UInt = UInt(config.xlen bits)

  // TODO: move these to metadata (or remove)
  val realUpdate: Bool = Bool()
  val previousWaw: Flow[UInt] = Flow(UInt(robIndexBits))
  val activatingTaken: Bool = Bool()
  val mmac: UInt = UInt(config.xlen bits)
  val mmen: UInt = UInt(config.xlen bits)
  val mmex: UInt = UInt(config.xlen bits)

  override def clone(): CdbMessage = {
    CdbMessage(metaRegisters, robIndexBits)
  }
}

case class RdbMessage(retirementRegisters: DynBundle[PipelineData[Data]], robIndexBits: BitCount)(
    implicit config: Config
) extends Bundle {
  val robIndex = UInt(robIndexBits)
  val wawBuffer = Flow(UInt(config.xlen bits))
  val previousWaw: Flow[UInt] = Flow(UInt(robIndexBits))
  val registerMap: Bundle with DynBundleAccess[PipelineData[Data]] =
    retirementRegisters.createBundle

  override def clone(): RdbMessage = {
    RdbMessage(retirementRegisters, robIndexBits)
  }
}

trait CdbListener {
  def onCdbMessage(cdbMessage: CdbMessage)
}

class CommonDataBus(
    reservationStations: Seq[ReservationStation],
    loadManagers: Seq[LoadManager],
    rob: ReorderBuffer,
    metaRegisters: DynBundle[PipelineData[Data]]
)(implicit config: Config)
    extends Area {
  val inputs: Vec[Stream[CdbMessage]] =
    Vec(
      Stream(HardType(CdbMessage(metaRegisters, rob.indexBits))),
      reservationStations.size + loadManagers.size
    )

  private val arbitratedInputs = StreamArbiterFactory.roundRobin.noLock.on(inputs)

  def build(): Unit = {
    when(arbitratedInputs.valid) {
      arbitratedInputs.ready := True
      val listeners = reservationStations ++ loadManagers :+ rob
      for (listener <- listeners) {
        listener.onCdbMessage(arbitratedInputs.payload)
      }
    } otherwise {
      arbitratedInputs.ready := False
    }
  }
}

class DispatchBus(
    reservationStations: Seq[ReservationStation],
    rob: ReorderBuffer,
    dispatcher: Dispatcher,
    retirementRegisters: DynBundle[PipelineData[Data]]
)(implicit
    config: Config
) extends Area {
  val inputs: Vec[Stream[RdbMessage]] =
    Vec(Stream(HardType(RdbMessage(retirementRegisters, rob.indexBits))), reservationStations.size)
  private val arbitratedInputs = StreamArbiterFactory.roundRobin.noLock.on(inputs)

  def build(): Unit = {
    when(arbitratedInputs.valid) {
      arbitratedInputs.ready := dispatcher.processMessage(arbitratedInputs.payload)
    } otherwise {
      arbitratedInputs.ready := False
    }
  }
}

class RobDataBus(
    rob: ReorderBuffer,
    retirementRegisters: DynBundle[PipelineData[Data]],
    loadManagerCount: Int
)(implicit
    config: Config
) extends Area {
  val inputs: Vec[Stream[RdbMessage]] = Vec(
    Stream(HardType(RdbMessage(retirementRegisters, rob.indexBits))),
    loadManagerCount + 1
  ) // +1 for dispatcher
  private val arbitratedInputs = StreamArbiterFactory.roundRobin.noLock.on(inputs)

  def build(): Unit = {
    when(arbitratedInputs.valid) {
      arbitratedInputs.ready := True
      rob.onRdbMessage(arbitratedInputs.payload)
    } otherwise {
      arbitratedInputs.ready := False
    }
  }
}
