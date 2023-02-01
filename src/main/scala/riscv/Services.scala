package riscv

import spinal.core._
import spinal.lib._

trait MemoryService {

  /** Returns the Pipeline's instruction bus.
    */
  def getExternalIBus: MemBus

  /** Returns the Pipeline's data bus.
    */
  def getExternalDBus: MemBus

  /** Creates a new instruction bus to be used in stage.
    */
  def createInternalIBus(stage: Stage): MemBus

  /** Creates a new data bus to be used in stage.
    */
  def createInternalDBus(readStages: Seq[Stage], writeStage: Stage): (Seq[MemBus], MemBus)

  /** Return all stages that have an internal DBUS. I.e., the stages that are able to put requests
    * on the DBUS.
    */
  def getDBusStages: Seq[Stage]

  type MemBusFilter = (Stage, MemBus, MemBus) => Unit

  /** The `filter` function is called on every created data bus. The passed arguments are 1) the
    * stage in which the data bus is created, 2) the data bus that is used in this stage, and 2) the
    * data bus that will be connected to the external data bus. When `filter` is called, the two
    * data buses are disconnected. The `filter` function is called in the context of the top-level
    * Pipeline component.
    */
  def filterDBus(filter: MemBusFilter): Unit

  type MemBusObserver = (Stage, MemBus) => Unit

  /** The `observer` function is called on every created data bus. The passed arguments are 1) the
    * stage in which the data bus is created, and 2) the data bus that is used in this stage. The
    * `observer` function is called in the context of the top-level Pipeline component.
    */
  def observeDBus(observer: MemBusObserver): Unit
}

trait FetchAddressTranslator {

  /** This method can be used to change the address used to fetch instructions. The input address is
    * the one calculated by the IF stage (i.e., the one generated by software) and the return value
    * will be put on the IBUS. If necessary, the pipeline can be stalled from this method using the
    * stage's arbitration. This method is called in the context of the given stage.
    */
  def translate(stage: Stage, address: UInt): UInt
}

trait FetchService {
  def setAddressTranslator(translator: FetchAddressTranslator): Unit
  def flushCache(): Unit
  def flushCache(stage: Stage): Unit
}

trait DecoderService {
  type IrMapper = (Stage, UInt) => UInt

  type Action = Map[PipelineData[_ <: Data], Data]

  trait DecoderConfig {

    /** The IR mapper can be used to analyze and/or transform the value of the instruction register
      * (IR) prior to the actual decoding. It is useful for adding logic that is common to a set of
      * instructions. The passed arguments are 1) the stage in which the instruction is decoded, and
      * 2) the value of the IR register. The return value is the new value of the IR register, or
      * the old value if no transformation was performed.
      */
    def setIrMapper(map: IrMapper): Unit

    def addDecoding(opcode: MaskedLiteral, itype: InstructionType, action: Action): Unit
    def addDecoding(opcode: MaskedLiteral, action: Action): Unit

    /** Hardcode the register operands used by the given opcode. This means the operands will not be
      * decoded from the opcode but the given values will be used instead.
      */
    def setFixedRegisters(
        opcode: MaskedLiteral,
        rs1: Option[Int] = None,
        rs2: Option[Int] = None,
        rd: Option[Int] = None
    ): Unit

    def addDefault(action: Action): Unit
    def addDefault(data: PipelineData[_ <: Data], value: Data): Unit
  }

  protected val decoderConfig: DecoderConfig
  protected def stage: Stage

  def configure(f: DecoderConfig => Unit): Unit = {
    stage.rework(f(decoderConfig))
  }

  def getSupportedOpcodes: Iterable[MaskedLiteral]
}

trait IssueService {
  def setDestination(opcode: MaskedLiteral, stage: Stage): Unit = {
    setDestinations(opcode, Set(stage))
  }

  def setDestinations(opcode: MaskedLiteral, stages: Set[Stage]): Unit
}

trait IntAluService {
  object AluOp extends SpinalEnum {
    val ADD, SUB, SLT, SLTU, XOR, OR, AND, SRC2 = newElement()
  }

  object Src1Select extends SpinalEnum {
    val RS1, PC = newElement()
  }

  object Src2Select extends SpinalEnum {
    val RS2, IMM = newElement()
  }

  def addOperation(
      opcode: MaskedLiteral,
      op: SpinalEnumElement[AluOp.type],
      src1: SpinalEnumElement[Src1Select.type],
      src2: SpinalEnumElement[Src2Select.type]
  ): Unit

  def resultData: PipelineData[UInt]
}

object LsuOperationType extends SpinalEnum {
  val NONE, LOAD, STORE = newElement()
}

object LsuAccessWidth extends SpinalEnum {
  val B, H, W = newElement()
}

trait LsuAddressTranslator {

  /** This method can be used to change the address used by the LSU. The input address is the one
    * calculated by the LSU (i.e., the one generated by software) and the return value will be used
    * by the LSU. If the operation is NONE, it means the the current instruction is not an LSU
    * operation. If necessary, the pipeline can be stalled from this method using the stage's
    * arbitration. This method is called in the context of the given stage.
    */
  def translate(
      stage: Stage,
      address: UInt,
      operation: SpinalEnumCraft[LsuOperationType.type],
      width: SpinalEnumCraft[LsuAccessWidth.type]
  ): UInt
}

trait LsuService {
  def setAddressTranslator(translator: LsuAddressTranslator): Unit

  /** Add a custom store instruction.
    *
    * Custom store instructions are processed like normal ones except their target address is taken
    * from the one given to [[setAddress]]. So to add a custom store instruction, call this method
    * with your opcode during the setup phase and call [[setAddress]] in the build phase when your
    * opcode was detected.
    */
  def addStore(opcode: MaskedLiteral, width: SpinalEnumCraft[LsuAccessWidth.type]): Unit

  /** Add a custom load instruction.
    *
    * @see
    *   [[addStore]]
    */
  def addLoad(
      opcode: MaskedLiteral,
      width: SpinalEnumCraft[LsuAccessWidth.type],
      unsigned: Boolean
  ): Unit

  /** Set the address used by custom loads/stores.
    *
    * Must be called in the context of [[stage]].
    *
    * @see
    *   [[addStore]]
    */
  def setAddress(address: UInt): Unit

  /** The [[Stage]] of the LSU.
    */
  def stage: Stage

  /** Get the [[LsuOperationType]] of the instruction in `stage`.
    */
  def operation(stage: Stage): SpinalEnumCraft[LsuOperationType.type]

  /** Get the [[LsuAccessWidth]] of the instruction in `stage`.
    */
  def width(stage: Stage): SpinalEnumCraft[LsuAccessWidth.type]

  def operationOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Data

  def addressValidOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool

  def addressOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): UInt

  def operationOutput(stage: Stage): SpinalEnumCraft[LsuOperationType.type]
}

trait ScheduleService {

  /** Give `stage` exclusive access to the pipeline.
    *
    * All earlier instructions are executed until completion and all later instructions are
    * canceled. Once this method returns True, `stage` is the only stage in the pipeline holding a
    * valid instruction. This will continue to be true as long as this method is called. In the
    * first cycle this method isn't called anymore, the pipeline is restarted from the instruction
    * after the one in `stage`.
    */
  def claimPipeline(stage: Stage): Bool
}

trait PcPayload[T <: Data] {

  /** The payload's data type.
    */
  def dataType: HardType[T]

  /** The payload's initial value. The first executed instruction will see this value as the
    * payload.
    */
  def initValue: T

  /** Called when the given Stage causes PC to change. The return value should be the payload's
    * value for the target instruction.
    */
  def get(stage: Stage): T

  /** Called to inject the payload in a Stage.
    */
  def set(stage: Stage, value: T)
}

trait JumpType

object JumpType {
  case object Normal extends JumpType
  case object Trap extends JumpType
  case object TrapReturn extends JumpType
}

trait JumpService {

  /** Jump initiated from outside a stage
    */
  def jump(target: UInt): Unit

  def jump(
      stage: Stage,
      target: UInt,
      jumpType: JumpType = JumpType.Normal,
      checkAlignment: Boolean = true
  ): Unit

  def flushPipeline(stage: Stage): Unit

  /** Has a jump been requested by the instruction in `stage`?
    */
  def jumpRequested(stage: Stage): Bool

  /** Add a payload that travels through the pipeline along with PC. A payload will typically be a
    * pipeline register but PcPayload provides a more abstract interface: whenever PC is updated,
    * the payload is extracted from the stage using PcPayload.get() and later injected into the
    * first stage of the target instruction using PcPayload.set().
    */
  def addPcPayload[T <: Data](pcPayload: PcPayload[T])

  type PcUpdateObserver = (Stage, UInt, UInt) => Unit

  /** Register a callback to be called whenever the program counter is updated; whether explicitly
    * through a jump or implicitly after each instruction. The parameters passed to `observer` are
    * 1) the stage that caused the update, 2) the current PC, and 3) the new PC. It is called on the
    * context of the top-level Pipeline.
    */
  def onPcUpdate(observer: PcUpdateObserver): Unit

  type JumpObserver = (Stage, UInt, UInt, JumpType) => Unit

  /** Like onPcUpdate but only called for jumps.
    */
  def onJump(observer: JumpObserver): Unit

  /** Override the next PC for the fetch stage.
    *
    * Note that this is not the same as performing a jump! For example, no instructions are
    * invalidated by this method.
    */
  def setFetchPc(pc: UInt): Unit

  def disableJump(stage: Stage): Unit

  def jumpOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool
}

trait BranchService {
  type BranchObserver = (Stage, UInt, Bool) => Unit

  /** Register a callback to be called whenever a conditional branch is executed. The parameters
    * passed to `observer` are 1) the stage that caused the update, 2) the target PC if the branch
    * would be taken, and 3) whether the branch is taken.. It is called on the context of the passed
    * stage.
    */
  def onBranch(observer: BranchObserver): Unit
}

trait BranchTargetPredictorService {
  def predictedPc(stage: Stage): UInt
  def predictionForAddress(
      address: UInt
  ): UInt // TODO: should it be Flow[UInt] to signal no prediction instead of forcing + 4?
  def setPredictedPc(stage: Stage, pc: UInt): Unit
}

trait TrapService {
  def trap(stage: Stage, cause: TrapCause): Unit
  def hasTrapped(stage: Stage): Bool
  def hasException(stage: Stage): Bool
  def hasInterrupt(stage: Stage): Bool

  type TrapCommitCallback = (Stage, Bool, UInt) => Unit

  /** The given callback will be called at the point where the logic to commit traps is created. It
    * is called during the build() phase which means the callback should be registered during the
    * setup() phase.
    */
  def onTrapCommit(cb: TrapCommitCallback)
}

trait Csr extends Area {
  def read(): UInt

  /** Called for internal writes through the CsrIo interface.
    */
  def write(value: UInt): Unit = assert(false, "Cannot write RO CSR")

  /** Called for software writes (CSR* instructions).
    */
  def swWrite(value: UInt): Unit = write(value)
}

class CsrIo(implicit config: Config) extends Bundle with IMasterSlave {
  val rdata, wdata = UInt(config.xlen bits)
  val write = Bool()

  def read(): UInt = rdata

  def write(value: UInt): Unit = {
    write := True
    wdata := value
  }

  override def asMaster(): Unit = {
    in(wdata, write)
    out(rdata)
  }

  override def asSlave(): Unit = {
    super.asSlave()

    write := False
    write.allowOverride
    wdata.assignDontCare()
    wdata.allowOverride
  }
}

trait CsrService {
  def registerCsr[T <: Csr](id: Int, reg: => T): T
  def getCsr(id: Int): CsrIo
  def csrWriteInCycle(): Bool
  def isCsrInstruction(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool
}

class IrqIo extends Bundle with IMasterSlave {
  val update = Bool()
  val interruptPending = Bool()

  def init(): Unit = {
    if (isMasterInterface) {
      update := False
      interruptPending := False
    }
  }

  def postInterrupt(): Unit = {
    assert(isMasterInterface)

    update := True
    interruptPending := True
  }

  def clearInterrupt(): Unit = {
    assert(isMasterInterface)

    update := True
    interruptPending := False
  }

  override def asMaster(): Unit = {
    out(update, interruptPending)
  }
}

trait InterruptService {
  def getMachineTimerIrqIo: IrqIo
  def getExternalIrqIo: IrqIo
}

case class DataHazardInfo[T <: Data](
    registerType: SpinalEnumElement[RegisterType.type],
    rs1Data: PipelineData[T],
    rs2Data: PipelineData[T],
    rdData: PipelineData[T]
)

trait DataHazardService {
  def addHazard[T <: Data](info: DataHazardInfo[T]): Unit

  /** Stalls pipeline stages until the given hazard is resolved. More specifically, conflicts() is
    * called for each stage. Its first argument is the stage itself, the second one is a sequence of
    * all stages holding non-retired earlier instructions. When there is a potential conflict (e.g.,
    * the stage reads a register that will be written by one of the later stages) conflicts() should
    * return True and the stage will be stalled.
    *
    * Note: this function should be called from Plugin.finish().
    */
  def resolveHazard(conflicts: (Stage, Seq[Stage]) => Bool)
}

trait FormalService {
  def lsuDefault(stage: Stage)
  def lsuOnLoad(stage: Stage, addr: UInt, rmask: Bits, rdata: UInt)
  def lsuOnStore(stage: Stage, addr: UInt, wmask: Bits, wdata: UInt)
  def lsuOnMisaligned(stage: Stage): Unit
}

trait Resettable {
  def pipelineReset(): Unit
}
