package riscv.plugins.mimicry

import riscv._
import spinal.core._
import spinal.lib.slave

class DynamicMimicry(exeStages: Seq[Stage]) extends Plugin[Pipeline] with MimicryService {

  override def getImplementedExtensions = Seq('X')

  // The mimicry state is stored in CSRs (activation counter, entry address, and
  //  exit address) with the following idendifiers:
  private val CSR_MMAC = 0x7ff
  private val CSR_MMENTRY = 0x7df // CSR identifier
  private val CSR_MMEXIT = 0x7ef // CSR identifier

  private val CSR_MMADDR_NONE = 0x7fffffff // TODO: which value?

  def isJump(ir: UInt): Bool = {
    (ir === Opcodes.JAL) ||
    (ir === Opcodes.JALR)
  }

  def isConditional(ir: UInt): Bool = {
    (ir === Opcodes.BEQ) ||
    (ir === Opcodes.BNE) ||
    (ir === Opcodes.BLT) ||
    (ir === Opcodes.BGE) ||
    (ir === Opcodes.BLTU) ||
    (ir === Opcodes.BGEU)
  }

  // TODO: Prefix with MM
  private object Data {
    // Mimic execution
    object GHOST extends PipelineData(Bool()) // Ghost instruction
    object MIMIC extends PipelineData(Bool()) // Always mimic
    object PERSISTENT extends PipelineData(Bool()) // Always execute

    // Activating control-flow
    object AJUMP extends PipelineData(Bool()) // Activating jump
    object ABRANCH extends PipelineData(Bool()) // Activating branch
    object OUTCOME extends PipelineData(Bool()) // Branch outcome

    // Constant-timeness
    object CTBRANCH extends PipelineData(Bool()) // CT conditional branch

    // CSR data
    object MMEXIT extends PipelineData(UInt(32 bits)) // Mimicry exit address
    object MMENTRY extends PipelineData(UInt(32 bits)) // Mimicry entry address
    object MMAC extends PipelineData(UInt(32 bits)) // Activation counter
    object MM_WRITE_AC extends PipelineData(Bool()) // Update AC?
    object MM_WRITE_BOUNDS extends PipelineData(Bool()) // Update entry/exit?
  }

  // Mimicry mode entry
  private class MmEntry(implicit config: Config) extends Csr {

    val entry = Reg(UInt(config.xlen bits)).init(CSR_MMADDR_NONE)

    override def read(): UInt = entry

    override def write(addr: UInt): Unit = entry := addr
  }

  // Mimicry mode exit
  private class MmExit(implicit config: Config) extends Csr {

    val exit = Reg(UInt(config.xlen bits)).init(CSR_MMADDR_NONE)

    override def read(): UInt = exit

    override def write(addr: UInt): Unit = exit := addr
  }

  // Mimicry mode activation counter (AC)
  private class MmAC(implicit config: Config) extends Csr {
    val AC = Reg(UInt(config.xlen bits)).init(0)

    override def read(): UInt = AC

    override def write(value: UInt): Unit = AC := value
  }

  override def setup(): Unit = {

    val csrService = pipeline.service[CsrService]
    csrService.registerCsr(CSR_MMAC, new MmAC)
    csrService.registerCsr(CSR_MMENTRY, new MmEntry)
    csrService.registerCsr(CSR_MMEXIT, new MmExit)

    pipeline.service[DecoderService].configure { config =>
      config.addDefault(
        Map(
          // Mimic execution
          Data.GHOST -> False,
          Data.MIMIC -> False,
          Data.PERSISTENT -> False,

          // Activating control-flow
          Data.AJUMP -> False,
          Data.ABRANCH -> False,
          Data.OUTCOME -> False,

          // Constant-timeness
          Data.CTBRANCH -> False,

          // CSR updates
          Data.MM_WRITE_AC -> False,
          Data.MM_WRITE_BOUNDS -> False,
          Data.MMAC -> U(0),
          Data.MMENTRY -> U(0),
          Data.MMEXIT -> U(0)
        )
      )

      val stage = pipeline.retirementStage

      config.setIrMapper((stage, ir) => {
        val result = ir | 3
        val conditional = isConditional(result)
        val jump = isJump(result)

        switch(ir(1 downto 0)) {
          is(0) {
            when(conditional) {
              stage.output(Data.CTBRANCH) := True
            } otherwise {
              stage.output(Data.GHOST) := True
            }
          }
          is(1) {
            when(jump) {
              stage.output(Data.AJUMP) := True
            } elsewhen (conditional) {
              stage.output(Data.ABRANCH) := True
            } otherwise {
              stage.output(Data.MIMIC) := True
            }
          }
          is(2) {
            stage.output(Data.PERSISTENT) := True
          }
          is(3) {
            when(jump) {
              // Jumps are persistent by default, i.e., rd is always written
              stage.output(Data.PERSISTENT) := True
            }
          }
        }

        result
      })
    }

    pipeline.service[BranchService].onBranch { (stage, _, taken) =>
      when(!taken && stage.value(Data.CTBRANCH)) {
        stage.arbitration.jumpRequested := True // probably not needed
        pipeline.service[JumpService].jumpRequested(stage) := True
        //        pipeline.service[FetchService].flushCache(stage)
      }
    }

    // Since the CSR forwarding logic only accesses these WB output in the finish() phase, they are
    // not automatically routed (because the routing uses the information available *before*
    // finish()). Therefore, we have to manually inform the router that these outputs will be
    // needed.
    for (stage <- pipeline.stages) {
      stage.output(Data.MM_WRITE_BOUNDS)
      stage.output(Data.MM_WRITE_AC)
      stage.output(Data.MMAC)
      stage.output(Data.MMENTRY)
      stage.output(Data.MMEXIT)
      stage.output(Data.CTBRANCH)
      stage.output(Data.AJUMP)
      stage.output(Data.ABRANCH)
      stage.output(Data.OUTCOME)
      stage.output(Data.GHOST)
      stage.output(Data.MIMIC)
      stage.output(Data.PERSISTENT)
    }

    if (config.debug) {
      // Needed for the tests
    }
  }

  override def build(): Unit = {
    val ret = pipeline.retirementStage
    ret plug new Area {
      when(ret.value(Data.ABRANCH)) {
        pipeline.service[JumpService].disableJump(ret)
      }
    }

    val wbStage = pipeline.retirementStage

    val csrArea = wbStage plug new Area {
      import wbStage._

      val mmAC = slave(new CsrIo)
      val mmentry = slave(new CsrIo)
      val mmexit = slave(new CsrIo)

      when(arbitration.isDone) {
        mmentry.write(value(Data.MMENTRY))
        mmexit.write(value(Data.MMEXIT))
        mmAC.write(value(Data.MMAC))
      }
    }

    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]
      csrArea.mmAC <> csrService.getCsr(CSR_MMAC)
      csrArea.mmentry <> csrService.getCsr(CSR_MMENTRY)
      csrArea.mmexit <> csrService.getCsr(CSR_MMEXIT)
    }
  }

  override def inputMeta(regs: PipelineRegs, mmac: UInt, mmen: UInt, mmex: UInt): Unit = {
    regs.setReg(Data.MMAC, mmac)
    regs.setReg(Data.MMENTRY, mmen)
    regs.setReg(Data.MMEXIT, mmex)
  }

  override def inputMeta(stage: Stage, mmac: UInt, mmen: UInt, mmex: UInt): Unit = {
    stage.input(Data.MMAC) := mmac
    stage.input(Data.MMENTRY) := mmen
    stage.input(Data.MMEXIT) := mmex
  }

  override def getMeta(stage: Stage): (UInt, UInt, UInt) = {
    (stage.output(Data.MMAC), stage.output(Data.MMENTRY), stage.output(Data.MMEXIT))
  }

  override def isGhost(stage: Stage): Bool = {
    stage.output(Data.GHOST)
  }

  override def isPersistent(stage: Stage): Bool = {
    stage.output(Data.PERSISTENT)
  }

  override def isMimic(stage: Stage): Bool = {
    stage.output(Data.MIMIC)
  }

  override def isABranch(stage: Stage): Bool = {
    stage.output(Data.ABRANCH)
  }

  override def isAJump(stage: Stage): Bool = {
    stage.output(Data.AJUMP)
  }

  override def isActivating(stage: Stage): Bool = {
    isAJump(stage) || isABranch(stage)
  }

  override def acOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): UInt = {
    bundle.elementAs[UInt](Data.MMAC.asInstanceOf[PipelineData[Data]])
  }

  override def enOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): UInt = {
    bundle.elementAs[UInt](Data.MMENTRY.asInstanceOf[PipelineData[Data]])
  }

  override def exOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): UInt = {
    bundle.elementAs[UInt](Data.MMEXIT.asInstanceOf[PipelineData[Data]])
  }

  override def isCtBranchOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Bool = {
    bundle.elementAs[Bool](Data.CTBRANCH.asInstanceOf[PipelineData[Data]])
  }

  override def isSensitiveBranch(stage: Stage): Bool = {
    stage.output(Data.CTBRANCH) || stage.output(Data.ABRANCH)
  }

  override def determineOutcomes(
      mmac: UInt,
      mmen: UInt,
      mmex: UInt,
      pc: UInt,
      nextPc: UInt,
      ajump: Bool,
      abranch: Bool,
      branchTaken: Bool,
      mimic: Bool,
      ghost: Bool,
      persistent: Bool
  ): (UInt, UInt, UInt, Bool) = {
    val reactivation = False
    val mimicked = Bool()
    mimicked := False

    val outac = UInt(config.xlen bits)
    outac := mmac
    val outen = UInt(config.xlen bits)
    outen := mmen
    val outex = UInt(config.xlen bits)
    outex := mmex

    val isExit = (mmac === 1) && (pc === mmex)

    // 1) Is mimicry mode disabled?
    when((mmac === 0) || isExit) {
      // 1.1) Are we dealing with an activating jump? 1.2) Are we dealing with an activating branch?
      when(ajump || (abranch && branchTaken)) {
        reactivation := True

        outen := pc
        outex := nextPc

        outac := 1
      }
    }

    // 2) Is the current program counter registered as the entry address?
    when(pc === mmen && mmac > 0) {
      outac := mmac + 1
    }

    // 3) Is the current program counter registered as the exit address?
    when(!reactivation) {
      when(pc === mmex && mmac > 0) {
        when(mmac === 1) {
          // We are exiting mimicry mode
          outen := CSR_MMADDR_NONE
          outex := CSR_MMADDR_NONE
        }

        outac := mmac - 1
      }
    }

    // 4) Do we need to mimic the execution?
    when(mimic) {
      mimicked := True
    }

    when(ghost) {
      when((mmac === 0) || isExit) {
        mimicked := True
      }
    } elsewhen (!persistent) {
      when((mmac > 0) && (!isExit)) {
        mimicked := True
      }
    }
    (outac, outen, outex, mimicked)
  }
}
