import ProteusVCD
import util

#############################################################################
class ProteusTest:

  ###########################################################################
  def __init__(self, verbose):
    vcdname = "%s.vcd" % self.__class__.__name__
    self.verbose = verbose
    self.vcd = ProteusVCD.ProteusVCD(vcdname, [])
    self.run()

  #########################################################################
  # TODO: Generate the callbacks
  def run(self):

    vcd = self.vcd
    m_epoch = self.vcd.m_epoch
    m_addr = self.vcd.m_addr

    # ID
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.decode_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.decode_out_IR, t)
        instr = util.disassemble(instr)
      self.on_change_decode_pc(self.vcd, m_addr, t, pc)

    # EX
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.execute_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.execute_out_IR, t)
        instr = util.disassemble(instr)
      self.on_change_execute_pc(self.vcd, m_addr, t, pc)

    # MEM
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.memory_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.memory_out_IR, t)
        instr = util.disassemble(instr)
      self.on_change_memory_pc(self.vcd, m_addr, t, pc)

    # WB
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.writeback_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.writeback_out_IR, t)
        instr = util.disassemble(instr)
        print("%d %08x %s" % (t, pc, instr))
      self.on_change_writeback_pc(self.vcd, m_addr, t, pc)

  #########################################################################
  def on_change_decode_pc(self, vcd, m_addr, t, pc):
    pass

  #########################################################################
  def on_change_execute_pc(self, vcd, m_addr, t, pc):
    pass

  #########################################################################
  def on_change_memory_pc(self, vcd, m_addr, t, pc):
    pass

  #########################################################################
  def on_change_writeback_pc(self, vcd, m_addr, t, pc):
    pass

  ###########################################################################
  def assertEqual(self, l, r):
    assert l == r, (l, r)

  ###########################################################################
  def assertTrue(self, v):
    assert v

  ###########################################################################
  def assertFalse(self, v):
    assert not v
