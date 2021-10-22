import re
import subprocess
import tempfile
from vcdvcd import VCDVCD
from types import SimpleNamespace

# TODO: Split into different modules

###########################################################################
# TODO: Move to utility library
def disassemble(bytez):

  assert len(bytez) == 4

  with tempfile.NamedTemporaryFile() as f:
    f.write(bytez)
    f.flush()

    objdump  = "riscv64-unknown-elf-objdump"
    objdump += " -D"
    objdump += " -b binary"
    objdump += " -m riscv"
    objdump += " -M no-aliases"
    objdump += " %s" % f.name

    proc = subprocess.run(objdump.split(),
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE,
                          universal_newlines=True)

    if proc.returncode == 0:
      for line in proc.stdout.split('\n'):
        parts = line.split()
        if len(parts) >= 3 and parts[0] == '0:':
          return ' '.join(parts[2:])

    return bytez.tostring()
 
#############################################################################
# TODO: Move to utility library
class NestedNamespace(SimpleNamespace):
  def __init__(self, dictionary, **kwargs):
    super().__init__(**kwargs)
    for key, value in dictionary.items():
      if isinstance(value, dict):
        self.__setattr__(key, NestedNamespace(value))
      else:
        self.__setattr__(key, value)

#############################################################################
# VCDVCD wrapper
class MyVCDVCD:

  ###########################################################################
  def __init__(self, vcdname, signals=[]):
    # Build the signal namespace
    self.vcd = VCDVCD(vcdname, only_sigs=True)
    self.TOP = self.build_signal_namespace()
    self.PL  = self.TOP.Core.pipeline_1
    self.ID  = self.TOP.Core.pipeline_1.decode
    self.WB  = self.TOP.Core.pipeline_1.writeback
    self.EX  = self.TOP.Core.pipeline_1.execute
    self.MEM = self.TOP.Core.pipeline_1.memory
    self.RF  = self.TOP.Core.pipeline_1.RegisterFileAccessor

    # Load the data for the signals we are interested in.
    # The empty list selects all signals.
    self.vcd = VCDVCD(vcdname, signals)
    if len(signals) > 0:
      assert set(signals) == set(self.vcd.signals), "Missing signals"

    # Get marker info
    self.m_epoch = self.find_marker()
    assert self.m_epoch, "Marker not found"
    self.m_addr = self.as_int(self.PL.fetch_out_PC, self.m_epoch)
    m_ir = self.as_bytes(self.PL.fetch_out_IR, self.m_epoch)
    m_instr = disassemble(m_ir)
    #assert m_instr == "addi t0,t0,1", "Unexpected instr: '%s'" % m_instr
    print("MARKER: EPOCH=%d ADDR=%08x INSTR=%s" % \
                                        (self.m_epoch, self.m_addr, m_instr))

  ###########################################################################
  def build_signal_namespace(self):
    signal_dict = {}
    d = signal_dict
    for l in [x.split(".") for x in self.vcd.signals]:
      d = signal_dict
      for component in l[1:-1]:
        if not component in d:
          d[component] = {}
        d = d[component]
      n = re.sub(r'\[.*\]', '', l[-1])
      d[n] = '.'.join(l)
    return NestedNamespace(signal_dict)

  ###########################################################################
  def find_marker(self):
    signal = self.PL.decode_out_MARK
    for t, v in [(t, int(v, 2)) for (t, v) in self.vcd[signal].tv]:
      if v == 1:
        return t
    return None

  ###########################################################################
  def signal(self, name):
    return self.vcd[name]

  ###########################################################################
  def as_int(self, signal, time):
    return int(self.vcd[signal][time], 2)

  ###########################################################################
  def as_bytes(self, signal, time):
    ident = self.vcd.references_to_ids[signal]
    assert ident != None, "Invalid signal: '%s'" % signal
    size = int(self.vcd.data[ident].size)
    return int(self.vcd[signal][time], 2).to_bytes((size+7)//8, 'little')

  ###########################################################################
  """
  Returns time t, n clock cycles later
  """
  def nextt(self, t, n=1):
    return t + (n * 10)

  #########################################################################
  def x5(self, t):
    return self.as_int(self.RF.x5_t0, t)

  #########################################################################
  def x6(self, t):
    return self.as_int(self.RF.x6_t1, t)

  #########################################################################
  def x7(self, t):
    return self.as_int(self.RF.x7_t2, t)

#############################################################################
class ProteusTest:

  ###########################################################################
  def __init__(self, verbose):
    vcdname = "%s.vcd" % self.__class__.__name__
    self.verbose = verbose
    self.vcd = MyVCDVCD(vcdname, [])
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
        instr = disassemble(instr)
      self.on_change_decode_pc(self.vcd, m_addr, t, pc)

    # EX
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.execute_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.execute_out_IR, t)
        instr = disassemble(instr)
      self.on_change_execute_pc(self.vcd, m_addr, t, pc)

    # MEM
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.memory_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.memory_out_IR, t)
        instr = disassemble(instr)
      self.on_change_memory_pc(self.vcd, m_addr, t, pc)

    # WB
    spc = vcd.signal(vcd.TOP.Core.pipeline_1.writeback_out_PC)
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.writeback_out_IR, t)
        instr = disassemble(instr)
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
