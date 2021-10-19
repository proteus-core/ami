import re
import subprocess
import tempfile
from vcdvcd import VCDVCD
from types import SimpleNamespace
 
#############################################################################
class NestedNamespace(SimpleNamespace):
  def __init__(self, dictionary, **kwargs):
    super().__init__(**kwargs)
    for key, value in dictionary.items():
      if isinstance(value, dict):
        self.__setattr__(key, NestedNamespace(value))
      else:
        self.__setattr__(key, value)

#############################################################################
class ProteusTest:

  ###########################################################################
  def __init__(self, verbose):
    vcdname = "%s.vcd" % self.__class__.__name__
    self.verbose = verbose

    # Build the signal namespace
    self.vcd = VCDVCD(vcdname, only_sigs=True)
    self.TOP = self.build_signal_namespace()
    self.PL  = self.TOP.Core.pipeline_1
    self.ID  = self.TOP.Core.pipeline_1.decode
    self.WB  = self.TOP.Core.pipeline_1.writeback
    self.EX  = self.TOP.Core.pipeline_1.execute
    self.MEM = self.TOP.Core.pipeline_1.memory
    self.RF  = self.TOP.Core.pipeline_1.RegisterFileAccessor

    # Load the data for the signals we are interested in. The empty list selects
    # all signals.
    signals = [
    ]
    self.vcd = VCDVCD(vcdname, signals=signals)
    if len(signals) > 0:
      assert set(signals) == set(self.vcd.signals), "Missing signals"

    # Get marker info
    self.m_epoch = self.find_marker()
    assert self.m_epoch, "Marker not found"
    self.m_addr = self.as_int(self.vcd, self.PL.fetch_out_PC, self.m_epoch)
    m_ir = self.as_bytes(self.vcd, self.PL.fetch_out_IR, self.m_epoch)
    m_instr = self.disassemble(m_ir)
    #assert m_instr == "addi t0,t0,1", "Unexpected instr: '%s'" % m_instr
    if self.verbose:
      print("MARKER: EPOCH=%d ADDR=%08x INSTR=%s" % \
                                      (self.m_epoch, self.m_addr, m_instr))
    self.run()

  ###########################################################################
  def disassemble(self, bytez):
  
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
  def as_int(self, vcd, signal, time):
    return int(vcd[signal][time], 2)

  ###########################################################################
  def as_bytes(self, vcd, signal, time):
    ident = vcd.references_to_ids[signal]
    assert ident != None, "Invalid signal: '%s'" % signal
    size = int(vcd.data[ident].size)
    return int(vcd[signal][time], 2).to_bytes((size+7)//8, 'little')

  ###########################################################################
  def assertEqual(self, l, r):
    assert l == r, (l, r)

  #########################################################################
  def run(self):
    # ID
    spc = self.vcd[self.TOP.Core.pipeline_1.decode_out_PC]
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= self.m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.decode_out_IR, t)
        instr = self.disassemble(instr)
      self.on_change_decode_pc(self.vcd, self.m_addr, t, t+10, pc)

    # EX
    spc = self.vcd[self.TOP.Core.pipeline_1.execute_out_PC]
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= self.m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.execute_out_IR, t)
        instr = self.disassemble(instr)
      self.on_change_execute_pc(self.vcd, self.m_addr, t, t+10, pc)

    # MEM
    spc = self.vcd[self.TOP.Core.pipeline_1.memory_out_PC]
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= self.m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.memory_out_IR, t)
        instr = self.disassemble(instr)
      self.on_change_memory_pc(self.vcd, self.m_addr, t, t+10, pc)

    # WB
    spc = self.vcd[self.TOP.Core.pipeline_1.writeback_out_PC]
    for t, pc in [(t, int(v, 2)) for (t, v) in spc.tv if t >= self.m_epoch]:
      if self.verbose:
        instr = self.as_bytes(self.vcd, self.PL.writeback_out_IR, t)
        instr = self.disassemble(instr)
        print("%d %08x %s" % (t, pc, instr))
      self.on_change_writeback_pc(self.vcd, self.m_addr, t, t+10, pc)

  #########################################################################
  def on_change_decode_pc(self, vcd, m_addr, t, tn, pc):
    pass

  #########################################################################
  def on_change_execute_pc(self, vcd, m_addr, t, tn, pc):
    pass

  #########################################################################
  def on_change_memory_pc(self, vcd, m_addr, t, tn, pc):
    pass

  #########################################################################
  def on_change_writeback_pc(self, vcd, m_addr, t, tn, pc):
    pass

