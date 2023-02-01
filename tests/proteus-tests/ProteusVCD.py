import re
import vcdvcd
import util

#############################################################################
class Mark:

  ###########################################################################
  def __init__(self, n, addr):
    self.n    = n
    self.addr = addr

    self.IF  = []
    self.ID  = []
    self.EX  = []
    self.MEM = []
    self.WB  = []

#############################################################################
class ProteusVCD:

  ###########################################################################
  def __init__(self, vcdname, signals=[]):
    # Build the signal namespace
    self.vcd = vcdvcd.VCDVCD(vcdname, only_sigs=True)
    self.TOP = self.build_signal_namespace()
    self.PL  = self.TOP.Core.pipeline
    self.ID  = self.TOP.Core.pipeline.decode
    self.WB  = self.TOP.Core.pipeline.writeback
    self.EX  = self.TOP.Core.pipeline.execute
    self.MEM = self.TOP.Core.pipeline.memory
    self.RF  = self.TOP.Core.pipeline.RegisterFileAccessor
    self.RF  = self.TOP.Core.pipeline.RegisterFileAccessor
    self.CSR = self.TOP.Core.pipeline.CsrFile

    # Load the data for the signals we are interested in.
    # The empty list selects all signals.
    self.vcd = vcdvcd.VCDVCD(vcdname, signals)
    if len(signals) > 0:
      assert set(signals) == set(self.vcd.signals), "Missing signals"

    self.init_marks()

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
    return util.NestedNamespace(signal_dict)

  ###########################################################################
  def init_marks(self):

    self.marks = {}

    signal = self.PL.clk
    for t, v in [(t, int(v, 2)) for (t, v) in self.vcd[signal].tv]:
      if v == 1:
        is_done = self.as_int(self.PL.decode_arbitration_isDone, t) == 1
        is_mark = self.as_int(self.PL.decode_out_MARK, t) == 1
        if is_done and is_mark:
          n = self.as_int(self.PL.decode_out_IMM, t)
          addr = self.as_int(self.PL.fetch_out_PC, t)
          if not n in self.marks:
            self.marks[n] = Mark(n, addr)

    # WB
    # TODO: Clean this up (unify WB and WB2)
    signal = self.signal(self.TOP.Core.pipeline.writeback_out_PC)
    self.WB2 = {}
    for t, pc in [(t, int(v, 2)) for (t, v) in signal.tv]:
      if not pc in self.WB2:
        self.WB2[pc] = []
      self.WB2[pc].append(t)
      for (n, mark) in self.marks.items():
        if mark.addr == pc:
          mark.WB.append(t)

  ###########################################################################
  def get_mark(self, n=0):
    assert n in self.marks, "No such mark: %d" % n 
    return self.marks[n]

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

  #########################################################################
  def t0(self, t):
    return self.x5(t)

  #########################################################################
  def t1(self, t):
    return self.x6(t)
