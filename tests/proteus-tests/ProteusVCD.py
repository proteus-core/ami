import re
import vcdvcd
import util

#############################################################################
class Marker:

  ###########################################################################
  def __init__(self, epoch, addr, n):
    self.epoch = epoch
    self.addr  = addr
    self.n     = n

#############################################################################
class ProteusVCD:

  ###########################################################################
  def __init__(self, vcdname, signals=[]):
    # Build the signal namespace
    self.vcd = vcdvcd.VCDVCD(vcdname, only_sigs=True)
    self.TOP = self.build_signal_namespace()
    self.PL  = self.TOP.Core.pipeline_1
    self.ID  = self.TOP.Core.pipeline_1.decode
    self.WB  = self.TOP.Core.pipeline_1.writeback
    self.EX  = self.TOP.Core.pipeline_1.execute
    self.MEM = self.TOP.Core.pipeline_1.memory
    self.RF  = self.TOP.Core.pipeline_1.RegisterFileAccessor
    self.RF  = self.TOP.Core.pipeline_1.RegisterFileAccessor
    self.CSR = self.TOP.Core.pipeline_1.CsrFile_1

    # Load the data for the signals we are interested in.
    # The empty list selects all signals.
    self.vcd = vcdvcd.VCDVCD(vcdname, signals)
    if len(signals) > 0:
      assert set(signals) == set(self.vcd.signals), "Missing signals"

    self.init_marker_info()

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
  def locate_markers(self):
    result = []  
    signal = self.PL.decode_out_MARK
    for t, v in [(t, int(v, 2)) for (t, v) in self.vcd[signal].tv]:
      if v == 1:
        result.append(t)
    return result

  ###########################################################################
  def init_marker_info(self):
    self.markers = {}
    l = self.locate_markers()
    for t in l:
      addr = self.as_int(self.PL.fetch_out_PC, t)
      n = self.as_int(self.PL.decode_out_IMM, t)
      assert not n in self.markers, "Duplicate marker"
      self.markers[n] = Marker(t, addr, n)

  ###########################################################################
  def get_marker(self, n=0):
    assert n in self.markers, "No such marker: " % n
    return self.markers[n]

  ###########################################################################
  def get_marker_addr(self, n=0):
    return self.markers[n].addr

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

