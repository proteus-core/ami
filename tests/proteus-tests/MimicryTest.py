import ProteusTest

"""
Helper class for mimicry mode regression tests
"""
class MimicryTest(ProteusTest.ProteusTest):

  """
  Returns true if the inMimicryMode signal is set at time t
  """
  def in_mm(self, vcd, t): 
    return vcd.as_int(vcd.CSR.CsrFile_mime, t) == 1

  def mm_depth(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_depth, t)

  def mm_exit(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_exit, t)

  def is_mimic(self, vcd, t):
    return vcd.as_int(vcd.WB.value_MIMIC, t) == 1

  def is_ghost(self, vcd, t):
    return vcd.as_int(vcd.WB.value_GHOST, t) == 1

  def is_persistent(self, vcd, t):
    return vcd.as_int(vcd.WB.value_PERSISTENT, t) == 1

  def is_jump(self, vcd, t):
    return vcd.as_int(vcd.WB.value_ISJUMP, t) == 1

  def is_activate(self, vcd, t):
    return vcd.as_int(vcd.WB.value_ACTIVATE, t) == 1

  def is_deactivate(self, vcd, t):
    return vcd.as_int(vcd.WB.value_DEACTIVATE, t) == 1

  def is_invert(self, vcd, t):
    return vcd.as_int(vcd.WB.value_INVERT, t) == 1

  def is_taken(self, vcd, t):
    return vcd.as_int(vcd.WB.value_OUTCOME, t) == 1
