import ProteusTest

"""
Helper class for mimicry mode regression tests
"""
class MimicryTest(ProteusTest.ProteusTest):

  """
  Returns true if the inMimicryMode signal is set at time t
  """
  def mm_depth(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_depth, t)

  def in_mm(self, vcd, t): 
    return self.mm_depth(vcd, t) > 0

  def mm_entry(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_entry, t)

  def mm_exit(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_exit, t)

  def is_mimic(self, vcd, t):
    return vcd.as_int(vcd.WB.value_MIMIC, t) == 1

  def is_ghost(self, vcd, t):
    return vcd.as_int(vcd.WB.value_GHOST, t) == 1

  def is_persistent(self, vcd, t):
    return vcd.as_int(vcd.WB.value_PERSISTENT, t) == 1

  def is_ajump(self, vcd, t):
    return vcd.as_int(vcd.WB.value_AJUMP, t) == 1

  def is_abranch(self, vcd, t):
    return vcd.as_int(vcd.WB.value_ABRANCH, t) == 1

  def is_taken(self, vcd, t):
    return vcd.as_int(vcd.WB.value_OUTCOME, t) == 1
