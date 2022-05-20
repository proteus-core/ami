import ProteusTest

MIMIC_REG_TYPE = 2

"""
Helper class for mimicry mode regression tests
"""
class MimicryTest(ProteusTest.ProteusTest):

  """
  Returns true if the inMimicryMode signal is set at time t
  """
  def mm_depth(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_depth, t)

  def mm_pdepth(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_pdepth, t)

  def in_mm(self, vcd, t): 
    return self.mm_depth(vcd, t) > 0

  def mm_entry(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_entry, t)

  def mm_exit(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_exit, t)

  def is_mimic(self, vcd, t):
    return vcd.as_int(vcd.WB.out_MIMIC, t) == 1

  def is_ghost(self, vcd, t):
    return vcd.as_int(vcd.WB.out_GHOST, t) == 1

  def is_persistent(self, vcd, t):
    return vcd.as_int(vcd.WB.out_PERSISTENT, t) == 1

  def is_ajump(self, vcd, t):
    return vcd.as_int(vcd.WB.out_AJUMP, t) == 1

  def is_abranch(self, vcd, t):
    return vcd.as_int(vcd.WB.out_ABRANCH, t) == 1

  def is_taken(self, vcd, t):
    return vcd.as_int(vcd.WB.out_OUTCOME, t) == 1
