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
