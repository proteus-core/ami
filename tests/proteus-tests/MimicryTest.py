import ProteusTest

"""
Helper class for mimicry mode regression tests
"""
class MimicryTest(ProteusTest.ProteusTest):

  """
  Returns true when the inMimicryMode signal is set at time t
  """
  def inMM(self, vcd, t): 
    return vcd.as_int(vcd.ID.Mimicry_inMimicryMode, t) == 1
