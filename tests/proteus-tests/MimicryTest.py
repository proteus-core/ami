import ProteusTest

"""
Helper class for mimicry mode regression tests
"""
class MimicryTest(ProteusTest.ProteusTest):

  """
  Returns true when the inMimicryMode signal is set at time t
  """
  def inMM(self, vcd, t): 
    return self.as_int(vcd, self.ID.Mimicry_inMimicryMode, t) == 1
