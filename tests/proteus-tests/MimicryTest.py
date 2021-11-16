import ProteusTest

"""
Helper class for mimicry mode regression tests
"""
class MimicryTest(ProteusTest.ProteusTest):

  """
  Returns true if the inMimicryMode signal is set at time t
  """
  def inMM(self, vcd, t): 
    return vcd.as_int(vcd.CSR.CsrFile_mmime, t) == 1

  """
  Returns activation nesting level at time t
  """
  def mmDepth(self, vcd, t):
    return vcd.as_int(vcd.CSR.CsrFile_mdepth, t)

  """
  Returns true if the instruction in the wb stage at time t is a ghost
  instruction.
  """
  def is_ghost(self, vcd, t):
    return vcd.as_int(vcd.WB.value_GHOST, t) == 1

  """
  Returns true if the instruction in the wb stage at time t is an execute
  instruction.
  """
  def is_execute(self, vcd, t):
    return vcd.as_int(vcd.WB.value_EXECUTE, t) == 1

  """
  Returns true if the instruction in the wb stage at time t is a mimic
  instruction.
  """
  def is_mimic(self, vcd, t):
    return vcd.as_int(vcd.WB.value_MIMIC, t) == 1
