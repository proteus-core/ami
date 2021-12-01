import ProteusVCD
import util

#############################################################################
class ProteusTest:

  ###########################################################################
  def __init__(self, verbose):
    self.verbose = verbose
    vcdname = "%s.vcd" % self.__class__.__name__
    self.run(ProteusVCD.ProteusVCD(vcdname, []))

  ###########################################################################
  def assertEqual(self, l, r):
    assert l == r, (l, r)

  ###########################################################################
  def assertTrue(self, v):
    assert v

  ###########################################################################
  def assertFalse(self, v):
    assert not v

  #########################################################################
  def run(self, vcd):
    assert False, 'Abstract method not implemented'
