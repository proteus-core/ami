#!/usr/bin/env python3

import sys
import MimicryTest

class mmaddi(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()

    t = mark.WB[0]
    self.assertEqual(vcd.x5(vcd.nextt(t)), 0)
    self.assertEqual(vcd.x6(vcd.nextt(t)), 0)
    self.assertEqual(vcd.x7(vcd.nextt(t)), 0)

    t = vcd.WB2[mark.addr+4][0]
    self.assertEqual(vcd.x5(vcd.nextt(t)), 0)
    self.assertEqual(vcd.x6(vcd.nextt(t)), 0)
    self.assertEqual(vcd.x7(vcd.nextt(t)), 0)

    t = vcd.WB2[mark.addr+8][0]
    self.assertEqual(vcd.x5(vcd.nextt(t)), 0)
    self.assertEqual(vcd.x6(vcd.nextt(t)), 0)
    self.assertEqual(vcd.x7(vcd.nextt(t)), 1)

    """
    if pc == m_addr + 16:
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 1)
      self.assertEqual(x7, 1)
    """

if __name__ == '__main__':
  mmaddi(len(sys.argv) > 1)
