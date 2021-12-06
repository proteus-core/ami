#!/usr/bin/env python3

import sys
import MimicryTest

class paddi(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()
    t = mark.WB[0]
    self.assertTrue(self.is_persistent(vcd, t))
    self.assertEqual(vcd.x5(vcd.nextt(t)), 1)

    t = vcd.WB2[mark.addr+4][0]
    self.assertTrue(self.is_persistent(vcd, t))
    self.assertEqual(vcd.x5(vcd.nextt(t)), 2)

if __name__ == '__main__':
  paddi(len(sys.argv) > 1)
