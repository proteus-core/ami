#!/usr/bin/env python3

import sys
import MimicryTest

class gaddi(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()
    t = mark.WB[0]
    tn = vcd.nextt(t)
    self.assertTrue(self.is_ghost(vcd, t))
    self.assertEqual(vcd.x5(tn), 1)

    t = vcd.WB2[mark.addr+4][0]
    tn = vcd.nextt(t)
    self.assertTrue(self.is_ghost(vcd, t))
    self.assertEqual(vcd.x5(tn), 1)

if __name__ == '__main__':
  gaddi(len(sys.argv) > 1)
