#!/usr/bin/env python3

import sys
import MimicryTest

class nemm(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()
    t = mark.WB[0]
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 0)

    t = vcd.WB2[mark.addr+4][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    t = vcd.WB2[mark.addr+8][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 2)

    t = vcd.WB2[mark.addr+12][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    t = vcd.WB2[mark.addr+16][0]
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 0)

if __name__ == '__main__':
  nemm(len(sys.argv) > 1)
