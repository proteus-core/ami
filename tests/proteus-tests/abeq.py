#!/usr/bin/env python3

import sys
import MimicryTest

class ibeq(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark0 = vcd.get_mark(0x00)
    t = mark0.WB[0]
    self.assertTrue(self.is_invert(vcd, t))
    self.assertTrue(self.is_taken(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 0)

    t = vcd.WB2[mark0.addr+4][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    mark1 = vcd.get_mark(0x01)
    t = mark1.WB[0]
    self.assertTrue(self.is_invert(vcd, t))
    self.assertFalse(self.is_taken(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    t = vcd.WB2[mark1.addr+4][0]
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 0)

if __name__ == '__main__':
  ibeq(len(sys.argv) > 1)
