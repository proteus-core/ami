#!/usr/bin/env python3

import sys
import MimicryTest

class ite(MimicryTest.MimicryTest):

  def run(self, vcd):

    # Mark 0
    mark0 = vcd.get_mark(0)
    t = mark0.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+4][0]
    self.assertEqual(vcd.as_int(vcd.WB.out_RD_TYPE, t), 1)
    tn = vcd.nextt(t)
    self.assertFalse(self.in_mm(vcd, tn))

    # Mark 1
    mark1 = vcd.get_mark(1)
    t = mark1.WB[0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark1.addr+4][0]
    self.assertFalse(self.in_mm(vcd, t))

    # Mark 2
    mark2 = vcd.get_mark(2)
    t = mark2.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    t = vcd.WB2[mark2.addr+8][0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark2.addr+12][0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark2.addr+16][0]
    self.assertFalse(self.in_mm(vcd, t))

    # Mark 3
    mark3 = vcd.get_mark(3)
    t = mark3.WB[0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark3.addr+4][0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark3.addr+8][0]
    self.assertTrue(self.in_mm(vcd, t))
    t = vcd.WB2[mark3.addr+12][0]
    self.assertEqual(vcd.as_int(vcd.WB.out_RD_TYPE, t), 1)

if __name__ == '__main__':
  ite(len(sys.argv) > 1)
