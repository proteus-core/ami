#!/usr/bin/env python3

import sys
import MimicryTest

class ite(MimicryTest.MimicryTest):

  def run(self, vcd):

    # Mark 0
    mark0 = vcd.get_mark(0x00)
    t = mark0.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+8][0]
    self.assertFalse(self.in_mm(vcd, t))

    # Mark 1
    mark1 = vcd.get_mark(0x01)
    t = mark1.WB[0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+8][0]
    self.assertFalse(self.in_mm(vcd, t))

    # Mark 2
    mark2 = vcd.get_mark(0x02)
    t = mark2.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+8][0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+16][0]
    self.assertFalse(self.in_mm(vcd, t))

    # Mark 3
    mark3 = vcd.get_mark(0x03)
    t = mark3.WB[0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+8][0]
    self.assertFalse(self.in_mm(vcd, t))
    t = vcd.WB2[mark0.addr+16][0]
    self.assertFalse(self.in_mm(vcd, t))

if __name__ == '__main__':
  ite(len(sys.argv) > 1)
