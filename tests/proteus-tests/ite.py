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

    # Mark 4
    mark4 = vcd.get_mark(4)
    t = mark4.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark4.addr-8)
    self.assertEqual(self.mm_exit(vcd, t), mark4.addr+16)
    t = vcd.WB2[mark4.addr+8][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark4.addr-8)
    self.assertEqual(self.mm_exit(vcd, t), mark4.addr+16)
    t = vcd.WB2[mark4.addr+12][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark4.addr-8)
    self.assertEqual(self.mm_exit(vcd, t), mark4.addr+16)
    t = vcd.WB2[mark4.addr+16][0]
    tn = vcd.nextt(t)
    self.assertFalse(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_AC(vcd, tn), 0)
    self.assertEqual(self.mm_entry(vcd, tn), 0x7FFFFFFF)
    self.assertEqual(self.mm_exit(vcd, tn), 0x7FFFFFFF)

if __name__ == '__main__':
  ite(len(sys.argv) > 1)
