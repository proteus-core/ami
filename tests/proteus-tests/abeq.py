#!/usr/bin/env python3

import sys
import MimicryTest

class abeq(MimicryTest.MimicryTest):

  def run(self, vcd):

    # Mark 0
    mark0 = vcd.get_mark(0x00)
    t = mark0.WB[0]
    self.assertTrue(self.is_abranch(vcd, t))
    self.assertTrue(self.is_taken(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))
    tn =  vcd.nextt(t)
    self.assertTrue(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_entry(vcd, tn), mark0.addr)
    self.assertEqual(self.mm_exit(vcd, tn), mark0.addr+8)

    t = vcd.WB2[mark0.addr+4][0]
    self.assertTrue(self.in_mm(vcd, t))

    t = vcd.WB2[mark0.addr+8][0]
    self.assertEqual(vcd.as_int(vcd.WB.out_RD_TYPE, t), 1)
    tn =  vcd.nextt(t)
    self.assertFalse(self.in_mm(vcd, tn))
    self.assertEqual(vcd.x5(tn), 43)
    self.assertEqual(self.mm_entry(vcd, tn), 0x7FFFFFFF)
    self.assertEqual(self.mm_exit(vcd, tn), 0x7FFFFFFF)

    # Mark 1
    mark1 = vcd.get_mark(0x01)
    t = mark1.WB[0]
    self.assertTrue(self.is_abranch(vcd, t))
    self.assertFalse(self.is_taken(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))
    tn =  vcd.nextt(t)
    self.assertFalse(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_entry(vcd, tn), 0x7FFFFFFF)
    self.assertEqual(self.mm_exit(vcd, tn), 0x7FFFFFFF)

    t = vcd.WB2[mark1.addr+4][0]
    self.assertFalse(self.in_mm(vcd, t))

if __name__ == '__main__':
  abeq(len(sys.argv) > 1)
