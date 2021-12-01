#!/usr/bin/env python3

import sys
import MimicryTest

class abeq(MimicryTest.MimicryTest):

  def run(self, vcd):
    
    # Mark 0
    mark0 = vcd.get_mark(0x00)
    t = mark0.WB[0]
    self.assertTrue(self.is_activate(vcd, t))
    self.assertTrue(self.is_taken(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 0)

    t = vcd.WB2[mark0.addr+4][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    # Mark 1
    mark1 = vcd.get_mark(0x01)
    t = mark1.WB[0]
    self.assertTrue(self.is_activate(vcd, t))
    self.assertFalse(self.is_taken(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    t = vcd.WB2[mark1.addr+4][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    # Mark 2
    mark2 = vcd.get_mark(0x02)
    t = mark2.WB[0]
    self.assertTrue(self.is_activate(vcd, t))
    self.assertTrue(self.is_taken(vcd, t))
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)

    t = vcd.WB2[mark2.addr+4][0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 2)

if __name__ == '__main__':
  abeq(len(sys.argv) > 1)
