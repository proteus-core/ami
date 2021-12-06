#!/usr/bin/env python3

import sys
import MimicryTest

class mj(MimicryTest.MimicryTest):

  def run(self, vcd):

    # Mark 1
    mark1 = vcd.get_mark(0x01)

    t = mark1.WB[0]
    self.assertTrue(self.is_ajump(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))

    tn = vcd.nextt(t)
    self.assertTrue(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_exit(vcd, tn), mark1.addr+4)

    # Mark 2
    mark2 = vcd.get_mark(0x02)

    t = mark2.WB[0]
    self.assertTrue(self.is_ajump(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 0)

    tn = vcd.nextt(t)
    self.assertTrue(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_depth(vcd, tn), 1)
    self.assertEqual(self.mm_exit(vcd, tn), mark2.addr+4)

    # Mark 11
    mark11 = vcd.get_mark(0x11)

    t = mark11.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)
    self.assertEqual(self.mm_exit(vcd, t), mark1.addr+4)

    # Mark 22
    mark22 = vcd.get_mark(0x22)

    t = mark22.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)
    self.assertEqual(self.mm_exit(vcd, t), mark2.addr+4)
      
    # Mark 222 - Non-recursive call
    mark222 = vcd.get_mark(0x222)

    t = mark222.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)
    self.assertEqual(self.mm_exit(vcd, t), mark2.addr+4)

    # Mark 222 - Recursive call
    mark222 = vcd.get_mark(0x222)

    t = mark222.WB[1]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_depth(vcd, t), 1)
    self.assertEqual(self.mm_exit(vcd, t), mark2.addr+4)

if __name__ == '__main__':
  mj(len(sys.argv) > 1)
