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
    self.assertEqual(self.mm_entry(vcd, tn), mark1.addr)
    self.assertEqual(self.mm_exit(vcd, tn), mark1.addr+4)

    # Mark 2
    mark2 = vcd.get_mark(0x02)

    t = mark2.WB[0]
    self.assertTrue(self.is_ajump(vcd, t))
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 0)

    tn = vcd.nextt(t)
    self.assertTrue(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_AC(vcd, tn), 1)
    self.assertEqual(self.mm_entry(vcd, tn), mark2.addr)
    self.assertEqual(self.mm_exit(vcd, tn), mark2.addr+4)

    # Mark 11
    mark11 = vcd.get_mark(0x11)

    t = mark11.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark1.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark1.addr+4)

    # Mark 22
    mark22 = vcd.get_mark(0x22)

    t = mark22.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark2.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark2.addr+4)
      
    # Mark 222 - Non-recursive call
    mark222 = vcd.get_mark(0x222)

    t = mark222.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark2.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark2.addr+4)

    # Mark 222 - Recursive call
    mark222 = vcd.get_mark(0x222)

    t = mark222.WB[1]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark2.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark2.addr+4)

    # Mark 33x - Tests recursive calls with AC > 1
    mark330 = vcd.get_mark(0x330)
    mark331 = vcd.get_mark(0x331)
    mark332 = vcd.get_mark(0x332)
    assert(len(mark330.WB) == 3)
    assert(len(mark331.WB) == 2)
    assert(len(mark332.WB) == 3)

    # Mark 330 (start)
    t = mark330.WB[0]
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 0)
    self.assertEqual(self.mm_entry(vcd, t), 0x7FFFFFFF)
    self.assertEqual(self.mm_exit(vcd, t), 0x7FFFFFFF)

    t = mark330.WB[1]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark331.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark331.addr+4)

    t = mark330.WB[2]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 2)
    self.assertEqual(self.mm_entry(vcd, t), mark331.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark331.addr+4)

    # Mark 332 (return)
    t = mark332.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 2)
    self.assertEqual(self.mm_entry(vcd, t), mark331.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark331.addr+4)

    t = mark332.WB[1]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 1)
    self.assertEqual(self.mm_entry(vcd, t), mark331.addr)
    self.assertEqual(self.mm_exit(vcd, t), mark331.addr+4)

    t = mark332.WB[2]
    self.assertFalse(self.in_mm(vcd, t))
    self.assertEqual(self.mm_AC(vcd, t), 0)
    self.assertEqual(self.mm_entry(vcd, t), 0x7FFFFFFF)
    self.assertEqual(self.mm_exit(vcd, t), 0x7FFFFFFF)

if __name__ == '__main__':
  mj(len(sys.argv) > 1)
