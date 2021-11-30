#!/usr/bin/env python3

import sys
import MimicryTest

class abeq(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):
    
    mark0 = vcd.get_marked_instr_addr(0x00)
    mark1 = vcd.get_marked_instr_addr(0x01)
    mark2 = vcd.get_marked_instr_addr(0x02)

    is_activate = vcd.as_int(vcd.WB.value_ACTIVATE, t) == 1
    is_taken = vcd.as_int(vcd.WB.value_OUTCOME, t) == 1
    depth = vcd.as_int(vcd.CSR.CsrFile_depth, t)

    if pc == mark0:
      self.assertTrue(is_activate)
      self.assertTrue(is_taken)
      self.assertFalse(self.in_mm(vcd, t))
      self.assertEqual(depth, 0)

    if pc == mark0 + 4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

    if pc == mark1:
      self.assertTrue(is_activate)
      self.assertFalse(is_taken)
      self.assertEqual(depth, 1)

    if pc == mark1 + 4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

    if pc == mark2:
      self.assertTrue(is_activate)
      self.assertTrue(is_taken)
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

    if pc == mark2 + 4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 2)

if __name__ == '__main__':
  abeq(len(sys.argv) > 1)
