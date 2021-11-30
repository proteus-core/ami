#!/usr/bin/env python3

import sys
import MimicryTest

class dbeq(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    mark0 = vcd.get_addr_of_marked_instr(0x00)
    mark1 = vcd.get_addr_of_marked_instr(0x01)

    is_deactivate = vcd.as_int(vcd.WB.value_DEACTIVATE, t) == 1
    is_taken = vcd.as_int(vcd.WB.value_OUTCOME, t) == 1

    depth = vcd.as_int(vcd.CSR.CsrFile_depth, t)

    if pc == mark0:
      self.assertTrue(is_deactivate)
      self.assertTrue(is_taken)
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

    if pc == mark0 + 4:
      self.assertFalse(self.in_mm(vcd, t))
      self.assertEqual(depth, 0)

    if pc == mark1:
      self.assertTrue(is_deactivate)
      self.assertFalse(is_taken)
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

    if pc == mark1 + 4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

if __name__ == '__main__':
  dbeq(len(sys.argv) > 1)
