#!/usr/bin/env python3

import sys
import MimicryTest

class mj(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    tn = vcd.nextt(t)

    mark1   = vcd.get_addr_of_marked_instr(0x01)
    mark2   = vcd.get_addr_of_marked_instr(0x02)
    mark11  = vcd.get_addr_of_marked_instr(0x11)
    mark22  = vcd.get_addr_of_marked_instr(0x22)
    mark222 = vcd.get_addr_of_marked_instr(0x222)

    is_mimic = vcd.as_int(vcd.WB.value_MIMIC, t) == 1
    is_jump = vcd.as_int(vcd.WB.value_ISJUMP, t) == 1

    depth  = vcd.as_int(vcd.CSR.CsrFile_depth, t)
    depthn = vcd.as_int(vcd.CSR.CsrFile_depth, tn)

    exit = vcd.as_int(vcd.CSR.CsrFile_exit, t)
    exitn = vcd.as_int(vcd.CSR.CsrFile_exit, tn)

    if pc == mark1:
      self.assertTrue(is_mimic)
      self.assertTrue(is_jump)
      self.assertFalse(self.in_mm(vcd, t))
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(depth, 0)
      self.assertEqual(depthn, 1)
      self.assertEqual(exitn, mark1+4)

    if pc == mark2:
      self.assertTrue(is_mimic)
      self.assertTrue(is_jump)
      self.assertFalse(self.in_mm(vcd, t))
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(depth, 0)
      self.assertEqual(depthn, 1)
      self.assertEqual(exitn, mark2+4)

    if pc == mark11:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)
      self.assertEqual(exit, mark1+4)

    if pc == mark22:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)
      self.assertEqual(exit, mark2+4)
      
    if pc == mark222:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)
      self.assertEqual(exit, mark2+4)


if __name__ == '__main__':
  mj(len(sys.argv) > 1)
