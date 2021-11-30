#!/usr/bin/env python3

import sys
import MimicryTest

class abeq(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    depth = vcd.as_int(vcd.CSR.CsrFile_depth, t)

    if pc == vcd.get_marked_instr_addr()+4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(depth, 1)

if __name__ == '__main__':
  abeq(len(sys.argv) > 1)
