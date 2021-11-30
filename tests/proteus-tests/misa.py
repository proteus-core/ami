#!/usr/bin/env python3

import sys
import MimicryTest

class misa(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, t, pc):

    if pc == vcd.get_addr_of_marked_instr():
      X = 23
      v = vcd.as_int(vcd.CSR.CsrFile_extensions, t) & (1 << X)
      self.assertTrue(v != 0)

if __name__ == '__main__':
  misa(len(sys.argv) > 1)
