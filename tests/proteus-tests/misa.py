#!/usr/bin/env python3

import sys
import MimicryTest

class misa(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr:
      X = 23
      v = vcd.as_int(vcd.CSR.CsrFile_extensions, t) & (1 << X)
      self.assertTrue(v != 0)

if __name__ == '__main__':
  misa(len(sys.argv) > 1)
