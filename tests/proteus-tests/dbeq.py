#!/usr/bin/env python3

import sys
import MimicryTest

class dbeq(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    depth = vcd.as_int(vcd.CSR.CsrFile_depth, t)

    if pc == m_addr:
      pass

if __name__ == '__main__':
  dbeq(len(sys.argv) > 1)
