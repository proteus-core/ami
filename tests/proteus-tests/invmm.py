#!/usr/bin/env python3

import sys
import MimicryTest

class invmm(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    tn = vcd.nextt(t)
    last = vcd.as_int(vcd.CSR.CsrFile_last, tn)
    depth = vcd.as_int(vcd.CSR.CsrFile_depth, tn)

    if pc == m_addr:
      self.assertTrue(last == 0)
      self.assertTrue(depth == 0)

    if pc == m_addr+4:
      self.assertTrue(last == 1)
      self.assertTrue(depth == 1)

    if pc == m_addr+8:
      self.assertTrue(last == 1)
      self.assertTrue(depth == 2)

    if pc == m_addr+12:
      self.assertTrue(last == 0)
      self.assertTrue(depth == 1)

    if pc == m_addr+16:
      self.assertTrue(last == 1)
      self.assertTrue(depth == 2)

    if pc == m_addr+20:
      self.assertTrue(last == 0)
      self.assertTrue(depth == 1)

    if pc == m_addr+24:
      self.assertTrue(last == 0)
      self.assertTrue(depth == 0)

if __name__ == '__main__':
  invmm(len(sys.argv) > 1)
