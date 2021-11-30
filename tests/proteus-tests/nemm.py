#!/usr/bin/env python3

import sys
import MimicryTest

class nemm(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    m_addr = vcd.get_marked_instr_addr()

    tn = vcd.nextt(t)
    depth = vcd.as_int(vcd.CSR.CsrFile_depth, tn)

    if pc == m_addr:
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(depth, 0)

    if pc == m_addr+4:
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(depth, 1)

    if pc == m_addr+8:
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(depth, 2)

    if pc == m_addr+12:
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(depth, 1)

    if pc == m_addr+16:
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(depth, 0)

    if pc == m_addr+20:
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(depth, 0)

if __name__ == '__main__':
  nemm(len(sys.argv) > 1)
