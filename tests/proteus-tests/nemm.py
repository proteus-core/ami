#!/usr/bin/env python3

import sys
import MimicryTest

class nemm(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    tn = vcd.nextt(t)

    if pc == m_addr:
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(self.mm_depth(vcd, tn), 0)

    if pc == m_addr+4:
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(self.mm_depth(vcd, tn), 1)

    if pc == m_addr+8:
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(self.mm_depth(vcd, tn), 2)

    if pc == m_addr+12:
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(self.mm_depth(vcd, tn), 1)

    if pc == m_addr+16:
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(self.mm_depth(vcd, tn), 0)

    if pc == m_addr+20:
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(self.mm_depth(vcd, tn), 0)

if __name__ == '__main__':
  nemm(len(sys.argv) > 1)
