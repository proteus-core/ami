#!/usr/bin/env python3

import sys
import MimicryTest

class maddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    x5 = vcd.x5(vcd.nextt(t))
    x6 = vcd.x6(vcd.nextt(t));
    x7 = vcd.x7(vcd.nextt(t));

    if pc in (m_addr, m_addr+4, m_addr+8):
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 0)
      self.assertEqual(x7, 0)

    if pc == m_addr + 12:
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 1)
      self.assertEqual(x7, 0)

    if pc == m_addr + 16:
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 1)
      self.assertEqual(x7, 1)

if __name__ == '__main__':
  maddi(len(sys.argv) > 1)
