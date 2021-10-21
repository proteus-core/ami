#!/usr/bin/env python3

import sys
import MimicryTest

class maddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    x5 = self.x5(vcd, self.nextt(t))
    x6 = self.x6(vcd, self.nextt(t));

    if pc == m_addr:
      self.assertEqual(x5, 42)
      self.assertEqual(x6, 42)

    if pc == m_addr + 4:
      self.assertEqual(x5, 42)
      self.assertEqual(x6, 42)

    if pc == m_addr + 8:
      self.assertEqual(x5, 42)
      self.assertEqual(x6, 43)

if __name__ == '__main__':
  maddi(len(sys.argv) > 1)
