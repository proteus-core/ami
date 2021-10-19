#!/usr/bin/env python3

import sys
import ProteusTest

class maddi(ProteusTest.ProteusTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    x5 = self.as_int(vcd, self.RF.x5_t0, self.nextt(t))
    x6 = self.as_int(vcd, self.RF.x6_t1, self.nextt(t));

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
