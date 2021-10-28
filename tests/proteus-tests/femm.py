#!/usr/bin/env python3

import sys
import MimicryTest

class femm(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr:
      self.assertTrue(self.inMM(vcd, self.nextt(t)))

if __name__ == '__main__':
  femm(len(sys.argv) > 1)
