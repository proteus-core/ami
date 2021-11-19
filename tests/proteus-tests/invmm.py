#!/usr/bin/env python3

import sys
import MimicryTest

class invmm(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr:
      pass

if __name__ == '__main__':
  invmm(len(sys.argv) > 1)
