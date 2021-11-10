#!/usr/bin/env python3

import sys
import MimicryTest

class gaddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    x5 = vcd.x5(vcd.nextt(t))

    if pc in (m_addr, m_addr+4, m_addr+8):
      self.assertEqual(x5, 1)

if __name__ == '__main__':
  gaddi(len(sys.argv) > 1)
