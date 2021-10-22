#!/usr/bin/env python3

import sys
import MimicryTest

class mlw(MimicryTest.MimicryTest):

  def on_change_memory_pc(self, vcd, m_addr, t, pc):

    x6 = vcd.x6(vcd.nextt(t))

    if pc in (m_addr, m_addr+4):
      self.assertEqual(x6, 0)

    if pc == m_addr + 12:
      self.assertEqual(x6, 0xdeadbeef)

if __name__ == '__main__':
  mlw(len(sys.argv) > 1)
