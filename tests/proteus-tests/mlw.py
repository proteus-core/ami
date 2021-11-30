#!/usr/bin/env python3

import sys
import MimicryTest

class mlw(MimicryTest.MimicryTest):

  def on_change_memory_pc(self, vcd, t, pc):

    m_addr = vcd.get_marked_instr_addr()

    x6 = vcd.x6(vcd.nextt(t))

    if pc == m_addr:
      self.assertEqual(x6, 0)

    if pc == m_addr+4:
      self.assertEqual(x6, 0)

    if pc == m_addr + 12:
      self.assertEqual(x6, 0xdeadbeef)

if __name__ == '__main__':
  mlw(len(sys.argv) > 1)
