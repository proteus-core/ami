#!/usr/bin/env python3

import sys
import MimicryTest

class gaddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    m_addr = vcd.get_addr_of_marked_instr()
    tn = vcd.nextt(t)

    is_ghost = vcd.as_int(vcd.WB.value_GHOST, t) == 1
    x5 = vcd.x5(tn)

    if pc == m_addr  :
      self.assertTrue(is_ghost)
      self.assertEqual(x5, 1)

    if pc == m_addr+4:
      self.assertFalse(is_ghost)
      self.assertEqual(x5, 1)

    if pc == m_addr+8:
      self.assertTrue(is_ghost)
      self.assertEqual(x5, 1)

if __name__ == '__main__':
  gaddi(len(sys.argv) > 1)
