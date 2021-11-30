#!/usr/bin/env python3

import sys
import MimicryTest

class paddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    m_addr = vcd.get_marked_instr_addr()
    tn = vcd.nextt(t)

    is_persistent = vcd.as_int(vcd.WB.value_PERSISTENT, t) == 1
    x5 = vcd.x5(tn)

    if pc == m_addr:
      self.assertTrue(is_persistent)
      self.assertEqual(x5, 1)

    if pc == m_addr+4:
      self.assertFalse(is_persistent)
      self.assertEqual(x5, 1)

    if pc == m_addr+8:
      self.assertTrue(is_persistent)
      self.assertEqual(x5, 2)

if __name__ == '__main__':
  paddi(len(sys.argv) > 1)
