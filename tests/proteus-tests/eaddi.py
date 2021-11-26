#!/usr/bin/env python3

import sys
import MimicryTest

class eaddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    m_addr = vcd.get_marker_addr()
    tn = vcd.nextt(t)

    is_execute = vcd.as_int(vcd.WB.value_EXECUTE, t) == 1
    x5 = vcd.x5(tn)

    if pc == m_addr:
      self.assertTrue(is_execute)
      self.assertEqual(x5, 1)

    if pc == m_addr+4:
      self.assertFalse(is_execute)
      self.assertEqual(x5, 1)

    if pc == m_addr+8:
      self.assertTrue(is_execute)
      self.assertEqual(x5, 2)

if __name__ == '__main__':
  eaddi(len(sys.argv) > 1)
