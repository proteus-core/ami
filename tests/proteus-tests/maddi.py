#!/usr/bin/env python3

import sys
import MimicryTest

class maddi(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    m_addr = vcd.get_addr_of_marked_instr()
    tn = vcd.nextt(t)

    x5 = vcd.x5(tn)
    x6 = vcd.x6(tn)
    x7 = vcd.x7(tn)
    is_mimic = vcd.as_int(vcd.WB.value_MIMIC, t) == 1

    if pc == m_addr:
      self.assertTrue(is_mimic)
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 0)
      self.assertEqual(x7, 0)

    if pc == m_addr+4:
      self.assertTrue(is_mimic)
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 0)
      self.assertEqual(x7, 0)

    if pc == m_addr+8:
      self.assertFalse(is_mimic)
      self.assertEqual(x5, 0)
      self.assertEqual(x6, 0)
      self.assertEqual(x7, 1)

if __name__ == '__main__':
  maddi(len(sys.argv) > 1)
