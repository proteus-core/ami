#!/usr/bin/env python3

import sys
import MimicryTest

class emm(MimicryTest.MimicryTest):

  """
  def on_change_decode_pc(self, vcd, t, pc):

    if pc == m_addr:
      tn = vcd.nextt(t)
      self.assertTrue(self.in_mm(vcd, tn))
      self.assertEqual(vcd.as_int(vcd.PL.decode_out_RD_TYPE, tn), 2)

    if pc == m_addr+4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(vcd.as_int(vcd.PL.decode_out_RD_TYPE, t), 2)

  def on_change_execute_pc(self, vcd, m_addr, t, pc):
    #TODO: if pc in (m_addr, m_addr+4):
    if pc == m_addr+4:
      self.assertEqual(vcd.as_int(vcd.PL.execute_out_RD_TYPE, t), 2)

  def on_change_memory_pc(self, vcd, m_addr, t, pc):
    if pc == m_addr+4:
      self.assertEqual(vcd.as_int(vcd.PL.memory_out_RD_TYPE, t), 2)
  """

  def run(self, vcd):
    t = vcd.get_mark().WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(vcd.as_int(vcd.PL.writeback_out_RD_TYPE, t), 0)

if __name__ == '__main__':
  emm(len(sys.argv) > 1)
