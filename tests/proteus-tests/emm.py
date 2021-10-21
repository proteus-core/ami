#!/usr/bin/env python3

import sys
import MimicryTest

class emm(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr:
      tn = self.nextt(t)
      self.assertTrue(self.inMM(vcd, tn))
      self.assertEqual(self.as_int(vcd, self.PL.decode_out_RD_TYPE, tn), 2)

    if pc == m_addr+4:
      self.assertTrue(self.inMM(vcd, t))
      self.assertEqual(self.as_int(vcd, self.PL.decode_out_RD_TYPE, t), 2)

  def on_change_execute_pc(self, vcd, m_addr, t, pc):
    #TODO: if pc in (m_addr, m_addr+4):
    if pc == m_addr+4:
      self.assertEqual(self.as_int(vcd, self.PL.execute_out_RD_TYPE, t), 2)

  def on_change_memory_pc(self, vcd, m_addr, t, pc):
    if pc == m_addr+4:
      self.assertEqual(self.as_int(vcd, self.PL.memory_out_RD_TYPE, t), 2)

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):
    if pc == m_addr+4:
      self.assertEqual(self.as_int(vcd, self.PL.writeback_out_RD_TYPE, t), 2)

if __name__ == '__main__':
  emm(len(sys.argv) > 1)
