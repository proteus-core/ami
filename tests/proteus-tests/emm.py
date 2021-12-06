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

    mark = vcd.get_mark()
    t = mark.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(vcd.as_int(vcd.PL.writeback_out_RD_TYPE, t), 0)
    tn =  vcd.nextt(t)
    self.assertEqual(self.mm_entry(vcd, tn), mark.addr-8)
    self.assertEqual(self.mm_exit(vcd, tn), mark.addr+4)

    t = vcd.WB2[mark.addr+4][0]
    self.assertEqual(vcd.as_int(vcd.WB.out_RD_TYPE, t), 1)
    tn =  vcd.nextt(t)
    self.assertFalse(self.in_mm(vcd, tn))
    self.assertEqual(self.mm_entry(vcd, tn), 0x7FFFFFFF)
    self.assertEqual(self.mm_exit(vcd, tn), 0x7FFFFFFF)

if __name__ == '__main__':
  emm(len(sys.argv) > 1)
