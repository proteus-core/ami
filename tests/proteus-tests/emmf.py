#!/usr/bin/env python3

import sys
import MimicryTest

class emmf(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    m_addr = vcd.get_addr_of_marked_instr()

    if pc == m_addr:
      # @t
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(vcd.as_int(vcd.CSR.CsrFile_mime, t), 1)
      self.assertEqual(vcd.as_int(vcd.CSR.CsrFile_pmime, t), 0)

      # @nextt
      tn = vcd.nextt(t)
      self.assertFalse(self.in_mm(vcd, tn))
      self.assertEqual(vcd.as_int(vcd.CSR.CsrFile_mime, tn), 0)
      self.assertEqual(vcd.as_int(vcd.CSR.CsrFile_pmime, tn), 1)
 
    if pc == m_addr+4:
      self.assertTrue(self.in_mm(vcd, t))
      self.assertEqual(vcd.as_int(vcd.CSR.CsrFile_mime, t), 1)
      self.assertEqual(vcd.as_int(vcd.CSR.CsrFile_pmime, t), 0)

if __name__ == '__main__':
  emmf(len(sys.argv) > 1)
