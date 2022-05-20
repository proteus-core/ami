#!/usr/bin/env python3

import sys
import MimicryTest

class emm(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()
    t = mark.WB[0]
    self.assertTrue(self.in_mm(vcd, t))
    self.assertEqual(vcd.as_int(vcd.PL.writeback_out_RD_TYPE, t), MimicryTest.MIMIC_REG_TYPE)
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
