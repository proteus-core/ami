#!/usr/bin/env python3

import sys
import MimicryTest

class femm(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()
    t = mark.WB[0]
    self.assertEqual(vcd.as_int(vcd.WB.arbitration_isValid, t), 1)
    t = vcd.nextt(t)
    while vcd.as_int(vcd.WB.arbitration_isValid, t) == 0:
      self.assertFalse(self.in_mm(vcd, t))
      t = vcd.nextt(t)
    self.assertEqual(vcd.t0(t), vcd.as_int(vcd.WB.in_PC, t))

if __name__ == '__main__':
  femm(len(sys.argv) > 1)
