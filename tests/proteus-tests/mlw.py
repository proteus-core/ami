#!/usr/bin/env python3

import sys
import MimicryTest

class mlw(MimicryTest.MimicryTest):

  def run(self, vcd):

    mark = vcd.get_mark()
    t = mark.WB[0]
    self.assertEqual(vcd.x6(vcd.nextt(t)), 0)
    t = vcd.WB2[mark.addr+4][0]
    self.assertEqual(vcd.x6(vcd.nextt(t)), 0)
    t = vcd.WB2[mark.addr+12][0]
    self.assertEqual(vcd.x6(vcd.nextt(t)), 0xdeadbeef)

if __name__ == '__main__':
  mlw(len(sys.argv) > 1)
