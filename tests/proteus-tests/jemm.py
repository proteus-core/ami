#!/usr/bin/env python3

import sys
import MimicryTest

class jemm(MimicryTest.MimicryTest):

  def run(self, vcd):
    t = vcd.get_mark().WB[0]
    self.assertFalse(self.in_mm(vcd, t))

if __name__ == '__main__':
  jemm(len(sys.argv) > 1)
