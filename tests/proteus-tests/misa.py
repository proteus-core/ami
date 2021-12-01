#!/usr/bin/env python3

import sys
import MimicryTest

class misa(MimicryTest.MimicryTest):

  def run(self, vcd):

    t = vcd.get_mark().WB[0]
    X = 23
    v = vcd.as_int(vcd.CSR.CsrFile_extensions, t) & (1 << X)
    self.assertTrue(v != 0)

if __name__ == '__main__':
  misa(len(sys.argv) > 1)
