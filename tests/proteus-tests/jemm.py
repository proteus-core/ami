#!/usr/bin/env python3

import sys
import MimicryTest

class jemm(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, t, pc):

    if pc == vcd.get_marker_addr():
      self.assertFalse(self.in_mm(vcd, vcd.nextt(t)))

if __name__ == '__main__':
  jemm(len(sys.argv) > 1)
