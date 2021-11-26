#!/usr/bin/env python3

import sys
import MimicryTest

class femm(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):


    if pc == vcd.get_marker_addr():
      self.assertTrue(self.in_mm(vcd, self.nextt(t)))

if __name__ == '__main__':
  femm(len(sys.argv) > 1)
