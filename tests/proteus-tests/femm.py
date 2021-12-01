#!/usr/bin/env python3

import sys
import MimicryTest

class femm(MimicryTest.MimicryTest):

  def run(self, vcd):

    # TODO:
    pass
    #if pc == vcd.get_addr_of_marked_instr():
      #self.assertTrue(self.in_mm(vcd, self.nextt(t)))

if __name__ == '__main__':
  femm(len(sys.argv) > 1)
