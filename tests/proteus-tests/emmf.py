#!/usr/bin/env python3

import sys
import MimicryTest

class emmf(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr + 4:
      self.assertTrue(self.inMM(vcd, self.nextt(t)))

if __name__ == '__main__':
  emmf(len(sys.argv) > 1)
