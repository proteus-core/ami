#!/usr/bin/env python3

import sys
import MimicryTest

class emm(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr:
      self.assertTrue(self.inMM(vcd, self.nextt(t)))

if __name__ == '__main__':
  emm(len(sys.argv) > 1)
