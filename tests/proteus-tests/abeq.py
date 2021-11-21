#!/usr/bin/env python3

import sys
import MimicryTest

class abeq(MimicryTest.MimicryTest):

  def on_change_decode_pc(self, vcd, m_addr, t, pc):

    if pc == m_addr:
      pass

if __name__ == '__main__':
  abeq(len(sys.argv) > 1)
