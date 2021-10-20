#!/usr/bin/env python3

import sys
import ProteusTest

class emmf(ProteusTest.ProteusTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, pc):

    inMM = self.as_int(vcd, self.WB.Mimicry_inMimicryMode, self.nextt(t))

    if pc == m_addr + 4:
      self.assertEqual(inMM, 1)

if __name__ == '__main__':
  emmf(len(sys.argv) > 1)
