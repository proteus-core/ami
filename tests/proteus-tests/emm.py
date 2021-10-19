#!/usr/bin/env python3

import sys
import ProteusTest

class emm(ProteusTest.ProteusTest):

  def on_change_writeback_pc(self, vcd, m_addr, t, tn, pc):

    inMimicryMode = self.as_int(vcd, self.WB.Mimicry_inMimicryMode, tn)

    if pc == m_addr:
      self.assertEqual(inMimicryMode, 1)

if __name__ == '__main__':
  emm(len(sys.argv) > 1)
