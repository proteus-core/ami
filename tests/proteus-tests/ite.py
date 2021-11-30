#!/usr/bin/env python3

import sys
import MimicryTest

class ite(MimicryTest.MimicryTest):

  def on_change_writeback_pc(self, vcd, t, pc):

    mark0 = vcd.get_addr_of_marked_instr(0x00)
    mark1 = vcd.get_addr_of_marked_instr(0x01)
    mark2 = vcd.get_addr_of_marked_instr(0x02)
    mark3 = vcd.get_addr_of_marked_instr(0x03)

    # Mark 0
    if pc == mark0  : self.assertTrue(self.in_mm(vcd, t))
    if pc == mark0+8: self.assertFalse(self.in_mm(vcd, t))

    # Mark 1
    if pc == mark1   : self.assertFalse(self.in_mm(vcd, t))
    if pc == mark0+8: self.assertFalse(self.in_mm(vcd, t))

    # Mark 2
    if pc == mark2   : self.assertTrue(self.in_mm(vcd, t))
    if pc == mark2+8 : self.assertFalse(self.in_mm(vcd, t))
    if pc == mark2+16: self.assertFalse(self.in_mm(vcd, t))

    # Mark 3
    if pc == mark3   : self.assertFalse(self.in_mm(vcd, t))
    if pc == mark3+8 : self.assertTrue(self.in_mm(vcd, t))
    if pc == mark3+16: self.assertFalse(self.in_mm(vcd, t))

if __name__ == '__main__':
  ite(len(sys.argv) > 1)
