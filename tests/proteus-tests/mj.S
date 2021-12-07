#*****************************************************************************
# mj.S
#-----------------------------------------------------------------------------
#
# Test a.jal(r)
#   - Direct activating jump
#   - Indirect activating jump
#   - Nested calls
#   - Recursive calls
#   - Non activating jump with callee performing a reccursive activating jump
#     (To test depths > 1)
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  mark 0x01
  a.jal ra, foo11     # Direct activating jump

  la t1, foo22
  mark 0x02
  a.jalr ra, 0(t1)    # Indirect activating jump

  la sp, stack
  li a0, 2
  jal ra, foo33       # Non activating jump

  TEST_PASSFAIL

foo11:
  mark 0x11
  nop
  ret

foo22:
  mark 0x22
  p.addi s0, ra, 0
  p.addi a0, x0, 1
  a.jal ra, foo222    # Nested call
  p.addi ra, s0, 0
  ret

foo222:
  mark 0x222
  beq a0, x0, 1f
  p.addi a0, x0, 0
  p.addi s1, ra, 0
  a.jal ra, foo222    # Recursive call which does not make depth grow
  p.addi ra, s1, 0
1:
  ret

foo33:
  mark 0x330
  p.addi sp, sp, -4
  beq a0, x0, 1f
  p.addi a0, a0, -1
  sw ra, 0(sp)
  mark 0x331
  a.jal ra, foo33     # Recursive call which makes depth grow
  p.lw ra, 0(sp)
1:
  p.addi sp, sp, 4
  mark 0x332
  ret

RVTEST_CODE_END

RVTEST_DATA_BEGIN

  .word 0x00
  .word 0x00
stack:
  .word 0x00

RVTEST_DATA_END
