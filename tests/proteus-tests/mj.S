#*****************************************************************************
# mj.S
#-----------------------------------------------------------------------------
#
# Test a.jal(r)
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  mark 0x01
  a.jal ra, foo11    # Direct jump

  la t1, foo22
  mark 0x02
  a.jalr ra, 0(t1)   # Indirect jump

  TEST_PASSFAIL

foo11:
  mark 0x11
  nop
  ret

foo22:
  mark 0x22
  p.addi s0, ra, 0
  p.addi a0, x0, 1
  a.jal ra, foo222      # Nested call
  p.addi ra, s0, 0
  ret

foo222:
  mark 0x222
  beq a0, x0, 1f
  p.addi a0, x0, 0
  p.addi s1, ra, 0
  a.jal ra, foo222     # Recursive call
  p.addi ra, s1, 0
1:
  ret

  # TODO: Add test wich test with depth > 1

RVTEST_CODE_END
