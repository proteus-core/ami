#*****************************************************************************
# mj.S
#-----------------------------------------------------------------------------
#
# Test m.jal(r)
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_N(1)
  m.jal ra, foo11    # Direct jump

  la t1, foo22
  RVTEST_MARK_N(2)
  m.jalr ra, 0(t1)   # Indirect jump

  TEST_PASSFAIL

foo11:
  RVTEST_MARK_NN(11)
  nop
  ret

foo22:
  RVTEST_MARK_NN(22)
  p.addi s0, ra, 0
  p.addi a0, x0, 1
  m.jal ra, foo222      # Nested call
  p.addi ra, s0, 0
  ret

foo222:
  RVTEST_MARK_NNN(222)
  beq a0, x0, 1f
  p.addi a0, x0, 0
  p.addi s1, ra, 0
  m.jal ra, foo222     # Recursive call
  p.addi ra, s1, 0
1:
  ret

RVTEST_CODE_END
