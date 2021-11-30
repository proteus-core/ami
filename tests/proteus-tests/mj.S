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
  m.jal ra, foo11

  la t1, foo22
  RVTEST_MARK_N(2)
  m.jalr ra, 0(t1)

  # TODO: Add recursion tests (requires recursive marks)

  TEST_PASSFAIL

foo11:
  RVTEST_MARK_NN(11)
  nop
  ret

foo22:
  RVTEST_MARK_NN(22)
  mv s0, ra
  m.jal ra, foo222
  mv ra, s0
  ret

foo222:
  RVTEST_MARK_NNN(222)
  nop
  ret

RVTEST_CODE_END
