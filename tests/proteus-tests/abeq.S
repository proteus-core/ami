#*****************************************************************************
# abeq.S
#-----------------------------------------------------------------------------
#
# Test a.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_ZERO
  a.beq x0, x0, 1f
  nop

1:
  li x5, 42 
  RVTEST_MARK_ONE
  a.beq x0, x5, 2f
  nop

2:
  RVTEST_MARK_TWO
  a.beq x0, x0, 3f
  nop

3:
  TEST_PASSFAIL

RVTEST_CODE_END
