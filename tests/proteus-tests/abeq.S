#*****************************************************************************
# abeq.S
#-----------------------------------------------------------------------------
#
# Test a.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 42 

  RVTEST_MARK_ZERO
  a.beq x0, x0, 1f
  nop
1:
  nop

  RVTEST_MARK_ONE
  a.beq x0, x5, 1f
  nop
1:
  nop

  RVTEST_MARK_TWO
  a.beq x0, x0, 1f
  nop
1:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
