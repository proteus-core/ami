#*****************************************************************************
# ibeq.S
#-----------------------------------------------------------------------------
#
# Test i.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_ZERO
  i.beq x0, x0, 1f
  nop

1:
  li x5, 42 
  RVTEST_MARK_ONE
  i.beq x0, x5, 2f
  nop

2:
  TEST_PASSFAIL

RVTEST_CODE_END
