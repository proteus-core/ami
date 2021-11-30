#*****************************************************************************
# dbeq.S
#-----------------------------------------------------------------------------
#
# Test d.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  MIMICRY_EMM
  RVTEST_MARK_ZERO
  d.beq x0, x0, 1f

1:
  MIMICRY_EMM
  li x5, 42 
  RVTEST_MARK_ONE
  d.beq x0, x5, 2f

2:
  TEST_PASSFAIL

RVTEST_CODE_END
