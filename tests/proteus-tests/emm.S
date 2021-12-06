#*****************************************************************************
# emm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  emm 1f
  mark 0
  nop
1:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
