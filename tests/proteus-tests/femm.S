#*****************************************************************************
# femm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction that follows an instruction that
#  triggers a fatal exception in the memory stage
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, -1
  sw zero, 0(x5)
  emm 1f
  nop

1:
  TEST_PASSFAIL

RVTEST_CODE_END
