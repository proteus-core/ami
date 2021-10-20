#*****************************************************************************
# emmf.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction that follows an instruction that
#  triggers a fatal exception in the memory stage
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 0xffffffff
  RVTEST_MARK_NEXT
  sw zero, 0(zero)
  MIMICRY_EMM

  TEST_PASSFAIL

RVTEST_CODE_END
