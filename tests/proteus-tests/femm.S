#*****************************************************************************
# femm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction followed by an instruction that
#  triggers a fatal exception in the memory stage
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, -1
  MIMICRY_EMM
  sw zero, 0(x5)
  RVTEST_MARK_NEXT
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
