#*****************************************************************************
# emm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_NEXT
  MIMICRY_EMM
  MIMICRY_DMM

  TEST_PASSFAIL

RVTEST_CODE_END
