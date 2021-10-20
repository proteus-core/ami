#*****************************************************************************
# dmm.S
#-----------------------------------------------------------------------------
#
# Test 'disable mimicry mode' instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  MIMICRY_EMM
  RVTEST_MARK_NEXT
  MIMICRY_DMM

  TEST_PASSFAIL

RVTEST_CODE_END
