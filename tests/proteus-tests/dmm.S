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
  MIMICRY_DMM
  RVTEST_MARK
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
