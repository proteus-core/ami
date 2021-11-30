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
  RVTEST_MARK
  MIMICRY_DMM

  TEST_PASSFAIL

RVTEST_CODE_END
