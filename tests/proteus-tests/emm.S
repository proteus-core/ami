#*****************************************************************************
# emm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK
  MIMICRY_EMM
  nop
  MIMICRY_DMM

  TEST_PASSFAIL

RVTEST_CODE_END
