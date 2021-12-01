#*****************************************************************************
# nemm.S
#-----------------------------------------------------------------------------
#
# Test nesting of mimicry activations
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK
  MIMICRY_EMM
  MIMICRY_EMM
  MIMICRY_DMM
  MIMICRY_DMM
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
