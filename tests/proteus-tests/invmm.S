#*****************************************************************************
# invmm.S
#-----------------------------------------------------------------------------
#
# Test 'invert mimicry mode' instruction 
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_NEXT
  MIMICRY_EMM
  MIMICRY_INVERT

  TEST_PASSFAIL

RVTEST_CODE_END
