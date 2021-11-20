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
  nop
  MIMICRY_INVERT # TODO
  MIMICRY_EMM
  MIMICRY_INVERT
  MIMICRY_INVERT
  MIMICRY_INVERT
  MIMICRY_DMM

  TEST_PASSFAIL

RVTEST_CODE_END
