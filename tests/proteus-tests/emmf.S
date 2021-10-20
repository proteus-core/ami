#*****************************************************************************
# emmf.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction that follows an instruction that
#  triggers a fatal exception
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_NEXT
  .word 0x00
  MIMICRY_EMM

  TEST_PASSFAIL

RVTEST_CODE_END
