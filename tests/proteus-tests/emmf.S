#*****************************************************************************
# emmf.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction that follows an instruction that
#  triggers a fatal exception
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_NEXT
  # TODO: trigger fatal exception
  MIMICRY_EMM

  RVTEST_PASS

RVTEST_CODE_END
