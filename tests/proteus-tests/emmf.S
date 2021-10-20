#*****************************************************************************
# emmf.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction the follows an instruction that causes
#  a fatal exception
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_NEXT
  MIMICRY_EMM
  MIMICRY_DMM

  RVTEST_PASS

RVTEST_CODE_END
