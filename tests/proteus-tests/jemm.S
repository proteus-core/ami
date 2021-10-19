#*****************************************************************************
# jemm.S
#-----------------------------------------------------------------------------
#
# Test jump over 'enable mimicry mode' instruction.
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

  j 1f
  MIMICRY_EMM
1:
  RVTEST_MARK_NEXT
  nop

RVTEST_PASS
RVTEST_CODE_END
