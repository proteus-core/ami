#*****************************************************************************
# jemm.S
#-----------------------------------------------------------------------------
#
# Test jump over 'enable mimicry mode' instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  j 1f
  emm 2f
1:
  mark 0
2:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
