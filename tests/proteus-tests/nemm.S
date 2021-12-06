#*****************************************************************************
# nemm.S
#-----------------------------------------------------------------------------
#
# Test nesting of mimicry activations
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  mark 0
  emm 1f
  emm 2f
  nop
2:
  nop
1:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
