#*****************************************************************************
# femm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction that follows an instruction that
#  triggers a fatal exception in the memory stage
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  la t0, trap_vector
  li t1, -1
  mark 0
  sw zero, 0(t1)
  emm 1f
  nop

1:
  TEST_PASSFAIL

RVTEST_CODE_END
