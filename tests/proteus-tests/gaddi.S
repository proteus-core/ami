#*****************************************************************************
# gaddi.S
#-----------------------------------------------------------------------------
#
# 1. Test g.addi instruction after enabling mimicry mode
# 2. Test g.addi instruction after disabling mimicry mode
#
# This test also verifies the forwarding logic at the boundary of mimicry mode
# and non-mimicry mode.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 0
  
  MIMICRY_EMM
  RVTEST_MARK_NEXT
  g.addi x5, x5, 1   # assert x5 == 1
  MIMICRY_DMM
  g.addi x5, x5, 1   # assert x5 == 1 

  TEST_PASSFAIL

RVTEST_CODE_END
