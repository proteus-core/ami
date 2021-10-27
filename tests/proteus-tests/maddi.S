#*****************************************************************************
# maddi.S
#-----------------------------------------------------------------------------
#
# 1. Test addi instruction after enabling mimicry mode
# 2. Test addi instruction after disabling mimicry mode
#
# This test also verifies the forwarding logic at the boundary of mimicry mode
# and non-mimicry mode.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 0
  li x6, 0
  li x7, 0
  
  MIMICRY_EMM
  RVTEST_MARK_NEXT
  addi x5, x5, 1     # assert x5 == 0
  addi x6, x6, 1     # assert x6 == 0
  
  MIMICRY_DMM
  #TODO: addi x6, x6, 1     # Tests forwarding, assert x6 == 1 (assert x6 != 2 )
  addi x7, x7, 1     # assert x7 == 1 

  TEST_PASSFAIL

RVTEST_CODE_END
