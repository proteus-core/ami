#*****************************************************************************
# paddi.S
#-----------------------------------------------------------------------------
#
# 1. Test p.addi instruction after enabling mimicry mode
# 2. Test p.addi instruction after disabling mimicry mode
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 0
  
  MIMICRY_EMM
  RVTEST_MARK
  p.addi x5, x5, 1   # assert x5 == 1
  MIMICRY_DMM
  p.addi x5, x5, 1   # assert x5 == 2 

  TEST_PASSFAIL

RVTEST_CODE_END
