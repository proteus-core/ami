#*****************************************************************************
# maddi.S
#-----------------------------------------------------------------------------
#
# 1. Test addi instruction after enabling mimicry mode
# 2. Test addi instruction after disabling mimicry mode
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

  MIMICRY_DMM

  li x5, 42
  li x6, 42
  
  MIMICRY_EMM
  RVTEST_MARK_NEXT
  addi x5, x5, 1     # assert x5 == 42
  
  MIMICRY_DMM
  #TODO: addi x5, x5, 1     # assert x5 == 43    (x5 == 44 would be wrong !)
  addi x6, x6, 1     # assert x6 == 43

  RVTEST_PASS

RVTEST_CODE_END
