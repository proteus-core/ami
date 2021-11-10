#*****************************************************************************
# maddi.S
#-----------------------------------------------------------------------------
#
# 1. Test m.addi instruction
#
# This test also verifies the forwarding logic between mimics and ordinary
# instructions.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 0
  li x6, 0
  li x7, 0
  
  RVTEST_MARK_NEXT
  m.addi x5, x5, 1     # assert x5 == 0
  m.addi x6, x6, 1     # assert x6 == 0
  
  #TODO: addi x6, x6, 1     # Tests forwarding, assert x6 == 1 (assert x6 != 2 )
  addi x7, x7, 1     # assert x7 == 1 

  TEST_PASSFAIL

RVTEST_CODE_END
