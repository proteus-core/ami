#*****************************************************************************
# abeq.S
#-----------------------------------------------------------------------------
#
# Test a.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 42
  mark 0
  a.beq x5, x5, 1f
  nop
1:
  addi x5, x5, 1

  li x5, 42 
  mark 1
  a.beq x0, x5, 2f
  nop
2:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
