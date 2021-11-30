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

  RVTEST_MARK
  a.beq x0, x0, label
  nop
label:
  nop

  // TODO: Also test when outcome is false 
  //     a.beq x0, x5, label

  TEST_PASSFAIL

RVTEST_CODE_END
