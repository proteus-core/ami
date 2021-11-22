#*****************************************************************************
# dbeq.S
#-----------------------------------------------------------------------------
#
# Test d.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, 42 

  MIMICRY_EMM
  RVTEST_MARK_NEXT
  d.beq x0, x0, label
  nop
label:
  nop

  // TODO: Also test when outcome is false 
  //     d.beq x0, x5, label

  TEST_PASSFAIL

RVTEST_CODE_END
