#*****************************************************************************
# abeq.S
#-----------------------------------------------------------------------------
#
# Test a.beq instruction
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_NEXT
  a.beq x0, x0, label
  nop
label:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
