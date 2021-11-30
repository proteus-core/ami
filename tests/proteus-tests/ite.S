#*****************************************************************************
# ite.S
#-----------------------------------------------------------------------------
#
# Test "if-then" and "if-then-else" 
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN
  
  # Mark 0: Test if-true-then
  a.beq x0, x0, 1f
  RVTEST_MARK_N(0)
  nop
1: 
  d.beq x0, x0, 2f
2:
  nop

  # Mark 1: Test if-false-then
  a.bne x0, x0, 1f
  RVTEST_MARK_N(1)
  nop
1: 
  d.bne x0, x0, 2f
2:
  nop

  # Mark 2: Test if-true-then-else
  a.beq x0, x0, 1f
  RVTEST_MARK_N(2)
  nop
1: 
  i.bne x0, x0, 2f
  nop
2:
  d.bne x0, x0, 3f
3:
  nop

  # Mark 3: Test if-false-then-else
  a.bne x0, x0, 1f
  RVTEST_MARK_N(3)
  nop
1: 
  i.beq x0, x0, 2f
  nop
2:
  d.beq x0, x0, 3f
3:
  nop

  # 5. Test nested if-then-else
  # TODO

  TEST_PASSFAIL

RVTEST_CODE_END
