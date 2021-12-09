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
  a.beq x1, x1, 1f
  mark 0
  nop
1: 
  nop

  # Mark 1: Test if-false-then
  a.bne x1, x1, 1f
  mark 1
  nop
1: 
  nop

  # Mark 2: Test if-true-then-else
  a.beq x1, x1, 1f
  mark 2
  nop
1: 
  a.bne x0, x0, 2f
  nop
2:
  nop

  # Mark 3: Test if-false-then-else
  a.bne x0, x0, 1f
  mark 3
  nop
1: 
  a.beq x1, x1, 2f
  nop
2:
  nop

  # Mark 4. Test nested activating branch
  a.beq x0, x0, 1f
  mark 4
  nop
  a.beq x0, x0, 2f
  nop
2:
  nop
1:
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
