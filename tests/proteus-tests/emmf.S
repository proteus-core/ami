#*****************************************************************************
# emmf.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction followed by an instruction that
#  triggers a fatal exception in the memory stage
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  li x5, -1
  emm 1f
  mark 0
  sw zero, 0(x5)
  nop

1:
  TEST_PASSFAIL

mtvec_handler:
  csrr x5, CSR_MEPC
  p.addi x5, x5, 4
  csrw CSR_MEPC, x5
  mret

RVTEST_CODE_END
