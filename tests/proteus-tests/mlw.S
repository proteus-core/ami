#*****************************************************************************
# mlw.S
#-----------------------------------------------------------------------------
#
# Test lw instruction in mimicry mode
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  la x5, tdata
  li x6, 0

  emm 1f
  mark 0
  lw x6, 0(x5)  # assert x6 == 0
1:
  lw x6, 0(x5)  # assert x6 == 0xdeadbeef

  TEST_PASSFAIL

RVTEST_CODE_END

RVTEST_DATA_BEGIN

  tdata: .word 0xdeadbeef

RVTEST_DATA_END
