#*****************************************************************************
# mj.S
#-----------------------------------------------------------------------------
#
# Test m.jal(r)
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK_ONE
  m.jal ra, foo

  la t1, foo
  RVTEST_MARK_TWO
  m.jalr ra, 0(t1)

  TEST_PASSFAIL

foo:
  nop
  ret

RVTEST_CODE_END
