#*****************************************************************************
# misa.S
#-----------------------------------------------------------------------------
#
# Make sure 'X' (Non-standard extensions present) is set in extensions field
# in misa CSR register
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_CODE_BEGIN

  RVTEST_MARK
  nop

  TEST_PASSFAIL

RVTEST_CODE_END
