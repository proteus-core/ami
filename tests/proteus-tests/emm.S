#*****************************************************************************
# emm.S
#-----------------------------------------------------------------------------
#
# Test 'enable mimicry mode' instruction.
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

RVTEST_MARK_NEXT
MIMICRY_EMM
MIMICRY_DMM

RVTEST_PASS
RVTEST_CODE_END
