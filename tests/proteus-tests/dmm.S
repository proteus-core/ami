#*****************************************************************************
# dmm.S
#-----------------------------------------------------------------------------
#
# Test 'disable mimicry mode' instruction.
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

MIMICRY_EMM
RVTEST_MARK_NEXT
MIMICRY_DMM

RVTEST_PASS
RVTEST_CODE_END
