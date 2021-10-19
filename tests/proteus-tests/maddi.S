#*****************************************************************************
# maddi.S
#-----------------------------------------------------------------------------
#
# Test addi instruction when mimicry mode is enabled and disabled
#

#include "riscv_test.h"

RVTEST_CODE_BEGIN

MIMICRY_DMM

li x5, 42
li x6, 42

MIMICRY_EMM

RVTEST_MARK_NEXT
addi x5, x5, 1

MIMICRY_DMM

# TODO: "addi x5, x5, 1" fails
addi x6, x6, 1

RVTEST_PASS
RVTEST_CODE_END
