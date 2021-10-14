#include "tdefs.h"
#include "chardev.h"

  .globl mark_start_of_test
mark_start_of_test:
  li x1, CHARDEV_ADDR
  li x2, MARK_START_OF_TEST
  sb x2, 0(x1)

  .globl enable_mimicry_mode
enable_mimicry_mode:
  .word 0x0000000B
  ret

  .globl disable_mimicry_mode
disable_mimicry_mode:
  .word 0x0010000B
  ret
