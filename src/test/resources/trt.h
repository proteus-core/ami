#ifndef TEST_RT_H
#define TEST_RT_H

#include "tdefs.h"

void mark_start_of_test(void);

void enable_mimicry_mode(void);

void disable_mimicry_mode(void);

__attribute__((noreturn)) void _halt();

#endif
