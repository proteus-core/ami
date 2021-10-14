KERNEL_DIR = ../../../../NewlibSupport

TESTS =
TESTS += test-mimicry-enable-disable.c

HEX = $(TESTS:.c=.ihex)

all: $(HEX)

test-mimicry-enable-disable.elf: trt.o

clean:
	$(RM) *.o
	$(RM) *.ihex
	$(RM) *.elf

include ../../../../NewlibSupport/Makefile.include

ASFLAGS = $(CFLAGS)

CFLAGS += -ffunction-sections
CFLAGS += -fdata-sections

LDFLAGS += -Wl,--gc-sections
