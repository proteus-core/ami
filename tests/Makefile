TESTS_DIR = $(abspath .)
BASE_DIR = $(abspath $(TESTS_DIR)/..)
RISCV_TESTS_DIR = $(TESTS_DIR)/riscv-tests
ISA_TESTS_DIR = $(RISCV_TESTS_DIR)/isa
PROTEUS_TESTS_DIR=$(TESTS_DIR)/proteus-tests
BUILD_DIR = $(TESTS_DIR)/build
SIM_DIR = $(BASE_DIR)/sim
SIM_EXE = $(SIM_DIR)/build/sim
CORE ?= riscv.CoreExtMem
BUILD_CORE ?= 1

PYTHON = python3
RISCV_PREFIX ?= riscv64-unknown-elf
CC = $(RISCV_PREFIX)-gcc
LD = $(RISCV_PREFIX)-gcc
OBJDUMP = $(RISCV_PREFIX)-objdump
OBJCOPY = $(RISCV_PREFIX)-objcopy
ARCHFLAGS ?= -march=rv32imzicsr -mabi=ilp32
CFLAGS = $(ARCHFLAGS)

MORPHEUS_TOOLCHAIN_DIR = /usr/local/morpheus
OBJDUMP = $(MORPHEUS_TOOLCHAIN_DIR)/bin/llvm-objdump
CC = $(MORPHEUS_TOOLCHAIN_DIR)/bin/clang
CFLAGS =
CFLAGS += --target=riscv32
CFLAGS += -march=rv32im
CFLAGS += -nostdlib

ifeq ($(BUILD_CORE),1)
FORCE = FORCE
else
FORCE =
endif

-include $(CUSTOM_TESTS_DIR)/Makefile.include

TEST_DIRS ?= $(ISA_TESTS_DIR)/rv32ui $(ISA_TESTS_DIR)/rv32mi $(ISA_TESTS_DIR)/rv32um $(CUSTOM_TESTS_DIR)
ALL_ISA_TESTS = $(shell find $(TEST_DIRS) -name '*.S')
PROTEUS_TESTS = $(shell find $(PROTEUS_TESTS_DIR) -name '*.S')

UNSUPPORTED_TESTS =
UNSUPPORTED_TESTS += %fence_i.S
UNSUPPORTED_TESTS += %breakpoint.S
UNSUPPORTED_TESTS += %illegal.S
UNSUPPORTED_TESTS += %emmf.S

ALL_TESTS = $(filter-out $(UNSUPPORTED_TESTS),$(ALL_ISA_TESTS))
ALL_PROTEUS_TESTS = $(filter-out $(UNSUPPORTED_TESTS),$(PROTEUS_TESTS))

all: $(addsuffix .pass,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_TESTS)))))
#all: $(addsuffix .objdump,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_PROTEUS_TESTS)))))
#all: $(addsuffix .prot,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_PROTEUS_TESTS)))))

.SECONDARY:

VPATH  = $(TEST_DIRS)
VPATH += $(PROTEUS_TESTS_DIR)

$(BUILD_DIR)/%.o: %.S
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -I$(TESTS_DIR) -I$(ISA_TESTS_DIR)/macros/scalar -c -o $@ $^

%.elf: %.o
	$(LD) $(ARCHFLAGS) -ffreestanding -nostdlib -T tests.ld -o $@ $^

%.bin: %.elf
	$(OBJCOPY) -O binary $^ $@

%.objdump: %.elf
	$(OBJDUMP) -d $< > $@

%.vcd: %.bin $(SIM_EXE)
	cd $(BUILD_DIR); $(SIM_EXE) $<
	cd $(BUILD_DIR); cp sim.vcd $@

$(SIM_EXE): $(FORCE)
	make -C $(SIM_DIR) CORE=$(CORE)

.PHONY: FORCE

%.pass: %.bin $(SIM_EXE)
	cd $(BASE_DIR); $(SIM_EXE) $<
	touch $@

%.prot: %.vcd
	cd $(BUILD_DIR); $(PYTHON) $(PROTEUS_TESTS_DIR)/$(notdir $*).py $(VERBOSE)
	touch $@
