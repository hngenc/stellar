include $(abs_top_srcdir)/Makefrag

tests = \
	conv_1 \
	# conv_2 \
	conv_3 \
	conv_4 \
	conv_5 \
	conv_10 \
	conv_20 \
	conv_30 \
	conv_40 \
	conv_50 \

tests_baremetal = $(tests:=-baremetal)

ifdef BAREMETAL_ONLY
	tests_linux =
	tests_pk =
else
	tests_linux = $(tests:=-linux)
	tests_pk = $(tests:=-pk)
endif

RISCV_TESTS = $(abs_top_srcdir)/../../../toolchains/riscv-tools/riscv-tests
BENCH_COMMON = $(RISCV_TESTS)/benchmarks/common
GENERATED_HEADERS = $(src_dir)/alexnet_params.h
EINSTEIN_HEADERS = $(abs_top_srcdir)/include/stellar.h $(abs_top_srcdir)/include/stellar_params.h $(GENERATED_HEADERS)

CFLAGS := $(CFLAGS) \
	-DPREALLOCATE=1 \
	-DMULTITHREAD=1 \
	-mcmodel=medany \
	-std=gnu99 \
	-O2 \
	-ffast-math \
	-fno-common \
	-fno-builtin-printf \
	-fno-tree-loop-distribute-patterns \
	-march=rv64gc -Wa,-march=rv64gc \
	-lm \
	-lgcc \
	-I$(RISCV_TESTS) \
	-I$(RISCV_TESTS)/env \
	-I$(abs_top_srcdir) \
	-I$(BENCH_COMMON) \
	-DID_STRING=$(ID_STRING) \

CFLAGS_PK := \
	$(CFLAGS) \
	-static \
	-DBAREMETAL=1 \

CFLAGS_BAREMETAL := \
	$(CFLAGS) \
	-nostdlib \
	-nostartfiles \
	-static \
	-T $(BENCH_COMMON)/test.ld \
	-DBAREMETAL=1 \

all: $(tests_baremetal) $(tests_linux) $(tests_pk)

vpath %.c $(src_dir)

$(GENERATED_HEADERS): $(abs_top_srcdir)/gen_headers.py
	cd $(abs_top_srcdir) && python3 $(abs_top_srcdir)/gen_headers.py $@

conv_%-baremetal: $(src_dir)/conv.c $(EINSTEIN_HEADERS)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -DLAYER=$$(sed 's/.*_\([0-9]\+\)-.*/\1/' <<< $(@)) $(src_dir)/conv.c $(LFLAGS) -o $@ \
		$(wildcard $(BENCH_COMMON)/*.c) $(wildcard $(BENCH_COMMON)/*.S) $(LIBS)

conv_%-linux: $(src_dir)/conv.c $(EINSTEIN_HEADERS)
	$(CC_LINUX) $(CFLAGS) -DLAYER=$$(sed 's/.*_\([0-9]\+\)-.*/\1/' <<< $(@)) $(src_dir)/conv.c $(LFLAGS) -o $@

conv_%-pk: $(src_dir)/conv.c $(EINSTEIN_HEADERS)
	$(CC_LINUX) $(CFLAGS_PK) -DLAYER=$$(sed 's/.*_\([0-9]\+\)-.*/\1/' <<< $(@)) $(src_dir)/conv.c $(LFLAGS) -o $@

run-baremetal: $(runs_baremetal)

%-baremetal.run: %-baremetal
	$(RUNNER)$(abs_top_srcdir)/build/alexnet/$^

junk += $(tests_baremetal) $(tests_linux) $(tests_pk)

