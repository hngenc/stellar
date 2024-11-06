include $(abs_top_srcdir)/Makefrag

nsplits = 1

tests = \
    wiki_Vote \
    # cit_Patents \
    p2p_Gnutella31 \
    email_Enron \
    ca_CondMat \
    poisson3Da \
    patents_main \
    scircuit \
    m133_b3 \
    2cubes_sphere \
    filter3D \
    cage12 \
    amazon0312 \
    cop20k_A \
    mario002 \
    offshore \
    roadNet_CA \
    webbase_1M \
    web_Google \
    # TODO Add Facebook matrix as well. For some reason, I can't find it online

numbered_tests = $(foreach i,$(shell seq 0 $$(( $(nsplits) - 1 )) ),$(tests:=_split$i))

tests_baremetal = $(numbered_tests:=-flat-baremetal) $(numbered_tests:=-grouped-baremetal)

ifdef BAREMETAL_ONLY
	tests_linux =
	tests_pk =
else
	tests_linux = $(numbered_tests:=-flat-linux) $(numbered_tests:=-grouped-linux)
	tests_pk = $(numbered_tests:=-flat-pk) $(numbered_tests:=-grouped-pk)
endif

params_flat_baremetal = $(numbered_tests:=_params_flat_baremetal.o)
params_grouped_baremetal = $(numbered_tests:=_params_grouped_baremetal.o)

params_flat_linux = $(numbered_tests:=_params_flat_linux.o)
params_grouped_linux = $(numbered_tests:=_params_grouped_linux.o)

params_flat_pk = $(numbered_tests:=_params_flat_pk.o)
params_grouped_pk = $(numbered_tests:=_params_grouped_pk.o)

sim_outputs = $(tests_baremetal:=.log)

RISCV_TESTS = $(abs_top_srcdir)/../../../toolchains/riscv-tools/riscv-tests
BENCH_COMMON = $(RISCV_TESTS)/benchmarks/common
GENERATED_HEADERS = $(addprefix $(src_dir)/,$(tests:=_params.h))
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

# all: $(GENERATED_HEADERS)
all: $(tests_baremetal) $(params_flat_baremetal) $(params_grouped_baremetal)
# all: $(sim_outputs)

vpath %.c $(src_dir)

$(GENERATED_HEADERS): $(abs_top_srcdir)/gen_headers.py
	cd $(abs_top_srcdir) && python3 $(abs_top_srcdir)/gen_headers.py $@ $(nsplits)

%_params_flat_baremetal.o: $(abs_top_srcdir)/include/stellar_params.h $(GENERATED_HEADERS)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -c $(src_dir)/$*_params_flat.c $(LFLAGS) -o $@

%_params_grouped_baremetal.o: $(abs_top_srcdir)/include/stellar_params.h $(GENERATED_HEADERS)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -c $(src_dir)/$*_params_grouped.c $(LFLAGS) -o $@

%-flat-baremetal: $(src_dir)/merge.c $(EINSTEIN_HEADERS) $(params_flat_baremetal)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -DMATRIX=$(*) -DFLAT $(src_dir)/merge.c $(*)_params_flat_baremetal.o $(LFLAGS) \
		-o $@ $(wildcard $(BENCH_COMMON)/*.c) $(wildcard $(BENCH_COMMON)/*.S) $(LIBS)

%-grouped-baremetal: $(src_dir)/merge.c $(EINSTEIN_HEADERS) $(params_grouped_baremetal)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -DMATRIX=$(*) $(src_dir)/merge.c $(*)_params_grouped_baremetal.o $(LFLAGS) \
		-o $@ $(wildcard $(BENCH_COMMON)/*.c) $(wildcard $(BENCH_COMMON)/*.S) $(LIBS)

FLAT_SIZE=Large
GROUPED_SIZE=Larger
SIMULATOR=verilator

%-flat-baremetal.log: $(tests_baremetal)
	if grep -q 'N_MERGED_MATS 0' $(src_dir)/$*_params.h; then \
		echo 'Total cycles = 0' > $(abs_top_srcdir)/build/sparch/$@; \
	else \
		cd $(abs_top_srcdir)/../../../sims/$(SIMULATOR)/ && \
			make CONFIG=$(FLAT_SIZE)ExpensiveDummySpArchMergerStellarRocketConfig run-binary-hex BINARY=../../generators/stellar/software/build/sparch/$*-flat-baremetal &> $(abs_top_srcdir)/build/sparch/$@; \
	fi

%-grouped-baremetal.log: $(tests_baremetal)
	if grep -q 'N_MERGED_MATS 0' $(src_dir)/$*_params.h; then \
		echo 'Total cycles = 0' > $(abs_top_srcdir)/build/sparch/$@; \
	else \
		cd $(abs_top_srcdir)/../../../sims/$(SIMULATOR)/ && \
			make CONFIG=$(GROUPED_SIZE)CheapDummySpArchMergerStellarRocketConfig run-binary-hex BINARY=../../generators/stellar/software/build/sparch/$*-grouped-baremetal &> $(abs_top_srcdir)/build/sparch/$@; \
	fi

junk += $(tests_baremetal) $(params_flat_baremetal) $(params_grouped_baremetal) $(GENERATED_HEADERS)

