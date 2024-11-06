include $(abs_top_srcdir)/Makefrag

operation = matmul
# operation = merge

nsplits = 1

tests = \
    ca_CondMat \
    # patents_main \
    wiki_Vote \
    m133_b3 \
    p2p_Gnutella31 \
    scircuit \
    # email_Enron \
	2cubes_sphere \
	amazon0312 \
    cage12 \
    cit_Patents \
    cop20k_A \
    filter3D \
    mario002 \
    offshore \
    poisson3Da \
    roadNet_CA \
    webbase_1M \
    web_Google \
    # TODO Add Facebook matrix as well. For some reason, I can't find it online

numbered_tests = $(foreach i,$(shell seq 0 $$(( $(nsplits) - 1 )) ),$(tests:=_split$i))

tests_baremetal = $(numbered_tests:=-baremetal)
params_baremetal = $(numbered_tests:=_params_baremetal.o)

ifdef BAREMETAL_ONLY
	tests_linux =
	tests_pk =
	params_linux =
	params_pk =
else
	tests_linux = $(numbered_tests:=-linux)
	tests_pk = $(numbered_tests:=-pk)
	params_linux = $(numbered_tests:=_params_linux.o)
	params_pk = $(numbered_tests:=_params_pk.o)
endif

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

# all: $(tests_baremetal) $(tests_linux) $(tests_pk) $(params_baremetal) $(params_linux) $(params_pk)
all: $(tests_baremetal) $(params_baremetal)
# all: $(sim_outputs)

vpath %.c $(src_dir)

$(GENERATED_HEADERS): $(abs_top_srcdir)/gen_headers.py
	cd $(abs_top_srcdir) && python3 $(abs_top_srcdir)/gen_headers.py $@ $(nsplits)

%_params_baremetal.o: $(abs_top_srcdir)/include/stellar_params.h $(GENERATED_HEADERS)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -c $(src_dir)/$*_params.c $(LFLAGS) -o $@

%_params_linux.o: $(abs_top_srcdir)/include/stellar_params.h $(GENERATED_HEADERS)
	$(CC_LINUX) $(CFLAGS) -c $(src_dir)/$*_params.c $(LFLAGS) -o $@

%_params_pk.o: $(abs_top_srcdir)/include/stellar_params.h $(GENERATED_HEADERS)
	$(CC_LINUX) $(CFLAGS_PK) -c $(src_dir)/$*_params.c $(LFLAGS) -o $@

%-baremetal: $(src_dir)/$(operation).c $(EINSTEIN_HEADERS) $(params_baremetal)
	$(CC_BAREMETAL) $(CFLAGS_BAREMETAL) -DMATRIX=$(*) $(src_dir)/$(operation).c $(*)_params_baremetal.o $(LFLAGS) \
		-o $@ $(wildcard $(BENCH_COMMON)/*.c) $(wildcard $(BENCH_COMMON)/*.S) $(LIBS)

%-linux: $(src_dir)/$(operation).c $(EINSTEIN_HEADERS) $(params_linux)
	$(CC_LINUX) $(CFLAGS) -DMATRIX=$(*) $(src_dir)/$(operation).c $(*)_params_linux.o $(LFLAGS) -o $@

%-pk: $(src_dir)/$(operation).c $(EINSTEIN_HEADERS) $(params_pk)
	$(CC_LINUX) $(CFLAGS_PK) -DMATRIX=$(*) $(src_dir)/$(operation).c $(*)_params_pk.o $(LFLAGS) -o $@

SIMULATOR=verilator
# SIMULATOR=vcs

%-baremetal.log: $(tests_baremetal)
	if grep -q 'MULT_OPS = 0' $(src_dir)/$*_params.h; then \
		echo 'Cycles taken in this part = 0' > $(abs_top_srcdir)/build/sparsesuite/$@; \
	else \
		cd $(abs_top_srcdir)/../../../sims/$(SIMULATOR)/ && \
			make CONFIG=MidOuterSpaceMatmulStellarRocketConfig run-binary-hex BINARY=../../generators/stellar/software/build/sparsesuite/$*-baremetal &> $(abs_top_srcdir)/build/sparsesuite/$@; \
	fi

junk += $(tests_baremetal) $(tests_linux) $(tests_pk) $(params_baremetal) $(params_linux) $(params_pk)

