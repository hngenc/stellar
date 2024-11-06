// See LICENSE for license details.

#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef BAREMETAL
#include <sys/mman.h>
#endif

typedef int8_t elem_t;
typedef int32_t acc_t;
#define ELEM_T_DEFINED

#include "include/stellar.h"
#include "include/stellar_tests.h"

#define CHECK_RESULT
#include "matmul_params.h"

#define N_SRAMS 3

#if (MAT_DIM_J % DIM != 0) || (MAT_DIM_K % DIM != 0)
#error "we haven't added padding support yet"
#endif

// TODO We should check correctness with much larger matrices which require multiple tiles

static bool wait_for_moved_in_data = false;
static int a_buffer_id = 0, b_buffer_id = 0, c_buffer_id = 0, d_buffer_id = 0;

static void mvinD(const acc_t * bias, int span_i, int span_j, int j_dram) {
    wait_for_moved_in_data = true;

    if (DOUBLE_BUFFERED_CD)
        d_buffer_id = 1 - d_buffer_id;

    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, d_buffer_id * N_SRAMS + 2);

    stellar_set_data_address(FOR_SRC, bias + j_dram);

    stellar_set_span(FOR_BOTH, 0, span_j);
    stellar_set_span(FOR_BOTH, 1, span_i);

    // stellar_set_stride(FOR_SRC, 1, 0); // We repeat the 1d bias across multiple rows
    stellar_set_stride(FOR_DST, 1, span_j);

    stellar_issue();
}

static void mvinA(const elem_t * left, int dim_k, int span_i, int span_k, int i_dram, int k_dram) {
    wait_for_moved_in_data = true;

    if (DOUBLE_BUFFERED_AB)
        a_buffer_id = 1 - a_buffer_id;

    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, a_buffer_id * N_SRAMS + 0);

    stellar_set_data_address(FOR_SRC, left + i_dram * dim_k + k_dram);

    stellar_set_span(FOR_BOTH, 0, span_k);
    stellar_set_span(FOR_BOTH, 1, span_i);

    stellar_set_stride(FOR_SRC, 1, dim_k);
    stellar_set_stride(FOR_DST, 1, span_k);

    stellar_issue();
}

static void mvinB(const elem_t * right, int dim_j, int span_j, int span_k, int j_dram, int k_dram) {
    wait_for_moved_in_data = true;

    if (DOUBLE_BUFFERED_AB)
        b_buffer_id = 1 - b_buffer_id;

    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, b_buffer_id * N_SRAMS + 1);

    stellar_set_data_address(FOR_SRC, right + k_dram * dim_j + j_dram);

    stellar_set_span(FOR_BOTH, 0, span_j);
    stellar_set_span(FOR_BOTH, 1, span_k);

    stellar_set_stride(FOR_SRC, 1, dim_j);
    stellar_set_stride(FOR_DST, 1, span_j);

    stellar_issue();
}

static void mvoutC(elem_t * result, int dim_j, int span_i, int span_j, int i_dram, int j_dram) {
    if (DOUBLE_BUFFERED_CD)
        c_buffer_id = 1 - c_buffer_id;

    stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id * N_SRAMS + 2, SRC_DST_IS_DRAM, 0);

    stellar_set_data_address(FOR_DST, result + i_dram * dim_j + j_dram);

    stellar_set_span(FOR_BOTH, 0, span_j);
    stellar_set_span(FOR_BOTH, 1, span_i);

    stellar_set_stride(FOR_SRC, 1, span_j);
    stellar_set_stride(FOR_DST, 1, dim_j);

    stellar_issue();
}

static void tiled_matmul(int dim_i, int dim_j, int dim_k,
        const elem_t * left, const elem_t * right, const acc_t * bias,
        elem_t * result,
        int tile_i, int tile_j, int tile_k,
        bool double_buffered_ab, bool double_buffered_cd)
{
    // Note: we assume a one-dimensional bias

    assert(c_buffer_id == d_buffer_id);

    const bool single_j_tile = tile_j >= dim_j;
    const bool single_k_tile = tile_k >= dim_k;

    // DRAM levels
    for (int i_dram = 0; i_dram < dim_i; i_dram += tile_i) {
        const int span_i = (dim_i - i_dram) > tile_i ? tile_i : (dim_i - i_dram);

        for (int j_dram = 0; j_dram < dim_j; j_dram += tile_j) {
            const int span_j = (dim_j - j_dram) > tile_j ? tile_j : (dim_j - j_dram);
            assert(span_j % DIM == 0); // TODO currently, we assume here that span_j/i/k is a multiple of DIM

            for (int k_dram = 0; k_dram < dim_k; k_dram += tile_k) {
                const int span_k = (dim_k - k_dram) > tile_k ? tile_k : (dim_k - k_dram);
                assert(span_k % DIM == 0); // TODO currently, we assume here that span_j/i/k is a multiple of DIM

                const bool first = i_dram == 0 && j_dram == 0 && k_dram == 0;
                bool last = false;
                int i_dram_next = i_dram, j_dram_next = j_dram, k_dram_next = k_dram + tile_k;
                if (k_dram_next >= dim_k) {
                    k_dram_next = 0;
                    j_dram_next += tile_j;
                    if (j_dram_next >= dim_j) {
                        j_dram_next = 0;
                        i_dram_next += tile_i;
                        if (i_dram_next >= dim_i)
                            last = true;
                    }
                }
                const int span_i_next = (dim_i - i_dram_next) > tile_i ? tile_i : (dim_i - i_dram_next);
                const int span_j_next = (dim_j - j_dram_next) > tile_j ? tile_j : (dim_j - j_dram_next);
                const int span_k_next = (dim_k - k_dram_next) > tile_k ? tile_k : (dim_k - k_dram_next);

                if (first || !double_buffered_ab) {
                    // mv A from DRAM to SRAM
                    if (!single_k_tile || j_dram == 0)
                        mvinA(left, dim_k, span_i, span_k, i_dram, k_dram);

                    // mv B from DRAM to SRAM
                    if (!single_j_tile || !single_k_tile || i_dram == 0)
                        mvinB(right, dim_j, span_j, span_k, j_dram, k_dram);
                }

                // mv D from DRAM to SRAM
                if (k_dram == 0 && (first || !double_buffered_cd))
                    mvinD(bias, span_i, span_j, j_dram);

                if (wait_for_moved_in_data) {
                    stellar_fence();
                    wait_for_moved_in_data = false;
                }

                {
                    // Reg file levels
                    //   Loop order is k, i, j. (k is outermost)

                    // mv A from SRAM to reg-file
                    stellar_set_src_and_dst(SRC_DST_IS_SRAM, a_buffer_id * N_SRAMS + 0, SRC_DST_IS_REGFILE, 0);

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);

                    stellar_set_span(FOR_BOTH, 0, DIM); // k-inner
                    stellar_set_span(FOR_BOTH, 1, span_i); // i-inner
                    stellar_set_span(FOR_BOTH, 2, span_j / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM
                    stellar_set_span(FOR_BOTH, 3, span_k / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM

                    stellar_set_stride(FOR_BOTH, 1, span_k);
                    stellar_set_stride(FOR_BOTH, 3, DIM);

                    stellar_issue();

                    // mv B from SRAM to reg-file
                    stellar_set_src_and_dst(SRC_DST_IS_SRAM, b_buffer_id * N_SRAMS + 1, SRC_DST_IS_REGFILE, 0);

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);

                    stellar_set_span(FOR_BOTH, 0, DIM); // j-inner
                    stellar_set_span(FOR_BOTH, 1, DIM); // k-inner
                    stellar_set_span(FOR_BOTH, 2, span_j / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM
                    stellar_set_span(FOR_BOTH, 3, span_k / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM

                    stellar_set_stride(FOR_BOTH, 1, span_j);
                    stellar_set_stride(FOR_BOTH, 2, DIM);
                    stellar_set_stride(FOR_BOTH, 3, span_j * DIM);

                    stellar_issue();

                    // mv D from SRAM to reg-file
                    stellar_set_src_and_dst(SRC_DST_IS_SRAM, d_buffer_id * N_SRAMS + 2, SRC_DST_IS_REGFILE, 0);

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);
                    stellar_set_constant(CONSTANT_INTERLEAVEPOP, true);
                    stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 3);

                    stellar_set_span(FOR_BOTH, 0, DIM); // j-inner
                    stellar_set_span(FOR_BOTH, 1, span_i); // i-inner
                    stellar_set_span(FOR_BOTH, 2, span_j / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM
                    stellar_set_span(FOR_BOTH, 3, span_k / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM

                    stellar_set_stride(FOR_BOTH, 1, span_j);
                    stellar_set_stride(FOR_BOTH, 2, DIM);

                    stellar_issue();

                    // mv C from reg-file to SRAM
                    stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, d_buffer_id * N_SRAMS + 2); // We use "d_buffer_id" here instead of "c_buffer_id" because we want to make sure that we're accumulating into the same SRAM bank that we're reading D from

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);
                    stellar_set_constant(CONSTANT_INTERLEAVEPUSH, true);
                    stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 3);

                    stellar_set_span(FOR_BOTH, 0, DIM); // j-inner
                    stellar_set_span(FOR_BOTH, 1, span_i); // i-inner
                    stellar_set_span(FOR_BOTH, 2, span_j / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM
                    stellar_set_span(FOR_BOTH, 3, span_k / DIM); // TODO we assume here that span_j/i/k is a multiple of DIM

                    stellar_set_stride(FOR_BOTH, 1, span_j);
                    stellar_set_stride(FOR_BOTH, 2, DIM);

                    stellar_issue();
                }

                if (!last) {
                    if (double_buffered_ab) {
                        // mv A_next from DRAM to SRAM
                        if (!single_k_tile || j_dram_next == 0)
                            mvinA(left, dim_k, span_i_next, span_k_next, i_dram_next, k_dram_next);

                        // mv B_next from DRAM to SRAM
                        if (!single_j_tile || !single_k_tile || i_dram_next == 0)
                            mvinB(right, dim_j, span_j_next, span_k_next, j_dram_next, k_dram_next);
                    }

                    // mv D_next from DRAM to SRAM
                    if (k_dram_next == 0 && double_buffered_cd)
                        mvinD(bias, span_i_next, span_j_next, j_dram_next);
                }

                if (!double_buffered_ab) {
                    stellar_fence();
                    wait_for_moved_in_data = false;
                }
            }

            stellar_fence();
            wait_for_moved_in_data = false;

            // mv C from SRAM to DRAM
            mvoutC(result, dim_j, span_i, span_j, i_dram, j_dram);

            if (!double_buffered_cd) {
                stellar_fence();
                wait_for_moved_in_data = false;
            }
        }
    }

    stellar_fence();
}

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    printf("Flushing TLB...\n");
    stellar_flush();

    printf("Problem sizes: %d %d %d\n", MAT_DIM_I, MAT_DIM_J, MAT_DIM_K);
    printf("Tile sizes: %d %d %d\n", TILE_I, TILE_J, TILE_K);
    printf("DIM: %d\n", DIM);

    // Perform matmul on Stellar
    printf("Starting matmul...\n");
    uint64_t start = read_cycles();
    tiled_matmul(MAT_DIM_I, MAT_DIM_J, MAT_DIM_K,
            &left[0][0], &right[0][0], &bias[0], &result[0][0],
            TILE_I, TILE_J, TILE_K, DOUBLE_BUFFERED_AB, DOUBLE_BUFFERED_CD);
    uint64_t end = read_cycles();
    printf("Completed matmul\n");

    // Print stats
    uint64_t ideal_cycles = (((uint64_t)MAT_DIM_I)*((uint64_t)MAT_DIM_J)*((uint64_t)MAT_DIM_K)) / (DIM*DIM);
    printf("Ideal cycles: %llu\n", ideal_cycles);
    printf("Actual cycles: %llu\n", end-start);
    printf("Utilization: %llu%%\n", ((uint64_t)100 * ideal_cycles) / (end-start));

#ifdef CHECK_RESULT
    // Check result
    if (!buffers_are_equal((elem_t*)result, (elem_t*)golden, sizeof(result))) {
        printf("FAILED\n");

        printf("\nResult matrix (%p):\n", result);
        for (int i = 0; i < MAT_DIM_I; i++) {
            for (int j = 0; j < MAT_DIM_J; j++) {
                printf("%d ", result[i][j]);
            }
            printf("\n");
        }
    } else {
        printf("SUCCEEDED\n");
    }
#else
    printf("Not checked\n");
#endif

    exit(0);
}

