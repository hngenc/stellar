// See LICENSE for license details.

#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef BAREMETAL
#include <sys/mman.h>
#endif

#include "include/stellar.h"
#include "include/stellar_tests.h"

// We use a really hacky method here to include the right params header based on the SparseSuite matrix we want to use
#define HACKIDENT(x) x
#define HACKXSTR(x) #x
#define HACKSTR(x) HACKXSTR(x)
#define HACKPATH(x,y) HACKSTR(HACKIDENT(x)HACKIDENT(y))
#define HACKSUFFIX _params.h
#include HACKPATH(MATRIX,HACKSUFFIX)

#if INTERLEAVED_DATA_AND_COORDS == 0
#error "this test assumes that data and coords are interleaved"
#endif

#define CHECK_RESULT

#define CONFIG_RESET_C 3
#define CONFIG_MVIN_SCATTERED 9
#define CONFIG_MVOUT_MERGED 10
#define CONFIG_MERGE_SCATTERED 11
#define CONFIG_MERGE_MERGED 12

int scattered_buffer_id = 0;
// int scattered_buffer_id = 2;
// int merged_buffer_id = 3;

static inline void initScattered() {
    stellar_load_saved_config(CONFIG_RESET_C);

    // stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, scattered_buffer_id);
    // stellar_set_constant(CONSTANT_RESETRUNNINGSTATE, true);
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true);
    // stellar_issue();
}

static inline void mvinScattered(const int i_dram) {
    // stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, scattered_buffer_id);

    // stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);
    // stellar_set_axis(FOR_BOTH, 1, AXIS_LINKEDLIST);

    // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
    // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 2, DIM);

    stellar_set_data_address(FOR_SRC, golden_scattered_data_and_coord_heap);

#ifdef SMALL_I
    stellar_set_metadata_address(FOR_SRC, 0, METADATA_LL_COORD, scattered_data_and_coord_merge_overflow);
    stellar_set_metadata_address(FOR_SRC, 0, METADATA_LL_NEXT, scattered_ptr_packets_merge_overflow);
#endif

    stellar_set_metadata_address(FOR_SRC, 0, METADATA_LL_NEXT, golden_ptr_packets + i_dram * PTR_PACKET_SIZE);
    stellar_set_metadata_address(FOR_SRC, 1, METADATA_LL_NEXT, golden_ptr_packets + i_dram * PTR_PACKET_SIZE);

    stellar_set_metadata_stride(FOR_SRC, 2, 1, METADATA_LL_NEXT, 1 * PTR_PACKET_SIZE);
    // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1); // This line isn't necessary since we're using interleaved data/coords

    // stellar_issue();

    stellar_load_saved_config(CONFIG_MVIN_SCATTERED);
}

static inline void mergeScattered() {
    // stellar_set_src_and_dst(SRC_DST_IS_SRAM, scattered_buffer_id, SRC_DST_IS_REGFILE, 0);

    // stellar_set_constant(CONSTANT_LASTAXIS, 1);
    // stellar_set_constant(CONSTANT_LASTAXISLOGSIZE, LOG_DIM); // Set by hardcoding
    // stellar_set_constant(CONSTANT_SHOULDTRAIL, true);

    // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
    // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 2, DIM);

    // stellar_set_recursive(1, DIM);

    // stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_LL_HEAD, 1); // K-row address
    // stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_LL_COORD, 1); // K-col address
    // stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1); // J-row address
    // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1); // J-col address

    // stellar_wait(WAIT_FOR_DMA | WAIT_FOR_SRAM_WRITES(scattered_buffer_id) | WAIT_FOR_SRAM_WRITES_FROM_DRAM(scattered_buffer_id));

    // stellar_issue();

    stellar_load_saved_config(CONFIG_MERGE_SCATTERED);
}

static inline void mergeMerged() {
    // stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, scattered_buffer_id);

    // stellar_set_constant(CONSTANT_LASTAXIS, 1);
    // stellar_set_constant(CONSTANT_LASTAXISLOGSIZE, 0);

    // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
    // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 2, DIM);

    // stellar_set_recursive(1, DIM);

    // stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_LL_HEAD, 1); // K-row address
    // stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_LL_COORD, 1); // K-col address
    // stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1); // J-row address
    // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1); // J-col address

    // stellar_wait(WAIT_FOR_DMA | WAIT_FOR_SRAM_READS(scattered_buffer_id) | WAIT_FOR_SRAM_WRITES(scattered_buffer_id) | WAIT_FOR_SRAM_WRITES_FROM_DRAM(scattered_buffer_id));
    // stellar_fence();

    // stellar_issue();

    stellar_load_saved_config(CONFIG_MERGE_MERGED);
}

static inline void mvoutMerged(const int i_dram) {
    // stellar_set_src_and_dst(SRC_DST_IS_SRAM, scattered_buffer_id, SRC_DST_IS_DRAM, 0);

    // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
    // stellar_set_span(FOR_BOTH, 1, 1);
    stellar_set_span(FOR_BOTH, 2, DIM);

    stellar_set_data_address(FOR_DST, merged_C_data);

    stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_COORD, merged_C_indices);
    stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_HEAD, merged_C_indptrs + i_dram);

    // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1);
    // stellar_set_metadata_stride(FOR_BOTH, 2, 0, METADATA_LL_HEAD, 1);

    // stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

    // stellar_wait(WAIT_FOR_SRAM_WRITES(scattered_buffer_id));

    // stellar_issue();

    stellar_load_saved_config(CONFIG_MVOUT_MERGED);
}

static void tiled_merge() {
    for (int i_dram = 0; i_dram < I; i_dram += DIM) {
        initScattered();
        mvinScattered(i_dram);

        mergeMerged();
        mergeScattered();

        mvoutMerged(i_dram);
    }

    stellar_fence();
}

void check_result() {
    /*
    printf("Indptrs:\n\t");
    for (int i = 0; i <= I; i++)
        printf("%d ", merged_C_indptrs[i]);
    printf("\nIndices:\n\t");
    for (int i = 0; i < sizeof(merged_C_indices)/sizeof(merged_C_indices[0]); i++)
        printf("%d ", merged_C_indices[i]);
    printf("\nData:\n\t");
    for (int i = 0; i < sizeof(merged_C_data)/sizeof(merged_C_data[0]); i++)
        printf("%d ", merged_C_data[i]);
    printf("\n");
    */

    assert(buffers_are_equal(merged_C_indptrs, golden_merged_C_indptrs, sizeof(golden_merged_C_indptrs)));
    assert(buffers_are_equal(merged_C_indices, golden_merged_C_indices, sizeof(merged_C_indices)));
    assert(buffers_are_equal(merged_C_data, golden_merged_C_data, sizeof(merged_C_data)));
}

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    printf("MATRIX = " HACKSTR(MATRIX) "\n");
    printf("I = %d | J = %d | K = %d | DIM = %d\n", I, J, K, DIM);
    assert(I % DIM == 0);

    stellar_flush();

    uint64_t start = read_cycles();
    tiled_merge();
    uint64_t end = read_cycles();

    printf("Cycles taken = %llu\n", end-start);

#ifdef CHECK_RESULT
    printf("Check result\n");
    check_result();
#else
    printf("Didn't check result\n");
#endif

    printf("SUCCEEDED\n");

    return 0;
}


