// See LICENSE for license details.

#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef BAREMETAL
#include <sys/mman.h>
#endif

#define CHECK_RESULT

#define ELEM_T_DEFINED
typedef
#ifdef FLAT
int64_t
#else
int32_t
#endif
elem_t;
#include "include/stellar_params.h"

#include "include/stellar.h"
#include "include/stellar_tests.h"

// We use a really hacky method here to include the right params header based on the SparseSuite matrix we want to use
#define HACKIDENT(x) x
#define HACKXSTR(x) #x
#define HACKSTR(x) HACKXSTR(x)
#define HACKPATH(x,y) HACKSTR(HACKIDENT(x)HACKIDENT(y))
#define HACKSUFFIX _params.h
#include HACKPATH(MATRIX,HACKSUFFIX)

#define UNMERGED_BUFFER_ID 0
#define MERGED_BUFFER_ID 1
#define ROW_ID_BUFFER_ID 2

#define CONFIG_MERGED 0
#define CONFIG_UNMERGED 1

#ifdef FLAT
#define FORMAT_STR "%llx"
#else
#define FORMAT_STR "%lld"
#endif

#define PRINT_ARRAY(array) { \
    printf(#array " (%p):\n\t", array); \
    for (size_t i = 0; i < sizeof(array) / sizeof(array[0]); i++) { \
        printf(FORMAT_STR " ", (int64_t)array[i]); \
    } \
    printf("\n"); \
}

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    printf("MATRIX = " HACKSTR(MATRIX) " "
#ifdef FLAT
    "FLAT"
#else
    "GROUPED"
#endif
    " | RADIX = %d | N_MERGED_MATS = %d | N_PARTIAL_MATS = %d\n", RADIX, N_MERGED_MATS, N_PARTIAL_MATS);

    uint64_t total_cycles = 0, start, end;

    for (size_t merged_mat_id = 0; merged_mat_id < N_MERGED_MATS; merged_mat_id++) {
        if ((merged_mat_id+1) % 20 == 0 /* || (merged_mat_id+1) > 90 */)
            printf(/*"\n"*/ "Merge=%d\n", merged_mat_id+1);

        const size_t partial_mats_id = merged_mat_id <= LAST_MERGE_ID ? merged_mat_id * RADIX : LAST_MERGE_ID*RADIX + (merged_mat_id - LAST_MERGE_ID) * LAST_MERGE_RADIX;

        int _radix = merged_mat_id < LAST_MERGE_ID ? RADIX : LAST_MERGE_RADIX;
        if (partial_mats_id + _radix > N_PARTIAL_MATS)
            _radix = N_PARTIAL_MATS - partial_mats_id;

        // Initialize merged SRAM
        {
            size_t n_inner_heads = 1;

#ifndef FLAT
            n_inner_heads = golden_grouped_merged_axis_1_row_ids[merged_mat_id + 1] - golden_grouped_merged_axis_1_row_ids[merged_mat_id];

            if (n_inner_heads == 0)
                continue;

            // Reset outer axis pointers
            // printf("\tReset outer axis pointers\n");
            stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
            stellar_set_constant(CONSTANT_RESETRUNNINGSTATE, true);
            stellar_set_constant(CONSTANT_AXIS, 1);
            stellar_issue();
            stellar_fence();

            // Initialize outer head pointers
            // printf("\tInitialize outer head pointers\n");
            stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
            stellar_set_constant(CONSTANT_ISDATA, false);
            stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_HEAD);
            stellar_set_constant(CONSTANT_AXIS, 1);
            stellar_issue();
            stellar_fence();

            // Write garbage values into outer axis
            // printf("Write garbage values into outer axis\n");
            if (n_inner_heads > 0) {
                stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
                stellar_set_constant(CONSTANT_ISDATA, false);
                stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_COORD);
                stellar_set_constant(CONSTANT_AXIS, 1);
                stellar_set_span(FOR_BOTH, 1, n_inner_heads);
                stellar_issue();
                stellar_fence();
            }
#endif

            // Reset innermost axis pointers
            // printf("\tReset inner axis pointers\n");
            stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
            stellar_set_constant(CONSTANT_RESETRUNNINGSTATE, true);
            stellar_issue();
            stellar_fence();

            // Initialize inner head pointers
            // printf("\tInitialize inner head pointers\n\t\tn_inner_heads=%d\n", n_inner_heads);
            if (n_inner_heads > 0) {
                stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
                stellar_set_constant(CONSTANT_ISDATA, false);
                stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_HEAD);
                stellar_set_span(FOR_BOTH, 1, n_inner_heads);
                stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1);
                stellar_issue();
                stellar_fence();
            }

#ifndef FLAT
            // Remove garbage values from outer-axis which we added earlier so we could initialize inner-axis head-pointers
            stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
            stellar_set_constant(CONSTANT_RESETRUNNINGSTATE, true);
            stellar_set_constant(CONSTANT_AXIS, 1);
            stellar_issue();
            stellar_fence();

            stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, MERGED_BUFFER_ID);
            stellar_set_constant(CONSTANT_ISDATA, false);
            stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_HEAD);
            stellar_set_constant(CONSTANT_AXIS, 1);
            stellar_issue();
#endif
        }

        stellar_fence();

        // Move-in unmerged matrices from DRAM
        // printf("Move-in unmerged matrices from DRAM\n");
        {
#ifdef FLAT
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, UNMERGED_BUFFER_ID);
            stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);
            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, _radix);
            stellar_set_data_address(FOR_SRC, flat_unmerged_axis_0_coords);
            stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, flat_unmerged_axis_0_row_ids + partial_mats_id);
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);
#else

            size_t n_inner_heads = grouped_unmerged_axis_1_row_ids[partial_mats_id + _radix] - grouped_unmerged_axis_1_row_ids[partial_mats_id];

            // Move-in outer unmerged CSR
            // printf("\tMove-in outer unmerged CSR\n");
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, UNMERGED_BUFFER_ID);
            stellar_set_axis(FOR_BOTH, 1, AXIS_COMPRESSED);
            stellar_set_span(FOR_BOTH, 0, 0);
            stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 2, _radix);
            stellar_set_metadata_address(FOR_SRC, 1, METADATA_COMPRESSED_INNER, grouped_unmerged_axis_1_coords);
            stellar_set_metadata_address(FOR_SRC, 1, METADATA_COMPRESSED_OUTER, grouped_unmerged_axis_1_row_ids + partial_mats_id);
            stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_COMPRESSED_OUTER, 1);

            stellar_issue();
            stellar_fence();

            // Move-in inner unmerged CSR
            // printf("\tMove-in inner unmerged CSR\n\t\tunmerged n_inner_heads=%d\n", n_inner_heads);
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, UNMERGED_BUFFER_ID);
            stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);
            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, n_inner_heads);
            stellar_set_data_address(FOR_SRC, grouped_unmerged_axis_0_coords);
            stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, grouped_unmerged_axis_0_row_ids + grouped_unmerged_axis_1_row_ids[partial_mats_id]);
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_END, ((uint64_t)1 << 31) | _radix);
#endif
            stellar_issue();
        }

        // Move-in row-ids to sort from DRAM
#ifndef FLAT
        // printf("Move-in row-ids to sort from DRAM\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, ROW_ID_BUFFER_ID);
            stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);
            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, _radix);
            stellar_set_data_address(FOR_SRC, grouped_unmerged_axis_1_coords);
            stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, grouped_unmerged_axis_1_row_ids + partial_mats_id);
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

            stellar_issue();
        }
#endif

        stellar_fence();

        // printf("Execute merge\n");

        // Move row-ids into regfiles
#ifndef FLAT
        {
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, ROW_ID_BUFFER_ID, SRC_DST_IS_REGFILE, 0);

            stellar_set_constant(CONSTANT_LASTAXIS, 1);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, _radix);

            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

            stellar_issue();
        }
#endif

        // Move unmerged matrices into regfiles
        {
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, UNMERGED_BUFFER_ID, SRC_DST_IS_REGFILE, 0);

            stellar_load_saved_config(CONFIG_UNMERGED);

            // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1)); // From saved config
            // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

#ifdef FLAT
            stellar_set_constant(CONSTANT_LASTAXIS, 1);
            stellar_set_span(FOR_BOTH, 1, _radix);
            // stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1); // From saved config
#else
            stellar_set_constant(CONSTANT_LASTAXIS, 2);
            // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1)); // From saved config
            stellar_set_span(FOR_BOTH, 2, _radix);
            // stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);
            // stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_INNER, 1);
            // stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_COMPRESSED_OUTER, 1); // From saved config
#endif

            start = read_cycles();
            stellar_issue();
        }

        // Move result from regfiles into SRAM
        {
            stellar_set_src_and_dst(SRC_DST_IS_REGFILE,
#ifdef FLAT
            0
#else
            3
#endif
            , SRC_DST_IS_SRAM, MERGED_BUFFER_ID);

            stellar_load_saved_config(CONFIG_MERGED);

            // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1)); // Set by saved-config
            // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1); // Set by saved-config

#ifdef FLAT
            stellar_set_constant(CONSTANT_LASTAXIS, 0);
#else
            stellar_set_constant(CONSTANT_LASTAXIS, 1);
            // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1)); // Set by saved-config
            // stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1); // Set by saved-config
            // stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_LL_COORD, 1); // Set by saved-config
#endif

            stellar_issue();
        }

        stellar_fence();
        end = read_cycles();

        total_cycles += end-start;

#ifdef CHECK_RESULT
        // Move result into DRAM
        // printf("Move result into DRAM\n");
        {
#ifdef FLAT
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, MERGED_BUFFER_ID, SRC_DST_IS_DRAM, 0);
            stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);
            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

            stellar_set_data_address(FOR_DST, flat_merged_axis_0_coords + golden_flat_merged_axis_0_row_ids[merged_mat_id]);
            stellar_set_metadata_address(FOR_DST, 0, METADATA_COMPRESSED_OUTER, flat_merged_axis_0_row_ids + merged_mat_id);
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);
#else // ifndef FLAT
            // Mvout inner coords
            // printf("Mvout inner coords\n\t%p\n\t%p\n", grouped_merged_axis_0_coords, grouped_merged_axis_0_coords + golden_grouped_merged_axis_0_row_ids[golden_grouped_merged_axis_1_row_ids[merged_mat_id]]);
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, MERGED_BUFFER_ID, SRC_DST_IS_DRAM, 0);
            for (size_t axis = 0; axis < 2; axis++) {
                stellar_set_axis(FOR_DST, axis, AXIS_LINKEDLIST);
                stellar_set_span(FOR_DST, axis, (uint32_t)(-1));
            }
            stellar_set_data_address(FOR_DST, grouped_merged_axis_0_coords);
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_INNER, 1);
            stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

#endif

            stellar_issue();
        }

        stellar_fence();
#endif
    }

    const uint64_t ideal_cycles =
#ifdef FLAT
            N_FLAT_CYCLES
#else
            N_GROUPED_CYCLES
#endif
    ;
    printf("\nTotal cycles = %llu\n", total_cycles);
    printf("Ideal cycles = %llu\n", ideal_cycles);
    if (total_cycles > 0) printf("Scale = %llu%%\n\n", (ideal_cycles * 100) / total_cycles);

#ifdef CHECK_RESULT
    // Test outputs
    printf("Test outputs\n");
    if (!(
#ifdef FLAT
            buffers_are_equal(flat_merged_axis_0_coords, golden_flat_merged_axis_0_coords, sizeof(flat_merged_axis_0_coords))
#else // ifndef FLAT
            buffers_are_equal(grouped_merged_axis_0_coords, golden_grouped_merged_axis_0_coords, sizeof(grouped_merged_axis_0_coords))
#endif
    )) {
        printf("FAILED\n");
#ifdef FLAT
        PRINT_ARRAY(flat_merged_axis_0_coords);
#else
        PRINT_ARRAY(grouped_merged_axis_0_coords);
#endif

        stellar_fence();

        exit(1);
    }

    printf("SUCCEEDED\n");
#else // ifndef CHECK_RESULT
    printf("Not checked\n");
#endif

    exit(0);
}

