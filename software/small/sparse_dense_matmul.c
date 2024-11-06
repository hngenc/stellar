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

#define DIM 2
#define N_AXES 5
#define N_METADATA_BUFFERS 4

#define GOLDEN_COLS 4

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    stellar_flush();

    // Initialize problem
    const elem_t left[DIM][DIM] stellar_align = {{39, 71}, {64, 6}};

    const elem_t right[DIM*DIM] stellar_align = {41, -40, -36, -18};
    const elem_t right_col_ids[DIM * DIM] stellar_align = {4, 12, 7, 9};
    const elem_t right_row_ids[DIM+1] stellar_align = {0, DIM, DIM*2};

    const elem_t bias_row_ids[DIM+1] stellar_align = {0, 0, 0};

    elem_t result[DIM * GOLDEN_COLS] stellar_align;
    elem_t result_col_ids[DIM * GOLDEN_COLS] stellar_align;
    elem_t result_row_ids[DIM+1] stellar_align;

    const elem_t golden[DIM * GOLDEN_COLS] = {1599, -2556, -1278, -1560,
        2624, -216, -108, -2560};
    const elem_t golden_col_ids[DIM * GOLDEN_COLS] = {4, 7, 9, 12, 4, 7, 9, 12};
    const elem_t golden_row_ids[DIM+1] = {0, GOLDEN_COLS, GOLDEN_COLS*2};

    // Calculate matmul on accelerator
    {
        // mv A from DRAM to SRAM
        printf("mv A from DRAM to SRAM\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 0);

            stellar_set_data_address(FOR_SRC, left);

            for (int axis = 0; axis < 2; axis++)
                stellar_set_span(FOR_BOTH, axis, DIM);

            stellar_set_stride(FOR_BOTH, 1, DIM);

            stellar_issue();
            stellar_fence();
        }

        // mv B from DRAM to SRAM
        printf("mv B from DRAM to SRAM\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 1);

            stellar_set_data_address(FOR_SRC, right);

            for (int axis = 0; axis < 2; axis++)
                stellar_set_span(FOR_BOTH, axis, DIM);

            stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);

            stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, right_row_ids);
            stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_INNER, right_col_ids);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

            stellar_issue();
            stellar_fence();
        }

        // mv D from DRAM to SRAM
        printf("mv D from DRAM to SRAM\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 2);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, DIM);

            stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

            stellar_set_metadata_address(FOR_SRC, 0, METADATA_LL_HEAD, bias_row_ids);

            // The outer (dense) axis contributes to the innermost (compressed)
            // axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1);

            stellar_issue();
            stellar_fence();
        }

        printf("Perform matmul\n");

        // mv A from SRAM to reg-file
        // printf("mv A from SRAM to reg-file\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, 0, SRC_DST_IS_REGFILE, 0);

            stellar_set_constant(CONSTANT_LASTAXIS, 1);

            for (int axis = 0; axis < 2; axis++)
                stellar_set_span(FOR_BOTH, axis, DIM);

            stellar_set_stride(FOR_BOTH, 1, DIM);

            stellar_issue();
            stellar_fence();
        }

        // mv B from SRAM to reg-file
        // printf("mv B from SRAM to reg-file\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, 1, SRC_DST_IS_REGFILE, 0);

            stellar_set_constant(CONSTANT_LASTAXIS, 1);

            for (int axis = 0; axis < 2; axis++)
                stellar_set_span(FOR_BOTH, axis, DIM);

            stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

            stellar_issue();
            stellar_fence();
        }

        // printf("mv D from SRAM to reg-file and C from reg-file to SRAM\n");

        // mv D from SRAM to reg-file
        {
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, 2, SRC_DST_IS_REGFILE, 0);

            stellar_set_constant(CONSTANT_LASTAXIS, 1);
            stellar_set_constant(CONSTANT_INTERLEAVEPOP, true);
            stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 2);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, DIM);

            stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

            stellar_issue();
        }

        // mv C from reg-file to SRAM
        {
            stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, 2);

            stellar_set_constant(CONSTANT_LASTAXIS, 1);
            stellar_set_constant(CONSTANT_INTERLEAVEPUSH, true);
            stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 2);
            stellar_set_constant(CONSTANT_SHOULDTRAIL, true);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, DIM);

            stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

            // Set from-regfile coords
            stellar_set_from_regfile_metadata(FOR_DST, 0, METADATA_LL_COORD, true, 1);

            stellar_issue();
            stellar_fence();
        }

        // mv C from SRAM to DRAM
        {
            printf("mv C from SRAM to DRAM\n");

            stellar_set_src_and_dst(SRC_DST_IS_SRAM, 2, SRC_DST_IS_DRAM, 0);

            stellar_set_data_address(FOR_DST, result);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, DIM);

            stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

            stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_HEAD, result_row_ids);
            stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_COORD, result_col_ids);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1);

            stellar_issue();
            stellar_fence();
        }
    }

    printf("Final fence\n");
    stellar_fence();

    // Check if matrices are the same
    bool is_correct = buffers_are_equal(result, golden, DIM * GOLDEN_COLS * sizeof(elem_t)) &&
        buffers_are_equal(result_col_ids, golden_col_ids, DIM * GOLDEN_COLS * sizeof(elem_t)) &&
        buffers_are_equal(result_row_ids, golden_row_ids, (DIM + 1) * sizeof(elem_t));

    if (is_correct)
        printf("SUCCEEDED\n");
    else
        printf("FAILED\n");

    // Print matrices
    printf("\nGolden matrix (%p):\n", golden);
    for (int i = 0; i < DIM * GOLDEN_COLS; i++)
        printf("%d ", golden[i]);
    printf("\n");

    printf("\nGolden matrix col coords (%p):\n", golden_col_ids);
    for (int i = 0; i < DIM * GOLDEN_COLS; i++)
        printf("%d ", golden_col_ids[i]);
    printf("\n");

    printf("\nGolden matrix row coords (%p):\n", golden_row_ids);
    for (int i = 0; i <= DIM; i++) {
        printf("%d ", golden_row_ids[i]);
    }
    printf("\n");

    printf("\nResult matrix (%p):\n", result);
    for (int i = 0; i < DIM * GOLDEN_COLS; i++)
        printf("%d ", result[i]);
    printf("\n");

    printf("\nResult matrix col coords (%p):\n", result_col_ids);
    for (int i = 0; i < DIM * GOLDEN_COLS; i++)
        printf("%d ", result_col_ids[i]);
    printf("\n");

    printf("\nResult matrix row coords (%p):\n", result_row_ids);
    for (int i = 0; i <= DIM; i++) {
        printf("%d ", result_row_ids[i]);
    }
    printf("\n");

    exit(0);
}

