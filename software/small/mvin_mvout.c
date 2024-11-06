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

#define DATA_SIZE 21
#define ROWS 4

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    stellar_flush();

    // Initialize problem
    const elem_t in_data[DATA_SIZE] = {1, 20, -3, 400,
        -1, 2, -3, 4, -5, 6, -17, 8, 9, -10, -111, -12, 13, 14, 15, 61, 71};
    const elem_t in_col_ids[DATA_SIZE] = {4, 10, 11, 1,
        0, 2, 4, 6, 10, 11, 12, 13, 20, 30, 31, 32, 33, 34, 55, 66, 77};
    const elem_t in_row_ids[ROWS+1] = {0, 3, 3, 4, 21};

    elem_t csr_out_data[DATA_SIZE];
    elem_t csr_out_col_ids[DATA_SIZE];
    elem_t csr_out_row_ids[ROWS+1];

    elem_t ll_out_data[DATA_SIZE];
    elem_t ll_out_col_ids[DATA_SIZE];
    elem_t ll_out_row_ids[ROWS+1];

    /*
    printf("Addresses\n");
    printf("\tin_data: %p\n", in_data);
    printf("\tin_col_ids: %p\n", in_col_ids);
    printf("\tin_row_ids: %p\n\n", in_row_ids);
    printf("\tcsr_out_data: %p\n", csr_out_data);
    printf("\tcsr_out_col_ids: %p\n", csr_out_col_ids);
    printf("\tcsr_out_row_ids: %p\n\n", csr_out_row_ids);
    printf("\tll_out_data: %p\n", ll_out_data);
    printf("\tll_out_col_ids: %p\n", ll_out_col_ids);
    printf("\tll_out_row_ids: %p\n\n", ll_out_row_ids);
    */

#if AXIS_COMPRESSED >= AXIS_LINKEDLIST
#error The for loop below needs to be reordered
#endif
    for (int axis_type = AXIS_COMPRESSED; axis_type <= AXIS_LINKEDLIST; axis_type++) {
        elem_t * out_data = axis_type == AXIS_COMPRESSED ? csr_out_data : ll_out_data;
        elem_t * out_col_ids = axis_type == AXIS_COMPRESSED ? csr_out_col_ids : ll_out_col_ids;
        elem_t * out_row_ids = axis_type == AXIS_COMPRESSED ? csr_out_row_ids : ll_out_row_ids;

        int sram_code = axis_type == AXIS_COMPRESSED ? 1 : 2;

        int outer_coord_id = axis_type == AXIS_COMPRESSED ? METADATA_COMPRESSED_OUTER : METADATA_LL_HEAD;
        int inner_coord_id = axis_type == AXIS_COMPRESSED ? METADATA_COMPRESSED_INNER : METADATA_LL_COORD;

        if (axis_type == AXIS_COMPRESSED)
            printf("Testing CSR DMA\n");
        else
            printf("\nTesting LL DMA\n");

        // Mvin matrix
        printf("Mvin matrix\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, sram_code);

            stellar_set_data_address(FOR_SRC, in_data);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, ROWS);

            stellar_set_axis(FOR_BOTH, 0, axis_type);

            stellar_set_metadata_address(FOR_SRC, 0, outer_coord_id, in_row_ids);
            stellar_set_metadata_address(FOR_SRC, 0, inner_coord_id, in_col_ids);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, inner_coord_id, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, outer_coord_id, 1);

            stellar_issue();
            stellar_fence();
        }

        // Mvout matrix
        printf("Mvout matrix\n");
        {
            stellar_set_src_and_dst(SRC_DST_IS_SRAM, sram_code, SRC_DST_IS_DRAM, 0);

            stellar_set_data_address(FOR_DST, out_data);

            stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
            stellar_set_span(FOR_BOTH, 1, ROWS);

            stellar_set_axis(FOR_BOTH, 0, axis_type);

            stellar_set_metadata_address(FOR_DST, 0, outer_coord_id, out_row_ids);
            stellar_set_metadata_address(FOR_DST, 0, inner_coord_id, out_col_ids);

            // The innermost (compressed) axis contributes to its own col-coords
            // metadata address, and to nothing else
            stellar_set_metadata_stride(FOR_BOTH, 0, 0, inner_coord_id, 1);

            // The next outer (dense) axis contributes to the innermost
            // (compressed) axis's row-coord metadata address
            stellar_set_metadata_stride(FOR_BOTH, 1, 0, outer_coord_id, 1);

            stellar_issue();
            stellar_fence();
        }

        // Check if matrices are the same
        if (!(buffers_are_equal(in_data, out_data, DATA_SIZE * sizeof(elem_t)) &&
                    buffers_are_equal(in_col_ids, out_col_ids, DATA_SIZE * sizeof(elem_t)) &&
                    buffers_are_equal(in_row_ids, out_row_ids, (ROWS+1) * sizeof(elem_t)))) {
            printf("FAILED\n");

            printf("\nin_data (%p):\n", in_data);
            for (int i = 0; i < DATA_SIZE; i++) {
                printf("%d ", in_data[i]);
            }
            printf("\n");

            printf("\nin_col_ids (%p):\n", in_col_ids);
            for (int i = 0; i < DATA_SIZE; i++) {
                printf("%d ", in_col_ids[i]);
            }
            printf("\n");

            printf("\nin_row_ids (%p):\n", in_row_ids);
            for (int i = 0; i < ROWS+1; i++) {
                printf("%d ", in_row_ids[i]);
            }
            printf("\n");

            printf("\nout_data (%p):\n", out_data);
            for (int i = 0; i < DATA_SIZE; i++) {
                printf("%d ", out_data[i]);
            }
            printf("\n");

            printf("\nout_col_ids (%p):\n", out_col_ids);
            for (int i = 0; i < DATA_SIZE; i++) {
                printf("%d ", out_col_ids[i]);
            }
            printf("\n");

            printf("\nout_row_ids (%p):\n", out_row_ids);
            for (int i = 0; i < ROWS+1; i++) {
                printf("%d ", out_row_ids[i]);
            }
            printf("\n");

            exit(1);
        }
    }

    printf("SUCCEEDED\n");

    exit(0);
}

