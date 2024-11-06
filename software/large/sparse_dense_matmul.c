// See LICENSE for license details.

// TODO Right now, this program uses the loop order i -> j -> k (from outermost
//   to innermost). We should instead use i -> k -> j.

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

#include "sparse_dense_matmul_params.h"

#define DIM 2
#define N_AXES 5
#define N_METADATA_BUFFERS 4

#define CHECK_RESULT

void find_tile_sizes(int dim_i, int dim_j, int dim_k,
        int max_sramA_elems, int max_sramB_elems, int max_sramC_elems,
        int * tile_i, int * tile_j, int * tile_k) {
    *tile_i = dim_i;
    *tile_j = dim_j;
    *tile_k = dim_k;

    int sramA_elems, sramB_elems, sramC_elems;
    int last_reduced = 0;
    bool reduced;

    do {
        sramA_elems = (*tile_i) * (*tile_k);
        sramB_elems = (*tile_k) * (*tile_j);

        const int max_output_cols = MAX_COORD < sramB_elems ? MAX_COORD : sramB_elems;

        sramC_elems = (*tile_i) * max_output_cols;

        reduced = false;

        if (sramC_elems > max_sramC_elems && (*tile_i > DIM || *tile_j > DIM)) {
            if (last_reduced == 0 && *tile_j > DIM) {
                *tile_j -= DIM;
                last_reduced = 1;
                reduced = true;
            } else if (*tile_i > DIM) {
                *tile_i -= DIM;
                last_reduced = 0;
                reduced = true;
            }
        } else if (sramA_elems > max_sramA_elems && (*tile_i > DIM || *tile_k > DIM)) {
            if (last_reduced == 0 && *tile_k > DIM) {
                *tile_k -= DIM;
                last_reduced = 2;
                reduced = true;
            } else if (*tile_i > DIM) {
                *tile_i -= DIM;
                last_reduced = 0;
                reduced = true;
            }
        } else if (sramB_elems > max_sramB_elems && (*tile_k > DIM || *tile_j > DIM)) {
            if (last_reduced == 1 && *tile_k > DIM) {
                *tile_k -= DIM;
                last_reduced = 2;
                reduced = true;
            } else if (*tile_j > DIM) {
                *tile_j -= DIM;
                last_reduced = 1;
                reduced = true;
            }
        }
    } while (reduced && (sramA_elems > max_sramA_elems ||
            sramB_elems > max_sramB_elems ||
            sramC_elems > max_sramC_elems));

    if (!(*tile_i > 0 && *tile_i <= dim_i)) {
        printf("tile_i is incorrect: %d\n", *tile_i);
        exit(1);
    } else if (!(*tile_j > 0 && *tile_j <= dim_j)) {
        printf("tile_j is incorrect: %d\n", *tile_j);
        exit(1);
    } else if (!(*tile_k > 0 && *tile_k <= dim_k)) {
        printf("tile_k is incorrect: %d\n", *tile_k);
        exit(1);
    }
}

void tiled_matmul(int dim_i, int dim_j, int dim_k,
        const elem_t * left, const elem_t * right, const elem_t * right_col_ids,
        const elem_t * right_row_ids, elem_t * result, elem_t * result_col_ids,
        elem_t * result_row_ids, int tile_i, int tile_j, int tile_k) {
    // DRAM levels
    for (int i_dram = 0; i_dram < dim_i; i_dram += tile_i) {
        for (int j_dram = 0; j_dram < dim_j; j_dram += tile_j) {
            // mv D from DRAM to SRAM
            {
                stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 2);

                stellar_set_data_address(FOR_SRC, 0); // TODO we only have this here to prevent an overly-cautious assertion from firing in the RTL. the code should still work without this line though, so we should remove it later

                stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
                stellar_set_span(FOR_BOTH, 1, tile_i);

                stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

                for (int axis = 0; axis < N_AXES; axis++)
                    stellar_set_metadata_address(FOR_SRC, axis, METADATA_LL_HEAD, bias_row_ids);

                // The innermost (compressed) axis contributes to its own col-coords
                // metadata address, and to nothing else
                stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1);

                // The next outer (dense) axis contributes to the innermost
                // (compressed) axis's row-coord metadata address
                stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1);

                stellar_issue();
            }

            for (int k_dram = 0; k_dram < dim_k; k_dram += tile_k) {
                // Scratchpad levels

                // mv A from DRAM to SRAM
                {
                    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 0);

                    stellar_set_data_address(FOR_SRC, left + i_dram * tile_k + k_dram);

                    stellar_set_span(FOR_BOTH, 0, tile_k);
                    stellar_set_span(FOR_BOTH, 1, tile_i);

                    stellar_set_stride(FOR_BOTH, 1, tile_k);

                    stellar_issue();
                }

                // mv B from DRAM to SRAM
                {
                    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 1);

                    stellar_set_data_address(FOR_SRC, right);
                    // stellar_set_data_address(FOR_SRC, right + right_row_ids[k_dram]); // TODO we need to support biases for the row-ids to do this

                    stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
                    stellar_set_span(FOR_BOTH, 1, tile_k);

                    stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);

                    stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, right_row_ids + k_dram);
                    stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_INNER, right_col_ids);
                    // stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_INNER, right_col_ids + right_row_ids[k_dram]); // TODO we need to support biases for the row-ids to do this

                    // The innermost (compressed) axis contributes to its own
                    // col-coords metadata address, and to nothing else
                    stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

                    // The next outer (dense) axis contributes to the innermost
                    // (compressed) axis's row-coord metadata address
                    stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

                    stellar_issue();
                }

                stellar_fence();

                {
                    // Reg file levels

                    // mv A from SRAM to reg-file
                    {
                        stellar_set_src_and_dst(SRC_DST_IS_SRAM, 0, SRC_DST_IS_REGFILE, 0);

                        stellar_set_constant(CONSTANT_LASTAXIS, 1);

                        stellar_set_span(FOR_BOTH, 0, DIM);
                        stellar_set_span(FOR_BOTH, 1, DIM);
                        stellar_set_span(FOR_BOTH, 2, tile_k / DIM);
                        stellar_set_span(FOR_BOTH, 3, tile_j / DIM);
                        stellar_set_span(FOR_BOTH, 4, tile_i / DIM);

                        stellar_set_stride(FOR_BOTH, 0, 1);
                        stellar_set_stride(FOR_BOTH, 1, tile_k);
                        stellar_set_stride(FOR_BOTH, 2, DIM);
                        stellar_set_stride(FOR_BOTH, 3, 0);
                        stellar_set_stride(FOR_BOTH, 4, tile_k * DIM);

                        stellar_issue();
                    }

                    // mv B from SRAM to reg-file
                    {
                        stellar_set_src_and_dst(SRC_DST_IS_SRAM, 1, SRC_DST_IS_REGFILE, 0);

                        stellar_set_constant(CONSTANT_LASTAXIS, 1);

                        for (int axis = 0; axis < 2; axis++)
                            stellar_set_span(FOR_BOTH, axis, DIM);
                        stellar_set_span(FOR_BOTH, 2, tile_k / DIM);
                        stellar_set_span(FOR_BOTH, 3, tile_j / DIM);
                        stellar_set_span(FOR_BOTH, 4, tile_i / DIM);

                        stellar_set_stride(FOR_BOTH, 3, DIM);

                        stellar_set_axis(FOR_BOTH, 0, AXIS_COMPRESSED);

                        // The innermost (compressed) axis contributes to its own col-coords
                        // metadata address, and to nothing else
                        stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

                        // The next outer (dense) axis contributes to the innermost
                        // (compressed) axis's row-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

                        // The following (also dense) outer axis contributes to the
                        // innermost (compressed) axis's row-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 2, 0, METADATA_COMPRESSED_OUTER, DIM);

                        // The following (also dense) outer axis contributes to the
                        // innermost (compressed) axis's col-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 3, 0, METADATA_COMPRESSED_INNER, DIM);

                        stellar_issue();
                    }

                    // mv D from SRAM to reg-file
                    {
                        stellar_set_src_and_dst(SRC_DST_IS_SRAM, 2, SRC_DST_IS_REGFILE, 0);

                        stellar_set_constant(CONSTANT_LASTAXIS, 1);
                        stellar_set_constant(CONSTANT_INTERLEAVEPOP, true);
                        stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 2);

                        stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
                        stellar_set_span(FOR_BOTH, 1, DIM);
                        stellar_set_span(FOR_BOTH, 2, tile_k / DIM);
                        stellar_set_span(FOR_BOTH, 3, tile_j / DIM);
                        stellar_set_span(FOR_BOTH, 4, tile_i / DIM);

                        stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

                        // The innermost (compressed) axis contributes to its own col-coords
                        // metadata address, and to nothing else
                        stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

                        // The next outer (dense) axis contributes to the innermost
                        // (compressed) axis's row-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

                        // The outermost (dense) axis contributes to the innermost
                        // (compressed) axis's row-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 4, 0, METADATA_COMPRESSED_OUTER, DIM);

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
                        stellar_set_span(FOR_BOTH, 2, tile_k / DIM);
                        stellar_set_span(FOR_BOTH, 3, tile_j / DIM);
                        stellar_set_span(FOR_BOTH, 4, tile_i / DIM);

                        stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

                        // The innermost (compressed) axis contributes to its own col-coords
                        // metadata address, and to nothing else
                        stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);

                        // The next outer (dense) axis contributes to the innermost
                        // (compressed) axis's row-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

                        // The outermost (dense) axis contributes to the innermost
                        // (compressed) axis's row-coord metadata address
                        stellar_set_metadata_stride(FOR_BOTH, 4, 0, METADATA_COMPRESSED_OUTER, DIM);

                        // Set from-regfile coords
                        stellar_set_from_regfile_metadata(FOR_DST, 0, METADATA_LL_COORD, true, 1);

                        stellar_issue();
                    }

                    stellar_fence();
                }
            }

            // mv C from SRAM to DRAM
            {
                stellar_set_src_and_dst(SRC_DST_IS_SRAM, 2, SRC_DST_IS_DRAM, 0);

                stellar_set_data_address(FOR_DST, result);
                // stellar_set_data_address(FOR_DST, result + result_row_ids[i_dram]); // TODO we need to support biases for the row-ids to do this

                stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
                stellar_set_span(FOR_BOTH, 1, tile_i);

                stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);

                stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_HEAD, result_row_ids + i_dram);
                stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_COORD, result_col_ids);
                // stellar_set_metadata_address(FOR_DST, 0, result_col_ids + result_row_ids[i_dram]); // TODO we need to support biases for the row-ids to do this

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
    }
}

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    printf("Flushing...\n");
    stellar_flush();

    // Find tile sizes
    int tile_i, tile_j, tile_k;

    // const int spad_partitions = 2;
    const int spad_partitions = 1;

    printf("Finding tile sizes...\n");
    find_tile_sizes(MAT_DIM_I, MAT_DIM_J, MAT_DIM_K,
        128*1024/spad_partitions, 128*1024/spad_partitions, 16*1024/spad_partitions, // TODO put these in an stellar-params file
        &tile_i, &tile_j, &tile_k);

    printf("Problem sizes: %d %d %d\n", MAT_DIM_I, MAT_DIM_J, MAT_DIM_K);
    printf("Tile sizes: %d %d %d\n", tile_i, tile_j, tile_k);
    printf("DIM: %d\n", DIM);

    // Perform matmul on Stellar
    printf("Starting matmul...\n");
    uint64_t start = read_cycles();
    tiled_matmul(MAT_DIM_I, MAT_DIM_J, MAT_DIM_K,
            &left[0][0],
            &right[0], &right_col_ids[0], &right_row_ids[0],
            &result[0], &result_col_ids[0], &result_row_ids[0],
            tile_i, tile_j, tile_k);
    uint64_t end = read_cycles();
    printf("Completed matmul\n");

#ifdef CHECK_RESULT
    // Check if matrices are the same
    bool is_correct = buffers_are_equal(result, golden, RESULT_SIZE) &&
        buffers_are_equal(result_col_ids, golden_col_ids, RESULT_SIZE) &&
        buffers_are_equal(result_row_ids, golden_row_ids, MAT_DIM_I);

    if (is_correct)
        printf("SUCCEEDED\n");
    else {
        printf("FAILED\n");

        // Print matrices
        printf("\nGolden matrix (%p):\n", golden);
        for (int i = 0; i < RESULT_SIZE; i++)
            printf("%d ", golden[i]);
        printf("\n");

        printf("\nGolden matrix col coords (%p):\n", golden_col_ids);
        for (int i = 0; i < RESULT_SIZE; i++)
            printf("%d ", golden_col_ids[i]);
        printf("\n");

        printf("\nGolden matrix row coords (%p):\n", golden_row_ids);
        for (int i = 0; i <= MAT_DIM_I; i++) {
            printf("%d ", golden_row_ids[i]);
        }
        printf("\n");

        printf("\nResult matrix (%p):\n", result);
        for (int i = 0; i < RESULT_SIZE; i++)
            printf("%d ", result[i]);
        printf("\n");

        printf("\nResult matrix col coords (%p):\n", result_col_ids);
        for (int i = 0; i < RESULT_SIZE; i++)
            printf("%d ", result_col_ids[i]);
        printf("\n");

        printf("\nResult matrix row coords (%p):\n", result_row_ids);
        for (int i = 0; i <= MAT_DIM_I; i++) {
            printf("%d ", result_row_ids[i]);
        }
        printf("\n");
    }
#else
    printf("Result not checked\n");
#endif

    printf("Cycles: %llu\n", end-start);

    exit(0);
}

