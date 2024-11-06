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

#include "scnn_params.h"

#define N_AXES 6

void mvinCSRs(int sram1, int sram2, int n_row_ids, int n_data,
        const elem_t * row_ids,
        const elem_t * data1, const elem_t * data2,
        const elem_t * coords1, const elem_t * coords2) {
    /* TODO This function was written a while ago, when the DMA was less
     * efficient and programmers could get better performance by moving in
     * row-ids and data seperately. With the newer DMA, a single CSR mvin should
     * be much faster than this, so we should replace this function. */

    const int srams[2] = {sram1, sram2};
    const int n_srams = sizeof(srams) / sizeof(srams[0]);

    // Mvin row-ids
    printf("\tMvin row-ids\n");
    {
        for (int sram_id = 0; sram_id < n_srams; sram_id++) {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, srams[sram_id]);

            stellar_set_constant(CONSTANT_ISDATA, false);
            stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_COMPRESSED_OUTER);

            stellar_set_data_address(FOR_SRC, row_ids);

            stellar_set_span(FOR_BOTH, 1, n_row_ids);

            stellar_set_stride(FOR_SRC, 1, 1);
            stellar_set_metadata_stride(FOR_DST, 1, 0, METADATA_COMPRESSED_OUTER, 1);

            stellar_issue();
        }

        stellar_fence();
    }

    // Mvin data
    printf("\tMvin data\n");
    {
        const elem_t* data_ptr[] = {data1, data2};

        for (int sram_id = 0; sram_id < n_srams; sram_id++) {
            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, srams[sram_id]);

            stellar_set_data_address(FOR_SRC, data_ptr[sram_id]);

            stellar_set_span(FOR_BOTH, 0, n_data);

            stellar_set_metadata_stride(FOR_DST, 0, 0, METADATA_COMPRESSED_INNER, 1);

            stellar_issue();
        }
    }

    // Mvin coords
    printf("\tMvin coords\n");
    {
        const elem_t* coords_ptr[] = {coords1, coords2};

        for (int sram_id = 0; sram_id < n_srams; sram_id++) {
            if (coords_ptr[sram_id] == 0)
                continue;

            stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, srams[sram_id]);

            stellar_set_constant(CONSTANT_ISDATA, false);
            stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_COMPRESSED_INNER);

            stellar_set_data_address(FOR_SRC, coords_ptr[sram_id]);

            stellar_set_span(FOR_BOTH, 0, n_data);

            stellar_set_metadata_stride(FOR_DST, 0, 0, METADATA_COMPRESSED_INNER, 1);

            stellar_issue();
        }
    }
}

void mvinImage() {
    mvinCSRs(SRAM_IMAGE_VALUE_AND_H, SRAM_IMAGE_W,
            sizeof(image_row_ids)/sizeof(image_row_ids[0]), sizeof(image_data)/sizeof(image_data[0]),
            image_row_ids,
            image_data, image_w,
            image_h, 0);
}

void mvinFilter() {
    mvinCSRs(SRAM_FILTER_VALUE_AND_H, SRAM_FILTER_W_AND_OUT_CHANNEL,
            sizeof(weight_row_ids)/sizeof(weight_row_ids[0]), sizeof(weight_data)/sizeof(weight_data[0]),
            weight_row_ids,
            weight_data, weight_w,
            weight_h, weight_out_channel);
}

void mvinParams() {
    static const elem_t params[] = {TILE_K, TILE_W, TILE_H, (TILE_K*TILE_W*TILE_H) / 32};
    const int n_params = sizeof(params)/sizeof(params[0]);

    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, SRAM_PARAMS);

    stellar_set_data_address(FOR_SRC, params);

    stellar_set_span(FOR_BOTH, 0, n_params);

    stellar_issue();
}

void mvoutImage() {
    const int srams[2] = {SRAM_IMAGE_VALUE_AND_H, SRAM_IMAGE_W};
    const int n_srams = sizeof(srams) / sizeof(srams[0]);

    for (int sram_id = 0; sram_id < n_srams; sram_id++) {
        stellar_set_src_and_dst(SRC_DST_IS_SRAM, srams[sram_id], SRC_DST_IS_REGFILE, 0);

        stellar_set_constant(CONSTANT_LASTAXIS, 1);

        stellar_set_span(FOR_BOTH, 0, MAX_I);
        stellar_set_span(FOR_BOTH, 1, MAX_WT * MAX_HT);
        stellar_set_span(FOR_BOTH, 2, I_DIV_MAX_I);
        stellar_set_span(FOR_BOTH, 3, K_DIV_KC);
        stellar_set_span(FOR_BOTH, 4, INPUT_CHANNELS);

        stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);
        stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);
        stellar_set_metadata_stride(FOR_BOTH, 2, 0, METADATA_COMPRESSED_OUTER, MAX_WT * MAX_HT);
        stellar_set_metadata_stride(FOR_BOTH, 4, 0, METADATA_COMPRESSED_OUTER, MAX_WT * MAX_HT * I_DIV_MAX_I);

        stellar_issue();
    }
}

void mvoutFilter() {
    const int srams[2] = {SRAM_FILTER_VALUE_AND_H, SRAM_FILTER_W_AND_OUT_CHANNEL};
    const int n_srams = sizeof(srams) / sizeof(srams[0]);

    for (int sram_id = 0; sram_id < n_srams; sram_id++) {
        stellar_set_src_and_dst(SRC_DST_IS_SRAM, srams[sram_id], SRC_DST_IS_REGFILE, 0);

        stellar_set_constant(CONSTANT_LASTAXIS, 1);

        stellar_set_span(FOR_BOTH, 0, -1);
        stellar_set_span(FOR_BOTH, 1, 1 /*MAX_WT * MAX_HT*/);
        stellar_set_span(FOR_BOTH, 2, I_DIV_MAX_I);
        stellar_set_span(FOR_BOTH, 3, K_DIV_KC);
        stellar_set_span(FOR_BOTH, 4, INPUT_CHANNELS);

        stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_COMPRESSED_INNER, 1);
        stellar_set_metadata_stride(FOR_BOTH, 3, 0, METADATA_COMPRESSED_OUTER, 1);
        stellar_set_metadata_stride(FOR_BOTH, 4, 0, METADATA_COMPRESSED_OUTER, K_DIV_KC);

        stellar_issue();
    }
}

void mvoutParams() {
    stellar_set_src_and_dst(SRC_DST_IS_SRAM, SRAM_PARAMS, SRC_DST_IS_REGFILE, 0);

    stellar_set_constant(CONSTANT_LASTAXIS, 1);

    stellar_set_span(FOR_BOTH, 0, 4);
    stellar_set_span(FOR_BOTH, 1, 1);
    stellar_set_span(FOR_BOTH, 2, I_DIV_MAX_I);
    stellar_set_span(FOR_BOTH, 3, K_DIV_KC);
    stellar_set_span(FOR_BOTH, 4, INPUT_CHANNELS);

    stellar_issue();
}

void mvinResult() {
    stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, SRAM_RESULT);

    stellar_set_constant(CONSTANT_LASTAXIS, 1);

    stellar_set_span(FOR_BOTH, 0, -1);
    stellar_set_span(FOR_BOTH, 1, N_BANKS);
    stellar_set_span(FOR_BOTH, 2, I_DIV_MAX_I);
    stellar_set_span(FOR_BOTH, 3, K_DIV_KC);
    stellar_set_span(FOR_BOTH, 4, INPUT_CHANNELS);

    stellar_issue();
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

    printf("Moving in image...\n");
    mvinImage();

    printf("Moving in filter...\n");
    mvinFilter();

    printf("Moving in params...\n");
    mvinParams();

    stellar_fence();

    printf("Performing conv...\n");
    uint64_t start = read_cycles();
    {
        /* Execution loop:
         *  for c
         *      for k_outer
         *          for i_outer
         *              for wt*ht: LAST_AXIS
         *                  parallel for i,j
         */

        mvoutImage();
        mvoutFilter();
        mvoutParams();
        mvinResult();

        stellar_fence();
    }
    uint64_t end = read_cycles();

    // Print stats
    printf("Cycles: %llu\n", end-start);

    if (end-start < IDEAL_CYCLES) {
        printf("Impossible utilization!\n");
        exit(1);
    }

    exit(0);
}

