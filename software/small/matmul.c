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

#define DIM 2
#define N_AXES 2

#if FOR_SRC >= FOR_DST
#error some parts of the code below assume that FOR_SRC is lower than FOR_DST
#endif

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    stellar_flush();

    // Initialize problem
    elem_t left[DIM][DIM] stellar_align;
    elem_t right[DIM][DIM] stellar_align;
    elem_t result[DIM][DIM] stellar_align;

    elem_t golden[DIM][DIM];

    for (int i = 0; i < DIM; i++)
        for (int j = 0; j < DIM; j++) {
            left[i][j] = i + j;
            right[i][j] = i - j;
        }

    for (int i = 0; i < DIM; i++)
        for (int j = 0; j < DIM; j++) {
            golden[i][j] = 0;
            for (int k = 0; k < DIM; k++)
                golden[i][j] += left[i][k] * right[k][j];
        }

    // Calculate matmul on accelerator
    {
        // mv A from DRAM to SRAM
        printf("mv A from DRAM to SRAM\n");

        stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 0);

        stellar_set_constant(CONSTANT_INTERLEAVEPUSH, false);
        stellar_set_constant(CONSTANT_INTERLEAVEPOP, false);
        stellar_set_constant(CONSTANT_SHOULDTRAIL, false);

        stellar_set_data_address(FOR_SRC, left);

        stellar_set_span(FOR_BOTH, 0, DIM);
        stellar_set_span(FOR_BOTH, 1, DIM);

        stellar_set_stride(FOR_BOTH, 0, 1);
        stellar_set_stride(FOR_BOTH, 1, DIM);

        stellar_set_axis(FOR_BOTH, 0, AXIS_DENSE);
        stellar_set_axis(FOR_BOTH, 1, AXIS_DENSE);

        stellar_issue();
        stellar_fence();

        // mv B from DRAM to SRAM
        printf("mv B from DRAM to SRAM\n");

        stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, 1);

        stellar_set_constant(CONSTANT_INTERLEAVEPUSH, false);
        stellar_set_constant(CONSTANT_INTERLEAVEPOP, false);
        stellar_set_constant(CONSTANT_SHOULDTRAIL, false);

        stellar_set_data_address(FOR_SRC, right);

        stellar_set_span(FOR_BOTH, 0, DIM);
        stellar_set_span(FOR_BOTH, 1, DIM);

        stellar_set_stride(FOR_BOTH, 0, 1);
        stellar_set_stride(FOR_BOTH, 1, DIM);

        stellar_set_axis(FOR_BOTH, 0, AXIS_DENSE);
        stellar_set_axis(FOR_BOTH, 1, AXIS_DENSE);

        stellar_issue();
        stellar_fence();

        printf("mv A and B from SRAM to reg-file, and C from reg-file to SRAM\n");

        // mv A from SRAM to reg-file
        // printf("mv A from SRAM to reg-file\n");

        stellar_set_src_and_dst(SRC_DST_IS_SRAM, 0, SRC_DST_IS_REGFILE, 0);

        stellar_set_constant(CONSTANT_ISDATA, true);
        stellar_set_constant(CONSTANT_AXIS, 0);
        stellar_set_constant(CONSTANT_LASTAXIS, 1);
        stellar_set_constant(CONSTANT_INTERLEAVEPUSH, false);
        stellar_set_constant(CONSTANT_INTERLEAVEPOP, false);

        stellar_set_span(FOR_BOTH, 0, DIM);
        stellar_set_span(FOR_BOTH, 1, DIM);

        stellar_set_stride(FOR_BOTH, 0, 1);
        stellar_set_stride(FOR_BOTH, 1, DIM);

        stellar_set_axis(FOR_BOTH, 0, AXIS_DENSE);
        stellar_set_axis(FOR_BOTH, 1, AXIS_DENSE);

        stellar_issue();
        stellar_fence();

        // mv B from SRAM to reg-file
        // printf("mv B from SRAM to reg-file\n");

        stellar_set_src_and_dst(SRC_DST_IS_SRAM, 1, SRC_DST_IS_REGFILE, 0);

        stellar_set_constant(CONSTANT_ISDATA, true);
        stellar_set_constant(CONSTANT_AXIS, 0);
        stellar_set_constant(CONSTANT_LASTAXIS, 1);
        stellar_set_constant(CONSTANT_INTERLEAVEPUSH, false);
        stellar_set_constant(CONSTANT_INTERLEAVEPOP, false);
        stellar_set_constant(CONSTANT_SHOULDTRAIL, false);

        stellar_set_span(FOR_BOTH, 0, DIM);
        stellar_set_span(FOR_BOTH, 1, DIM);

        stellar_set_stride(FOR_BOTH, 0, 1);
        stellar_set_stride(FOR_BOTH, 1, DIM);

        stellar_set_axis(FOR_BOTH, 0, AXIS_DENSE);
        stellar_set_axis(FOR_BOTH, 1, AXIS_DENSE);

        stellar_issue();
        stellar_fence();

        // mv C from reg-file to SRAM
        // printf("mv C from reg-file to SRAM\n");

        stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, 2);

        stellar_set_constant(CONSTANT_ISDATA, true);
        stellar_set_constant(CONSTANT_AXIS, 0);
        stellar_set_constant(CONSTANT_LASTAXIS, 1);
        stellar_set_constant(CONSTANT_INTERLEAVEPUSH, false);
        stellar_set_constant(CONSTANT_INTERLEAVEPOP, false);
        stellar_set_constant(CONSTANT_SHOULDTRAIL, false);

        stellar_set_span(FOR_BOTH, 0, DIM);
        stellar_set_span(FOR_BOTH, 1, DIM);

        stellar_set_stride(FOR_BOTH, 0, 1);
        stellar_set_stride(FOR_BOTH, 1, DIM);

        stellar_set_axis(FOR_BOTH, 0, AXIS_DENSE);
        stellar_set_axis(FOR_BOTH, 1, AXIS_DENSE);

        stellar_issue();
        stellar_fence();

        // mv C from SRAM to DRAM
        printf("mv C from SRAM to DRAM\n");

        stellar_set_src_and_dst(SRC_DST_IS_SRAM, 2, SRC_DST_IS_DRAM, 0);

        stellar_set_constant(CONSTANT_INTERLEAVEPUSH, false);
        stellar_set_constant(CONSTANT_INTERLEAVEPOP, false);

        stellar_set_data_address(FOR_DST, result);

        stellar_set_span(FOR_BOTH, 0, DIM);
        stellar_set_span(FOR_BOTH, 1, DIM);

        stellar_set_stride(FOR_BOTH, 0, 1);
        stellar_set_stride(FOR_BOTH, 1, DIM);

        stellar_set_axis(FOR_BOTH, 0, AXIS_DENSE);
        stellar_set_axis(FOR_BOTH, 1, AXIS_DENSE);

        stellar_issue();
        stellar_fence();
    }

    printf("Final fence\n");
    stellar_fence();

    // Check if matrices are the same
    bool is_correct = true;
    for (int i = 0; i < DIM; i++) {
        for (int j = 0; j < DIM; j++)
            if (result[i][j] != golden[i][j]) {
                is_correct = false;
                break;
            }

        if (!is_correct)
            break;
    }

    if (is_correct)
        printf("SUCCEEDED\n");
    else
        printf("FAILED\n");

    // Print matrices
    printf("\nLeft matrix (%p):\n", left);
    for (int i = 0; i < DIM; i++) {
        for (int j = 0; j < DIM; j++) {
            printf("%d ", left[i][j]);
        }
        printf("\n");
    }

    printf("\nRight matrix (%p):\n", right);
    for (int i = 0; i < DIM; i++) {
        for (int j = 0; j < DIM; j++) {
            printf("%d ", right[i][j]);
        }
        printf("\n");
    }

    printf("\nGolden matrix (%p):\n", golden);
    for (int i = 0; i < DIM; i++) {
        for (int j = 0; j < DIM; j++) {
            printf("%d ", golden[i][j]);
        }
        printf("\n");
    }

    printf("\nResult matrix (%p):\n", result);
    for (int i = 0; i < DIM; i++) {
        for (int j = 0; j < DIM; j++) {
            printf("%d ", result[i][j]);
        }
        printf("\n");
    }

    exit(0);
}

