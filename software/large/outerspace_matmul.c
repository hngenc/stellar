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

#include "outerspace_params.h"

#define N_SRAMS 4
#define N_AXES 4

int a_buffer_id = 0;
int b_buffer_id = 1;
int c_buffer_id = 2;

#ifndef UNTILED_I
#error "we do not yet support tiling over I"
#endif

#ifndef ALL_TILES_FIT
#error "we do not yet support falling back to CPU for tiles which do not fit"
#endif

static void mvin_csr_or_csc(int sram_id, int axis_type, int outer_span, int lowest_axis, const elem_t * data, const elem_t * indices, const elem_t * indptrs) {
    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, sram_id);

    stellar_set_data_address(FOR_SRC, data);

    for (int axis = 0; axis < lowest_axis; axis++)
        stellar_set_span(FOR_BOTH, axis, 0);
    stellar_set_span(FOR_BOTH, lowest_axis, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, lowest_axis+1, outer_span);

    stellar_set_axis(FOR_BOTH, lowest_axis, axis_type);

    stellar_set_metadata_address(FOR_SRC, lowest_axis, METADATA_COMPRESSED_OUTER, indptrs);
    stellar_set_metadata_address(FOR_SRC, lowest_axis, METADATA_COMPRESSED_INNER, indices);

    // The innermost (compressed) axis contributes to its own
    // col-coords metadata address, and to nothing else
    stellar_set_metadata_stride(FOR_BOTH, lowest_axis, lowest_axis, METADATA_COMPRESSED_INNER, 1);

    // The next outer (dense) axis contributes to the innermost
    // (compressed) axis's row-coord metadata address
    stellar_set_metadata_stride(FOR_BOTH, lowest_axis+1, lowest_axis, METADATA_COMPRESSED_OUTER, 1);

    stellar_issue();
}

static void mvinA(int k_dram) {
    mvin_csr_or_csc(a_buffer_id, AXIS_COMPRESSED, DIM, 0, A_data, A_indices, A_indptrs + k_dram);
}

static void mvinB(int k_dram) {
    mvin_csr_or_csc(b_buffer_id, AXIS_COMPRESSED, DIM, 0, B_data, B_indices, B_indptrs + k_dram);
}

static void initC(int k_dram) {
    // Reset j-next-free-pointer
    stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, c_buffer_id);
    stellar_set_constant(CONSTANT_RESETRUNNINGSTATE, true);
    stellar_issue();

    // Mvin A into outer 2 axes
    mvin_csr_or_csc(c_buffer_id, AXIS_COMPRESSED, DIM, 1, 0, 0, A_indptrs + k_dram);

    // Init j-head pointers
    stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, c_buffer_id);

    stellar_set_constant(CONSTANT_ISDATA, false);
    stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_HEAD);

    stellar_set_span(FOR_DST, 1, (uint32_t)(-1));

    stellar_set_metadata_stride_by_addr(FOR_DST, 1, 0, METADATA_LL_HEAD, 1);

    stellar_set_metadata_stride(FOR_DST, 1, 1, METADATA_COMPRESSED_END, DIM);

    // Fence for the row-ids to be written in before we init the j-head pointers
    stellar_fence();
    stellar_issue();
}

static void matmulA(int max_i_div_dim) {
    stellar_set_src_and_dst(SRC_DST_IS_SRAM, a_buffer_id, SRC_DST_IS_REGFILE, 0);

    stellar_set_constant(CONSTANT_LASTAXIS, 1);

    stellar_set_span(FOR_BOTH, 0, DIM);
    stellar_set_span(FOR_BOTH, 1, DIM);
    stellar_set_span(FOR_BOTH, 2, max_i_div_dim);

    stellar_set_stride(FOR_BOTH, 2, DIM);
    stellar_set_metadata_stride(FOR_BOTH, 2, 0, METADATA_COMPRESSED_INNER, DIM);

    stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1);

    stellar_issue();
}

static void matmulB(int max_i_div_dim) {
    stellar_set_src_and_dst(SRC_DST_IS_SRAM, b_buffer_id, SRC_DST_IS_REGFILE, 0);

    stellar_set_constant(CONSTANT_LASTAXIS, 1);

    stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 1, DIM);
    stellar_set_span(FOR_BOTH, 2, max_i_div_dim);

    stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1);

    stellar_issue();
}

static void matmulC(int max_i_div_dim) {
    stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, c_buffer_id);

    stellar_set_constant(CONSTANT_LASTAXIS, 1);
    stellar_set_constant(CONSTANT_LASTAXISLOGSIZE, LOG_DIM);

    stellar_set_span(FOR_BOTH, 0, J);
    stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 2, DIM);

    stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_LL_HEAD, 1); // I-row address
    stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_LL_COORD, 1); // I-col address
    stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1); // J-row address
    stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1); // J-col address

    // From-regfile-coords are hardcoded, so no need to set them programmatically

    stellar_issue();
}

static void mvoutC(int k_dram, bool set_free_ptr) {
    // mvout only coords
    stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id, SRC_DST_IS_DRAM, 0);

    stellar_set_constant(CONSTANT_ISDATA, false);
    stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_COORD);
    stellar_set_constant(CONSTANT_SHOULDTRAIL, true);

    stellar_set_axis(FOR_DST, 0, AXIS_LINKEDLIST);
    stellar_set_axis(FOR_DST, 1, AXIS_COMPRESSED);

    stellar_set_span(FOR_DST, 0, (uint32_t)(-1));
    stellar_set_span(FOR_DST, 1, (uint32_t)(-1));
    stellar_set_span(FOR_DST, 2, DIM);

    stellar_set_data_address(FOR_DST,
#if INTERLEAVED_DATA_AND_COORDS == 0
            scattered_coord_heap
#else
            scattered_data_and_coord_heap
#endif
    );

    stellar_set_metadata_stride(FOR_SRC, 2, 1, METADATA_COMPRESSED_OUTER, 1);
    stellar_set_metadata_stride(FOR_SRC, 1, 1, METADATA_COMPRESSED_INNER, 1);
    stellar_set_metadata_stride(FOR_SRC, 0, 0, METADATA_LL_COORD, 1);

    stellar_set_metadata_stride_by_addr(FOR_SRC, 1, 0, METADATA_LL_HEAD, 1);

    stellar_issue();

#if INTERLEAVED_DATA_AND_COORDS == 0
    // mvout only data (flattening the k-i dimensions)
    stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id, SRC_DST_IS_DRAM, 0);

    stellar_set_axis(FOR_DST, 0, AXIS_LINKEDLIST);
    stellar_set_axis(FOR_DST, 1, AXIS_COMPRESSED);

    stellar_set_span(FOR_DST, 0, (uint32_t)(-1));
    stellar_set_span(FOR_DST, 1, (uint32_t)(-1));

    stellar_set_data_address(FOR_DST, scattered_data_heap);

    stellar_set_metadata_stride(FOR_SRC, 2, 1, METADATA_COMPRESSED_OUTER, 1);
    stellar_set_metadata_stride(FOR_SRC, 1, 1, METADATA_COMPRESSED_INNER, 1);
    stellar_set_metadata_stride(FOR_SRC, 0, 0, METADATA_LL_COORD, 1);

    stellar_set_metadata_stride_by_addr(FOR_SRC, 1, 0, METADATA_LL_HEAD, 1);

    stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_END, DIM);

    stellar_fence();
    stellar_issue();
#else
    stellar_fence();
#endif

    // mvout only pointers
    stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id, SRC_DST_IS_DRAM, 0);

    stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true); // Wait for matmul to finish

    stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST);
    stellar_set_axis(FOR_BOTH, 1, AXIS_COMPRESSED);

    stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1));
    stellar_set_span(FOR_BOTH, 2, 1);

    stellar_set_data_address(FOR_DST, 0);

    stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_COORD, 0);
    stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_NEXT, ptr_packets);
    stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_LAST, set_free_ptr ? ptr_packets + I * PTR_PACKET_SIZE : 0);

    stellar_set_metadata_address(FOR_DST, 1, METADATA_COMPRESSED_OUTER, A_indptrs + k_dram);
    stellar_set_metadata_address(FOR_DST, 1, METADATA_COMPRESSED_INNER, A_indices);

    stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_COMPRESSED_OUTER, 1);
    stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_INNER, 1);

    stellar_set_metadata_stride_by_value(FOR_DST, 1, 0, METADATA_LL_NEXT, 1 * PTR_PACKET_SIZE);
    stellar_set_metadata_stride_by_addr(FOR_SRC, 1, 0, METADATA_LL_HEAD, 1);

    stellar_set_metadata_region(FOR_DST, 1, METADATA_LL_COORD, SRC_DST_IS_SRAM, a_buffer_id);
    stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_END, DIM);

    stellar_issue();
}

static void tiled_matmul() {
    for (int k_dram = 0; k_dram < K; k_dram += DIM) {
        // printf("k=%d\n", k_dram);

        mvinB(k_dram);

        int k_div_dim = k_dram / DIM;

        // for (int i_dram = 0; i_dram < max_is[k_div_dim]; i_dram += TILE_I) // TODO we don't yet support tiling of I, so there's no reason to run the code block below as a loop, so I just commented the loop part out for now to enable more compiler optimizations
        {
            // TODO we don't yet support tiling of I, so the addresses below aren't adjusted based on "i_dram".
            mvinA(k_dram);

            initC(k_dram);
            stellar_fence();

            if (max_is[k_div_dim] > 0) { // TODO this if-statement should be handled in hardware, not software
                const int max_i_div_dim = (max_is[k_div_dim] + DIM - 1) / DIM;

                matmulA(max_i_div_dim);
                matmulB(max_i_div_dim);
                matmulC(max_i_div_dim);
            }

            // stellar_fence(); // We put this fence in mvoutC instead

            mvoutC(k_dram, k_dram == 0 /* && i_dram == 0 TODO add this part back one we support tiling over I */);

            stellar_fence();
        }
    }

    stellar_fence();
}

static void check_result() {
    for (int i = 0; i < I; i++) {
        const elem_t * packet = ptr_packets + i * PTR_PACKET_SIZE;
        const elem_t * golden_packet = golden_ptr_packets + i * PTR_PACKET_SIZE;

        for (int k = 0; ; k++) {
            if ((uint16_t)packet[PTR_PACKET_ROWLEN] != (uint16_t)golden_packet[PTR_PACKET_ROWLEN]) {
                printf("FAILED! rowlens not equal.\n");
                printf("\ti = %d | k = %d\n", i, k);
                printf("\tactual offset = %d | golden offset = %d\n", packet[PTR_PACKET_DATA_OFF] , golden_packet[PTR_PACKET_DATA_OFF]);
                printf("\trowlen = %d | golden_rowlen = %d\n", (uint16_t)packet[PTR_PACKET_ROWLEN], (uint16_t)golden_packet[PTR_PACKET_ROWLEN]);
                exit(1);
            }

            uint16_t rowlen = packet[PTR_PACKET_ROWLEN];
            if (rowlen == (uint16_t)(-1))
                rowlen = 0;

            for (int j = 0; j < rowlen; j++) {
                elem_t actual_data =
#if INTERLEAVED_DATA_AND_COORDS == 0
                    scattered_data_heap[packet[PTR_PACKET_DATA_OFF] + j];
#else
                    scattered_data_and_coord_heap[packet[PTR_PACKET_DATA_OFF]*2 + j*2];
#endif

                elem_t actual_coord =
#if INTERLEAVED_DATA_AND_COORDS == 0
                    scattered_coord_heap[packet[PTR_PACKET_DATA_OFF] + j];
#else
                    scattered_data_and_coord_heap[packet[PTR_PACKET_DATA_OFF]*2 + j*2+1];
#endif

                elem_t golden_data =
#if INTERLEAVED_DATA_AND_COORDS == 0
                    golden_scattered_data_heap[golden_packet[PTR_PACKET_DATA_OFF] + j];
#else
                    golden_scattered_data_and_coord_heap[golden_packet[PTR_PACKET_DATA_OFF]*2 + j*2];
#endif

                elem_t golden_coord =
#if INTERLEAVED_DATA_AND_COORDS == 0
                    golden_scattered_coord_heap[golden_packet[PTR_PACKET_DATA_OFF] + j];
#else
                    golden_scattered_data_and_coord_heap[golden_packet[PTR_PACKET_DATA_OFF]*2 + j*2+1];
#endif

                bool data_and_coord_matches = actual_data == golden_data && actual_coord == actual_coord;

                if (!data_and_coord_matches) {
                    printf("FAILED! data or coords not equal.\n");
                    printf("\ti = %d | k = %d | j = %d\n", i, k, j);
                    printf("\trowlen = %d\n", (uint16_t)packet[PTR_PACKET_ROWLEN]);
                    printf("\tactual offset = %d | golden offset = %d\n", packet[PTR_PACKET_DATA_OFF] , golden_packet[PTR_PACKET_DATA_OFF]);
                    printf("\tcoords: actual = %d | golden = %d\n", actual_coord, golden_coord);
                    printf("\tdata:   actual = %d | golden = %d\n", actual_data, golden_data);

                    printf("\n\tactual coords = ");
                    for (int jj = 0; jj < rowlen; jj++)
                        printf("%d, ",
#if INTERLEAVED_DATA_AND_COORDS == 0
                                scattered_coord_heap[packet[PTR_PACKET_DATA_OFF] + jj]
#else
                                scattered_data_and_coord_heap[packet[PTR_PACKET_DATA_OFF]*2 + jj*2+1]
#endif
                        );
                    printf("\n");
                    printf("\tactual data   = ");
                    for (int jj = 0; jj < rowlen; jj++)
                        printf("%d, ",
#if INTERLEAVED_DATA_AND_COORDS == 0
                                scattered_data_heap[packet[PTR_PACKET_DATA_OFF] + jj]
#else
                                scattered_data_and_coord_heap[packet[PTR_PACKET_DATA_OFF]*2 + jj*2]
#endif
                        );
                    printf("\n");

                    exit(1);
                }
            }

            elem_t next_packet_rel_off = packet[PTR_PACKET_NEXT_REL_OFF];
            elem_t golden_next_packet_rel_off = golden_packet[PTR_PACKET_NEXT_REL_OFF];
            bool finished = next_packet_rel_off == ~((elem_t)0);
            bool golden_finished = golden_next_packet_rel_off == ~((elem_t)0);

            if (finished != golden_finished) {
                printf("FAILED! did not finish at same time.\n");
                printf("\ti = %d | k = %d\n", i, k);
                printf("\tactual rowlen = %d | golden rowlen = %d\n", (uint16_t)packet[PTR_PACKET_ROWLEN], (uint16_t)golden_packet[PTR_PACKET_ROWLEN]);
                printf("\tactual offset = %d | golden offset = %d\n", packet[PTR_PACKET_DATA_OFF], golden_packet[PTR_PACKET_DATA_OFF]);
                printf("\tactual next packet rel off = %d | golden = %d\n", next_packet_rel_off, golden_next_packet_rel_off);
                printf("\tactual last packet rel off = %d | golden = %d\n", packet[PTR_PACKET_LAST_REL_OFF], golden_packet[PTR_PACKET_LAST_REL_OFF]);
                exit(1);
            }

            if (k == K-1 && !finished) {
                printf("FAILED! did not finish when it should have.\n");
                exit(1);
            }

            if (finished)
                break;

            packet += next_packet_rel_off * PTR_PACKET_SIZE;
            golden_packet += golden_next_packet_rel_off * PTR_PACKET_SIZE;
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

    printf("I = %d | J = %d | K = %d | DIM = %d\n", I, J, K, DIM);
    // printf("ptr_packets = %p to %p\n", ptr_packets, ptr_packets + sizeof(ptr_packets)/sizeof(ptr_packets[0]));
    // printf("golden_ptr_packets = %p to %p\n", golden_ptr_packets, golden_ptr_packets + sizeof(golden_ptr_packets)/sizeof(golden_ptr_packets[0]));
    // printf("scattered_coord_heap = %p to %p\n", scattered_coord_heap, scattered_coord_heap + sizeof(scattered_coord_heap)/sizeof(scattered_coord_heap[0]));
    // printf("scattered_data_heap = %p to %p\n", scattered_data_heap, scattered_data_heap + sizeof(scattered_data_heap)/sizeof(scattered_data_heap[0]));

    stellar_flush();

    uint64_t start = read_cycles();
    tiled_matmul();
    uint64_t end = read_cycles();

    printf("Cycles taken = %llu\n", end-start);

    printf("Check result\n");
    check_result();

    printf("SUCCEEDED\n");

    return 0;
}

