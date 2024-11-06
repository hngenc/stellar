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

#define N_SRAMS 4
#define N_AXES 4

static const int a_buffer_id = 0;
static const int b_buffer_id = 1;
static const int c_buffer_id = 2;

#define CONFIG_MVIN_A 0
#define CONFIG_MVIN_B 1
#define CONFIG_MVIN_C 2
#define CONFIG_RESET_C 3
#define CONFIG_MVOUT_DATA 4
#define CONFIG_MVOUT_PTRS 5
#define CONFIG_MATMUL_A 6
#define CONFIG_MATMUL_B 7
#define CONFIG_MATMUL_C 8

static uint64_t mvin_a_cycles = 0;
static uint64_t mvin_b_cycles = 0;
static uint64_t init_c_cycles = 0;
static uint64_t mvout_cycles = 0;
static uint64_t matmul_cycles = 0;

#ifdef WRITE_GOLDEN
// #define CHECK_RESULT
#define ONLY_CHECK_DATA_AND_COORDS
#endif

static inline void mvin_csr_or_csc(int sram_id, int axis_type, int inner_addr, int outer_span, int inner_span, int lowest_axis, const elem_t * data, const elem_t * indices, const elem_t * indptrs) {
    const bool use_config = lowest_axis == 0;

    stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, sram_id);

    if (use_config)
        stellar_load_saved_config(CONFIG_MVIN_A);

    stellar_set_address(FOR_SRC, lowest_axis, inner_addr);

    stellar_set_data_address(FOR_SRC, data);

    for (int axis = 0; axis < lowest_axis; axis++)
        stellar_set_span(FOR_BOTH, axis, 0);
    stellar_set_span(FOR_BOTH, lowest_axis, inner_span);
    stellar_set_span(FOR_BOTH, lowest_axis+1, outer_span);

    if (!use_config)
        stellar_set_axis(FOR_BOTH, lowest_axis, axis_type);

    stellar_set_metadata_address(FOR_SRC, lowest_axis, METADATA_COMPRESSED_OUTER, indptrs);
    stellar_set_metadata_address(FOR_SRC, lowest_axis, METADATA_COMPRESSED_INNER, indices);

    if (!use_config) {
        // The innermost (compressed) axis contributes to its own
        // col-coords metadata address, and to nothing else
        stellar_set_metadata_stride(FOR_BOTH, lowest_axis, lowest_axis, METADATA_COMPRESSED_INNER, 1);

        // The next outer (dense) axis contributes to the innermost
        // (compressed) axis's row-coord metadata address
        stellar_set_metadata_stride(FOR_BOTH, lowest_axis+1, lowest_axis, METADATA_COMPRESSED_OUTER, 1);
    }
}

static inline void mvinA(int k_dram, int i_dram, int tile_k, int max_i) {
    // Old:
    // mvin_csr_or_csc(a_buffer_id, AXIS_COMPRESSED, i_dram, tile_k, max_i, 0, A_data, A_indices, A_indptrs + k_dram);
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true); // Wait for matmul to finish
    // stellar_issue();

    // New:
    stellar_set_address(FOR_SRC, 0, i_dram);

    stellar_set_data_address(FOR_SRC, A_data);

    stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, A_indptrs + k_dram);
    stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_INNER, A_indices);

    stellar_set_span(FOR_BOTH, 0, max_i);
    stellar_set_span(FOR_BOTH, 1, tile_k);

    stellar_load_saved_config(CONFIG_MVIN_A);
}

static inline void mvinB(int k_dram, int tile_k) {
    // Old:
    // mvin_csr_or_csc(b_buffer_id, AXIS_COMPRESSED, 0, tile_k, (uint32_t)(-1), 0, B_data, B_indices, B_indptrs + k_dram);
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true); // Wait for matmul to finish
    // stellar_set_constant(CONSTANT_DROPFOLLOWINGCMDSIFEMPTY, true);
    // stellar_issue();

    // New:
    stellar_set_data_address(FOR_SRC, B_data);

    stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_OUTER, B_indptrs + k_dram);
    stellar_set_metadata_address(FOR_SRC, 0, METADATA_COMPRESSED_INNER, B_indices);

    stellar_set_span(FOR_BOTH, 1, tile_k);

    stellar_load_saved_config(CONFIG_MVIN_B);
}

static inline void initC(int k_dram, int i_dram, int tile_k, int max_i) {
    // Reset j-next-free-pointer
    // stellar_set_src_and_dst(SRC_DST_IS_NOTHING, 0, SRC_DST_IS_SRAM, c_buffer_id);
    // stellar_set_constant(CONSTANT_RESETRUNNINGSTATE, true);
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true);
    // stellar_issue();
    stellar_load_saved_config(CONFIG_RESET_C);

    // Mvin A into outer 2 axes
    // mvin_csr_or_csc(c_buffer_id, AXIS_COMPRESSED, i_dram, tile_k, max_i, 1, 0, 0, A_indptrs + k_dram);
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true);
    // stellar_issue();

    stellar_set_address(FOR_SRC, 1, i_dram);

    stellar_set_metadata_address(FOR_SRC, 1, METADATA_COMPRESSED_OUTER, A_indptrs + k_dram);

    stellar_set_span(FOR_BOTH, 1, max_i);
    stellar_set_span(FOR_BOTH, 2, tile_k);

    stellar_load_saved_config(CONFIG_MVIN_C);
}

static inline void matmulA(int max_i_div_dim, int tile_k) {
    // stellar_set_src_and_dst(SRC_DST_IS_SRAM, a_buffer_id, SRC_DST_IS_REGFILE, 0); // set in saved config

    // stellar_set_constant(CONSTANT_LASTAXIS, 1); // This is already hardcoded

    // stellar_set_span(FOR_BOTH, 0, DIM); // set in saved config
    stellar_set_span(FOR_BOTH, 1, tile_k);
    stellar_set_span(FOR_BOTH, 2, max_i_div_dim);

    // stellar_set_stride(FOR_BOTH, 2, DIM); // This is already hardcoded
    // stellar_set_metadata_stride(FOR_BOTH, 2, 0, METADATA_LL_COORD, DIM); // This is already hardcoded

    // stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_COMPRESSED_OUTER, 1); // set in saved config

    // We fence here to make sure that SRAM A has completed its move-ins before we begin the matmul
    // stellar_wait(WAIT_FOR_DMA | WAIT_FOR_SRAM_WRITES_FROM_DRAM(a_buffer_id)); // set in saved config
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true); // set in saved config

    // stellar_issue(); // set in saved config

    stellar_load_saved_config(CONFIG_MATMUL_A);
}

static inline void matmulB(int max_i_div_dim, int tile_k) {
    // stellar_set_src_and_dst(SRC_DST_IS_SRAM, b_buffer_id, SRC_DST_IS_REGFILE, 0); // set in saved config

    // stellar_set_constant(CONSTANT_LASTAXIS, 1); // This is already hardcoded

    // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1)); // This is already hardcoded
    stellar_set_span(FOR_BOTH, 1, tile_k);
    stellar_set_span(FOR_BOTH, 2, max_i_div_dim);

    // stellar_set_metadata_stride(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1); // This is already hardcoded

    // We fence here to make sure that SRAM B has completed its move-ins before we begin the matmul
    // stellar_wait(WAIT_FOR_DMA | WAIT_FOR_SRAM_WRITES_FROM_DRAM(b_buffer_id)); // set in saved config
    // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true); // set in saved config

    // stellar_issue();

    stellar_load_saved_config(CONFIG_MATMUL_B);
}

static inline void matmulC(int max_i_div_dim, int tile_k) {
    // stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, c_buffer_id); // set in saved config

    // stellar_set_constant(CONSTANT_LASTAXIS, 1); // This is already hardcoded
    // stellar_set_constant(CONSTANT_LASTAXISLOGSIZE, LOG_DIM); // This is already hardcoded

    // stellar_set_span(FOR_BOTH, 0, J); // set in saved config
    // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1)); // set in saved config
    stellar_set_span(FOR_BOTH, 2, tile_k);

    // stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_LL_HEAD, 1); // I-row address // This is already hardcoded
    // stellar_set_metadata_stride(FOR_BOTH, 3, 1, METADATA_LL_COORD, DIM); // I-col address // This is already hardcoded
    // stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_LL_COORD, 1); // I-col address // This is already hardcoded
    // stellar_set_metadata_stride_by_addr(FOR_BOTH, 1, 0, METADATA_LL_HEAD, 1); // J-row address // set in saved config
    // stellar_set_metadata_stride(FOR_BOTH, 0, 0, METADATA_LL_COORD, 1); // J-col address // This is already hardcoded

    // From-regfile-coords are hardcoded, so no need to set them programmatically

    // We fence here to make sure that SRAM C has completed its move-outs from the prior matmul, and that initC has completed initializing the j-head pointers.
    // stellar_wait(WAIT_FOR_SRAM_READS(c_buffer_id) | WAIT_FOR_DMA); // set in saved config

    // stellar_issue();

    stellar_load_saved_config(CONFIG_MATMUL_C);
}

static inline void mvoutC(int k_dram, int i_dram, int tile_k, int max_i, bool set_free_ptr) {
    // mvout interleaved coords and data
    {
        // stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id, SRC_DST_IS_DRAM, 0); // set in saved config

        // stellar_set_constant(CONSTANT_ISDATA, false); // set in saved config
        // stellar_set_constant(CONSTANT_METADATABUFFERID, METADATA_LL_COORD); // set in saved config
        // stellar_set_constant(CONSTANT_SHOULDTRAIL, true); // set in saved config

        stellar_set_data_address(FOR_DST, scattered_data_and_coord_heap);

        stellar_set_span(FOR_DST, 2, tile_k);

        // stellar_issue(); // set in saved config

        stellar_load_saved_config(CONFIG_MVOUT_DATA);
    }

    // mvout only pointers
    {
        // stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id, SRC_DST_IS_DRAM, 0); // set in saved config

        // stellar_set_constant(CONSTANT_SHOULDTRAILCOARSE, true); // Wait for matmul to finish

        // stellar_set_axis(FOR_BOTH, 0, AXIS_LINKEDLIST); // set in saved config
        // stellar_set_axis(FOR_BOTH, 1, AXIS_COMPRESSED); // set in saved config

        // stellar_set_span(FOR_BOTH, 0, (uint32_t)(-1)); // set in saved config
        // stellar_set_span(FOR_BOTH, 1, (uint32_t)(-1)); // set in saved config

        // stellar_set_data_address(FOR_DST, 0); // set by default

        // stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_COORD, 0); // set by default
        stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_NEXT, ptr_packets);
        stellar_set_metadata_address(FOR_DST, 0, METADATA_LL_LAST, /* set_free_ptr ? */ ptr_packets + (I+FREE_PTR_OFFSET) * PTR_PACKET_SIZE /* : 0 */);

        // stellar_set_metadata_address(FOR_DST, 1, METADATA_COMPRESSED_OUTER, A_indptrs + k_dram);
        // stellar_set_metadata_address(FOR_DST, 1, METADATA_COMPRESSED_INNER, A_indices);

#ifndef UNTILED_I
        // stellar_set_address(FOR_DST, 1, i_dram);
#endif

        // stellar_set_metadata_stride(FOR_BOTH, 2, 1, METADATA_COMPRESSED_OUTER, 1); // set in saved config
        // stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_INNER, 1); // set in saved config

       //  stellar_set_metadata_stride_by_value(FOR_DST, 1, 0, METADATA_LL_NEXT, 1 * PTR_PACKET_SIZE); // set in saved config
        // stellar_set_metadata_stride_by_addr(FOR_SRC, 1, 0, METADATA_LL_HEAD, 1); // set in saved config

        // stellar_set_metadata_region(FOR_DST, 1, METADATA_LL_COORD, SRC_DST_IS_SRAM, a_buffer_id); // set in saved config
        stellar_set_metadata_stride(FOR_BOTH, 1, 1, METADATA_COMPRESSED_END, tile_k);

        // stellar_issue(); // set in saved config

        stellar_load_saved_config(CONFIG_MVOUT_PTRS);
    }
}

static void tiled_matmul() {
    static uint64_t total_cycles = 0;
    static uint64_t start;
    static uint64_t end;

    start = read_cycles();

    bool initialized_free_ptr = false;
    for (int k_dram = 0; k_dram < K;) {
        const int k_div_dim = k_dram / DIM;

#ifdef SMALL_K_EXISTS
        const int tile_k = small_ks[k_div_dim] ? 1 : DIM;
#else
        const int tile_k = DIM;
#endif

        mvinB(k_dram, tile_k);

#ifdef UNTILED_I
        const int i_dram = 0;
#else
        const int tile_i = tile_is[k_div_dim];
        for (int i_dram = 0; i_dram < max_is[k_div_dim]; i_dram += tile_i)
#endif
        {
            int max_i = max_is[k_div_dim] - i_dram;
            #ifndef UNTILED_I
                #ifdef SMALL_K_EXISTS
                    if (tile_k == 1)
                        max_i = A_indptrs[k_dram+1] - A_indptrs[k_dram] - i_dram;
                #endif // SMALL_K_EXISTS
                max_i = max_i < tile_i ? max_i : tile_i;
            #endif // UNTILED_I

            const int max_i_div_dim = (max_i + DIM - 1) / DIM;

            if (max_i > 0) {
                mvinA(k_dram, i_dram, tile_k, max_i);

                initC(k_dram, i_dram, tile_k, max_i);

                matmulA(max_i_div_dim, tile_k);
                // uint64_t matmul_start = read_cycles();
                matmulB(max_i_div_dim, tile_k);
                matmulC(max_i_div_dim, tile_k);

                mvoutC(k_dram, i_dram, tile_k, max_i, !initialized_free_ptr);
                initialized_free_ptr = true;
                // uint64_t matmul_end = read_cycles();
                // matmul_cycles += matmul_end - matmul_start;
            }
        }

        k_dram += tile_k;
    }

    stellar_fence();

    end = read_cycles();
    total_cycles += end - start;

    // printf("Finished!\n");
    printf("Cycles = %llu\n", end-start);
    // printf("Cycles taken in this part = %llu\n", end-start);
    // printf("Cycles taken so far = %llu\n", total_cycles);
    /*
    printf("Mvin A cycles so far = %llu\n", mvin_a_cycles);
    printf("Mvin B cycles so far = %llu\n", mvin_b_cycles);
    printf("Init C cycles so far = %llu\n", init_c_cycles);
    printf("Matmul cycles so far = %llu\n", matmul_cycles);
    printf("Mvout cycles so far = %llu\n", mvout_cycles);
    */
    printf("\n");
}

#ifdef CHECK_RESULT
static void check_result() {
#ifdef ONLY_CHECK_DATA_AND_COORDS
    if (
#if INTERLEAVED_DATA_AND_COORDS == 0
        buffers_are_equal(scattered_coord_heap, golden_scattered_coord_heap, sizeof(scattered_coord_heap)) &&
        buffers_are_equal(scattered_data_heap, golden_scattered_data_heap, sizeof(scattered_data_heap))
#else
        buffers_are_equal(scattered_data_and_coord_heap, golden_scattered_data_and_coord_heap, sizeof(scattered_data_and_coord_heap))
#endif
    )
        return;

    printf("Data and coords did not match!\n");

    // printf("Coords:\n");
    // for (int i = 0; i < sizeof(scattered_coord_heap) / sizeof(scattered_coord_heap[0]); i++)
    //     printf("%d ", scattered_coord_heap[i]);
    // printf("\n");
    // printf("Data (%p):\n", scattered_data_heap);
    // for (int i = 0; i < sizeof(scattered_coord_heap) / sizeof(scattered_coord_heap[0]); i++)
    //     printf("%d ", scattered_data_heap[i]);
    // printf("\n");

    for (int i = 0; i < sizeof(scattered_data_and_coord_heap) / sizeof(scattered_data_and_coord_heap[0]); i++)
        if (scattered_data_and_coord_heap[i] != golden_scattered_data_and_coord_heap[i]) {
            printf("Mismatch at %d\n", i);
            break;
        }
    /*
    printf("Data and coords (%p):\n", scattered_data_and_coord_heap);
    for (int i = 0; i < sizeof(scattered_data_and_coord_heap) / sizeof(scattered_data_and_coord_heap[0]); i++)
        printf("%d ", scattered_data_and_coord_heap[i]);
    printf("\n");
    */

    exit(1);
#endif

    for (int i = 0; i < I; i++) {
        const elem_t * packet = ptr_packets + i * PTR_PACKET_SIZE;
        const elem_t * golden_packet = golden_ptr_packets + i * PTR_PACKET_SIZE;

        for (int k = 0; ; k++) {
            // printf("i = %d | k = %d\n", i, k);
            // printf("\tpacket = %p | golden_packet = %p\n", packet, golden_packet);
            // printf("\tpacket_off = %d | golden_packet_off = %d\n", packet - ptr_packets, golden_packet - golden_ptr_packets);
            // printf("\tpacket_id = %d | golden_packet_id = %d\n", (packet - ptr_packets) / PTR_PACKET_SIZE, (golden_packet - golden_ptr_packets) / PTR_PACKET_SIZE);
            // printf("\tptr_packets + i * PTR_PACKET_SIZE = %p\n", ptr_packets + i * PTR_PACKET_SIZE);

            if ((uint16_t)packet[PTR_PACKET_ROWLEN] != (uint16_t)golden_packet[PTR_PACKET_ROWLEN]) {
                printf("FAILED! rowlens not equal.\n");
                printf("\ti = %d | k = %d\n", i, k);
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

                    printf("\n\tgolden coords = ");
                    for (int jj = 0; jj < rowlen; jj++)
                        printf("%d, ",
#if INTERLEAVED_DATA_AND_COORDS == 0
                                golden_scattered_coord_heap[golden_packet[PTR_PACKET_DATA_OFF] + jj]
#else
                                golden_scattered_data_and_coord_heap[golden_packet[PTR_PACKET_DATA_OFF]*2 + jj*2+1]
#endif
                        );
                    printf("\n");
                    printf("\tgolden data   = ");
                    for (int jj = 0; jj < rowlen; jj++)
                        printf("%d, ",
#if INTERLEAVED_DATA_AND_COORDS == 0
                                golden_scattered_data_heap[golden_packet[PTR_PACKET_DATA_OFF] + jj]
#else
                                golden_scattered_data_and_coord_heap[golden_packet[PTR_PACKET_DATA_OFF]*2 + jj*2]
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
#endif

int main() {
#ifndef BAREMETAL
    if (mlockall(MCL_CURRENT | MCL_FUTURE) != 0) {
      perror("mlockall failed");
      exit(1);
    }
#endif

    printf("MATRIX = " HACKSTR(MATRIX) "\n");
    printf("I = %d | J = %d | K = %d | DIM = %d\n", I, J, K, DIM);

    stellar_flush();

    tiled_matmul();

#ifdef CHECK_RESULT
    printf("Check result\n");
    check_result();
#endif

    printf("SUCCEEDED\n");

    printf("Printing stats...\n");
    stellar_flush();
    printf("Should be printed now!\n");

    return 0;
}

