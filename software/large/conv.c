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
#include "conv_params.h"

#define IROWS (((OROWS) * (STRIDE) + (KROWS) - (STRIDE)) + BIGGER_IMAGE)
#define ICOLS (((OCOLS) * (STRIDE) + (KCOLS) - (STRIDE)) + BIGGER_IMAGE)
#define DOWNSAMPLE ((STRIDE) == 2 && (KROWS) == 1 && (KCOLS) == 1 && (BIGGER_IMAGE))

#define N_SRAMS 3

#if (KCHS % DIM != 0) || (OCHS % DIM != 0)
#error "we haven't added padding support yet"
#endif

static bool wait_for_moved_in_data = false;
static int a_buffer_id = 0, b_buffer_id = 0, c_buffer_id = 0, d_buffer_id = 0;

static void mvinBias(const acc_t * bias, int span_batches, int span_orows, int span_ocols, int span_ochs, int och_dram) {
  wait_for_moved_in_data = true;

  if (DOUBLE_BUFFERED_CD)
    d_buffer_id = 1 - d_buffer_id;

  stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, d_buffer_id * N_SRAMS + 2);

  stellar_set_data_address(FOR_SRC, bias + och_dram);

  stellar_set_span(FOR_BOTH, 0, span_ochs);
  stellar_set_span(FOR_BOTH, 1, span_batches * span_orows * span_ocols);

  stellar_set_stride(FOR_DST, 1, span_ochs);

  stellar_issue();
}

static void mvinImage(const elem_t * image, int span_batches, int span_irows, int span_icols, int span_kchs, int batch_dram, int irow_dram, int icol_dram, int kch_dram) {
  wait_for_moved_in_data = true;

  if (DOUBLE_BUFFERED_AB)
    a_buffer_id = 1 - a_buffer_id;

  stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, a_buffer_id * N_SRAMS + 0);

  stellar_set_data_address(FOR_SRC, image + batch_dram*IROWS*ICOLS*KCHS + irow_dram*ICOLS*KCHS + icol_dram*KCHS + kch_dram);

  stellar_set_span(FOR_BOTH, 0, span_kchs);
  stellar_set_span(FOR_BOTH, 1, span_icols);
  stellar_set_span(FOR_BOTH, 2, span_irows);
  stellar_set_span(FOR_BOTH, 3, span_batches);

  stellar_set_stride(FOR_SRC, 1, (KCHS << DOWNSAMPLE));
  stellar_set_stride(FOR_DST, 1, span_kchs);
  stellar_set_stride(FOR_SRC, 2, ICOLS * (KCHS << DOWNSAMPLE));
  stellar_set_stride(FOR_DST, 2, span_icols * span_kchs);
  stellar_set_stride(FOR_SRC, 3, IROWS * ICOLS * KCHS);
  stellar_set_stride(FOR_DST, 3, span_irows * span_icols * span_kchs);

  stellar_issue();
}

static void mvinWeights(const elem_t * weights, int span_krows, int span_kcols, int span_kchs, int span_ochs, int krow_dram, int kcol_dram, int kch_dram, int och_dram) {
  wait_for_moved_in_data = true;

  if (DOUBLE_BUFFERED_AB)
    b_buffer_id = 1 - b_buffer_id;

  stellar_set_src_and_dst(SRC_DST_IS_DRAM, 0, SRC_DST_IS_SRAM, b_buffer_id * N_SRAMS + 1);

  stellar_set_data_address(FOR_SRC, weights + krow_dram*KCOLS*KCHS*OCHS + kcol_dram*KCHS*OCHS + kch_dram*OCHS + och_dram);

  stellar_set_span(FOR_BOTH, 0, span_ochs);
  stellar_set_span(FOR_BOTH, 1, span_kchs);
  stellar_set_span(FOR_BOTH, 2, span_kcols);
  stellar_set_span(FOR_BOTH, 3, span_krows);

  stellar_set_stride(FOR_SRC, 1, OCHS);
  stellar_set_stride(FOR_DST, 1, span_ochs);
  stellar_set_stride(FOR_SRC, 2, KCHS * OCHS);
  stellar_set_stride(FOR_DST, 2, span_kchs * span_ochs);
  stellar_set_stride(FOR_SRC, 3, KCOLS * KCHS * OCHS);
  stellar_set_stride(FOR_DST, 3, span_kcols * span_kchs * span_ochs);

  stellar_issue();
}

static void mvoutOutput(elem_t * output, int span_batches, int span_orows, int span_ocols, int span_ochs, int batch_dram, int orow_dram, int ocol_dram, int och_dram) {
  if (DOUBLE_BUFFERED_CD)
    c_buffer_id = 1 - c_buffer_id;

  stellar_set_src_and_dst(SRC_DST_IS_SRAM, c_buffer_id * N_SRAMS + 2, SRC_DST_IS_DRAM, 0);

  stellar_set_data_address(FOR_DST, output + batch_dram*OROWS*OCOLS*OCHS + orow_dram*OCOLS*OCHS + ocol_dram*OCHS + och_dram);

  stellar_set_span(FOR_BOTH, 0, span_ochs);
  stellar_set_span(FOR_BOTH, 1, span_ocols);
  stellar_set_span(FOR_BOTH, 2, span_orows);
  stellar_set_span(FOR_BOTH, 3, span_batches);

  stellar_set_stride(FOR_SRC, 1, span_ochs);
  stellar_set_stride(FOR_DST, 1, OCHS);
  stellar_set_stride(FOR_SRC, 2, span_ocols * span_ochs);
  stellar_set_stride(FOR_DST, 2, OCOLS * OCHS);
  stellar_set_stride(FOR_SRC, 3, span_orows * span_ocols * span_ochs);
  stellar_set_stride(FOR_DST, 3, OROWS * OCOLS * OCHS);

  stellar_issue();
}

static void tiled_conv() {
  assert(c_buffer_id == d_buffer_id);

  const bool single_batch_tile = TILE_BATCHES >= BATCHES;
  const bool single_orows_tile = TILE_OROWS >= OROWS;
  const bool single_ocols_tile = TILE_OCOLS >= OCOLS;
  const bool single_ochs_tile = TILE_OCHS >= OCHS;
  const bool single_krows_tile = TILE_KROWS >= KROWS;
  const bool single_kcols_tile = TILE_KCOLS >= KCOLS;
  const bool single_kchs_tile = TILE_KCHS >= KCHS;

  bool first = true;

  // DRAM levels
  for (int batch_dram = 0; batch_dram < BATCHES; batch_dram += TILE_BATCHES) {
    const int span_batches = (BATCHES - batch_dram) > TILE_BATCHES ? TILE_BATCHES : (BATCHES - batch_dram);

    for (int orow_dram = 0; orow_dram < OROWS; orow_dram += TILE_OROWS) {
      const int span_orows = (OROWS - orow_dram) > TILE_OROWS ? TILE_OROWS : (OROWS - orow_dram);

      for (int ocol_dram = 0; ocol_dram < OCOLS; ocol_dram += TILE_OCOLS) {
        const int span_ocols = (OCOLS - ocol_dram) > TILE_OCOLS ? TILE_OCOLS : (OCOLS - ocol_dram);

        for (int och_dram = 0; och_dram < OCHS; och_dram += TILE_OCHS) {
          const int span_ochs = (OCHS - och_dram) > TILE_OCHS ? TILE_OCHS : (OCHS - och_dram);
          assert(span_ochs % DIM == 0); // TODO get rid of this limitation

          for (int krow_dram = 0; krow_dram < KROWS; krow_dram += TILE_KROWS) {
            const int span_krows = (KROWS - krow_dram) > TILE_KROWS ? TILE_KROWS : (KROWS - krow_dram);
            const int span_irows = span_orows * (STRIDE >> DOWNSAMPLE) + span_krows - (STRIDE >> DOWNSAMPLE);
            const int irow_dram = orow_dram * STRIDE + krow_dram;

            for (int kcol_dram = 0; kcol_dram < KCOLS; kcol_dram += TILE_KCOLS) {
              const int span_kcols = (KCOLS - kcol_dram) > TILE_KCOLS ? TILE_KCOLS : (KCOLS - kcol_dram);
              const int span_icols = span_ocols * (STRIDE >> DOWNSAMPLE) + span_kcols - (STRIDE >> DOWNSAMPLE);
              const int icol_dram = ocol_dram * STRIDE + kcol_dram;

              for (int kch_dram = 0; kch_dram < KCHS; kch_dram += TILE_KCHS) {
                const int span_kchs = (KCHS - kch_dram) > TILE_KCHS ? TILE_KCHS : (KCHS - kch_dram);
                assert(span_kchs % DIM == 0); // TODO get rid of this limitation

                bool last = false;
                int batch_dram_next = batch_dram, orow_dram_next = orow_dram, ocol_dram_next = ocol_dram, och_dram_next = och_dram, krow_dram_next = krow_dram, kcol_dram_next = kcol_dram, kch_dram_next = kch_dram + TILE_KCHS;
                if (kch_dram_next >= KCHS) {
                  kch_dram_next = 0;
                  kcol_dram_next += TILE_KCOLS;
                  if (kcol_dram_next >= KCOLS) {
                    kcol_dram_next = 0;
                    krow_dram_next += TILE_KROWS;
                    if (krow_dram_next >= KROWS) {
                      krow_dram_next = 0;
                      och_dram_next += TILE_OCHS;
                      if (och_dram_next >= OCHS) {
                        och_dram_next = 0;
                        ocol_dram_next += TILE_OCOLS;
                        if (ocol_dram_next >= OCOLS) {
                          ocol_dram_next = 0;
                          orow_dram_next += TILE_OROWS;
                          if (orow_dram_next >= OROWS) {
                            orow_dram_next = 0;
                            batch_dram_next += TILE_BATCHES;
                            if (batch_dram_next >= BATCHES)
                              last = true;
                          }
                        }
                      }
                    }
                  }
                }
                const int span_kchs_next = (KCHS - kch_dram_next) > TILE_KCHS ? TILE_KCHS : (KCHS - kch_dram_next);
                const int span_kcols_next = (KCOLS - kcol_dram_next) > TILE_KCOLS ? TILE_KCOLS : (KCOLS - kcol_dram_next);
                const int span_krows_next = (KROWS - krow_dram_next) > TILE_KROWS ? TILE_KROWS : (KROWS - krow_dram_next);
                const int span_ochs_next = (OCHS - och_dram_next) > TILE_OCHS ? TILE_OCHS : (OCHS - och_dram_next);
                const int span_ocols_next = (OCOLS - ocol_dram_next) > TILE_OCOLS ? TILE_OCOLS : (OCOLS - ocol_dram_next);
                const int span_orows_next = (OROWS - orow_dram_next) > TILE_OROWS ? TILE_OROWS : (OROWS - orow_dram_next);
                const int span_batches_next = (BATCHES - batch_dram_next) > TILE_BATCHES ? TILE_BATCHES : (BATCHES - batch_dram_next);

                if (first || !DOUBLE_BUFFERED_AB) {
                  // mv image from DRAM to SRAM
                  mvinImage((elem_t*)image, span_batches, span_irows, span_icols, span_kchs, batch_dram, irow_dram, icol_dram, kch_dram);

                  // mv weights from DRAM to SRAM
                  if (first || !(single_ochs_tile && single_kchs_tile && single_kcols_tile && single_krows_tile))
                    mvinWeights((elem_t*)weights, span_krows, span_kcols, span_kchs, span_ochs, krow_dram, kcol_dram, kch_dram, och_dram);
                }

                // mv bias from DRAM to SRAM
                if (krow_dram == 0 && kcol_dram == 0 && kch_dram == 0 && (first || !DOUBLE_BUFFERED_CD))
                  mvinBias((acc_t*)bias, span_batches, span_orows, span_ocols, span_ochs, och_dram);

                if (wait_for_moved_in_data) {
                  stellar_fence();
                  wait_for_moved_in_data = false;
                }

                {
                  // Regfile levels
                  //   Loop order is och, krow, kcol, kch, b, orow, ocol. (och is outermost)

                  // mv image from SRAM to reg-file
                  {
                    stellar_set_src_and_dst(SRC_DST_IS_SRAM, a_buffer_id * N_SRAMS + 0, SRC_DST_IS_REGFILE, 0);

                    stellar_set_constant(CONSTANT_LASTAXIS, 3);

                    stellar_set_span(FOR_BOTH, 0, DIM); // kch-inner
                    stellar_set_span(FOR_BOTH, 1, span_ocols); // ocol
                    stellar_set_span(FOR_BOTH, 2, span_orows); // orows
                    stellar_set_span(FOR_BOTH, 3, span_batches); // batches
                    stellar_set_span(FOR_BOTH, 4, span_kchs / DIM); // kch-outer
                    stellar_set_span(FOR_BOTH, 5, span_kcols); // kcols
                    stellar_set_span(FOR_BOTH, 6, span_krows); // krows
                    stellar_set_span(FOR_BOTH, 7, span_ochs / DIM); // och-outer

                    // Calculating the SRAM address:
                    //   irow * icols * ichs + icol * ichs + ich
                    //   (orow * stride + krow) * icols * ichs + (ocol * stride + kcol) * ichs + ich
                    //   orow * stride * icols * ichs + krow * icols * ichs + ocol * stride * ichs + kcol * ichs + ich
                    stellar_set_stride(FOR_BOTH, 1, (STRIDE >> DOWNSAMPLE) * span_kchs); // ocol
                    stellar_set_stride(FOR_BOTH, 2, (STRIDE >> DOWNSAMPLE) * span_icols * span_kchs); // orows
                    stellar_set_stride(FOR_BOTH, 3, span_irows * span_icols * span_kchs); // batches
                    stellar_set_stride(FOR_BOTH, 4, DIM); // kch-outer
                    stellar_set_stride(FOR_BOTH, 5, span_kchs); // kcols
                    stellar_set_stride(FOR_BOTH, 6, span_icols * span_kchs); // krows

                    stellar_issue();
                  }

                  // mv weights from SRAM to reg-file
                  {
                    stellar_set_src_and_dst(SRC_DST_IS_SRAM, b_buffer_id * N_SRAMS + 1, SRC_DST_IS_REGFILE, 0);

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);

                    stellar_set_span(FOR_BOTH, 0, DIM); // och-inner
                    stellar_set_span(FOR_BOTH, 1, DIM); // kch-inner
                    stellar_set_span(FOR_BOTH, 2, span_kchs / DIM); // kch-outer
                    stellar_set_span(FOR_BOTH, 3, span_krows * span_kcols); // krow-kcol
                    stellar_set_span(FOR_BOTH, 4, span_ochs / DIM); // och-outer

                    stellar_set_stride(FOR_BOTH, 1, span_ochs); // kch-inner
                    stellar_set_stride(FOR_BOTH, 2, span_ochs * DIM); // kch-outer
                    stellar_set_stride(FOR_BOTH, 3, span_kchs * span_ochs); // krow-kcol
                    stellar_set_stride(FOR_BOTH, 4, DIM); // och-outer

                    stellar_issue();
                  }

                  // mv bias from SRAM to reg-file
                  {
                    stellar_set_src_and_dst(SRC_DST_IS_SRAM, d_buffer_id * N_SRAMS + 2, SRC_DST_IS_REGFILE, 0);

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);
                    stellar_set_constant(CONSTANT_INTERLEAVEPOP, true);
                    stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 2);

                    stellar_set_span(FOR_BOTH, 0, DIM); // och-inner
                    stellar_set_span(FOR_BOTH, 1, span_batches * span_orows * span_ocols); // batches-orows-ocols
                    stellar_set_span(FOR_BOTH, 2, span_krows * span_kcols * span_kchs / DIM); // krow-kcol-(kch-outer)
                    stellar_set_span(FOR_BOTH, 3, span_ochs / DIM); // och-outer

                    stellar_set_stride(FOR_BOTH, 1, span_ochs); // batches-orows-ocols
                    stellar_set_stride(FOR_BOTH, 3, DIM); // och-outer

                    stellar_issue();
                  }

                  // mv output from reg-file to SRAM
                  {
                    stellar_set_src_and_dst(SRC_DST_IS_REGFILE, 0, SRC_DST_IS_SRAM, d_buffer_id * N_SRAMS + 2); // We use "d_buffer_id" here instead of "c_buffer_id" because we want to make sure that we're accumulating into the same SRAM bank that we're reading D from

                    stellar_set_constant(CONSTANT_LASTAXIS, 1);
                    stellar_set_constant(CONSTANT_INTERLEAVEPUSH, true);
                    stellar_set_constant(CONSTANT_INTERLEAVEAXIS, 2);

                    stellar_set_span(FOR_BOTH, 0, DIM); // och-inner
                    stellar_set_span(FOR_BOTH, 1, span_batches * span_orows * span_ocols); // batches-orows-ocols
                    stellar_set_span(FOR_BOTH, 2, span_krows * span_kcols * span_kchs / DIM); // krow-kcol-(kch-outer)
                    stellar_set_span(FOR_BOTH, 3, span_ochs / DIM); // och-outer

                    stellar_set_stride(FOR_BOTH, 1, span_ochs); // batches-orows-ocols
                    stellar_set_stride(FOR_BOTH, 3, DIM); // och-outer

                    stellar_issue();
                  }
                }

                if (!last) {
                  if (DOUBLE_BUFFERED_AB) {
                    // mv image_next from DRAM to SRAM
                    const int irow_dram_next = orow_dram_next * STRIDE + krow_dram_next;
                    const int icol_dram_next = ocol_dram_next * STRIDE + kcol_dram_next;
                    const int span_irows_next = span_orows_next * (STRIDE >> DOWNSAMPLE) + span_krows_next - (STRIDE >> DOWNSAMPLE);
                    const int span_icols_next = span_ocols_next * (STRIDE >> DOWNSAMPLE) + span_kcols_next - (STRIDE >> DOWNSAMPLE);
                    mvinImage((elem_t*)image, span_batches_next, span_irows_next, span_icols_next, span_kchs_next, batch_dram_next, irow_dram_next, icol_dram_next, kch_dram_next);

                    // mv weights_next from DRAM to SRAM
                    if (!(single_ochs_tile && single_kchs_tile && single_kcols_tile && single_krows_tile))
                      mvinWeights((elem_t*)weights, span_krows_next, span_kcols_next, span_kchs_next, span_ochs_next, krow_dram_next, kcol_dram_next, kch_dram_next, och_dram_next);
                  }

                  // mv bias_next from DRAM to SRAM
                  if (krow_dram_next == 0 && kcol_dram_next == 0 && kch_dram_next == 0 && DOUBLE_BUFFERED_CD)
                    mvinBias((acc_t*)bias, span_batches_next, span_orows_next, span_ocols_next, span_ochs_next, och_dram_next);
                }

                if (!DOUBLE_BUFFERED_AB) {
                  stellar_fence();
                  wait_for_moved_in_data = false;
                }

                first = false;
              }
            }
          }

          stellar_fence();
          wait_for_moved_in_data = false;

          // mv output from SRAM to DRAM
          mvoutOutput((elem_t*)result, span_batches, span_orows, span_ocols, span_ochs, batch_dram, orow_dram, ocol_dram, och_dram);

          if (!DOUBLE_BUFFERED_CD) {
            stellar_fence();
            wait_for_moved_in_data = false;
          }
        }
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

  printf("Problem sizes: %d %d %d %d %d %d %d %d\n", BATCHES, OROWS, OCOLS, OCHS, KROWS, KCOLS, KCHS, STRIDE);
  printf("Image sizes: %d %d %d\n", IROWS, ICOLS, DOWNSAMPLE);
  printf("Tile sizes: %d %d %d %d %d %d %d\n", TILE_BATCHES, TILE_OROWS, TILE_OCOLS, TILE_OCHS, TILE_KROWS, TILE_KCOLS, TILE_KCHS);
  printf("DIM: %d\n", DIM);

  // Perform matmul on Stellar
  printf("Starting conv...\n");
  uint64_t start = read_cycles();
  tiled_conv();
  uint64_t end = read_cycles();
  printf("Completed conv\n");

  // Print stats
  uint64_t ideal_cycles = (((uint64_t)BATCHES)*((uint64_t)OROWS)*((uint64_t)OCOLS)*((uint64_t)OCHS)*((uint64_t)KROWS)*((uint64_t)KCOLS)*((uint64_t)KCHS)) / (DIM*DIM);
  printf("Ideal cycles: %llu\n", ideal_cycles);
  printf("Actual cycles: %llu\n", end-start);
  printf("Utilization: %llu%%\n", ((uint64_t)100 * ideal_cycles) / (end-start));

#ifdef CHECK_RESULT
  // Check result
  if (!buffers_are_equal((elem_t*)result, (elem_t*)golden, sizeof(result))) {
    printf("FAILED\n");

    printf("\nResult matrix (%p):\n", result);
    for (int i = 0; i < sizeof(result)/sizeof(elem_t); i++) {
      if (i % OCHS == 0)
          printf("%d  ", i / OCHS);
      if (i % OCHS < 2)
        printf("%d=%d ", *((elem_t*)result + i), *((elem_t*)golden + i));
      if ((i+1) % OCHS == 0)
          printf("\n");
    }
    printf("\n");
  } else {
    printf("SUCCEEDED\n");
  }
#else
  printf("Not checked\n");
#endif

  exit(0);
}

