#ifndef EINSTEIN_H
#define EINSTEIN_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdbool.h>

#include "include/stellar_params.h"

#include "rocc-software/src/xcustom.h"

#define k_ISSUE 0

#define k_SETSRCANDDST 1
#define k_SETCONSTANT 2

#define k_SETADDRESS 3
#define k_SETSPAN 4
#define k_SETSTRIDE 5
#define k_SETAXIS 6
#define k_SETDATAORMETADATAADDRESS 7
#define k_SETMETADATASTRIDE 8
#define k_SETMETADATASTRIDEBYADDR 9
#define k_SETMETADATASTRIDEBYVALUE 10
#define k_SETFROMREGFILEMETADATACOORD 11
#define k_SETMETADATAREGION 12
#define k_SETWAITS 13
#define k_SETRECURSIVE 14

#define k_ISSUELOADBALANCER 28
#define k_SETLOADBALANCINGCLSIZE 29
#define k_SETLOADBALANCINGRFSIZE 30
#define k_SETLOADBALANCINGAXISSIZE 31
#define k_FLUSH 32

#define k_LOADSAVEDCONFIG 64

#define FOR_SRC 0b01
#define FOR_DST 0b10
#define FOR_BOTH 0b11

#define AXIS_DENSE 0
#define AXIS_COMPRESSED 1
#define AXIS_LINKEDLIST 2
#define AXIS_BITVECTOR 3

#define METADATA_COMPRESSED_OUTER 0
#define METADATA_COMPRESSED_INNER 1
#define METADATA_COMPRESSED_END 2
#define METADATA_LL_HEAD 0
#define METADATA_LL_COORD 1
#define METADATA_LL_NEXT 2
#define METADATA_LL_LAST 3

#define SRC_DST_IS_REGFILE 0b100
#define SRC_DST_IS_SRAM 0b010
#define SRC_DST_IS_DRAM 0b001
#define SRC_DST_IS_NOTHING 0b000

#define CONSTANT_ISDATA 0
#define CONSTANT_AXIS 1
#define CONSTANT_METADATABUFFERID 2
#define CONSTANT_LASTAXIS 3
#define CONSTANT_INTERLEAVEPUSH 4
#define CONSTANT_INTERLEAVEPOP 5
#define CONSTANT_INTERLEAVEAXIS 6
#define CONSTANT_SHOULDTRAIL 7
#define CONSTANT_RESETRUNNINGSTATE 8
#define CONSTANT_SHOULDGATHER 9
#define CONSTANT_LASTAXISLOGSIZE 10
#define CONSTANT_SHOULDTRAILCOARSE 11
#define CONSTANT_DROPFOLLOWINGCMDSIFEMPTY 12

#define WAIT_FOR_DMA 1
#define WAIT_FOR_SRAM_READS(buffer_id) ((uint64_t)(1) << ((buffer_id)*3 + 1))
#define WAIT_FOR_SRAM_WRITES(buffer_id) ((uint64_t)(1) << ((buffer_id)*3 + 2))
#define WAIT_FOR_SRAM_WRITES_FROM_DRAM(buffer_id) ((uint64_t)(1) << ((buffer_id)*3 + 3))

#define ROCC_INSTRUCTION_RS1_RS2(rs1, rs2, funct) \
  ROCC_INSTRUCTION_0_R_R(XCUSTOM_ACC, rs1, rs2, funct)

#define stellar_issue() \
    ROCC_INSTRUCTION_RS1_RS2(0, 0, k_ISSUE)

#define stellar_set_src_and_dst(src_type, src, dst_type, dst) \
    ROCC_INSTRUCTION_RS1_RS2(((src_type) << 16) | (src), \
        ((dst_type) << 16) | (dst), k_SETSRCANDDST);

#define stellar_set_constant(constant_type, value) \
    ROCC_INSTRUCTION_RS1_RS2(constant_type, value, k_SETCONSTANT)

#define stellar_set_address(for_src_or_dst, axis_id, addr) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | ((uint64_t)(axis_id)), addr, k_SETADDRESS)

#define stellar_set_span(for_src_or_dst, axis_id, span) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | ((uint64_t)(axis_id)), span, k_SETSPAN)

#define stellar_set_stride(for_src_or_dst, axis_id, stride) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | ((uint64_t)(axis_id)), stride, k_SETSTRIDE)

#define stellar_set_axis(for_src_or_dst, axis_id, axis) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | ((uint64_t)(axis_id)), axis, k_SETAXIS)

#define stellar_set_data_address(for_src_or_dst, address) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | ((uint64_t)(255)), address, k_SETDATAORMETADATAADDRESS)

#define stellar_set_metadata_address(for_src_or_dst, axis_id, metadata_id, address) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | (((uint64_t)(metadata_id)) << 8) | ((uint64_t)(axis_id)), address, k_SETDATAORMETADATAADDRESS)

#define stellar_set_metadata_stride(for_src_or_dst, iterator_axis_id, changed_axis_id, metadata_id, stride) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | (((uint64_t)(metadata_id)) << 16) | (((uint64_t)(changed_axis_id)) << 8) | ((uint64_t)(iterator_axis_id)), stride, k_SETMETADATASTRIDE)

#define stellar_set_metadata_stride_by_addr(for_src_or_dst, iterator_axis_id, changed_axis_id, metadata_id, stride) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | (((uint64_t)(metadata_id)) << 16) | (((uint64_t)(changed_axis_id)) << 8) | ((uint64_t)(iterator_axis_id)), stride, k_SETMETADATASTRIDEBYADDR)

#define stellar_set_metadata_stride_by_value(for_src_or_dst, iterator_axis_id, changed_axis_id, metadata_id, stride) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | (((uint64_t)(metadata_id)) << 16) | (((uint64_t)(changed_axis_id)) << 8) | ((uint64_t)(iterator_axis_id)), stride, k_SETMETADATASTRIDEBYVALUE)

// TODO we should rename "SRC_DST" in other parts of the code to "region"
#define stellar_set_metadata_region(for_src_or_dst, axis_id, metadata_id /* TODO Should the metadata region depend on the metadata_id, or only on the axis_id? */, region, region_index) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | (((uint64_t)(metadata_id)) << 8) | ((uint64_t)(axis_id)), (((uint64_t)(region)) << 16) | ((uint64_t)(region_index)), k_SETMETADATAREGION)

#define stellar_set_from_regfile_metadata(for_src_or_dst, axis_id, metadata_id, valid, coord) \
    ROCC_INSTRUCTION_RS1_RS2((((uint64_t)(for_src_or_dst)) << 62) | (((uint64_t)(metadata_id)) << 8) | ((uint64_t)(axis_id)), (((uint64_t)(valid)) << 32) | ((uint64_t)(coord)), k_SETFROMREGFILEMETADATACOORD)

#define stellar_wait(waits) \
    ROCC_INSTRUCTION_RS1_RS2(waits, 0, k_SETWAITS)

#define stellar_set_recursive(axis, recursive_dim) \
    ROCC_INSTRUCTION_RS1_RS2(axis, recursive_dim, k_SETRECURSIVE)

#define stellar_flush() \
    ROCC_INSTRUCTION_RS1_RS2(0, 0, k_FLUSH)

#define stellar_fence() asm volatile("fence")

#define stellar_load_saved_config(config_id) \
    ROCC_INSTRUCTION_RS1_RS2(config_id, 0, k_LOADSAVEDCONFIG)

#define stellar_issue_load_balancer(lb_id) \
    ROCC_INSTRUCTION_RS1_RS2(0, (uint64_t)(lb_id) << 32, k_ISSUELOADBALANCER)

#define stellar_set_load_balancing_axis_size(lb_id, axisId, value) \
    ROCC_INSTRUCTION_RS1_RS2(axisId, ((uint64_t)(lb_id) << 32) | value, k_SETLOADBALANCINGAXISSIZE)

#define stellar_set_load_balancing_cl_size(lb_id, axisId0, axisId1, value) \
    ROCC_INSTRUCTION_RS1_RS2(((uint64_t)(axisId1) << 16) | axisId0, ((uint64_t)(lb_id) << 32) | value, k_SETLOADBALANCINGCLSIZE)

#define stellar_set_load_balancing_rf_size(lb_id, axisId0, axisId1, axisId2, value) \
    ROCC_INSTRUCTION_RS1_RS2(((uint64_t)(axisId2) << 32) | ((uint64_t)(axisId1) << 16) | axisId0, ((uint64_t)(lb_id) << 32) | value, k_SETLOADBALANCINGRFSIZE)

static uint64_t read_cycles() {
    uint64_t cycles;
    asm volatile ("rdcycle %0" : "=r" (cycles));
    return cycles;
}

#endif

