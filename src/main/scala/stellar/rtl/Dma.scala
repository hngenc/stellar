package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._
import stellar.{CompressedMetadata, FiberTreeAxis, FiberTreeAxisMetadata, LinkedListMetadata, OuterSpace}
import ChiselUtil._
import chisel3.printf.Printf
import stellar.rtl.Dma.addrBits

/*
  The user specifies certain configuration options for each axis. However, these configuration options
  can also be updated during the mvin/mvout process (especially the "base-address" option).

  Configuration options:
    * Base-address
      - Two base addresses for CSR
    * End-address
    * Span (in compressed terms)
*/

class DmaConfiguration(nAxes: Int, sramSpanBits: Int) extends Bundle {
  val axes = Vec(nAxes, FiberTreeAxis())
  val spans = Vec(nAxes, UInt(sramSpanBits.W))

  val addrs = Vec(nAxes, UInt(sramSpanBits.W))
  val data_base_addrs = Vec(nAxes, UInt(Dma.addrBits.W))
  val metadata_base_addrs = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.addrBits.W))))

  val data_strides = Vec(nAxes, UInt(Dma.spanBits.W))
  val metadata_strides = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))
  val metadata_strides_by_addr = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))
  val metadata_strides_by_value = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))

  val metadata_regions = Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, new Region))

  val sram_code = UInt(Dma.sramCodeBits.W)
  val sram_is_data = Bool()
  val sram_metadata_buffer_id = UInt(log2Up(FiberTreeAxisMetadata.n_metadata_buffers).W)
  val sram_base_addrs = Vec(nAxes, UInt(Dma.spanBits.W)) // TODO this doesn't seem to be used anywhere; should we remove it?
  val sram_data_strides = Vec(nAxes, UInt(Dma.spanBits.W))
  val sram_metadata_strides = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))
  val sram_metadata_strides_by_addr = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))
  val sram_should_trail = Bool()
  val sram_should_trail_coarse = Bool()

  val drop_following_cmds_if_empty = Bool()

  val flattened_mvout = Bool()

  val write = Bool()
}

class InFlightTableEntry(nAxes: Int, nSrams: Int, val maxReqLenInBytes: Int, sramSpanBits: Int) extends Bundle {
  val iterator_values = Vec(nAxes, UInt(sramSpanBits.W))
  val axis = UInt(log2Up(nAxes).W)

  val lens = Vec(nAxes, UInt(sramSpanBits.W))
  def len = lens.head
  val offset = UInt(log2Up(maxReqLenInBytes + 1).W)

  val sram_code = UInt(log2Up(nSrams).W)
  val sram_data_strides = Vec(nAxes, UInt(Dma.spanBits.W))
  val sram_metadata_strides = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))
  val sram_metadata_strides_by_addr = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(Dma.spanBits.W))))
  val sram_should_trail = Bool()
  val sram_should_trail_coarse = Bool()

  val is_data = Bool()
  val metadata_buffer_id = UInt(FiberTreeAxisMetadata.metadata_buffer_id_bits.W)

  val write = Bool()
  val read_into_dma = Bool()

  val to_free_node = Bool() // TODO I don't think "to_free_node" is actually used anywhere anymore?
  val use_running_offset = Bool()
}

class InFlightTableLookup(idBits: Int) extends Bundle {
  val id = UInt(idBits.W)
  val clear = Bool()
}

class DmaTLReq(smallestElemWidthInBytes: Int, maxReqLenInBytes: Int, logLenBits: Int, idBits: Int, beatWidthBits: Int, tableEntryT: InFlightTableEntry) extends Bundle {
  val id = UInt(idBits.W)

  val addr = UInt(Dma.addrBits.W)
  val logLen = UInt(logLenBits.W)

  val write = Bool()

  val flattened_mvout = Bool()
  val multiaxis_mvout = Bool() // TODO we should combine the "flattened_mvout" and "multiaxis_mvout" parameters

  val write_from_dma = Bool()
  val data = UInt(beatWidthBits.W) // This is only used when "write_from_dma" is true
  val mask = UInt((beatWidthBits/8).W) // This is only used when "write_from_dma" is true

  val tableEntry = getChiselType(tableEntryT)

  // The variables below are only used internally by the Dma module
  val span_step_size = UInt(log2Up(maxReqLenInBytes / smallestElemWidthInBytes + 1).W)
}

class DmaBeat(tableEntryT: InFlightTableEntry, beatWidthInBits: Int, maxInFlightReqsBits: Int) extends Bundle {
  val data = UInt(beatWidthInBits.W)
  val mask = UInt((beatWidthInBits / 8).W)

  val first = Bool()
  val last = Bool()

  val table_entry = getChiselType(tableEntryT)
  val id = UInt(maxInFlightReqsBits.W)
}

class InFlightTable[T <: Data](nAxes: Int, nSrams: Int, idBits: Int, maxReqLenInBytes: Int, nAllocPorts: Int, nLookupPorts: Int, sramSpanBits: Int)(allocPortVisibility: Seq[Seq[Int]] = (0 until (1 << idBits)).grouped((1 << idBits) / nAllocPorts).toSeq, lookupPortVisibility: Seq[Seq[Int]] = Seq.fill(nLookupPorts)(0 until (1 << idBits))) extends Module {
  def entry_t = new InFlightTableEntry(nAxes=nAxes, nSrams=nSrams, maxReqLenInBytes=maxReqLenInBytes, sramSpanBits=sramSpanBits)

  val n_entries = 1 << idBits

  require(allocPortVisibility.size == nAllocPorts, s"allocPortVisibility.size=${allocPortVisibility.size} nAllocPorts=$nAllocPorts")
  require(lookupPortVisibility.size == nLookupPorts, s"lookupPortVisibility.size=${lookupPortVisibility.size} nLookupPorts=$nLookupPorts")
  require(allocPortVisibility.flatten.sorted.toSet == (0 until n_entries).toSet, "alloc ports must cover all entries in table, without any duplicates")

  val io = IO(new Bundle {
    val alloc_reqs = Vec(nAllocPorts, Flipped(Decoupled(entry_t)))
    val alloc_ids = Output(Vec(nAllocPorts, UInt(idBits.W)))

    val lookup_reqs = Vec(nLookupPorts, Flipped(Valid(new InFlightTableLookup(idBits=idBits))))
    val lookup_resps = Vec(nLookupPorts, Valid(entry_t))

    val busy = Output(Bool())
    val read_busies = Output(Vec(nSrams, Bool()))
  })

  val all_entries = Reg(Vec(n_entries, entry_t))
  val all_entries_valid = RegInit(VecInit.fill(n_entries)(false.B))

  for ((((io_alloc_req, io_alloc_id), entryIds), allocPortId) <- io.alloc_reqs.zip(io.alloc_ids).zip(allocPortVisibility).zipWithIndex) {
    val alloc_id = MuxCase(n_entries.U, entryIds.map { entryId =>
      val is_distinct = all(allocPortVisibility.zip(io.alloc_ids).take(allocPortId).collect {
        case (priorEntryIds, priorAllocId) if priorEntryIds.contains(entryId) => priorAllocId =/= entryId.U
      })

      (!all_entries_valid(entryId) && is_distinct) -> entryId.U
    })
    io_alloc_id := alloc_id

    io_alloc_req.ready := alloc_id < n_entries.U

    when (io_alloc_req.fire) {
      all_entries(alloc_id) := io_alloc_req.bits
      all_entries_valid(alloc_id) := true.B
    }
  }

  io.lookup_reqs.zip(io.lookup_resps).zip(lookupPortVisibility).foreach { case ((lookup_req, lookup_resp), entryIds) =>
    if (entryIds.toSet == (0 until n_entries).toSet) {
      lookup_resp.valid := lookup_req.valid
      assert(!lookup_req.fire || all_entries_valid(lookup_req.bits.id), "looking up an in-flight req which doesn't even exist")
    } else {
      lookup_resp.valid := false.B
    }
    lookup_resp.bits := all_entries(entryIds.head)

    entryIds.foreach { entryId =>
      when (lookup_req.bits.id === entryId.U) {
        lookup_resp.valid := lookup_req.valid
        lookup_resp.bits := all_entries(entryId)

        when (lookup_req.fire && lookup_req.bits.clear) {
          all_entries_valid(entryId) := false.B
        }
      }
    }

    assert(!(lookup_req.fire && lookup_req.bits.clear) || all_entries_valid(lookup_req.bits.id) && any(entryIds.map(_.U === lookup_req.bits.id)),
      "trying to clear something you can't find")
  }

  io.busy := any(all_entries_valid)
  io.read_busies.zipWithIndex.foreach { case (busy, sramCode) =>
    busy := any(all_entries_valid.zip(all_entries).map { case (v, e) => v && !e.write && e.sram_code === sramCode.U && !e.read_into_dma })
  }

  // Assertions
  val stall_counter = RegInit(0.U(16.W))
  when (!any(all_entries_valid) || any(io.lookup_reqs.map(lookup => lookup.fire && lookup.bits.clear))) {
    stall_counter := 0.U
  }.otherwise {
    stall_counter := stall_counter + 1.U
  }
  assert(stall_counter <= 10000.U, "inflight table is stalling")
}

class Dma[T <: Data](elemTs: Seq[T], beatWidthInBits: Int, nAxes: Int, spanBits: Int, maxInFlightReqs: Int,
                     logMaxTlReqLen: Int, nNonDenseAxes: Int, nSramBanks: Seq[Int],
                     sram_read_req_t: ChiselSRAMReadReq, sram_read_resp_t: ChiselSRAMReadResp[T],
                     sram_write_req_t: ChiselSRAMWriteReq[T],
                     accesses_to_same_address_in_outer_memory_ordered: Boolean = false,
                     nTlReqPorts: Int = 1, nReadRespPorts: Int = 1,

                     coords_only_come_from_srams: Boolean = false, coords_only_used_for_ptr_prefetches: Boolean = false,
                     coords_only_requested_once: Boolean = false,
                     stride_by_value_fixed_to_ptr_packet_size: Boolean = false,
                     no_strides_in_waiting_for_lower_axes_to_wrap_around_states: Boolean = false,
                     axis0_writes_are_all_and_only_infinite: Boolean = false,
                     val has_repeated_bias_optimizations: Boolean = true,

                     nUselessAxes: Int = 0, sramSpanBits: Int = Dma.spanBits,
                     supportedAxes: Set[FiberTreeAxis.Type] = FiberTreeAxis.all.toSet,

                     ll_node_size: Int = 1, total_sram_nodes: Int = 2,
                     total_sram_heads: Int = 2,
                    ) extends Module {
  val nAxisBits = log2Ceil(nAxes)
  val maxInFlightReqsBits = log2Ceil(maxInFlightReqs)

  val nSrams = elemTs.size; require(nSramBanks.size == nSrams)

  val biggestElemT = elemTs.maxBy(_.getWidth)
  val smallestElemT = elemTs.minBy(_.getWidth)
  val biggestElemWidthInBytes = biggestElemT.getWidth / 8
  val smallestElemWidthInBytes = smallestElemT.getWidth / 8
  val smallestLogElemTWidthInBytes = log2Ceil(smallestElemWidthInBytes)
  val elemTWidthsInBytes = elemTs.map(_.getWidth / 8)
  val logElemTWidthsInBytes = elemTs.map(elemT => log2Ceil(elemT.getWidth / 8))

  val metadataBits = biggestElemT.getWidth
  val metadataBytes = metadataBits / 8
  val logMetadataBytes = log2Ceil(metadataBytes)

  val maxReqLenInBytes = 1 << logMaxTlReqLen
  val beatWidthInBytes = beatWidthInBits / 8
  val logBeatWidthBytes = log2Ceil(beatWidthInBytes)
  val alignTo = smallestElemWidthInBytes min metadataBytes

  require(biggestElemWidthInBytes <= maxReqLenInBytes)

  def inflight_entry_t = new InFlightTableEntry(nAxes=nAxes, nSrams=nSrams, maxReqLenInBytes=maxReqLenInBytes, sramSpanBits=sramSpanBits)
  def dma_beat_t = new DmaBeat(tableEntryT=inflight_entry_t, beatWidthInBits=beatWidthInBits, maxInFlightReqsBits=maxInFlightReqsBits)

  def is_axis(x: FiberTreeAxis.Type, y: FiberTreeAxis.Type): Bool = {
    require(y.isLit)
    if (supportedAxes.contains(y)) x === y else false.B
  }

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new DmaConfiguration(nAxes=nAxes, sramSpanBits=sramSpanBits)))

    val inflight_alloc_reqs = Vec(nTlReqPorts, Decoupled(inflight_entry_t))
    val inflight_alloc_ids = Vec(nTlReqPorts, Input(UInt(maxInFlightReqsBits.W)))

    val inflight_table_is_clearing = Flipped(Valid(UInt(maxInFlightReqsBits.W))) // This is only guaranteed to check for inflight reqs that are not headed towards the DMA. It might also report other reqs, but it's not guaranteed to.

    val clear_inflight_table_entries = Vec(nReadRespPorts, Valid(UInt(maxInFlightReqsBits.W)))

    val tl_reqs = Vec(nTlReqPorts, Decoupled(new DmaTLReq(
      smallestElemWidthInBytes=smallestElemWidthInBytes, maxReqLenInBytes=maxReqLenInBytes, logLenBits=log2Up(logMaxTlReqLen+1),
      idBits=maxInFlightReqsBits, beatWidthBits=beatWidthInBits, tableEntryT=inflight_entry_t
    )))

    val sram_read_req = Decoupled(getChiselType(sram_read_req_t)) // TODO also add an "sram_read_resps" port that enables us to read SRAM resps directly into the DMA instead of going through DRAM
    val sram_read_req_sram_code = Output(UInt(Dma.sramCodeBits.W))
    val sram_read_resp = Flipped(Decoupled(getChiselType(sram_read_resp_t)))

    val sram_write_req = Decoupled(new Bundle {
      val req = getChiselType(sram_write_req_t) // TODO also add an "sram_read_resps" port that enables us to read SRAM resps directly into the DMA instead of going through DRAM
      val sram_code = Output(UInt(Dma.sramCodeBits.W))
    })

    val sram_read_req_readies = Input(MixedVec(nSramBanks.map(nBanks => Vec(nBanks, Bool())))) // Right now, this is just used to optimize OuterSPACE-like rowlen mvouts

    val sram_read_busies = Input(Vec(nSrams, Bool())) // Right now, this is used to prevent OuterSPACE merger mvins from interfering with prior mvouts
    val sram_write_busies = Input(Vec(nSrams, Bool()))
    val sram_write_from_dram_busies = Input(Vec(nSrams, Bool()))

    // These beats are used to read/write configuration data (like CSR row and col ids)
    val read_beats = Vec(nReadRespPorts, Flipped(Decoupled(dma_beat_t)))

    val snoop_write_beat = Flipped(Valid(dma_beat_t))
    val snoop_write_beat_axis_span = Flipped(Valid(UInt(32.W))) // TODO fuse this with 'snoop_write_beat'

    val drop_cmds_until_dma_inst = Valid(new Bundle {
      val write = Bool()
      val sram_code = UInt(Dma.sramCodeBits.W)
    })

    val busy = Output(Bool())

    val debug_print = Input(Bool())
  })

  val req = Reg(getChiselType(io.req.bits))
  val req_valid = RegInit(false.B)

  val axis = Reg(UInt(nAxisBits.W))

  val span_iterators = Reg(Vec(nAxes, UInt(sramSpanBits.W)))
  val all_wrapped_arounds = Reg(Vec(nAxes, Bool()))

  val span = req.spans(axis)
  val span_iterator = span_iterators(axis)
  val data_base_addr = req.data_base_addrs(axis)
  val data_stride = req.data_strides(axis)
  val metadata_base_addrs = req.metadata_base_addrs(axis)
  val metadata_strides = req.metadata_strides(axis)
  val metadata_strides_by_addr = req.metadata_strides_by_addr(axis)
  val metadata_strides_by_value = req.metadata_strides_by_value(axis)
  val metadata_addrs = metadata_base_addrs(axis)
  val metadata_regions = req.metadata_regions(axis)

  val axis_type = Mux(req.flattened_mvout, FiberTreeAxis.Dense, req.axes(axis))
  val wrapped_around = all_wrapped_arounds(axis)
  val innermost_axis = Reg(UInt(log2Up(nAxes-nUselessAxes).W))
  val outermost_axis = Reg(UInt(log2Up(nAxes-nUselessAxes).W))
  val is_innermost = axis === innermost_axis
  val is_outermost = axis === outermost_axis

  val sram_spans = if (has_repeated_bias_optimizations) {
    // These are the spans that we will send to the SRAMs during mvins. It will almost always be the case that
    // "sram_spans.tail.forall(_ == 1)", because the DMA usually just performs 1D data transfers. For some specific
    // optimizations though, like with 1D bias mvins (common in DNNs), this can have different values.
    Reg(getChiselType(io.req.bits.spans))
  } else {
    VecInit.fill(nAxes)(1.U)
  }

  io.busy := req_valid
  assert(!req_valid || innermost_axis > 0.U || ChiselUtil.all(req.data_base_addrs.map(_ % smallestElemWidthInBytes.U === 0.U)), "unaligned data address")

  io.read_beats.foreach(_.ready := false.B)

  io.drop_cmds_until_dma_inst.valid := false.B
  io.drop_cmds_until_dma_inst.bits.write := req.write
  io.drop_cmds_until_dma_inst.bits.sram_code := req.sram_code

  // States
  object CompressedStates extends ChiselEnum {
    val reset_sram = Value

    val requesting_ptr, waiting_for_ptr = Value
    val requesting_next_ptr, waiting_for_next_ptr, waiting_for_lower_axes_to_wrap_around__next_ptr = Value

    val writing_outer_id_to_dram, waiting_for_outer_id_write = Value
    val requesting_outer_id, waiting_for_outer_id = Value
    val writing_next_outer_id_to_dram, waiting_for_next_outer_id_write = Value
    val requesting_next_outer_id, waiting_for_next_outer_id = Value

    val requesting_coord, waiting_for_coord, waiting_for_lower_axes_to_wrap_around__coord = Value

    val writing_inner_ids_to_dram_or_sram, writing_data_to_dram_or_sram = Value

    val update_last_node_in_ptr_packet, waiting_for_update_of_last_node = Value
    val update_next_node_in_ptr_packet, waiting_for_update_of_next_node = Value
  }
  import CompressedStates._
  val default_base_compressed_state = requesting_ptr
  val base_compressed_state = Reg(CompressedStates())

  class PtrPacket extends Bundle {
    val last_ptr_rel_offset = UInt(metadataBits.W)
    val next_ptr_rel_offset = UInt(metadataBits.W)
    val data_and_coord_addr_offset = UInt(metadataBits.W)

    assert(metadataBits % 2 == 0)
    val ll_len = UInt((metadataBits/2).W)
    val rowlen = UInt((metadataBits/2).W)

    def update(other: PtrPacket, last_ptr_rel_offset_guard: Bool, next_ptr_rel_offset_guard: Bool): Unit = {
      rowlen := other.rowlen
      ll_len := other.ll_len
      data_and_coord_addr_offset := other.data_and_coord_addr_offset
      next_ptr_rel_offset := other.next_ptr_rel_offset
      when (!last_ptr_rel_offset_guard) {
        last_ptr_rel_offset := other.last_ptr_rel_offset
      }
      when (!next_ptr_rel_offset_guard) {
        next_ptr_rel_offset := other.next_ptr_rel_offset
      }
    }

    def last_node_field_location_in_bits: Int = {
      val result = elements.takeWhile(_._1 != "last_ptr_rel_offset").map(_._2.getWidth).sum
      assert(result == 3 * metadataBits)
      result
    }
    def next_node_field_location_in_bits: Int = {
      val result = elements.takeWhile(_._1 != "next_ptr_rel_offset").map(_._2.getWidth).sum
      assert(result == 2 * metadataBits)
      result
    }
  }
  def ptr_packet_t = new PtrPacket
  val ptrPacketWidthInBits = ptr_packet_t.getWidth
  val ptrPacketWidthInBytes = ptrPacketWidthInBits / 8
  val ptrPacketWidthInMetadataElems = ptrPacketWidthInBytes / metadataBytes

  val n_prefetched_ptrs = 128 // 4.max(nTlReqPorts.min(nReadRespPorts))
  class PtrPrefetch extends Bundle {
    val addr = UInt(addrBits.W)
    val id = UInt(maxInFlightReqsBits.W)

    val packet = ptr_packet_t

    val requested = Bool()
    val returned = Bool()

    val last_node_updated = Bool() // This is used to deal with RAW hazards during mvouts
    val next_node_updated = Bool()

    val offset = getChiselType(io.read_beats.head.bits.table_entry.offset)

    val valid = Bool()

    val free_node_addr_opt = Option.when(support_ooo_ptr_mvouts)(UInt(addrBits.W))
    // val span_iterators = Vec(nAxes, UInt(sramSpanBits.W)) // TODO I suspect that storing a full Vec here is causing severe FIRRTL->Verilog slowdowns, so I replaced it with the line below which only save a single span_iterator value. But we should figure out later if using only one span_iterator is generalizable to all cases
    val outer_span_iterator = UInt(sramSpanBits.W) // TODO read the comment above for 'val span_iterators'

    // The variables below help with mvins
    val outer_id_written_into_sram = Bool()
    val inner_nodes_initialized = Bool()
    val prefetch_req_for_next_node_reserved = Bool()
    val node_iterator = UInt(sramSpanBits.W) // TODO read the comment above for 'val span_iterators'
    val row_iterator = UInt(sramSpanBits.W) // TODO read the comment above for 'val span_iterators'
    val total_nodes_moved_in = UInt(32.W) // TODO magic number

    def reset(): Unit = {
      requested := false.B
      returned := false.B
      last_node_updated := false.B
      next_node_updated := false.B
      outer_id_written_into_sram := false.B
      inner_nodes_initialized := false.B
      prefetch_req_for_next_node_reserved := false.B
      valid := false.B
    }

    def waiting_to_return: Bool = valid && requested && !returned
  }

  val n_prefetched_row_ids = 32.max(io.sram_write_req.bits.req.data.size * 2)
  val n_row_id_prefetch_reqs = 2 // This is the maximum number of in-flight row-id prefetch TL reqs we can have
  class RowIdPrefetchReq extends Bundle {
    val row_id_id = UInt(log2Up(n_prefetched_row_ids).W)
    val size = UInt(log2Up(n_prefetched_row_ids+1).W)

    val tl_id = UInt(maxInFlightReqsBits.W)

    val requested = Bool()

    def reset(): Unit = {
      requested := false.B
    }
  }

  val add_node_to_maximize_performance = true
  val support_ooo_ptr_mvouts = true

  class CompressedAxisStateData extends Bundle {
    val original_next_free_ptr_addr = UInt(addrBits.W) // TODO we don't currently have a way of "returning" the finaly incremented free-ptr to the programmer
    val original_span = UInt(sramSpanBits.W)

    val outer_dim_id = UInt(metadataBits.W)
    val tiled_outer_dim_id = UInt(sramSpanBits.W)

    val data_offset = UInt(spanBits.W) // TODO can this be fused with 'outer_dim_id' or some other variable?
    val coord_offset = UInt(spanBits.W) // TODO can this be fused with 'outer_dim_id' or some other variable?

    val inflight_req_id = UInt(maxInFlightReqsBits.W)

    val added_new_ll_node = Bool()
    val prev_last_node_addr = UInt(addrBits.W)
    val next_node_addr = UInt(addrBits.W) // TODO we should be able to fuse this with 'prev_last_node_addr' to save bits, because i don't think two registers are ever used simultaneously
    val reached_last_node = Bool()
    val updated_rowlen = Vec(n_prefetched_ptrs+1, Bool())
    val updated_last_node_ptr = Vec(n_prefetched_ptrs+1, Bool())
    val updated_next_node_ptr = Vec(n_prefetched_ptrs+1, Bool())

    val requested_coord_it = UInt(Dma.spanBits.W) // TODO can this be sramSpanBits?
    val requested_coord_iterated = Bool()

    val prefetched_ptrs = Vec(n_prefetched_ptrs, new PtrPrefetch)

    val prefetched_row_ids = Vec(n_prefetched_row_ids, UInt(metadataBits.W))
    val prefetched_row_id_valids = Vec(n_prefetched_row_ids, Bool())
    val prefetched_row_id_data_requested = Vec(n_prefetched_row_ids, Bool())
    val prefetched_row_id_written = Vec(n_prefetched_row_ids, Bool())
    val row_id_prefetch_reqs = Vec(n_row_id_prefetch_reqs, new RowIdPrefetchReq)

    val use_ll_ptr = Bool() // This variable doesn't actually need to be Reg; we just keep it as one to help shorten critical paths

    val state = CompressedStates()
  }
  val compressed_axis_state_datas = Reg(Vec(nNonDenseAxes max 1, new CompressedAxisStateData))

  val all_next_cycle_prefetched_ptrs = VecInit(compressed_axis_state_datas.map(_.prefetched_ptrs))
  compressed_axis_state_datas.map(_.prefetched_ptrs).zip(all_next_cycle_prefetched_ptrs).foreach { case (current, next) => current := next }

  val all_next_cycle_prefetched_row_ids = VecInit(compressed_axis_state_datas.map(_.prefetched_row_ids))
  val all_next_cycle_prefetched_row_id_valids = VecInit(compressed_axis_state_datas.map(_.prefetched_row_id_valids))
  compressed_axis_state_datas.map(_.prefetched_row_ids).zip(all_next_cycle_prefetched_row_ids).foreach { case (current, next) => current := next }
  compressed_axis_state_datas.map(_.prefetched_row_id_valids).zip(all_next_cycle_prefetched_row_id_valids).foreach { case (current, next) => current := next }

  // Make TileLink requests
  val logElemTWidthInBytes = Mux(req.write, smallestLogElemTWidthInBytes.U, selectFrom(logElemTWidthsInBytes.map(_.U), req.sram_code))
  val tl_req_addr = WireInit(Mux(is_axis(req.axes.head, FiberTreeAxis.Dense) || req.flattened_mvout, align(req.data_base_addrs.head, alignTo),
    align(req.data_base_addrs.head, smallestElemWidthInBytes) + (compressed_axis_state_datas.head.data_offset << logElemTWidthInBytes)(spanBits-1,0)
  ))
  val tl_req_len = WireInit(span - span_iterator)

  def generate_tl_req(req_addr: UInt, req_len: UInt, log_req_size: Int) = {
    val tl_req = Wire(getChiselType(io.tl_reqs.head.bits))

    val l = (1.U << log_req_size).asUInt
    val base_addr = req_addr
    val base_addr_aligned = ((base_addr >> log_req_size) << log_req_size).asUInt
    val end_addr = base_addr_aligned +& l

    val step_size = reduceWidth(((end_addr - base_addr) >> logElemTWidthInBytes).asUInt, sramSpanBits); require(nNonDenseAxes == 0 || elemTWidthsInBytes.forall(_ == metadataBytes))
    val n_elems = minOf(req_len, step_size)

    tl_req.id := DontCare
    tl_req.addr := base_addr_aligned
    tl_req.logLen := log_req_size.U
    tl_req.write := req.write
    tl_req.write_from_dma := false.B
    tl_req.data := DontCare
    tl_req.mask := DontCare
    tl_req.flattened_mvout := req.flattened_mvout
    tl_req.multiaxis_mvout := false.B
    tl_req.tableEntry.iterator_values := (req.sram_base_addrs zip span_iterators).map { case (x,y) => x + y }
    tl_req.tableEntry.sram_data_strides := req.sram_data_strides
    tl_req.tableEntry.sram_metadata_strides := req.sram_metadata_strides
    tl_req.tableEntry.sram_metadata_strides_by_addr := req.sram_metadata_strides_by_addr
    tl_req.tableEntry.sram_should_trail := req.sram_should_trail
    tl_req.tableEntry.sram_should_trail_coarse := req.sram_should_trail_coarse
    tl_req.tableEntry.sram_code := req.sram_code
    tl_req.tableEntry.axis := 0.U
    tl_req.tableEntry.len := n_elems
    connectVecs(tl_req.tableEntry.lens.tail, sram_spans.tail)
    tl_req.tableEntry.offset := align((base_addr % l).asTypeOf(UInt(logMaxTlReqLen.W)), alignTo)
    tl_req.tableEntry.is_data := req.sram_is_data
    tl_req.tableEntry.metadata_buffer_id := req.sram_metadata_buffer_id
    tl_req.tableEntry.write := req.write
    tl_req.tableEntry.read_into_dma := false.B
    tl_req.tableEntry.to_free_node := false.B
    tl_req.tableEntry.use_running_offset := req.flattened_mvout
    tl_req.span_step_size := step_size

    tl_req
  }

  val log_min_req_size = smallestLogElemTWidthInBytes.max(log2Up(beatWidthInBits/8))
  val potential_tl_reqs = (log_min_req_size to logMaxTlReqLen).map { ll =>
    generate_tl_req(tl_req_addr, tl_req_len, ll)
  }

  val infinite_tl_req = WireInit(potential_tl_reqs.last)
  when (req.flattened_mvout) {
    infinite_tl_req.tableEntry.lens := req.spans
  }
  infinite_tl_req.tableEntry.len := req.spans(axis) - span_iterator

  val use_infinite = WireInit(req.flattened_mvout)
  for (axis2 <- 0 until nNonDenseAxes) {
    val cond = if (axis0_writes_are_all_and_only_infinite) (axis2 == 0).B else { axis2.U === innermost_axis && req.axes(axis2) =/= FiberTreeAxis.Dense }
    when (req.write && axis2.U === axis && cond) {
      use_infinite := true.B
    }
  }

  val best_tl_req = Mux(use_infinite, infinite_tl_req,
    potential_tl_reqs.reduce { (acc, tlr) =>
      Mux(tlr.tableEntry.len > acc.tableEntry.len, tlr, acc)
    }
  )

  val tl_req_valids = VecInit.fill(nTlReqPorts)(false.B)
  val prefetch_tl_req_valids = VecInit.fill(nTlReqPorts)(false.B)
  io.tl_reqs.zip(io.inflight_alloc_reqs).zip(io.inflight_alloc_ids).zip(tl_req_valids).zip(prefetch_tl_req_valids).foreach { case ((((io_tl_req, io_inflight_alloc_req), io_inflight_alloc_id), tl_req_valid), prefetch_tl_req_valid) =>
    io_tl_req.valid := req_valid && io_inflight_alloc_req.ready && (tl_req_valid || prefetch_tl_req_valid)
    io_tl_req.bits := best_tl_req
    io_tl_req.bits.id := io_inflight_alloc_id
  }

  def mk_read_into_dma_req(addr: UInt, write: Bool = false.B, metadata_buffer_id: UInt = 0.U, portId: UInt = 0.U): UInt = {
    val io_tl_req = io.tl_reqs(portId)

    val addr_aligned = ((addr >> logBeatWidthBytes) << logBeatWidthBytes).asUInt
    val offset = align(addr - addr_aligned.asUInt, alignTo)

    io_tl_req.bits.addr := addr_aligned
    io_tl_req.bits.write := write
    io_tl_req.bits.logLen := logBeatWidthBytes.U
    io_tl_req.bits.tableEntry.axis := axis
    io_tl_req.bits.tableEntry.lens.foreach(_ := 1.U)
    io_tl_req.bits.tableEntry.offset := offset
    io_tl_req.bits.tableEntry.is_data := false.B
    io_tl_req.bits.tableEntry.metadata_buffer_id := metadata_buffer_id
    io_tl_req.bits.tableEntry.write := write
    io_tl_req.bits.tableEntry.read_into_dma := !write
    io_tl_req.bits.tableEntry.sram_should_trail_coarse := write && req.sram_should_trail_coarse
    io_tl_req.bits.tableEntry.to_free_node := false.B
    io_tl_req.bits.tableEntry.use_running_offset := false.B
    io_tl_req.bits.span_step_size := 1.U

    beatWidthInBytes.U - offset
  }

  def mk_write_from_dma_req(addr: UInt, data: UInt, bitWidth: Int, portId: UInt = 0.U): UInt = {
    val io_tl_req = io.tl_reqs(portId)

    val addr_aligned = (addr >> logBeatWidthBytes) << logBeatWidthBytes
    val offset = align((addr - addr_aligned.asUInt).asTypeOf(UInt(logBeatWidthBytes.W)), alignTo)

    io_tl_req.bits.addr := addr_aligned
    io_tl_req.bits.write := true.B
    io_tl_req.bits.write_from_dma := true.B
    io_tl_req.bits.flattened_mvout := false.B
    io_tl_req.bits.data := data << (offset * 8.U)
    if (bitWidth >= 0) // If bitwidth is -1, then the mask will be set by the caller
      io_tl_req.bits.mask := {
        val ones = ~(0.U((bitWidth/8).W))
        ones << offset
      }
    io_tl_req.bits.logLen := logBeatWidthBytes.U
    io_tl_req.bits.tableEntry.axis := axis
    io_tl_req.bits.tableEntry.lens.foreach(_ := 1.U)
    io_tl_req.bits.tableEntry.offset := offset
    io_tl_req.bits.tableEntry.is_data := false.B
    io_tl_req.bits.tableEntry.metadata_buffer_id := DontCare
    io_tl_req.bits.tableEntry.write := true.B
    io_tl_req.bits.tableEntry.read_into_dma := false.B
    io_tl_req.bits.tableEntry.to_free_node := false.B
    io_tl_req.bits.tableEntry.use_running_offset := false.B
    io_tl_req.bits.span_step_size := 1.U

    offset
  }

  io.inflight_alloc_reqs.zip(io.tl_reqs).zip(tl_req_valids).zip(prefetch_tl_req_valids).foreach { case (((io_inflight_alloc_req, io_tl_req), tl_req_valid), prefetch_tl_req_valid) =>
    io_inflight_alloc_req.valid := req_valid && io_tl_req.ready && (tl_req_valid || prefetch_tl_req_valid)
    io_inflight_alloc_req.bits := io_tl_req.bits.tableEntry

    assert((io_tl_req.fire && io_inflight_alloc_req.fire) || (!io_tl_req.fire && !io_inflight_alloc_req.fire), "making a TL req without allocating space for it in the inflight table")
  }

  io.clear_inflight_table_entries.zip(io.read_beats).foreach { case (clear, read_beat) =>
    clear.valid := read_beat.fire; assert(!read_beat.fire || read_beat.bits.last, "i'm not sure if it's fine to clear the inflight-table entry when the read-beat is larger than just one single beat")
    clear.bits := read_beat.bits.id
  }

  // Set direct SRAM read and write reqs
  io.sram_read_req.valid := false.B
  io.sram_read_req.bits := 0.U.asTypeOf(io.sram_read_req.bits)
  io.sram_read_req.bits.axis := axis
  io.sram_read_req.bits.spans.foreach(_ := 1.U)
  io.sram_read_req.bits.to_dma := true.B
  io.sram_read_req_sram_code := req.sram_code
  io.sram_read_resp.ready := false.B

  io.sram_write_req.valid := false.B
  io.sram_write_req.bits := 0.U.asTypeOf(io.sram_write_req.bits)
  io.sram_write_req.bits.req.is_recursive.foreach(_ := false.B)
  io.sram_write_req.bits.req.axis := axis
  io.sram_write_req.bits.req.spans.foreach(_ := 1.U)
  io.sram_write_req.bits.sram_code := req.sram_code

  // Increment counters
  val all_lower_wrapped_around = is_innermost || all_wrapped_arounds(axis - 1.U)
  val should_inc = WireInit(!is_innermost && all_lower_wrapped_around && is_axis(axis_type, FiberTreeAxis.Dense))
  val outer_span_step_size = RegInit(1.U(Dma.spanBits.W)) // This is normally just 1.U, but can rarely be set higher by inner axes to speed up outer-axis iteration to avoid bubble cycles. Currently, this is just used to speed up CSR mvins
  val span_step_size = Mux(is_innermost, best_tl_req.span_step_size, outer_span_step_size).asUInt
  val last_it = WireInit(req.spans(axis) <= span_iterator +& span_step_size || use_infinite)
  val should_wrap_around = should_inc && last_it
  val repeat_axis = WireInit(false.B)
  val should_move_down_axis_level = WireInit(!is_innermost && !repeat_axis && is_axis(axis_type, FiberTreeAxis.Dense))

  when (should_inc) {
    span_iterator := span_iterator + span_step_size
    wrapped_around := should_wrap_around

    when (!is_innermost) {
      outer_span_step_size := 1.U
    }

    // Move up and down loop levels
    when (should_wrap_around) {
      span_iterator := 0.U

      when (!repeat_axis) {
        axis := axis + 1.U

        when (is_outermost) {
          assert(!req_valid || !any(compressed_axis_state_datas.flatMap(_.row_id_prefetch_reqs.map(_.requested))), "in-flight prefetch reqs still remaining")
          req_valid := false.B
          io.drop_cmds_until_dma_inst.valid := req_valid && req.drop_following_cmds_if_empty && compressed_axis_state_datas.head.tiled_outer_dim_id === 0.U
        }
      }
    }.elsewhen (should_move_down_axis_level) {
      axis := axis - 1.U
      all_wrapped_arounds.foreach(_ := false.B)
    }
  }.elsewhen(should_move_down_axis_level) {
    axis := axis - 1.U
    all_wrapped_arounds.foreach(_ := false.B)
  }

  val debug_is_wasted = WireInit(false.B)

  val ptr_chaser_first_outer_id_returned = Reg(Bool())
  val ptr_chaser_first_outer_id = Reg(UInt(metadataBits.W))
  val ptr_chaser_inner_ids_written = Reg(Bool())
  val ptr_chaser_outer_ids_written = Reg(Bool())
  val ptr_chaser_data_written = Reg(Bool())
  val ptr_chaser_first_outer_dim_id_written = RegInit(false.B)
  val ptr_chaser_threshold_point_reached = Reg(Bool())
  val ptr_chaser_threshold_point_id = Reg(UInt(log2Up(n_prefetched_ptrs).W))
  val ptr_chaser_total_nodes_moved_in = Reg(UInt(Dma.spanBits.W))
  val ptr_chaser_merger_rowlen = Reg(UInt(Dma.spanBits.W))
  val ptr_chaser_rowlen_captured = Reg(Bool())
  val ptr_chaser_last_ptr_packet_addr = Reg(UInt(Dma.addrBits.W))
  val ptr_chaser_last_ptr_packet_addr_captured = Reg(Bool())

  // Update data and config pointers for all the different possibilities of axis types
  when (is_innermost && is_axis(axis_type, FiberTreeAxis.Dense)) {
    tl_req_valids.head := true.B
    when (io.tl_reqs.head.fire) {
      should_inc := true.B
      data_base_addr := (if (beatWidthInBytes == maxReqLenInBytes && isPow2(maxReqLenInBytes)) {
        // Some manual const-prop
        align(data_base_addr, maxReqLenInBytes, up=true)
      } else {
        align(data_base_addr, smallestElemWidthInBytes) + (best_tl_req.span_step_size << logElemTWidthInBytes).asUInt
      })
    }
  }.elsewhen((nNonDenseAxes > 0).B && req_valid && (is_axis(axis_type, FiberTreeAxis.Compressed) || is_axis(axis_type, FiberTreeAxis.LinkedList))) {
    val next_free_ptr_addr = compressed_axis_state_datas(axis).original_next_free_ptr_addr
    val original_span = compressed_axis_state_datas(axis).original_span

    val outer_dim_id = compressed_axis_state_datas(axis).outer_dim_id
    val tiled_outer_dim_id = compressed_axis_state_datas(axis).tiled_outer_dim_id

    val data_offset = compressed_axis_state_datas(axis).data_offset
    val coord_offset = compressed_axis_state_datas(axis).coord_offset

    val inflight_req_id = compressed_axis_state_datas(axis).inflight_req_id

    val added_new_ll_node = compressed_axis_state_datas(axis).added_new_ll_node
    val prev_last_node_addr = compressed_axis_state_datas(axis).prev_last_node_addr
    val next_node_addr = compressed_axis_state_datas(axis).next_node_addr
    val reached_last_node = compressed_axis_state_datas(axis).reached_last_node
    val updated_rowlen = compressed_axis_state_datas(axis).updated_rowlen
    val updated_last_node_ptr = compressed_axis_state_datas(axis).updated_last_node_ptr
    val updated_next_node_ptr = compressed_axis_state_datas(axis).updated_next_node_ptr

    val requested_coord_iterated = compressed_axis_state_datas(axis).requested_coord_iterated

    val compressed_state = compressed_axis_state_datas(axis).state

    val use_ll_ptr = compressed_axis_state_datas(axis).use_ll_ptr
    val ptr_chasing_mvin = use_ll_ptr && !req.write && compressed_axis_state_datas(axis - 1.U).use_ll_ptr && req.axes(axis +& 1.U) === FiberTreeAxis.Dense
    val multiaxis_squeezed_mvout = req.write && (axis === 0.U) && (nAxes >= 2).B && !use_ll_ptr &&
      req.axes.head === FiberTreeAxis.LinkedList && ChiselUtil.all(req.axes.tail.map(_ === FiberTreeAxis.Dense)) &&
      ChiselUtil.all(req.metadata_strides(axis +& 1.U)(axis).map(_ === 0.U)) && ChiselUtil.all(req.metadata_strides_by_addr(axis +& 1.U)(axis).map(_ === 0.U)) &&
      req.metadata_strides(axis +& 2.U)(axis)(LinkedListMetadata.head_ptr_buffer_id) =/= 0.U &&
      req.spans(axis +& 1.U) === 1.U

    val _axis = axis(log2Up(nNonDenseAxes)-1,0) // '_axis' is the same as 'axis'; this line is just for a little bit of manual const-prop

    val skip_data_reads_or_writes = data_base_addr === 0.U
    val skip_inner_coord_reads_or_writes = {
      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)
      req.metadata_base_addrs(_axis)(_axis)(metadata_buffer_id) === 0.U
    }

    val (request_coords_from_sram, request_coords_only_once) = {
      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)
      val metadata_region = metadata_regions(metadata_buffer_id)

      val metadata_region_is_sram = metadata_region.is_sram || coords_only_come_from_srams.B

      val sram_is_unique = metadata_region_is_sram && !any(req.metadata_regions.zipWithIndex.flatMap { case (regions, i) =>
        regions.zipWithIndex.map { case (region, j) =>
          !(i.U === axis && j.U === metadata_buffer_id) && region.is_sram && region.index === metadata_region.index
        }
      })

      if (coords_only_requested_once) assert(metadata_region_is_sram)

      (metadata_region_is_sram, sram_is_unique || coords_only_requested_once.B)
    }
    val first_coords_request = dropFromVec(span_iterators, _axis +& 1.U).asUInt === 0.U

    def updateSpan(new_span: UInt): Unit = {
      // The loop below was originally represented by just this one line, but we've done some manual const-prop:
      //   span := new_span
      req.spans.take(nNonDenseAxes).zipWithIndex.foreach { case (span, i) =>
        when (i.U === _axis) {
          span := new_span
        }
      }
    }

    // Pointer prefetching (continued further on down to deal with Chisel last-connect semantics)
    val prefetched_ptrs = compressed_axis_state_datas(axis).prefetched_ptrs
    val should_prefetch_pointers = use_ll_ptr && any(prefetched_ptrs.map(_.valid))
    val skip_requesting_outer_id = req.write && should_prefetch_pointers && skip_data_reads_or_writes && skip_inner_coord_reads_or_writes

    val next_cycle_prefetched_ptrs = all_next_cycle_prefetched_ptrs(_axis) // It's possible for pointers to return and be popped in the same cycle. This variable helps handle those edge cases
    val prefetched_ptr_ids_after_pop = VecInit.tabulate(n_prefetched_ptrs)(_.U(log2Up(n_prefetched_ptrs+1).W))
    prefetched_ptrs := selectIds(next_cycle_prefetched_ptrs, prefetched_ptr_ids_after_pop)

    val (returning_prefetch_read_port_ids, prefetches_returning) = prefetched_ptrs.map { prefetched_ptr =>
      val result = MuxCase(nReadRespPorts.U, io.read_beats.zipWithIndex.map { case (read_beat, portId) =>
        (read_beat.valid && prefetched_ptr.waiting_to_return && read_beat.bits.id === prefetched_ptr.id) -> portId.U
      })
      (result, result < nReadRespPorts.U)
    }.unzip

    val (returning_non_ptr_prefetch_port, non_ptr_prefetch_returning) = {
      val all_prefetch_pointers = compressed_axis_state_datas.flatMap(_.prefetched_ptrs)

      val portId = MuxCase(nReadRespPorts.U, io.read_beats.zipWithIndex.map { case (read_beat, portId) =>
        (read_beat.valid && !any(all_prefetch_pointers.map(prefetch_ptr =>
          prefetch_ptr.waiting_to_return && prefetch_ptr.id === read_beat.bits.id))) -> portId.U
      })

      (io.read_beats(portId), portId < nReadRespPorts.U)
    }

    val next_cycle_updated_rowlen = WireInit(updated_rowlen)
    val next_cycle_updated_last_node_ptr = WireInit(updated_last_node_ptr)
    val next_cycle_updated_next_node_ptr = WireInit(updated_next_node_ptr)
    // Seq(next_cycle_updated_rowlen, next_cycle_updated_last_node_ptr, next_cycle_updated_next_node_ptr).foreach(dontTouch(_))

    val ending = should_inc && last_it && !repeat_axis

    when (should_prefetch_pointers && ending) {
      // Iterate faster in the outer axes
      val (ptr_can_be_popped, ptr_adds_node) = next_cycle_prefetched_ptrs.zipWithIndex.map { case (ptr, ptr_id) =>
        val earlier_packet_has_same_addr = any(prefetched_ptrs.take(ptr_id).map(packet => packet.addr === ptr.addr && packet.valid)) ||
          !skip_requesting_outer_id && ptr.addr === metadata_addrs(LinkedListMetadata.next_ptr_buffer_id)
        val adds_new_node = !ptr.packet.last_ptr_rel_offset.andR || earlier_packet_has_same_addr

        val updated_id = ptr_id.U +& !skip_requesting_outer_id

        val can_pop = ptr.valid && ptr.returned &&
          next_cycle_updated_rowlen(updated_id) &&
          next_cycle_updated_last_node_ptr(updated_id) &&
          (!adds_new_node || next_cycle_updated_next_node_ptr(updated_id))

        (can_pop, add_node_to_maximize_performance.B || adds_new_node)
      }.unzip

      prefetched_ptr_ids_after_pop.zipWithIndex.foldLeft(0.U(log2Up(n_prefetched_ptrs+1).W)) { case (running_id, (id_after_drop, ind)) =>
        val later_non_pops = ptr_can_be_popped.zipWithIndex.map { case (p,i) => !p && i.U >= running_id }
        val next_non_popped = Mux(!ChiselUtil.any(later_non_pops), n_prefetched_ptrs.U, PriorityEncoder(later_non_pops))

        id_after_drop := next_non_popped
        assert(ind.U <= next_non_popped)

        Mux(next_non_popped >= n_prefetched_ptrs.U, n_prefetched_ptrs.U, next_non_popped + 1.U)
      }

      outer_span_step_size := PopCount(ptr_can_be_popped) +& !skip_requesting_outer_id

      if (!support_ooo_ptr_mvouts) {
        val nodes_added = PopCount(ptr_can_be_popped.zip(ptr_adds_node).map(t => t._1 && t._2))
        next_free_ptr_addr := next_free_ptr_addr + nodes_added * ptrPacketWidthInBytes.U
      }
    }

    when (ending) {
      connectVecs(updated_rowlen, selectIds(next_cycle_updated_rowlen, prefetched_ptr_ids_after_pop))
      connectVecs(updated_last_node_ptr, selectIds(next_cycle_updated_last_node_ptr, prefetched_ptr_ids_after_pop))
      connectVecs(updated_next_node_ptr, selectIds(next_cycle_updated_next_node_ptr, prefetched_ptr_ids_after_pop))

      when (!vecEqualsU(prefetched_ptr_ids_after_pop, Seq.tabulate(n_prefetched_ptrs)(_.U))) {
        updated_rowlen.last := false.B
        updated_last_node_ptr.last := false.B
        updated_next_node_ptr.last := false.B

        assert(!any(Seq(next_cycle_updated_rowlen.last, next_cycle_updated_last_node_ptr.last, next_cycle_updated_next_node_ptr.last)) || PopCount(prefetched_ptr_ids_after_pop.map(_ >= n_prefetched_ptrs.U)) <= 1.U,
          "not sure what to do if the last updated-value is True, but we then scatter it to multiple update_* Vec elements simultaneously")
      }

      assert(skip_requesting_outer_id || prefetched_ptr_ids_after_pop === VecInit.tabulate(n_prefetched_ptrs)(_.U) || prefetched_ptr_ids_after_pop === VecInit.tabulate(n_prefetched_ptrs)(_.U + 1.U),
        "not sure how to handle non-oldest prefetched pointer pops when the updated_* indices don't exactly line up with the prefetched-ptr indices")
    }.otherwise {
      updated_rowlen := next_cycle_updated_rowlen
      updated_last_node_ptr := next_cycle_updated_last_node_ptr
      updated_next_node_ptr := next_cycle_updated_next_node_ptr
    }

    // Row-id prefetching
    val prefetched_row_ids = compressed_axis_state_datas(axis).prefetched_row_ids
    val prefetched_row_id_valids = compressed_axis_state_datas(axis).prefetched_row_id_valids
    val prefetched_row_id_data_requested = compressed_axis_state_datas(axis).prefetched_row_id_data_requested
    val prefetched_row_id_written = compressed_axis_state_datas(axis).prefetched_row_id_written
    val row_id_prefetch_reqs = compressed_axis_state_datas(axis).row_id_prefetch_reqs

    val should_prefetch_row_ids = !req.write && is_axis(axis_type, FiberTreeAxis.Compressed) &&
      ChiselUtil.all((0 until nAxes).map { axisId =>
        axisId.U <= axis || (is_axis(req.axes(axisId), FiberTreeAxis.Dense) &&
          req.metadata_strides_by_value(axisId)(axis)(CompressedMetadata.outer_metadata_buffer_id) === 0.U &&
          req.metadata_strides(axisId)(axis)(CompressedMetadata.outer_metadata_buffer_id) === Mux(axisId.U === axis +& 1.U, 1.U, 0.U))
      })
    assert(!(should_prefetch_pointers && should_prefetch_row_ids), "we don't yet support prefetching both row-ids and ptrs simultaneously")
    val combine_outer_id_waits = should_prefetch_row_ids && !is_outermost && span_iterators(axis + 1.U) =/= 0.U

    val head_prefetched_row_id_already_popped = compressed_state > waiting_for_next_outer_id

    val prefetched_row_ids_count = PopCount(prefetched_row_id_valids) +& row_id_prefetch_reqs.map(r => Mux(r.requested, r.size, 0.U)).reduce(_ + _)
    val num_row_ids_to_prefetch = (minOf(req.spans(axis + 1.U) - span_iterators(axis + 1.U) + 1.U, n_prefetched_row_ids.U) - prefetched_row_ids_count) -
      Mux(head_prefetched_row_id_already_popped, outer_span_step_size, 0.U)

    val next_cycle_prefetched_row_ids = all_next_cycle_prefetched_row_ids(_axis) // It's possible for row-ids to return and be popped in the same cycle. This variable helps handle those edge cases
    val next_cycle_prefetched_row_id_valids = all_next_cycle_prefetched_row_id_valids(_axis) // It's possible for row-ids to return and be popped in the same cycle. This variable helps handle those edge cases
    val next_cycle_prefetched_row_id_data_requested = WireInit(prefetched_row_id_data_requested)
    val next_cycle_prefetched_row_id_written = WireInit(prefetched_row_id_written)
    val head_prefetched_row_id_popping = WireInit(0.U(log2Up(n_prefetched_row_ids+1).W))
    prefetched_row_ids := dropFromVec(next_cycle_prefetched_row_ids, head_prefetched_row_id_popping)
    prefetched_row_id_valids := dropFromVec(next_cycle_prefetched_row_id_valids, head_prefetched_row_id_popping)
    prefetched_row_id_data_requested := dropFromVec(next_cycle_prefetched_row_id_data_requested, head_prefetched_row_id_popping)
    prefetched_row_id_written := dropFromVec(next_cycle_prefetched_row_id_written, head_prefetched_row_id_popping)
    when (head_prefetched_row_id_popping > 0.U) {
      row_id_prefetch_reqs.foreach(r => r.row_id_id := r.row_id_id - head_prefetched_row_id_popping)
    }

    // dontTouch(next_cycle_prefetched_row_id_written)
    // dontTouch(head_prefetched_row_id_popping)

    // Other misc functions
    def write_outer_id_to_sram(outer_dim_id: UInt, should_write_into_sram: Bool, n_row_ids_to_wait_for: Int): Unit = {
      io.sram_write_req.valid := should_write_into_sram && (ptr_chasing_mvin || Mux(should_prefetch_row_ids,
        ChiselUtil.all(Seq.tabulate(n_row_ids_to_wait_for) { i =>
          prefetched_row_id_valids(i) || any(io.read_beats.map(read_beat => read_beat.valid && any(row_id_prefetch_reqs.map(r => r.requested && r.row_id_id === i.U && r.tl_id === read_beat.bits.id))))
        }),
        any(io.read_beats.map(read_beat => read_beat.valid && read_beat.bits.id === inflight_req_id))))
      connectVecs(io.sram_write_req.bits.req.address, (req.sram_base_addrs zip span_iterators).map { case (x, y) => x + y })
      io.sram_write_req.bits.req.spans.foreach(_ := 1.U)
      io.sram_write_req.bits.req.iteration_strides.foreach(_ := 1.U)
      connectVecOfVecsOfVecs(io.sram_write_req.bits.req.metadata_strides, req.sram_metadata_strides)
      connectVecOfVecsOfVecs(io.sram_write_req.bits.req.metadata_strides_by_addr, req.sram_metadata_strides_by_addr)
      io.sram_write_req.bits.req.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
      io.sram_write_req.bits.req.data := DontCare
      io.sram_write_req.bits.req.data.head := outer_dim_id.asTypeOf(io.sram_write_req.bits.req.data.head)
      io.sram_write_req.bits.req.is_data := false.B
      io.sram_write_req.bits.req.axis := axis
      io.sram_write_req.bits.req.metadata_buffer_id := Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.outer_metadata_buffer_id.U, LinkedListMetadata.head_ptr_buffer_id.U)
      io.sram_write_req.bits.req.from_regfile.foreach(_ := false.B)
      io.sram_write_req.bits.req.interleave.should_push := false.B
      io.sram_write_req.bits.req.interleave.should_pop := false.B
      io.sram_write_req.bits.req.should_trail_reads := false.B
      io.sram_write_req.bits.req.should_trail_reads_coarse_grained := req.sram_should_trail_coarse
      io.sram_write_req.bits.req.reset_running_state := false.B

      when (!ptr_chasing_mvin) {
        io.read_beats.foreach(read_beat => read_beat.ready := !should_write_into_sram || io.sram_write_req.ready /* The rest of the boolean expression is more correct, but doesn't seem to be necessary because we don't have aggressively-out-of-order requests returning: &&
        ((nReadPorts == 1).B || read_beat.bits.id === inflight_req_id) */)
      }
    }

    def update_last_or_next_node_ptr(portId: UInt, compr_state: CompressedStates.Type, prefetched_ptr_packet_id: Int = -1, ptr_packet_addr_opt: Option[UInt] = None, _next_free_ptr_addr: UInt = next_free_ptr_addr, /*next_free_ptr_addr_offset: UInt = 0.U,*/ _added_new_ll_node: Bool = added_new_ll_node, _prev_last_node_addr: UInt = prev_last_node_addr): Bool = {
      // Make DMA req
      val ptr_packet_addr = ptr_packet_addr_opt.getOrElse(metadata_addrs(LinkedListMetadata.next_ptr_buffer_id))

      val metadata_addr = Mux(compr_state === update_last_node_in_ptr_packet,
        align(ptr_packet_addr, ptrPacketWidthInBytes) + (ptr_packet_t.last_node_field_location_in_bits/8).U, // the last-node field of the head pointer
        align(_prev_last_node_addr, ptrPacketWidthInBytes) + (ptr_packet_t.next_node_field_location_in_bits/8).U // the next-node field of the prev-last pointer
      )

      val addr_of_node_to_update = align(Mux(compr_state === update_last_node_in_ptr_packet, ptr_packet_addr, _prev_last_node_addr), ptrPacketWidthInBytes)
      val rel_pos_of_newest_added_node = ((align(_next_free_ptr_addr, ptrPacketWidthInBytes) - ptrPacketWidthInBytes.U) - addr_of_node_to_update) / ptrPacketWidthInBytes.U
      val relpos = reduceWidth(Mux(compr_state === update_last_node_in_ptr_packet,
        Mux(_added_new_ll_node, rel_pos_of_newest_added_node,
          0.U // If no new node was added, then the last-node is just the head-node, and the relative offset between them is 0
        ),
        rel_pos_of_newest_added_node
      ), ptr_packet_t.last_ptr_rel_offset.getWidth); require(ptr_packet_t.last_ptr_rel_offset.getWidth == ptr_packet_t.next_ptr_rel_offset.getWidth)

      mk_write_from_dma_req(metadata_addr, relpos, metadataBits, portId = portId)

      // Update prefetched pointers
      prefetched_ptrs.zip(next_cycle_prefetched_ptrs).zipWithIndex.collect { case ((prefetched_ptr, next_cycle_prefetched_ptr), prefetched_ptr_id) if prefetched_ptr_id > prefetched_ptr_packet_id =>
        when (compr_state === update_last_node_in_ptr_packet) {
          when (prefetched_ptr.valid && align(prefetched_ptr.addr, ptrPacketWidthInBytes) === align(ptr_packet_addr, ptrPacketWidthInBytes)) {
            next_cycle_prefetched_ptr.packet.last_ptr_rel_offset := relpos
            next_cycle_prefetched_ptr.last_node_updated := true.B
          }
        }
      }

      tl_req_valids(portId) := true.B
      io.tl_reqs(portId).fire
    }

    // Resetting initial values
    when (compressed_state === base_compressed_state) {
      data_offset := 0.U
      coord_offset := 0.U
    }

    // FSM for compressed and linked-list axes
    when (ptr_chasing_mvin) {
      val evaluate_if_single_dimension_may_be_too_large = req.spans(_axis+&1.U) === 1.U

      when (compressed_state === requesting_ptr) {
        // Make requests for initial ptr-packets
        prefetched_ptrs.zipWithIndex.foreach { case (prefetched_ptr, prefetched_ptr_id) =>
          when (prefetched_ptr_id.U < req.spans(_axis+&1.U)) {
            prefetched_ptr.valid := true.B
            prefetched_ptr.addr := metadata_base_addrs(_axis)(LinkedListMetadata.next_ptr_buffer_id) +& (req.metadata_strides(_axis+&1.U)(_axis)(LinkedListMetadata.next_ptr_buffer_id) * metadataBytes.U * prefetched_ptr_id.U)

            prefetched_ptr.outer_span_iterator := prefetched_ptr_id.U
            prefetched_ptr.node_iterator := 0.U
            prefetched_ptr.row_iterator := 0.U

            when (evaluate_if_single_dimension_may_be_too_large) {
              prefetched_ptr.total_nodes_moved_in := 0.U
            }
          }.otherwise {
            assert(!prefetched_ptr.valid)
          }
        }
        assert(prefetched_ptrs.size.U >= req.spans(_axis+&1.U), "we are assuming that we can allocate prefetch reqs for all the head-ptrs simultaneously")

        when (!ptr_chaser_first_outer_dim_id_written) {
          write_outer_id_to_sram(0.U, true.B, 0)
          when (io.sram_write_req.fire) {
            compressed_state := waiting_for_ptr
            ptr_chaser_first_outer_dim_id_written := true.B
          }
        }.otherwise {
          compressed_state := waiting_for_ptr
        }
      }.elsewhen (compressed_state === waiting_for_ptr) {
        // When ptr_packets return, request the next-ptr as well
        {
          val free_prefetched_ptrs = VecInit(prefetched_ptrs.map(!_.valid))
          prefetched_ptrs.zipWithIndex.foldLeft(free_prefetched_ptrs) { case (free_ptrs, (prefetched_ptr, prefetched_ptr_id)) =>
            val next_free_ptrs = WireInit(free_ptrs)

            when (prefetched_ptr.returned && !prefetched_ptr.packet.next_ptr_rel_offset.andR && !prefetched_ptr.prefetch_req_for_next_node_reserved && any(free_ptrs)
              && (evaluate_if_single_dimension_may_be_too_large || prefetched_ptr.outer_id_written_into_sram && prefetched_ptr.inner_nodes_initialized))
            {
              assert(prefetched_ptr.valid && prefetched_ptr.requested)

              val next_node_addr = prefetched_ptr.addr + (prefetched_ptr.packet.next_ptr_rel_offset * ptrPacketWidthInBytes.U)

              val free_ptr_id = PriorityEncoder(free_ptrs)
              next_free_ptrs(free_ptr_id) := false.B

              prefetched_ptrs(free_ptr_id).valid := true.B
              prefetched_ptrs(free_ptr_id).addr := next_node_addr

              when (!evaluate_if_single_dimension_may_be_too_large) {
                prefetched_ptrs(free_ptr_id).outer_id_written_into_sram := true.B
                prefetched_ptrs(free_ptr_id).inner_nodes_initialized := true.B
              }

              prefetched_ptrs(free_ptr_id).outer_span_iterator := prefetched_ptr.outer_span_iterator
              prefetched_ptrs(free_ptr_id).node_iterator := prefetched_ptr.node_iterator + 1.U
              prefetched_ptrs(free_ptr_id).row_iterator := 0.U

              prefetched_ptrs(free_ptr_id).total_nodes_moved_in := {
                val elems_moved_in = prefetched_ptr.packet.rowlen
                val nodes_moved_in = Mux(elems_moved_in === 0.U, 1.U, align(elems_moved_in, ll_node_size, up=true) / ll_node_size.U)
                prefetched_ptr.total_nodes_moved_in +& nodes_moved_in
              }

              prefetched_ptr.prefetch_req_for_next_node_reserved := true.B
            }

            next_free_ptrs
          }
        }

        // Capture the last-packet-addr to help with too-large mvins
        {
          val prefetched_ptr = prefetched_ptrs.head
          when (prefetched_ptr.valid && prefetched_ptr.returned && !ptr_chaser_last_ptr_packet_addr_captured) {
            ptr_chaser_last_ptr_packet_addr_captured := true.B
            ptr_chaser_last_ptr_packet_addr := prefetched_ptr.addr +& prefetched_ptr.packet.last_ptr_rel_offset
          }
        }

        // Identify the current threshold point
        val must_wait_to_find_threshold_point = evaluate_if_single_dimension_may_be_too_large && !ptr_chaser_threshold_point_reached
        when (evaluate_if_single_dimension_may_be_too_large) {
          prefetched_ptrs.zipWithIndex.foreach { case (prefetched_ptr, prefetched_ptr_id) =>
            val elems_moved_in = prefetched_ptr.packet.rowlen
            val nodes_moved_in = Mux(elems_moved_in === 0.U, 1.U, align(elems_moved_in, ll_node_size, up=true) / ll_node_size.U)

            val crosses_threshold: Bool = prefetched_ptr.total_nodes_moved_in - ptr_chaser_total_nodes_moved_in <= total_sram_nodes.U &&
              prefetched_ptr.total_nodes_moved_in - ptr_chaser_total_nodes_moved_in + nodes_moved_in >= total_sram_nodes.U

            val is_final_ptr: Bool = prefetched_ptr.packet.next_ptr_rel_offset.andR &&
              prefetched_ptr.total_nodes_moved_in - ptr_chaser_total_nodes_moved_in + nodes_moved_in <= total_sram_nodes.U

            when (prefetched_ptr.valid && prefetched_ptr.returned && (crosses_threshold || is_final_ptr)) {
              ptr_chaser_threshold_point_reached := true.B
              ptr_chaser_threshold_point_id := prefetched_ptr_id.U

              ptr_chaser_total_nodes_moved_in := prefetched_ptr.total_nodes_moved_in +& nodes_moved_in
            }
          }
        }

        // When ptr_packets return, then write the CSR outer indices into the SRAM
        val outer_ids_being_written = {
          val result = Wire(Bool())

          when (evaluate_if_single_dimension_may_be_too_large) {
            val threshold_ptr = prefetched_ptrs(ptr_chaser_threshold_point_id)

            val can_write = ptr_chaser_threshold_point_reached && !threshold_ptr.outer_id_written_into_sram

            when (can_write && !io.sram_read_busies.head) {
              val ll_len = (threshold_ptr.node_iterator - span_iterators(1)) +& 1.U
              assert(ll_len >= 2.U)
              assert(threshold_ptr.node_iterator >= span_iterators(1))
              assert(ll_len +& 2.U <= total_sram_heads.U)

              write_outer_id_to_sram(ll_len +& 2.U, true.B, 0)

              io.sram_write_req.bits.req.spans(_axis) := 1.U

              io.sram_write_req.bits.req.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides(_axis+&1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
              io.sram_write_req.bits.req.metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
              assert(io.sram_write_req.bits.req.axis === _axis)

              io.sram_write_req.bits.req.address(_axis +& 1.U) := 0.U
              assert(req.spans(_axis+&2.U) === 1.U)

              when (io.sram_write_req.fire) {
                span_iterators(1) := threshold_ptr.node_iterator
                threshold_ptr.outer_id_written_into_sram := true.B
                prefetched_ptrs.foreach { p =>
                  when (p.valid && p.node_iterator <= threshold_ptr.node_iterator) {
                    assert(p.returned)
                    p.outer_id_written_into_sram := true.B
                  }
                }
              }
            }

            result := can_write
          }.otherwise {
            val should_write_outer_ids = any(prefetched_ptrs.map(p => p.valid && !p.outer_id_written_into_sram))

            val ptr_to_write_into_sram_id = PriorityEncoder(prefetched_ptrs.map(p => p.valid && !p.outer_id_written_into_sram))

            val can_write = {
              val p = prefetched_ptrs(ptr_to_write_into_sram_id)
              p.valid && p.returned && should_write_outer_ids && !p.outer_id_written_into_sram
            }

            when (can_write && !io.sram_read_busies.head && !must_wait_to_find_threshold_point) {
              val n_ptrs_to_write = popCountWhileTrue(
                prefetched_ptrs.map(p => p.valid && p.returned && !p.outer_id_written_into_sram),
                ptr_to_write_into_sram_id
              )
              assert(n_ptrs_to_write > 0.U)

              write_outer_id_to_sram(tiled_outer_dim_id, true.B, 0)

              val new_outer_dim_id = prefetched_ptrs.zipWithIndex.foldLeft(tiled_outer_dim_id) { case (outer_dim_id_to_write, (prefetched_ptr, prefetched_ptr_id)) =>
                // val next_outer_dim_id_to_write = WireInit(outer_dim_id_to_write) // This line caused FIRRTL to stall on the 'infer-widths' widths step, so we replace it with the equivalent code below
                val next_outer_dim_id_to_write = Wire(getChiselType(tiled_outer_dim_id))
                next_outer_dim_id_to_write := outer_dim_id_to_write

                when (prefetched_ptr_id.U >= ptr_to_write_into_sram_id && prefetched_ptr_id.U < ptr_to_write_into_sram_id +& n_ptrs_to_write) {
                  val ll_len = Mux(prefetched_ptr.packet.ll_len.andR, 0.U, prefetched_ptr.packet.ll_len)
                  val extended_len_for_recursive_ops = MuxCase(ll_len +& 2.U, Seq(
                    (ll_len <= 1.U) -> 1.U,
                  ))

                  val id = prefetched_ptr_id.U - ptr_to_write_into_sram_id
                  io.sram_write_req.bits.req.data(id) := (outer_dim_id_to_write + extended_len_for_recursive_ops).asTypeOf(io.sram_write_req.bits.req.data(id))

                  next_outer_dim_id_to_write := outer_dim_id_to_write +& extended_len_for_recursive_ops
                }
                next_outer_dim_id_to_write
              }

              io.sram_write_req.bits.req.spans(_axis) := n_ptrs_to_write

              io.sram_write_req.bits.req.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides(_axis +& 1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
              io.sram_write_req.bits.req.metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
              assert(io.sram_write_req.bits.req.axis === _axis)

              io.sram_write_req.bits.req.address(_axis +& 1.U) := span_iterators(_axis +& 1.U) + 1.U

              when (io.sram_write_req.fire) {
                tiled_outer_dim_id := new_outer_dim_id
                span_iterators(_axis +& 1.U) := span_iterators(_axis +& 1.U) + n_ptrs_to_write
                assert(span_iterators(_axis +& 1.U) +& n_ptrs_to_write <= req.spans(_axis +& 1.U))

                prefetched_ptrs.zipWithIndex.foreach { case (prefetched_ptr, prefetched_ptr_id) =>
                  when (prefetched_ptr_id.U >= ptr_to_write_into_sram_id && (prefetched_ptr_id.U - ptr_to_write_into_sram_id) < n_ptrs_to_write) {
                    prefetched_ptr.outer_id_written_into_sram := true.B
                  }
                }
              }
            }

            result := can_write
          }

          result
        }

        // Initialize inner-axis head ptrs
        when (!outer_ids_being_written && !io.sram_read_busies.head && !must_wait_to_find_threshold_point) {
          when (evaluate_if_single_dimension_may_be_too_large) {
            val threshold_ptr = prefetched_ptrs(ptr_chaser_threshold_point_id)

            val can_write = threshold_ptr.valid && threshold_ptr.returned && !threshold_ptr.inner_nodes_initialized && threshold_ptr.outer_id_written_into_sram

            when (can_write) {
              io.sram_write_req.valid := true.B
              io.sram_write_req.bits.req.address.foreach(_ := 0.U)
              io.sram_write_req.bits.req.spans.foreach(_ := 1.U)
              io.sram_write_req.bits.req.spans(_axis) := maxVal(io.sram_write_req.bits.req.spans(_axis))
              io.sram_write_req.bits.req.iteration_strides.foreach(_ := 1.U)
              io.sram_write_req.bits.req.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
              io.sram_write_req.bits.req.is_data := false.B
              io.sram_write_req.bits.req.axis := 0.U
              io.sram_write_req.bits.req.metadata_buffer_id := LinkedListMetadata.head_ptr_buffer_id.U
              io.sram_write_req.bits.req.from_regfile.foreach(_ := false.B)
              io.sram_write_req.bits.req.interleave.should_push := false.B
              io.sram_write_req.bits.req.interleave.should_pop := false.B
              io.sram_write_req.bits.req.should_trail_reads := false.B
              io.sram_write_req.bits.req.should_trail_reads_coarse_grained := req.sram_should_trail_coarse
              io.sram_write_req.bits.req.reset_running_state := false.B

              io.sram_write_req.bits.req.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides(_axis+&1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
              io.sram_write_req.bits.req.metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
              io.sram_write_req.bits.req.metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

              when (io.sram_write_req.fire) {
                threshold_ptr.inner_nodes_initialized := true.B
                prefetched_ptrs.foreach { p =>
                  when (p.valid && p.node_iterator <= threshold_ptr.node_iterator) {
                    assert(p.returned)
                    p.inner_nodes_initialized := true.B
                  }
                }
              }
            }
          }.otherwise {
            val should_initialize_inner_head_ptrs = any(prefetched_ptrs.map(p => p.valid && !p.inner_nodes_initialized))

            val ptr_to_write_into_sram_id = PriorityEncoder(prefetched_ptrs.map(p => p.valid && !p.inner_nodes_initialized))

            val base_ptr = prefetched_ptrs(ptr_to_write_into_sram_id)

            val can_write = base_ptr.valid && base_ptr.returned && should_initialize_inner_head_ptrs && !base_ptr.inner_nodes_initialized

            when (can_write) {
              val n_ptrs_to_write = popCountWhileTrue(
                prefetched_ptrs.map(p => p.valid && p.returned && !p.inner_nodes_initialized),
                ptr_to_write_into_sram_id
              )
              assert(n_ptrs_to_write > 0.U)

              io.sram_write_req.valid := true.B
              io.sram_write_req.bits.req.address.foreach(_ := 0.U)
              io.sram_write_req.bits.req.address(_axis+&1.U) := base_ptr.outer_span_iterator
              io.sram_write_req.bits.req.spans.foreach(_ := 1.U)
              io.sram_write_req.bits.req.spans(_axis) := maxVal(io.sram_write_req.bits.req.spans(_axis))
              io.sram_write_req.bits.req.spans(_axis+&1.U) := n_ptrs_to_write
              io.sram_write_req.bits.req.iteration_strides.foreach(_ := 1.U)
              io.sram_write_req.bits.req.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
              io.sram_write_req.bits.req.is_data := false.B
              io.sram_write_req.bits.req.axis := 0.U
              io.sram_write_req.bits.req.metadata_buffer_id := LinkedListMetadata.head_ptr_buffer_id.U
              io.sram_write_req.bits.req.from_regfile.foreach(_ := false.B)
              io.sram_write_req.bits.req.interleave.should_push := false.B
              io.sram_write_req.bits.req.interleave.should_pop := false.B
              io.sram_write_req.bits.req.should_trail_reads := false.B
              io.sram_write_req.bits.req.should_trail_reads_coarse_grained := req.sram_should_trail_coarse
              io.sram_write_req.bits.req.reset_running_state := false.B

              io.sram_write_req.bits.req.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
              io.sram_write_req.bits.req.metadata_strides(_axis+&1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
              io.sram_write_req.bits.req.metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
              io.sram_write_req.bits.req.metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

              when (io.sram_write_req.fire) {
                prefetched_ptrs.zipWithIndex.foreach { case (prefetched_ptr, prefetched_ptr_id) =>
                  when (prefetched_ptr_id.U >= ptr_to_write_into_sram_id && (prefetched_ptr_id.U - ptr_to_write_into_sram_id) < n_ptrs_to_write) {
                    prefetched_ptr.inner_nodes_initialized := true.B
                  }
                }
              }
            }
          }
        }

        // Make data-and-coord requests for prefetched-ptrs
        when (!io.sram_read_busies.head && !must_wait_to_find_threshold_point) {
          val should_have_data_be_moved_in_from_dram = prefetched_ptrs.map { p =>
            p.valid && p.returned && p.packet.rowlen > 0.U && !p.packet.rowlen.andR && p.outer_id_written_into_sram && p.inner_nodes_initialized
          }
          val next_should_have_data_be_moved_in_from_dram = VecInit(should_have_data_be_moved_in_from_dram)

          when (any(should_have_data_be_moved_in_from_dram)) {
            val ptr_to_mvin_data_from_id = PriorityEncoder(should_have_data_be_moved_in_from_dram)
            next_should_have_data_be_moved_in_from_dram(ptr_to_mvin_data_from_id) := false.B

            val prefetched_ptr = prefetched_ptrs(ptr_to_mvin_data_from_id)

            tl_req_addr := data_base_addr +& (prefetched_ptr.packet.data_and_coord_addr_offset * (metadataBytes * 2).U)
            tl_req_len := prefetched_ptr.packet.rowlen * 2.U
            tl_req_valids.head := true.B

            io.tl_reqs.head.bits.tableEntry.iterator_values.head := prefetched_ptr.row_iterator * 2.U
            io.tl_reqs.head.bits.tableEntry.iterator_values(_axis) := prefetched_ptr.node_iterator
            io.tl_reqs.head.bits.tableEntry.iterator_values(_axis+&1.U) := prefetched_ptr.outer_span_iterator

            io.tl_reqs.head.bits.tableEntry.sram_metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

            when (io.tl_reqs.head.fire) {
              val len = io.tl_reqs.head.bits.tableEntry.lens.head / 2.U; assert(io.tl_reqs.head.bits.tableEntry.lens.head % 2.U === 0.U)
              prefetched_ptr.packet.data_and_coord_addr_offset := prefetched_ptr.packet.data_and_coord_addr_offset +& len
              prefetched_ptr.packet.rowlen := prefetched_ptr.packet.rowlen - len; assert(prefetched_ptr.packet.rowlen >= len)
              prefetched_ptr.row_iterator := prefetched_ptr.row_iterator + len
            }
          }

          if (maxReqLenInBytes == beatWidthInBytes)
            (1 until nTlReqPorts).foldLeft(next_should_have_data_be_moved_in_from_dram) { (should_mvin_data, tl_req_port_id) =>
              val next_should_mvin_data = WireInit(should_mvin_data)

              when (any(should_mvin_data)) {
                val ptr_to_mvin_data_from_id = PriorityEncoder(should_mvin_data)
                next_should_mvin_data(ptr_to_mvin_data_from_id) := false.B

                val prefetched_ptr = prefetched_ptrs(ptr_to_mvin_data_from_id)

                val addr = data_base_addr +& (prefetched_ptr.packet.data_and_coord_addr_offset * (metadataBytes * 2).U)

                tl_req_valids(tl_req_port_id) := true.B

                io.tl_reqs(tl_req_port_id).bits := generate_tl_req(addr, prefetched_ptr.packet.rowlen * 2.U, logBeatWidthBytes)

                io.tl_reqs(tl_req_port_id).bits.id := io.inflight_alloc_ids(tl_req_port_id)

                io.tl_reqs(tl_req_port_id).bits.tableEntry.iterator_values(_axis-1.U) := prefetched_ptr.row_iterator * 2.U
                io.tl_reqs(tl_req_port_id).bits.tableEntry.iterator_values(_axis) := prefetched_ptr.node_iterator
                io.tl_reqs(tl_req_port_id).bits.tableEntry.iterator_values(_axis+&1.U) := prefetched_ptr.outer_span_iterator

                io.tl_reqs(tl_req_port_id).bits.tableEntry.sram_metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

                when (io.tl_reqs(tl_req_port_id).fire) {
                  val len = io.tl_reqs(tl_req_port_id).bits.tableEntry.lens.head / 2.U; assert(io.tl_reqs(tl_req_port_id).bits.tableEntry.lens.head % 2.U === 0.U)
                  prefetched_ptr.packet.data_and_coord_addr_offset := prefetched_ptr.packet.data_and_coord_addr_offset +& len
                  prefetched_ptr.packet.rowlen := prefetched_ptr.packet.rowlen - len; assert(prefetched_ptr.packet.rowlen >= len)
                  prefetched_ptr.row_iterator := prefetched_ptr.row_iterator + len
                }
              }

              next_should_mvin_data
            }
        }

        // Clear out entries for prefetched-ptr-packets which have no more use
        {
          prefetched_ptrs.zipWithIndex.foreach { case (prefetched_ptr, prefetched_ptr_id) =>
            when (prefetched_ptr.valid && prefetched_ptr.returned && prefetched_ptr.outer_id_written_into_sram && prefetched_ptr.inner_nodes_initialized && (prefetched_ptr.packet.rowlen === 0.U || prefetched_ptr.packet.rowlen.andR) && (prefetched_ptr.prefetch_req_for_next_node_reserved || prefetched_ptr.packet.next_ptr_rel_offset.andR)) {
              prefetched_ptr.reset()
            }
          }
        }

        when (!any(prefetched_ptrs.map(_.valid))) {
          // End the DMA request when everything has concluded
          req_valid := false.B
        }.elsewhen (evaluate_if_single_dimension_may_be_too_large && !must_wait_to_find_threshold_point) {
          // Move to next state for too-large mvins
          val threshold_ptr = prefetched_ptrs(ptr_chaser_threshold_point_id)
          val all_reqs_issued: Bool = ChiselUtil.all(prefetched_ptrs.map(p => !p.valid || p.node_iterator > threshold_ptr.node_iterator))

          when (all_reqs_issued) {
            compressed_state := requesting_next_ptr
          }
        }
      }.elsewhen (compressed_state === requesting_next_ptr) {
        // Wait for SRAM to finish all mvins
        assert(evaluate_if_single_dimension_may_be_too_large)

        val must_wait_for_mvins_to_complete = io.sram_read_busies.head || io.sram_write_busies.head || io.sram_write_from_dram_busies.head

        when (!must_wait_for_mvins_to_complete) {
          compressed_state := waiting_for_next_ptr
        }
      }.elsewhen (compressed_state === waiting_for_next_ptr) {
        // Make mergeMerged write req
        assert(evaluate_if_single_dimension_may_be_too_large)

        io.sram_write_req.valid := true.B

        io.sram_write_req.bits.req.address.foreach(_ := 0.U)
        io.sram_write_req.bits.req.spans.foreach(_ := 1.U)
        io.sram_write_req.bits.req.spans(_axis-1.U) := maxVal(io.sram_write_req.bits.req.spans(0))
        io.sram_write_req.bits.req.spans(_axis) := maxVal(io.sram_write_req.bits.req.spans(1))
        io.sram_write_req.bits.req.iteration_strides.foreach(_ := 1.U)
        io.sram_write_req.bits.req.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
        io.sram_write_req.bits.req.is_data := true.B
        io.sram_write_req.bits.req.axis := 0.U
        io.sram_write_req.bits.req.from_regfile.foreach(_ := false.B)
        io.sram_write_req.bits.req.from_regfile.head := true.B
        io.sram_write_req.bits.req.interleave.should_push := false.B
        io.sram_write_req.bits.req.interleave.should_pop := false.B
        io.sram_write_req.bits.req.should_trail_reads := false.B
        io.sram_write_req.bits.req.should_trail_reads_coarse_grained := false.B
        io.sram_write_req.bits.req.reset_running_state := false.B
        io.sram_write_req.bits.req.from_regfile_last_axis := 1.U
        io.sram_write_req.bits.req.from_regfile_last_axis_log_size := 0.U
        io.sram_write_req.bits.req.is_recursive(_axis) := true.B

        io.sram_write_req.bits.req.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
        io.sram_write_req.bits.req.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
        io.sram_write_req.bits.req.metadata_strides(_axis+&1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
        io.sram_write_req.bits.req.metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
        io.sram_write_req.bits.req.metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

        when (io.sram_write_req.fire) {
          compressed_state := waiting_for_lower_axes_to_wrap_around__next_ptr
        }
      }.elsewhen (compressed_state === waiting_for_lower_axes_to_wrap_around__next_ptr) {
        // Make mergeScattered read req
        assert(evaluate_if_single_dimension_may_be_too_large)

        io.sram_read_req.valid := true.B

        io.sram_read_req.bits.spans.foreach(_ := 1.U)
        io.sram_read_req.bits.spans.take(2).foreach(_ := maxVal(io.sram_read_req.bits.spans.head))
        io.sram_read_req.bits.to_regfile_last_axis := 1.U
        io.sram_read_req.bits.to_dma := false.B
        io.sram_read_req.bits.is_recursive(_axis) := true.B
        io.sram_read_req.bits.should_trail_writes := true.B

        io.sram_read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
        io.sram_read_req.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
        io.sram_read_req.bits.metadata_strides(_axis+&1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
        io.sram_read_req.bits.metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
        io.sram_read_req.bits.metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

        when (io.sram_read_req.fire) {
          compressed_state := writing_outer_id_to_dram
        }
      }.elsewhen (compressed_state === writing_outer_id_to_dram) {
        // Make mvout req (interleaved) to temporary data buffer
        assert(evaluate_if_single_dimension_may_be_too_large)

        val overflow_data_and_coords_addr = metadata_base_addrs(_axis-1.U)(LinkedListMetadata.coord_buffer_id)

        tl_req_valids.head := true.B
        tl_req_addr := overflow_data_and_coords_addr
        use_infinite := true.B

        io.tl_reqs.head.bits.tableEntry.is_data := true.B
        io.tl_reqs.head.bits.tableEntry.metadata_buffer_id := LinkedListMetadata.coord_buffer_id.U

        io.tl_reqs.head.bits.tableEntry.lens(_axis +& 2.U) := maxVal(io.tl_reqs.head.bits.tableEntry.lens(_axis +& 2.U))

        io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+1.U)(_axis)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
        io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis)(_axis)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
        io.tl_reqs.head.bits.tableEntry.sram_metadata_strides_by_addr(_axis)(_axis-1.U)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

        io.tl_reqs.head.bits.tableEntry.use_running_offset := false.B

        io.tl_reqs.head.bits.tableEntry.sram_should_trail := true.B

        ptr_chaser_rowlen_captured := false.B

        when (io.tl_reqs.head.fire) {
          compressed_state := waiting_for_outer_id_write
        }
      }.elsewhen (compressed_state === waiting_for_outer_id_write) {
        // Update the existing ptr-packets since we now have a new one created
        assert(evaluate_if_single_dimension_may_be_too_large)

        val new_ptr_packet_addr = metadata_base_addrs(0)(LinkedListMetadata.next_ptr_buffer_id)
        val rel_offset = new_ptr_packet_addr - ptr_chaser_last_ptr_packet_addr; assert(new_ptr_packet_addr > ptr_chaser_last_ptr_packet_addr)

        // Update any pointers that are already in the table currently with new relative-next-offset
        val last_ptr_already_in_table = WireInit(false.B)
        prefetched_ptrs.foreach { prefetched_ptr =>
          when (prefetched_ptr.valid && prefetched_ptr.addr === ptr_chaser_last_ptr_packet_addr) {
            prefetched_ptr.packet.next_ptr_rel_offset := rel_offset
            prefetched_ptr.next_node_updated := true.B
            last_ptr_already_in_table := true.B
          }
        }

        // Write the new relative offset to the last-ptr
        when (!last_ptr_already_in_table) {
          mk_write_from_dma_req(ptr_chaser_last_ptr_packet_addr, rel_offset, metadataBits)
          tl_req_valids.head := true.B
        }

        // Account for the possibility that the axis-spans return earlier-than expected over here
        when (io.snoop_write_beat_axis_span.valid) {
          ptr_chaser_merger_rowlen := io.snoop_write_beat_axis_span.bits
          ptr_chaser_rowlen_captured := true.B
        }

        when (last_ptr_already_in_table || io.tl_reqs.head.fire) {
          ptr_chaser_last_ptr_packet_addr := new_ptr_packet_addr

          compressed_state := Mux(ptr_chaser_rowlen_captured || io.snoop_write_beat_axis_span.valid, waiting_for_outer_id, requesting_outer_id)
        }
      }.elsewhen (compressed_state === requesting_outer_id) {
        // Wait for interleaved mvout req to return
        assert(evaluate_if_single_dimension_may_be_too_large)

        when (io.snoop_write_beat_axis_span.valid) {
          ptr_chaser_merger_rowlen := io.snoop_write_beat_axis_span.bits
          compressed_state := waiting_for_outer_id
        }
      }.elsewhen (compressed_state === waiting_for_outer_id) {
        // Write out the rowlen and data-and-coord-offset to the new ptr-packet
        assert(evaluate_if_single_dimension_may_be_too_large)

        val overflow_data_and_coords_addr = metadata_base_addrs(_axis-1.U)(LinkedListMetadata.coord_buffer_id)

        val new_ptr_packet_addr = metadata_base_addrs(_axis-1.U)(LinkedListMetadata.next_ptr_buffer_id)

        val new_ptr_packet = Wire(new PtrPacket)
        new_ptr_packet.rowlen := ptr_chaser_merger_rowlen / 2.U
        new_ptr_packet.next_ptr_rel_offset := maxVal(new_ptr_packet.next_ptr_rel_offset)
        new_ptr_packet.data_and_coord_addr_offset := overflow_data_and_coords_addr
        new_ptr_packet.last_ptr_rel_offset := 0.U
        new_ptr_packet.ll_len := 0.U

        mk_write_from_dma_req(new_ptr_packet_addr, new_ptr_packet.asUInt, ptrPacketWidthInBits)

        tl_req_valids.head := true.B
        when (io.tl_reqs.head.fire) {
          overflow_data_and_coords_addr := overflow_data_and_coords_addr +& ptr_chaser_merger_rowlen
          new_ptr_packet_addr := new_ptr_packet_addr +& ptrPacketWidthInBytes.U
          compressed_state := waiting_for_ptr // Back to beginning
        }
      }
    }.elsewhen (multiaxis_squeezed_mvout) {
      {
        val data_addr = WireInit(data_base_addr)
        val coord_addr = WireInit(metadata_addrs(CompressedMetadata.inner_metadata_buffer_id))

        val wrote_or_writing_outer_ids = WireInit(ptr_chaser_outer_ids_written)
        val wrote_or_writing_inner_ids = WireInit(ptr_chaser_inner_ids_written)
        val wrote_or_writing_data = WireInit(ptr_chaser_data_written)

        when (!ptr_chaser_inner_ids_written) {
          tl_req_valids.head := true.B
          tl_req_addr := coord_addr
          use_infinite := true.B

          io.tl_reqs.head.bits.tableEntry.is_data := false.B
          io.tl_reqs.head.bits.tableEntry.metadata_buffer_id := CompressedMetadata.inner_metadata_buffer_id.U

          io.tl_reqs.head.bits.tableEntry.lens(_axis +& 2.U) := req.spans(_axis +& 2.U)

          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+&2.U)(_axis+&1.U)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+&1.U)(_axis+&1.U)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides_by_addr(_axis+&1.U)(_axis)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

          io.tl_reqs.head.bits.tableEntry.use_running_offset := true.B
          io.tl_reqs.head.bits.multiaxis_mvout := true.B

          io.tl_reqs.head.bits.tableEntry.sram_should_trail := true.B

          when (io.tl_reqs.head.fire) {
            ptr_chaser_inner_ids_written := true.B
            wrote_or_writing_inner_ids := true.B
          }
        }

        when (ptr_chaser_inner_ids_written && !ptr_chaser_data_written) {
          tl_req_valids.head := true.B
          tl_req_addr := data_addr
          use_infinite := true.B

          io.tl_reqs.head.bits.tableEntry.is_data := true.B

          io.tl_reqs.head.bits.tableEntry.lens(_axis+&2.U) := req.spans(_axis+&2.U)

          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+&2.U)(_axis+&1.U)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+&1.U)(_axis+&1.U)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides_by_addr(_axis+&1.U)(_axis)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

          io.tl_reqs.head.bits.tableEntry.use_running_offset := true.B
          io.tl_reqs.head.bits.multiaxis_mvout := true.B

          when (io.tl_reqs.head.fire) {
            ptr_chaser_data_written := true.B
            wrote_or_writing_data := true.B
          }
        }

        when (ptr_chaser_data_written && !ptr_chaser_outer_ids_written) {
          tl_req_valids.head := true.B

          tl_req_valids.head := true.B
          tl_req_addr := metadata_addrs(CompressedMetadata.outer_metadata_buffer_id) +& metadataBytes.U
          tl_req_len := req.spans(_axis+&2.U)

          io.tl_reqs.head.bits.tableEntry.is_data := false.B
          io.tl_reqs.head.bits.tableEntry.metadata_buffer_id := CompressedMetadata.outer_metadata_buffer_id.U

          io.tl_reqs.head.bits.tableEntry.lens.foreach(_ := 1.U)
          io.tl_reqs.head.bits.tableEntry.lens.head := req.spans(_axis+&2.U)

          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+&2.U)(_axis+&1.U)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides(_axis+&1.U)(_axis+&1.U)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
          io.tl_reqs.head.bits.tableEntry.sram_metadata_strides_by_addr(_axis+&1.U)(_axis)(LinkedListMetadata.head_ptr_buffer_id) := 1.U

          io.tl_reqs.head.bits.multiaxis_mvout := true.B

          when (io.tl_reqs.head.fire) {
            ptr_chaser_outer_ids_written := true.B
            wrote_or_writing_outer_ids := true.B
          }
        }

        // End the request
        when (wrote_or_writing_outer_ids && wrote_or_writing_data && wrote_or_writing_inner_ids) {
          assert(ptr_chaser_inner_ids_written)
          req_valid := false.B
        }
      }
    }.elsewhen (compressed_state === reset_sram) {
      io.sram_read_req.valid := req.write && is_innermost && use_ll_ptr
      io.sram_read_req.bits.reset_running_state := true.B

      when (io.sram_read_req.fire || !io.sram_read_req.valid) {
        compressed_state := requesting_ptr
      }
    }.elsewhen (compressed_state === requesting_ptr && (!use_ll_ptr || !should_prefetch_pointers)) {
      val metadata_buffer_id = LinkedListMetadata.next_ptr_buffer_id
      val metadata_addr = metadata_addrs(metadata_buffer_id)
      mk_read_into_dma_req(metadata_addr, metadata_buffer_id = metadata_buffer_id.U /* TODO we only set the 'metadata_buffer_id' here to help the unit test, we don't actually need it for end-to-end tests */)

      assert((beatWidthInBits >= ptrPacketWidthInBits).B &&
        metadata_addr.asUInt % ptrPacketWidthInBytes.U === 0.U, s"right now, we try to read the full ptr-packet in a single beat | beatWidthInBits = $beatWidthInBits | packetWidth = ${ptrPacketWidthInBits}")

      debug_is_wasted := !use_ll_ptr || should_prefetch_pointers

      tl_req_valids.head := use_ll_ptr && !should_prefetch_pointers
      when ((tl_req_valids.head && io.tl_reqs.head.fire) || should_prefetch_pointers) {
        compressed_state := waiting_for_ptr
      }.elsewhen(!tl_req_valids.head) {
        compressed_state := writing_outer_id_to_dram
      }
    }.elsewhen (compressed_state === waiting_for_ptr || compressed_state === requesting_ptr) {
      val ptr_packet = Mux(!should_prefetch_pointers || prefetches_returning.head,
        {
          val portId = Mux(should_prefetch_pointers, returning_prefetch_read_port_ids.head, non_ptr_prefetch_returning)
          val read_beat = io.read_beats(portId)
          val offset = align(read_beat.bits.table_entry.offset, ptrPacketWidthInBytes)
          (read_beat.bits.data >> (offset * 8.U)).asTypeOf(ptr_packet_t)
        },
        prefetched_ptrs.head.packet
      )
      val ptr_returned = Mux(should_prefetch_pointers, prefetches_returning.head || prefetched_ptrs.head.returned, non_ptr_prefetch_returning)

      io.read_beats.foreach(_.ready := true.B)
      when (ptr_returned) {
        val last_ptr_rel_offset = Mux(should_prefetch_pointers && prefetched_ptrs.head.last_node_updated, prefetched_ptrs.head.packet.last_ptr_rel_offset, ptr_packet.last_ptr_rel_offset)

        val nonempty_node = !last_ptr_rel_offset.andR || should_prefetch_pointers && prefetched_ptrs.head.last_node_updated
        val add_new_node = req.write && nonempty_node

        added_new_ll_node := add_new_node
        prev_last_node_addr := metadata_addrs(LinkedListMetadata.next_ptr_buffer_id) + (last_ptr_rel_offset * ptrPacketWidthInBytes.U)

        // These two variables should only be used when doing DRAM -> SRAM reads
        next_node_addr := metadata_addrs(LinkedListMetadata.next_ptr_buffer_id) + (ptr_packet.next_ptr_rel_offset * ptrPacketWidthInBytes.U)
        reached_last_node := ptr_packet.next_ptr_rel_offset.andR

        metadata_addrs(LinkedListMetadata.head_ptr_buffer_id) := Mux(add_new_node, next_free_ptr_addr, metadata_addrs(LinkedListMetadata.next_ptr_buffer_id)) // The rowlen is written directly into the lowest-significant ptr-packet field
        when (add_node_to_maximize_performance.B || add_new_node) {
          next_free_ptr_addr := next_free_ptr_addr + ptrPacketWidthInBytes.U
          assert(!support_ooo_ptr_mvouts.B, "when ptrs return out-of-order, then outer axes are supposed to be responsible for updated the next_free_ptr_addr")
        }

        val is_iterating_across_ptrs = !req.write && !is_innermost

        when (is_iterating_across_ptrs) {
          should_move_down_axis_level := true.B

          metadata_base_addrs.zip(req.metadata_base_addrs(axis - 1.U)).zip(metadata_strides_by_value).foreach { case ((addrs, inner_axis_addrs), strides_by_value) =>
            addrs.zip(inner_axis_addrs).zip(strides_by_value).foreach { case ((addr, inner_axis_addr), stride_by_value) =>
              // "Stride_by_value" has a really tortured definition here, to be honest. In this state, we are basically
              // just using it as a switch to determine whether or not the 'next_ptr' address should be directly passed
              // down to the lower axis
              inner_axis_addr := Mux(stride_by_value(0), metadata_addrs(LinkedListMetadata.next_ptr_buffer_id), addr)
            }
          }
          req.data_base_addrs(axis - 1.U) := data_base_addr
        }

        when (should_prefetch_pointers) {
          prefetched_ptr_ids_after_pop := (1 to n_prefetched_ptrs).map(_.U)
        }

        compressed_state := Mux(is_iterating_across_ptrs, waiting_for_lower_axes_to_wrap_around__next_ptr,
          Mux(is_innermost,
            Mux(skip_inner_coord_reads_or_writes, writing_next_outer_id_to_dram, writing_inner_ids_to_dram_or_sram),
            Mux(request_coords_from_sram, Mux(request_coords_only_once && !first_coords_request, waiting_for_coord, requesting_coord),
              requesting_outer_id)))
      }
    }.elsewhen (compressed_state === requesting_next_ptr) {
      val metadata_addr = next_node_addr
      mk_read_into_dma_req(metadata_addr, metadata_buffer_id = LinkedListMetadata.next_ptr_buffer_id.U /* TODO we only set the 'metadata_buffer_id' here to help the unit test, we don't actually need it for end-to-end tests */)

      assert((beatWidthInBits >= ptrPacketWidthInBits).B &&
        metadata_addr.asUInt % ptrPacketWidthInBytes.U === 0.U, s"right now, we try to read the full ptr-packet in a single beat | beatWidthInBits = $beatWidthInBits | packetWidth = $ptrPacketWidthInBits")

      tl_req_valids.head := true.B
      when (tl_req_valids.head && io.tl_reqs.head.fire) {
        compressed_state := waiting_for_next_ptr
      }
    }.elsewhen (compressed_state === waiting_for_next_ptr) {
      val offset = align(returning_non_ptr_prefetch_port.bits.table_entry.offset, ptrPacketWidthInBytes)
      val ptr_packet = (returning_non_ptr_prefetch_port.bits.data >> (offset * 8.U)).asTypeOf(ptr_packet_t)

      io.read_beats.foreach(_.ready := true.B)
      when (non_ptr_prefetch_returning) {
        next_node_addr := next_node_addr + (ptr_packet.next_ptr_rel_offset * ptrPacketWidthInBytes.U)
        reached_last_node := ptr_packet.next_ptr_rel_offset.andR

        metadata_base_addrs.zip(req.metadata_base_addrs(axis - 1.U)).zip(metadata_strides_by_value).foreach { case ((addrs, inner_axis_addrs), strides_by_value) =>
          addrs.zip(inner_axis_addrs).zip(strides_by_value).foreach { case ((addr, inner_axis_addr), stride_by_value) =>
            // "Stride_by_value" has a really tortured definition here, to be honest. In this state, we are basically
            // just using it as a switch to determine whether or not the 'next_ptr' address should be directly passed
            // down to the lower axis
            inner_axis_addr := Mux(stride_by_value(0), next_node_addr, addr)
          }
        }
        req.data_base_addrs(axis - 1.U) := data_base_addr

        should_move_down_axis_level := true.B

        compressed_state := waiting_for_lower_axes_to_wrap_around__next_ptr
      }
    }.elsewhen (compressed_state === writing_outer_id_to_dram || compressed_state === writing_next_outer_id_to_dram) {
      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.outer_metadata_buffer_id.U, LinkedListMetadata.head_ptr_buffer_id.U)

      val metadata_addr = metadata_addrs(metadata_buffer_id) +&
        Mux(compressed_state === writing_next_outer_id_to_dram && !use_ll_ptr, metadataBytes.U, 0.U)

      val only_use_prefetched_ptrs = req.write && use_ll_ptr && should_prefetch_pointers && compressed_state === writing_next_outer_id_to_dram && skip_requesting_outer_id
      assert(!only_use_prefetched_ptrs || prefetched_ptrs.head.valid)

      when (!only_use_prefetched_ptrs) {
        mk_read_into_dma_req(metadata_addr, write = true.B, metadata_buffer_id = metadata_buffer_id)
        io.tl_reqs.head.bits.tableEntry.read_into_dma := false.B
        io.tl_reqs.head.bits.tableEntry.to_free_node := req.write && use_ll_ptr
      }

      when(compressed_state === writing_next_outer_id_to_dram && !use_ll_ptr) {
        val outer_axis = _axis +& 1.U
        io.tl_reqs.head.bits.tableEntry.iterator_values(outer_axis) := req.sram_base_addrs(outer_axis) + span_iterators(outer_axis) + 1.U
      }

      val updating_last_node_ptr = WireInit(updated_last_node_ptr.head)
      val updating_next_node_ptr = WireInit(updated_next_node_ptr.head || !added_new_ll_node)
      when (compressed_state === writing_next_outer_id_to_dram && use_ll_ptr && accesses_to_same_address_in_outer_memory_ordered.B) {
        // Try to update last-node and next-node ptrs early
        assert(req.write && is_innermost, "i don't think we can enter this when-clause if we're not writing to the innermost loop. but if this turns out to be incorrect, then just add a 'req.write' to the when-clause")

        val n_sram_banks = selectFrom(nSramBanks.map(_.U), req.sram_code)

        def get_free(occupied: Seq[Bool], reverse: Boolean = false): UInt = {
          val free = occupied.map(!_)
          if (reverse) (nTlReqPorts-1).U - PriorityEncoder(free.reverse)
          else PriorityEncoder(free)
        }

        var occupied_tl_reqs = VecInit.fill(nTlReqPorts)(false.B) // We use this for Verilog-style blocking assignments // TODO using bitvectors instead of uints might be way more expensive in hardware

        when (!only_use_prefetched_ptrs && tl_req_valids.head && io.tl_reqs.head.fire) {
          next_cycle_updated_rowlen.head := true.B
        }
        occupied_tl_reqs.head := !only_use_prefetched_ptrs

        var prev_occupied_tl_reqs = occupied_tl_reqs
        occupied_tl_reqs = WireInit(occupied_tl_reqs)

        when (!updated_last_node_ptr.head && !only_use_prefetched_ptrs && !ChiselUtil.all(prev_occupied_tl_reqs)) {
          val port_id = get_free(prev_occupied_tl_reqs.reverse, reverse = true)
          updating_last_node_ptr := update_last_or_next_node_ptr(port_id, update_last_node_in_ptr_packet)
          next_cycle_updated_last_node_ptr.head := updating_last_node_ptr
          occupied_tl_reqs(port_id) := true.B
        }

        prev_occupied_tl_reqs = occupied_tl_reqs
        occupied_tl_reqs = WireInit(occupied_tl_reqs)

        when (!updated_next_node_ptr.head && added_new_ll_node && !only_use_prefetched_ptrs && !ChiselUtil.all(prev_occupied_tl_reqs)) {
          val port_id = get_free(prev_occupied_tl_reqs, reverse = true)
          updating_next_node_ptr := update_last_or_next_node_ptr(port_id, update_next_node_in_ptr_packet)
          next_cycle_updated_next_node_ptr.head := updating_next_node_ptr
          occupied_tl_reqs(port_id) := true.B
        }

        val debug_rowlen_id = Wire(Vec(n_prefetched_ptrs, UInt(32.W)))
        // dontTouch(debug_rowlen_id)
        val debug_can_update_rowlen = Wire(Vec(n_prefetched_ptrs, Bool()))
        // dontTouch(debug_can_update_rowlen)
        val debug_updated_id = Wire(Vec(n_prefetched_ptrs, UInt(32.W)))
        // dontTouch(debug_updated_id)

        val debug_tl_req_port_is_written_by = VecInit.fill(nTlReqPorts)(n_prefetched_ptrs.U)
        // dontTouch(debug_tl_req_port_is_written_by)

        (0 until n_prefetched_ptrs).foldLeft((true.B, occupied_tl_reqs, 0.U(log2Up(n_prefetched_ptrs+1).W))) { case ((earlier_all_returned, _occupied_tl_reqs, new_nodes_added), prefetched_ptr_packet_id) =>
          val prefetched_packet = prefetched_ptrs(prefetched_ptr_packet_id)
          val returned = prefetched_packet.valid && prefetched_packet.returned

          val updated_id = prefetched_ptr_packet_id.U +& !only_use_prefetched_ptrs // This is used to index into the "updated_*" Vecs
          debug_updated_id(prefetched_ptr_packet_id) := updated_id

          val earlier_packet_has_same_addr = any(prefetched_ptrs.zipWithIndex.take(prefetched_ptr_packet_id).map { case (packet, packet_id) =>
            val _updated_id = packet_id.U +& !only_use_prefetched_ptrs
            packet.addr === prefetched_packet.addr && packet.valid &&
              !(packet.returned && updated_rowlen(_updated_id) && updated_last_node_ptr(_updated_id) && (packet.packet.last_ptr_rel_offset.andR || updated_next_node_ptr(_updated_id)))
          }) || !only_use_prefetched_ptrs && prefetched_packet.addr === metadata_addrs(LinkedListMetadata.next_ptr_buffer_id)
          val adds_new_node = !prefetched_packet.packet.last_ptr_rel_offset.andR || earlier_packet_has_same_addr
          val _prev_last_node_addr = prefetched_packet.addr + (prefetched_packet.packet.last_ptr_rel_offset * ptrPacketWidthInBytes.U)

          var prev_occupied_tl_reqs = _occupied_tl_reqs
          var occupied_tl_reqs = WireInit(_occupied_tl_reqs)

          val rowlen_occupied_tl_reqs = WireInit(prev_occupied_tl_reqs)
          io.sram_read_req_readies.zipWithIndex.foreach { case (readies, sramId) =>
            when (req.sram_code === sramId.U) {
              readies.zip(rowlen_occupied_tl_reqs).foreach { case (ready, occupied) =>
                when (!ready) {
                  occupied := true.B
                }
              }
            }
          }
          val update_rowlen_port_id = get_free(rowlen_occupied_tl_reqs)
          val can_update_rowlen = !ChiselUtil.all(rowlen_occupied_tl_reqs) && update_rowlen_port_id < n_sram_banks &&
            update_rowlen_port_id < nTlReqPorts.U && (add_node_to_maximize_performance.B || earlier_all_returned) && returned &&
            !updated_rowlen(updated_id) && !earlier_packet_has_same_addr
          debug_rowlen_id(prefetched_ptr_packet_id) := update_rowlen_port_id
          debug_can_update_rowlen(prefetched_ptr_packet_id) := can_update_rowlen

          when (can_update_rowlen) { occupied_tl_reqs(update_rowlen_port_id) := true.B }
          prev_occupied_tl_reqs = occupied_tl_reqs
          occupied_tl_reqs = WireInit(occupied_tl_reqs)

          val update_last_node_port_id = get_free(prev_occupied_tl_reqs, reverse = true)
          val can_update_last_node_ptr = !ChiselUtil.all(prev_occupied_tl_reqs) && update_last_node_port_id < nTlReqPorts.U &&
            (add_node_to_maximize_performance.B || earlier_all_returned) && returned && !updated_last_node_ptr(updated_id) && !earlier_packet_has_same_addr

          when (can_update_last_node_ptr) { occupied_tl_reqs(update_last_node_port_id) := true.B }
          prev_occupied_tl_reqs = occupied_tl_reqs
          occupied_tl_reqs = WireInit(occupied_tl_reqs)

          val update_next_node_port_id = get_free(prev_occupied_tl_reqs, reverse = true)
          val can_update_next_node_ptr = !ChiselUtil.all(prev_occupied_tl_reqs) && update_next_node_port_id < nTlReqPorts.U &&
            (add_node_to_maximize_performance.B || earlier_all_returned) && returned && !updated_next_node_ptr(updated_id) && !earlier_packet_has_same_addr && adds_new_node

          when (can_update_next_node_ptr) { occupied_tl_reqs(update_next_node_port_id) := true.B }

          def update_ptr(port_id: UInt, compr_state: CompressedStates.Type) = update_last_or_next_node_ptr(
            port_id, compr_state, prefetched_ptr_packet_id = prefetched_ptr_packet_id,
            ptr_packet_addr_opt = Some(prefetched_packet.addr),
            _next_free_ptr_addr = prefetched_packet.free_node_addr_opt.getOrElse(next_free_ptr_addr + new_nodes_added * ptrPacketWidthInBytes.U) + (add_node_to_maximize_performance.B || adds_new_node) * ptrPacketWidthInBytes.U,
            _added_new_ll_node = adds_new_node, _prev_last_node_addr = _prev_last_node_addr,
          )

          when (can_update_rowlen) {
            val _head_ptr_addr = Mux(adds_new_node,
              prefetched_packet.free_node_addr_opt.getOrElse(next_free_ptr_addr + new_nodes_added * ptrPacketWidthInBytes.U),
              prefetched_packet.addr)

            mk_read_into_dma_req(_head_ptr_addr, write = true.B, metadata_buffer_id = metadata_buffer_id, portId = update_rowlen_port_id)
            io.tl_reqs(update_rowlen_port_id).bits.tableEntry.read_into_dma := false.B
            io.tl_reqs(update_rowlen_port_id).bits.tableEntry.to_free_node := true.B
            val outer_axis = _axis +& 1.U
            io.tl_reqs(update_rowlen_port_id).bits.tableEntry.iterator_values(outer_axis) := req.sram_base_addrs(outer_axis) + (if (!support_ooo_ptr_mvouts) span_iterators(outer_axis) + updated_id else prefetched_packet.outer_span_iterator/*span_iterators(outer_axis)*/)

            tl_req_valids(update_rowlen_port_id) := true.B
            next_cycle_updated_rowlen(updated_id) := io.tl_reqs(update_rowlen_port_id).fire

            debug_tl_req_port_is_written_by(update_rowlen_port_id) := prefetched_ptr_packet_id.U
          }

          when (can_update_last_node_ptr) {
            next_cycle_updated_last_node_ptr(updated_id) := update_ptr(update_last_node_port_id, update_last_node_in_ptr_packet)
            debug_tl_req_port_is_written_by(update_last_node_port_id) := prefetched_ptr_packet_id.U
          }

          when (can_update_next_node_ptr) {
            next_cycle_updated_next_node_ptr(updated_id) := update_ptr(update_next_node_port_id, update_next_node_in_ptr_packet)
            debug_tl_req_port_is_written_by(update_next_node_port_id) := prefetched_ptr_packet_id.U
          }

          (earlier_all_returned && returned, occupied_tl_reqs, new_nodes_added + (add_node_to_maximize_performance.B || adds_new_node))
        }
      }

      val write_row_ids_to_dram = req.write && is_innermost && !use_ll_ptr // We can either write CSR-style row-ids, or LL-style non-cumulative row-lens

      debug_is_wasted := !tl_req_valids.head

      val already_wrote_row_len = compressed_state === writing_next_outer_id_to_dram && updated_rowlen.head
      when (!only_use_prefetched_ptrs) {
        tl_req_valids.head := write_row_ids_to_dram || (compressed_state === writing_next_outer_id_to_dram && !already_wrote_row_len)
      }
      val fire = Mux(only_use_prefetched_ptrs,
        if (!support_ooo_ptr_mvouts)
          prefetched_ptrs.head.returned && next_cycle_updated_rowlen.head && next_cycle_updated_last_node_ptr.head && (prefetched_ptrs.head.packet.last_ptr_rel_offset.andR || next_cycle_updated_next_node_ptr.head)
        else {
          // We leave this state if there are any invalid prefetch-ptrs which can be made valid by the outer state, or
          // if any of the prefetch-ptrs are about to be popped (in which case they are about to be made invalid)
          val worth_going_into_outer_axis = compressed_axis_state_datas(_axis+&1.U).requested_coord_it =/= 0.U

          val ptr_is_done = Seq.tabulate(n_prefetched_ptrs)(i => prefetched_ptrs(i).returned && next_cycle_updated_rowlen(i) && next_cycle_updated_last_node_ptr(i) && (prefetched_ptrs(i).packet.last_ptr_rel_offset.andR || next_cycle_updated_next_node_ptr(i)))

          Mux(worth_going_into_outer_axis, any(prefetched_ptrs.map(!_.valid)) || any(ptr_is_done), ChiselUtil.all(vecOr(ptr_is_done, prefetched_ptrs.map(!_.valid))))
        }, tl_req_valids.head && io.tl_reqs.head.fire || already_wrote_row_len)

      when (fire) {
        inflight_req_id := io.inflight_alloc_ids.head

        // In some cases, we finish this axis over here, in others, we finish later on in the "writing_data_to_dram_or_sram" state
        val finished = compressed_state === writing_next_outer_id_to_dram && (axis =/= 0.U ||
          only_use_prefetched_ptrs ||
          skip_data_reads_or_writes && updating_last_node_ptr && updating_next_node_ptr)
        when (finished) {
          should_inc := true.B
          last_it := true.B
        }

        // TODO when reading the next-outer-dim-id, it might be possible to avoid waiting for the TL request to return
        compressed_state := MuxCase(compressed_state.next, Seq(
          finished -> base_compressed_state, // finished
          (is_axis(axis_type, FiberTreeAxis.LinkedList) && compressed_state === writing_next_outer_id_to_dram) -> Mux(use_ll_ptr, Mux(updating_last_node_ptr, Mux(updating_next_node_ptr, writing_data_to_dram_or_sram, update_next_node_in_ptr_packet), update_last_node_in_ptr_packet), writing_data_to_dram_or_sram),
          (req.write && compressed_state === writing_next_outer_id_to_dram) -> writing_data_to_dram_or_sram,
        ))
      }.elsewhen (!tl_req_valids.head && !only_use_prefetched_ptrs) {
        // When we're not on the innermost axis, then we skip writing outer-ids to DRAM/SRAM, and just skip ahead to
        // requesting the outer-id from DRAM so that we can read the inner-coord into the DMA.
        // Alternatively, if we're on the innermost axis and using the ll-ptr, then we can just skip ahead to
        // reading/writing the coords.
        compressed_state := Mux(is_innermost,
          Mux(use_ll_ptr,
            Mux(skip_inner_coord_reads_or_writes, writing_next_outer_id_to_dram, writing_inner_ids_to_dram_or_sram),
            requesting_outer_id),
          Mux(request_coords_from_sram, Mux(request_coords_only_once && !first_coords_request, waiting_for_coord, requesting_coord),
            requesting_outer_id))
      }
    }.elsewhen (compressed_state === waiting_for_outer_id_write || compressed_state === waiting_for_next_outer_id_write) {
      val check_issue = accesses_to_same_address_in_outer_memory_ordered.B || !use_ll_ptr
      val returning = Mux(check_issue,
        io.snoop_write_beat.valid && io.snoop_write_beat.bits.id === inflight_req_id,
        io.inflight_table_is_clearing.valid && io.inflight_table_is_clearing.bits === inflight_req_id
      )

      when (returning) {
        when (!use_ll_ptr) {
          assert(check_issue, "we are expecting to get the outer-id from the snoop_write_beat")

          val byte_offset = align(io.snoop_write_beat.bits.table_entry.offset, alignTo) * 8.U
          val _outer_dim_id = (io.snoop_write_beat.bits.data >> byte_offset)(metadataBits-1, 0)
          val __outer_dim_id = _outer_dim_id + req.addrs(axis); assert(req.addrs(axis) === 0.U || metadata_strides(axis)(LinkedListMetadata.coord_buffer_id) === 1.U)

          data_offset := __outer_dim_id
          coord_offset := __outer_dim_id
        }

        compressed_state := writing_inner_ids_to_dram_or_sram
      }
    }.elsewhen ((compressed_state === requesting_outer_id || compressed_state === requesting_next_outer_id) && !should_prefetch_row_ids)
    {
      assert(beatWidthInBits >= metadataBits, "we need the outer-dim (ROW_ID) metadata to fit in a single beat right now")

      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.outer_metadata_buffer_id.U, LinkedListMetadata.head_ptr_buffer_id.U)
      val metadata_addr = metadata_addrs(metadata_buffer_id) +&
        Mux(compressed_state === requesting_next_outer_id, metadataBytes.U, 0.U)

      mk_read_into_dma_req(metadata_addr, metadata_buffer_id = metadata_buffer_id /* TODO we only set the 'metadata_buffer_id' here to help the unit test, we don't actually need it for end-to-end tests */)

      tl_req_valids.head := !should_prefetch_row_ids
      when (tl_req_valids.head && io.tl_reqs.head.fire) {
        inflight_req_id := io.inflight_alloc_ids.head
        compressed_state := compressed_state.next
      }.elsewhen(!tl_req_valids.head) {
        compressed_state := compressed_state.next
      }
    }.elsewhen ((compressed_state === requesting_outer_id && !combine_outer_id_waits) || compressed_state === waiting_for_outer_id) {
      assert(!non_ptr_prefetch_returning || returning_non_ptr_prefetch_port.bits.table_entry.offset % metadataBytes.U === 0.U && isPow2(metadataBytes).B, "to make the shifter cheaper, we are assuming that the outer-id is aligned in DRAM")
      val _outer_dim_id = Mux(should_prefetch_row_ids, next_cycle_prefetched_row_ids.head, {
        val byte_offset = align(returning_non_ptr_prefetch_port.bits.table_entry.offset, alignTo) * 8.U
        (returning_non_ptr_prefetch_port.bits.data >> byte_offset)(metadataBits-1, 0)
      })

      val should_write_into_sram = !req.write && is_innermost && (is_axis(axis_type, FiberTreeAxis.LinkedList) || is_outermost || span_iterators(axis + 1.U) === 0.U) // There's no need to write the outer-id past the first iteration, because the previous iteration's "waiting_for_next_outer_id" will have already written it
      write_outer_id_to_sram(tiled_outer_dim_id, should_write_into_sram, 1)

      val fire = Mux(should_prefetch_row_ids,
        next_cycle_prefetched_row_id_valids.head && (!should_write_into_sram || io.sram_write_req.ready),
        returning_non_ptr_prefetch_port.fire)

      assert(!io.sram_write_req.fire || fire, "made spurious write to SRAM")

      when (fire) {
        assert(!should_write_into_sram || io.sram_write_req.fire)

        when (use_ll_ptr) {
          span := Mux(_outer_dim_id.andR,
            0.U, // When the row-len is all 1s, then we interpret that as "invalid" or "uninitialized" which indicates a span of 0
            minOf(span, _outer_dim_id)); assert(req.addrs(axis) === 0.U, "not sure how to set span when we want a bias in this example")
        }.otherwise {
          val __outer_dim_id = _outer_dim_id + req.addrs(axis); assert(req.addrs(axis) === 0.U || metadata_strides(axis)(LinkedListMetadata.coord_buffer_id) === 1.U)
          outer_dim_id := __outer_dim_id
          data_offset := __outer_dim_id
          coord_offset := __outer_dim_id
        }

        compressed_state := Mux((req.write || use_ll_ptr) && is_innermost, writing_inner_ids_to_dram_or_sram, requesting_next_outer_id)
      }
    }.elsewhen (compressed_state === requesting_outer_id || compressed_state === requesting_next_outer_id || compressed_state === waiting_for_next_outer_id) {
      assert(!non_ptr_prefetch_returning || returning_non_ptr_prefetch_port.bits.table_entry.offset % metadataBytes.U === 0.U && isPow2(metadataBytes).B, "to make the shifter cheaper, we are assuming that the outer-id is aligned in DRAM")

      val next_outer_dim_id = Mux(should_prefetch_row_ids, next_cycle_prefetched_row_ids(1),
        {
          val byte_offset = align(returning_non_ptr_prefetch_port.bits.table_entry.offset, alignTo) * 8.U
          (returning_non_ptr_prefetch_port.bits.data >> byte_offset)(metadataBits-1, 0)
        })

      val _outer_dim_id = Mux(compressed_state === requesting_outer_id, next_cycle_prefetched_row_ids.head + req.addrs(_axis), outer_dim_id)

      val new_span = floorSub(next_outer_dim_id, _outer_dim_id)(sramSpanBits-1,0).asUInt
      val new_span_clipped = minOf(new_span, original_span)

      val (lookahead_data, should_lookahead) = {
        // When the CSR data/coords are contiguous across multiple rows, we may be able to just read them in one big TL
        // req, rather than in many small TL reqs. Over here, we try to find such opportunities
        val rvs = next_cycle_prefetched_row_ids.zip(next_cycle_prefetched_row_id_valids)
        val (spans, span_valids) = rvs.tail.zip(rvs).map { case ((tail, tail_valid), (head, head_valid)) =>
          (tail - head, head_valid && tail_valid)
        }.unzip

        val result = WireInit((n_prefetched_row_ids-1).U)
        spans.zip(span_valids).zipWithIndex.reverse.foreach { case ((span, span_valid), i) =>
          when (!span_valid || span > original_span) {
            result := i.U
          }
        }

        val should = !req.write && should_prefetch_row_ids && !any(prefetched_row_id_data_requested) && req.addrs(_axis) === 0.U && !is_outermost

        (result, should && result > 0.U)
      }

      val should_write_into_sram = !req.write && is_innermost && is_axis(axis_type, FiberTreeAxis.Compressed) &&
        !(should_prefetch_row_ids && prefetched_row_id_written.head)
      write_outer_id_to_sram(tiled_outer_dim_id + new_span_clipped, should_write_into_sram, 2)

      val outer_axis = _axis +& 1.U
      io.sram_write_req.bits.req.address(outer_axis) := req.sram_base_addrs(outer_axis) + span_iterators(outer_axis) +& 1.U

      val lookahead_row_ids = minOf(lookahead_data, io.sram_write_req.bits.req.data.size.U); require(io.sram_write_req.bits.req.data.size <= n_prefetched_row_ids)
      io.sram_write_req.bits.req.data.zipWithIndex.tail.foreach { case (sram_data_port, i) =>
        sram_data_port := ((next_cycle_prefetched_row_ids(i+1) - _outer_dim_id) +& tiled_outer_dim_id).asTypeOf(sram_data_port)
      }
      when (should_lookahead) {
        io.sram_write_req.bits.req.spans(_axis) := lookahead_row_ids
      }

      val fire = Mux(should_prefetch_row_ids,
        ChiselUtil.all(next_cycle_prefetched_row_id_valids.take(2)) && (!should_write_into_sram || io.sram_write_req.ready),
        returning_non_ptr_prefetch_port.fire)

      assert(!io.sram_write_req.fire || fire, "made spurious write to SRAM when writing next-outer-id")

      when (fire) {
        assert(!should_write_into_sram || io.sram_write_req.fire)

        tiled_outer_dim_id := tiled_outer_dim_id + new_span_clipped

        when (should_lookahead) {
          updateSpan(next_cycle_prefetched_row_ids(lookahead_data) - _outer_dim_id)
          next_cycle_prefetched_row_id_data_requested := ((1.U << lookahead_data).asUInt - 1.U).asTypeOf(next_cycle_prefetched_row_id_data_requested)
          next_cycle_prefetched_row_id_written := ((1.U << lookahead_row_ids).asUInt - 1.U).asTypeOf(next_cycle_prefetched_row_id_written)

          // Increment outer axis to avoid bubble cycles here
          assert(!is_outermost && lookahead_row_ids > 0.U)
          outer_span_step_size := lookahead_row_ids

          val metadata_addr = req.metadata_base_addrs(axis + 1.U)(axis)(CompressedMetadata.outer_metadata_buffer_id)
          metadata_addr := metadata_addr + (lookahead_row_ids-1.U) * metadataBytes.U

          tiled_outer_dim_id := io.sram_write_req.bits.req.data(lookahead_row_ids-1.U).asTypeOf(tiled_outer_dim_id)
        }.otherwise {
          updateSpan(new_span_clipped)
        }

        when (new_span === 0.U) {
          // If we find here that the span is actually zero, then it doesn't make sense to step down into lower axes.
          // Instead, we just mark all the lower axes as done right here, and then move to end this axis as well.
          all_wrapped_arounds.zipWithIndex.foreach { case (w, i) => when (i.U < _axis) { w := true.B }}
        }
        assert(original_span > 0.U)

        when (compressed_state === requesting_outer_id) {
          outer_dim_id := _outer_dim_id
          data_offset := _outer_dim_id
          coord_offset := _outer_dim_id
        }

        head_prefetched_row_id_popping := Mux(should_lookahead, lookahead_row_ids, 1.U)

        val finished = should_prefetch_row_ids && is_innermost && (prefetched_row_id_data_requested.head || skip_data_reads_or_writes && skip_inner_coord_reads_or_writes)
        when (finished) {
          should_inc := true.B
          last_it := true.B
          updateSpan(original_span)
        }

        compressed_state := Mux(is_innermost,
          Mux(finished, base_compressed_state, writing_inner_ids_to_dram_or_sram),
          Mux(new_span === 0.U, waiting_for_lower_axes_to_wrap_around__coord,
            Mux(!request_coords_only_once || first_coords_request, requesting_coord, waiting_for_coord)))
      }
    }.elsewhen (compressed_state === requesting_coord) {
      assert(beatWidthInBits >= metadataBits, "we need the inner-coord metadata to fit in a single beat right now")

      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)
      val metadata_region = metadata_regions(metadata_buffer_id)

      val fire = Wire(Bool())

      when (!coords_only_come_from_srams.B && metadata_region.is_dram) {
        val metadata_addr = metadata_addrs(metadata_buffer_id)
        mk_read_into_dma_req(align(metadata_addr, metadataBytes) + (coord_offset << logMetadataBytes), metadata_buffer_id = metadata_buffer_id /* TODO we only set the 'metadata_buffer_id' here to help the unit test, we don't actually need it for end-to-end tests */)
        tl_req_valids.head := true.B
        fire := tl_req_valids.head && io.tl_reqs.head.fire
      }.otherwise {
        assert(!req_valid || metadata_region.is_sram)

        when (request_coords_only_once) {
          io.sram_read_req.bits.address.foreach(_ := 0.U)
        }.otherwise {
          connectVecs(io.sram_read_req.bits.address, dropFromVec(span_iterators, _axis))
        }
        when (request_coords_only_once) {
          connectVecs(io.sram_read_req.bits.spans, dropFromVec(req.spans, _axis, fillOpt = Some(1.U)))
          io.sram_read_req.bits.spans.head := original_span
        }.otherwise {
          io.sram_read_req.bits.spans.foreach(_ := 1.U)
          io.sram_read_req.bits.spans.head := span
        }
        io.sram_read_req.bits.iteration_strides.foreach(_ := 1.U)
        io.sram_read_req.bits.data_strides.foreach(_ := 0.U)
        io.sram_read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
        for (outer <- 0 until nAxes; inner <- 0 until nAxes; buffer_id <- 0 until FiberTreeAxisMetadata.n_metadata_buffers)
          if (outer < io.sram_read_req.bits.metadata_strides.size && inner < io.sram_read_req.bits.metadata_strides(outer).size)
            when (outer.U +& axis < nAxes.U && inner.U +& axis < nAxes.U) {
              io.sram_read_req.bits.metadata_strides(outer)(inner)(buffer_id) := req.metadata_strides(outer.U +& axis)(inner.U +& axis)(buffer_id)
            }
        io.sram_read_req.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
        io.sram_read_req.bits.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
        io.sram_read_req.bits.should_read_data := false.B
        io.sram_read_req.bits.should_read_metadata := true.B
        io.sram_read_req.bits.axis := 0.U
        io.sram_read_req.bits.metadata_buffer_id := metadata_buffer_id
        io.sram_read_req.bits.to_regfile := false.B
        io.sram_read_req.bits.interleave.should_pop := false.B
        io.sram_read_req.bits.interleave.should_push := false.B
        io.sram_read_req.bits.reset_running_state := false.B

        io.sram_read_req.valid := true.B
        io.sram_read_req.bits.to_dma := true.B
        io.sram_read_req_sram_code := metadata_region.index
        fire := io.sram_read_req.fire
      }

      when (fire) {
        compressed_state := waiting_for_coord
      }
    }.elsewhen (compressed_state === waiting_for_coord ||
      (no_strides_in_waiting_for_lower_axes_to_wrap_around_states.B &&
        compressed_state === waiting_for_lower_axes_to_wrap_around__coord &&
        span_iterator + 1.U < span)) {
      assert(!req_valid || !is_innermost, "we should only be in this state if we're an outer axis")
      assert(!req_valid || req.write, "we should only be in this state when writing from SRAM to DRAM")

      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)
      val metadata_region = metadata_regions(metadata_buffer_id)

      val span_it = span_iterator + (compressed_state === waiting_for_lower_axes_to_wrap_around__coord) * outer_span_step_size

      val sram_read_resp_port_size: UInt = (io.sram_read_resp.bits.data.size/2).U
      val requested_coord_it = compressed_axis_state_datas(axis).requested_coord_it
      val mod_span_iterator = span_it % sram_read_resp_port_size
      val mod_requested_coord_it = requested_coord_it % sram_read_resp_port_size
      val sram_read_resp_span = reduceWidth(io.sram_read_resp.bits.spans.head, sramSpanBits); assert(io.sram_read_resp.valid || io.sram_read_resp.bits.spans.head === sram_read_resp_span)
      assert(!io.sram_read_resp.valid || sram_read_resp_span === sram_read_resp_port_size || io.sram_read_resp.bits.last_in_axis.head, "the `mod_*` variables assume that we fill each sram-read-resp as much as possible")

      val finished_requesting_coords_from_sram = requested_coord_it === 0.U && requested_coord_iterated
      val only_used_for_ptr_prefetches = coords_only_used_for_ptr_prefetches.B || (axis > 0.U && req.write && is_axis(req.axes(axis-1.U), FiberTreeAxis.LinkedList) &&
        req.metadata_base_addrs(axis)(axis-1.U)(LinkedListMetadata.next_ptr_buffer_id) =/= 0.U && // TODO is the check on this specific line needed?
        ChiselUtil.all(metadata_strides_by_value.zipWithIndex.flatMap { case (strides, targetAxis) =>
          strides.zipWithIndex.map { case (stride, metadataBufferId) =>
            targetAxis.U === axis - 1.U && metadataBufferId.U === LinkedListMetadata.next_ptr_buffer_id.U ||
              stride === 0.U
          }}))
      val requested_coord_it_incr = MuxCase(1.U, Seq(
        finished_requesting_coords_from_sram -> 0.U,
        only_used_for_ptr_prefetches -> minOf(
          PopCount(compressed_axis_state_datas(_axis - 1.U).prefetched_ptrs.map(!_.valid)),
          sram_read_resp_span - mod_requested_coord_it
        )))
      val sram_read_resp_pop = !finished_requesting_coords_from_sram && sram_read_resp_span <= mod_requested_coord_it +& requested_coord_it_incr

      val fire = Wire(Bool())
      val coord = Wire(UInt(metadataBits.W))

      val future_coords = Wire(Vec(n_prefetched_ptrs, UInt(metadataBits.W)))
      val future_coord_valids = VecInit.fill(n_prefetched_ptrs)(false.B)
      future_coords := DontCare

      when (!coords_only_come_from_srams.B && metadata_region.is_dram) {
        io.read_beats.foreach(_.ready := true.B)
        fire := non_ptr_prefetch_returning

        val byte_offset = align(returning_non_ptr_prefetch_port.bits.table_entry.offset, alignTo) * 8.U
        coord := (returning_non_ptr_prefetch_port.bits.data >> byte_offset)(metadataBits - 1, 0)
      }.otherwise {
        assert(!req_valid || metadata_region.is_sram)

        io.sram_read_resp.ready := sram_read_resp_pop
        fire := io.sram_read_resp.valid || finished_requesting_coords_from_sram

        coord := io.sram_read_resp.bits.data(mod_span_iterator).asUInt

        future_coords.zip(future_coord_valids).zipWithIndex.foreach { case ((future_coord, v), i) =>
          val off = mod_requested_coord_it +& i.U
          future_coord := io.sram_read_resp.bits.data(off).asUInt
          v := sram_read_resp_span > off && i.U < requested_coord_it_incr
        }
      }

      when (fire) {
        metadata_base_addrs.zip(req.metadata_base_addrs(axis - 1.U)).zip(metadata_strides_by_value).foreach { case ((addrs, inner_axis_addrs), strides_by_value) =>
          addrs.zip(inner_axis_addrs).zip(strides_by_value).foreach { case ((addr, inner_axis_addr), stride_by_value) =>
            val _stride_by_value = if (stride_by_value_fixed_to_ptr_packet_size) {
              require(isPow2(ptrPacketWidthInMetadataElems))
              (stride_by_value / ptrPacketWidthInMetadataElems.U)(0).asUInt * ptrPacketWidthInMetadataElems.U
            } else {
              stride_by_value
            }
            inner_axis_addr := addr + coord * (_stride_by_value << logMetadataBytes).asUInt
          }
        }
        req.data_base_addrs(axis - 1.U) := data_base_addr

        {
          // Over here, we update the "prefetched_ptrs" for the next lower axis, just as a performance hack
          assert(axis > 0.U, "we have bunch of code in this block that looks into 'axis - 1.U'")

          val ptrs_to_prefetch = compressed_axis_state_datas(axis - 1.U).prefetched_ptrs

          val addr = metadata_base_addrs(axis - 1.U)(LinkedListMetadata.next_ptr_buffer_id)
          val stride_by_it = metadata_strides(axis - 1.U)(LinkedListMetadata.next_ptr_buffer_id)
          val stride_by_value = metadata_strides_by_value(axis - 1.U)(LinkedListMetadata.next_ptr_buffer_id)

          assert(stride_by_it === 0.U, "we haven't yet bothered to add support for striding-by-iteration for setting future-ptrs")

          val prefetched_ptrs_are_stored_contiguously = true
          future_coords.zip(future_coord_valids).zipWithIndex.foreach { case ((coord, v), i) =>
            val start = if (prefetched_ptrs_are_stored_contiguously) PopCount(ptrs_to_prefetch.map(_.valid)) else Mux(any(ptrs_to_prefetch.map(_.valid)), n_prefetched_ptrs.U - PriorityEncoder(ptrs_to_prefetch.map(_.valid).reverse), 0.U)

            val ptr = ptrs_to_prefetch(i.U +& start)
            when (v) {
              assert(!ptr.valid && i.U +& start < n_prefetched_ptrs.U)

              val _stride_by_value = if (stride_by_value_fixed_to_ptr_packet_size) {
                require(isPow2(ptrPacketWidthInMetadataElems))
                (stride_by_value / ptrPacketWidthInMetadataElems.U)(0).asUInt * ptrPacketWidthInMetadataElems.U
              } else {
                stride_by_value
              }

              ptr.valid := v
              ptr.addr := addr + coord * (_stride_by_value << logMetadataBytes).asUInt
              ptr.free_node_addr_opt.foreach { free_node_addr =>
                val inner_next_free_ptr_addr = compressed_axis_state_datas(_axis-1.U).original_next_free_ptr_addr
                free_node_addr := inner_next_free_ptr_addr + (i * ptrPacketWidthInBytes).U
                inner_next_free_ptr_addr := inner_next_free_ptr_addr + ((i+1) * ptrPacketWidthInBytes).U; assert(add_node_to_maximize_performance)
              }
              ptr.outer_span_iterator := requested_coord_it +& i.U
            }
          }

          when (ptrs_to_prefetch.head.valid && only_used_for_ptr_prefetches) {
            // This code block originally just looked like this:
            //   'req.metadata_base_addrs(axis - 1.U)(axis - 1.U)(LinkedListMetadata.next_ptr_buffer_id) := ptrs_to_prefetch.head.addr'
            // Unfortunately, Chisel's const-prop seems to work poorly for that line, so we replaced it with the
            // equivalent for-loop below:
            for (axis2 <- 0 until nNonDenseAxes)
              when ((axis2+1).U === _axis) {
                req.metadata_base_addrs(axis2)(axis2)(LinkedListMetadata.next_ptr_buffer_id) := ptrs_to_prefetch.head.addr
              }
          }
        }

        should_move_down_axis_level := true.B

        val ptr_chasing_mvout = req.write && _axis > 0.U && compressed_axis_state_datas(_axis-1.U).use_ll_ptr && {
          val skip_data_reads_or_writes = data_base_addr === 0.U
          val skip_inner_coord_reads_or_writes = {
            val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)
            req.metadata_base_addrs(_axis-1.U)(_axis-1.U)(metadata_buffer_id) === 0.U
          }
          skip_data_reads_or_writes && skip_inner_coord_reads_or_writes
        }
        when (ptr_chasing_mvout) {
          compressed_axis_state_datas(_axis-1.U).state := writing_next_outer_id_to_dram
        }

        when (request_coords_from_sram) {
          val last_in_axis = io.sram_read_resp.bits.last_in_axis.head && sram_read_resp_pop

          requested_coord_it := Mux(last_in_axis, 0.U, requested_coord_it + requested_coord_it_incr)
          requested_coord_iterated := true.B

          when (last_in_axis) {
            val new_span = reduceWidth(io.sram_read_resp.bits.compressed_address.head +& sram_read_resp_span, sramSpanBits)
            when (new_span === 0.U) {
              // If we find here that the span is actually zero, then it doesn't make sense to step down into lower axes.
              // Instead, we just mark all the lower axes as done right here, and then move to end this axis as well.
              all_wrapped_arounds.zipWithIndex.foreach { case (w, i) => when (i.U < _axis) { w := true.B }}
              should_move_down_axis_level := false.B
            }
            assert(original_span > 0.U)

            updateSpan(new_span)
          }

          assert(!io.sram_read_resp.ready || ChiselUtil.all((io.sram_read_resp.bits.compressed_address.drop(1) zip dropFromVec(span_iterators, axis +& 1.U)).map { case (x,y) => x === y }), "coords don't match")
        }

        when (compressed_state === waiting_for_lower_axes_to_wrap_around__coord) {
          should_inc := true.B
        }

        compressed_state := waiting_for_lower_axes_to_wrap_around__coord
      }
    }.elsewhen (compressed_state === waiting_for_lower_axes_to_wrap_around__coord || compressed_state === waiting_for_lower_axes_to_wrap_around__next_ptr) {
      /* TODO it's bad that we can't loop straight back to "requesting_coord" from this state. The only reason this
          state exists is because I'm not sure if it's OK to set "should_inc" to true until the lower axes have all
          wrapped around, but it's really tricky to update the metadata-addrs, update wrap_around, or request a new
          coord until we've set should_inc to true and update span_completed. Ideally, we will be able to get rid of
          this buffer state later.
      */
      {
        should_inc := true.B
        when (compressed_state === waiting_for_lower_axes_to_wrap_around__next_ptr) {
          last_it := reached_last_node
        }

        if (!no_strides_in_waiting_for_lower_axes_to_wrap_around_states) {
          data_base_addr := align(data_base_addr, smallestElemWidthInBytes) + (data_stride << logElemTWidthInBytes)
          metadata_base_addrs.zip(metadata_strides).foreach { case (addrs, strides) =>
            addrs.zip(strides).foreach { case (addr, stride) =>
              addr := align(addr, metadataBytes) + (stride << logMetadataBytes)
            }
          }
        }

        when (last_it) {
          span := original_span
          requested_coord_iterated := false.B
        }

        compressed_state := Mux(last_it,
          base_compressed_state, // finished // TODO in outer-axes, we just skip the "writing_outer_id_to_sram_or_dram" step, so maybe we shouldn't transition to it to signal that we're done
          Mux(compressed_state === waiting_for_lower_axes_to_wrap_around__coord, {
            val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)
            val metadata_region = metadata_regions(metadata_buffer_id)
            Mux(metadata_region.is_sram || coords_only_come_from_srams.B, waiting_for_coord, requesting_coord)
          }, requesting_next_ptr))
      }
    }.elsewhen (compressed_state === writing_inner_ids_to_dram_or_sram || compressed_state === writing_data_to_dram_or_sram) {
      assert(compressed_state =/= writing_data_to_dram_or_sram || axis === 0.U, "only the innermost axis can access data")

      val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.inner_metadata_buffer_id.U, LinkedListMetadata.coord_buffer_id.U)

      when(compressed_state === writing_inner_ids_to_dram_or_sram) {
        // The for-loop below is just manual const-prop for this line:
        //   tl_req_addr := align(metadata_addrs(metadata_buffer_id), metadataBytes) + (coord_offset << logMetadataBytes)
        for (axis2 <- 0 until nNonDenseAxes) {
          when (axis2.U === _axis) {
            tl_req_addr := align(req.metadata_base_addrs(axis2)(axis2)(metadata_buffer_id), metadataBytes) +
              (compressed_axis_state_datas(axis2).coord_offset << logMetadataBytes)
          }
        }
      }

      io.tl_reqs.head.bits.tableEntry.is_data := compressed_state === writing_data_to_dram_or_sram
      io.tl_reqs.head.bits.tableEntry.axis := _axis; assert(!io.tl_reqs.head.fire || is_innermost)
      io.tl_reqs.head.bits.tableEntry.metadata_buffer_id := metadata_buffer_id
      io.tl_reqs.head.bits.tableEntry.use_running_offset := req.write && use_ll_ptr

      repeat_axis := _axis === 0.U && compressed_state === writing_inner_ids_to_dram_or_sram

      val skip = {
        // If the DRAM address is the null pointer, then we skip this read/write
        Mux(compressed_state === writing_data_to_dram_or_sram, skip_data_reads_or_writes, skip_inner_coord_reads_or_writes)
      }

      debug_is_wasted := !tl_req_valids.head

      tl_req_valids.head := span > 0.U && !skip
      when(tl_req_valids.head && io.tl_reqs.head.fire || !tl_req_valids.head) {
        should_inc := true.B

        when(compressed_state === writing_inner_ids_to_dram_or_sram) {
          coord_offset := coord_offset + best_tl_req.span_step_size

          when (skip_inner_coord_reads_or_writes) {
            last_it := true.B
          }

          when(last_it) {
            val finished = _axis > 0.U
            when (finished) {
              updateSpan(original_span)
            }

            compressed_state := Mux(req.write, writing_next_outer_id_to_dram,
              Mux(finished, base_compressed_state, writing_data_to_dram_or_sram))
          }
        }.otherwise {
          assert(axis === 0.U, "the code in this 'otherwise'-block assumes that 'axis === 0.U'")
          compressed_axis_state_datas.head.data_offset := compressed_axis_state_datas.head.data_offset + best_tl_req.span_step_size

          when(last_it) {
            req.spans.head := compressed_axis_state_datas.head.original_span
            compressed_axis_state_datas.head.state := base_compressed_state // finished
          }
        }
      }
    }.elsewhen (compressed_state === update_last_node_in_ptr_packet || compressed_state === update_next_node_in_ptr_packet) {
      val skip = compressed_state === update_last_node_in_ptr_packet && updated_last_node_ptr.head ||
        compressed_state === update_next_node_in_ptr_packet && updated_next_node_ptr.head

      val fire = WireInit(true.B)
      when (!skip) {
        fire := update_last_or_next_node_ptr(0.U, compressed_state)
      }

      // Move to next state
      when (fire) {
        inflight_req_id := io.inflight_alloc_ids.head

        val finished = accesses_to_same_address_in_outer_memory_ordered.B && !(compressed_state === update_last_node_in_ptr_packet && added_new_ll_node && !updated_next_node_ptr.head) && skip_data_reads_or_writes // This is just a performance hack to end early when we wouldn't have written data out anyways in OuterSPACE mvouts
        when (finished) {
          should_inc := true.B
          last_it := true.B
        }

        compressed_state := Mux(accesses_to_same_address_in_outer_memory_ordered.B || skip,
          Mux(compressed_state === update_last_node_in_ptr_packet && added_new_ll_node && !updated_next_node_ptr.head, update_next_node_in_ptr_packet,
            Mux(finished, base_compressed_state, writing_data_to_dram_or_sram)),
          compressed_state.next)
      }
    }.elsewhen (compressed_state === waiting_for_update_of_last_node || compressed_state === waiting_for_update_of_next_node) {
      when (io.inflight_table_is_clearing.valid && io.inflight_table_is_clearing.bits === inflight_req_id) {
        compressed_state := Mux(compressed_state === waiting_for_update_of_last_node && added_new_ll_node,
          update_next_node_in_ptr_packet, writing_data_to_dram_or_sram)
      }
    }

    // Prefetching row-ids (continued from up above to deal with Chisel's last-connect semantics)
    when (should_prefetch_row_ids) {
      assert(!should_prefetch_pointers)

      val waiting_to_be_requested = row_id_prefetch_reqs.map(r => !r.requested)
      when (any(waiting_to_be_requested) && num_row_ids_to_prefetch > 0.U) {
        val prefetch_req_id = PriorityEncoder(waiting_to_be_requested)
        val (port_id, port_is_free) = if (nTlReqPorts > 1) (1, true.B) else (0, !tl_req_valids.head)

        prefetch_tl_req_valids(port_id) := true.B

        when (port_is_free) {
          val metadata_buffer_id = Mux(is_axis(axis_type, FiberTreeAxis.Compressed), CompressedMetadata.outer_metadata_buffer_id.U, LinkedListMetadata.head_ptr_buffer_id.U)
          val metadata_addr = metadata_addrs(metadata_buffer_id) +
            (prefetched_row_ids_count +& Mux(head_prefetched_row_id_already_popped, outer_span_step_size, 0.U)) * metadataBytes.U

          val bytes_requested = mk_read_into_dma_req(metadata_addr, portId = port_id.U)
          val row_ids_requested = minOf(num_row_ids_to_prefetch, bytes_requested / metadataBytes.U); assert(bytes_requested % metadataBytes.U === 0.U)

          when (io.tl_reqs(port_id).fire) {
            val row_id_prefetch_req = row_id_prefetch_reqs(prefetch_req_id)

            row_id_prefetch_req.row_id_id := prefetched_row_ids_count - head_prefetched_row_id_popping
            row_id_prefetch_req.requested := true.B
            row_id_prefetch_req.size := row_ids_requested

            row_id_prefetch_req.tl_id := io.inflight_alloc_ids(port_id)
          }
        }
      }
    }
  }.elsewhen (!is_innermost && is_axis(axis_type, FiberTreeAxis.Dense)) {
    when (should_inc) {
      data_base_addr := align(data_base_addr, smallestElemWidthInBytes) + (data_stride << logElemTWidthInBytes)
      req.data_base_addrs(axis - 1.U) := align(data_base_addr, smallestElemWidthInBytes) + (data_stride << logElemTWidthInBytes)

      metadata_base_addrs.zip(req.metadata_base_addrs(axis - 1.U)).zip(metadata_strides).foreach { case ((addrs, inner_axis_addrs), strides) =>
        addrs.zip(inner_axis_addrs).zip(strides).foreach { case ((addr, inner_axis_addr), stride) =>
          addr := align(addr, metadataBytes) + (stride << logMetadataBytes)
          inner_axis_addr := align(addr, metadataBytes) + (stride << logMetadataBytes)
        }
      }
    }.otherwise {
      req.data_base_addrs(axis - 1.U) := align(data_base_addr, smallestElemWidthInBytes)
      metadata_base_addrs.zip(req.metadata_base_addrs(axis - 1.U)).foreach { case (addrs, inner_axis_addrs) =>
        addrs.zip(inner_axis_addrs).foreach { case (addr, inner_axis_addr) =>
          inner_axis_addr := align(addr, metadataBytes)
        }
      }
    }
  }

  // Read in DMA requests
  io.req.ready := !req_valid

  val loaded_original_free_ptr = RegInit(false.B) // I added this because the 'CONSTANT_DROPFOLLOWINGCMDSIFEMPTY' command caused the first initialization of the next-free-ptr to be dropped before it could be captured by the DMA

  when (io.req.fire) {
    req_valid := true.B
    req := io.req.bits
    span_iterators.foreach(_ := 0.U)
    all_wrapped_arounds.foreach(_ := false.B)

    val new_innermost_axis = WireInit(0.U(log2Up(nAxes-nUselessAxes).W))
    for (axisId <- 0 until (nAxes-nUselessAxes))
      when (io.req.bits.spans(axisId) === 0.U) {
        new_innermost_axis := (axisId+1).U
      }
    innermost_axis := new_innermost_axis

    val new_outermost_axis = {
      val result = WireInit(new_innermost_axis)
      io.req.bits.axes.zip(io.req.bits.spans).zipWithIndex.dropRight(nUselessAxes).foreach { case ((axis, span), axisId) =>
        val is_singleton_axis = is_axis(axis, FiberTreeAxis.Dense) && span === 1.U
        when (!is_singleton_axis && axisId.U >= new_innermost_axis) {
          result := axisId.U
        }
      }

      // The row-id prefetching code for CSR mvins currently doesn't work correctly if the Compressed axis is the
      // outer most axis, so the line below is meant to make sure that the compressed axis isn't set as the outermost
      // axis, even for single-row CSR matrices
      Mux(io.req.bits.axes(result) === FiberTreeAxis.Compressed && result < (nAxes-1).U, result +& 1.U, result)
    }
    axis := new_outermost_axis
    outermost_axis := new_outermost_axis

    val base_compressed_state_wire = WireInit(default_base_compressed_state)
    when (!io.req.bits.write && !ChiselUtil.any(io.req.bits.axes.map(_ === FiberTreeAxis.LinkedList))) {
      base_compressed_state_wire := requesting_outer_id
    }
    base_compressed_state := base_compressed_state_wire
    compressed_axis_state_datas.foreach(_.state := base_compressed_state_wire)

    compressed_axis_state_datas.zip(io.req.bits.spans).foreach { case (axis_data, span) =>
      axis_data.original_span := span
    }
    compressed_axis_state_datas.zip(io.req.bits.metadata_base_addrs.zipWithIndex.map(t => t._1(t._2)(LinkedListMetadata.last_node_len_buffer_id))).foreach { case (axis_data, next_free_node_ptr) =>
      when (next_free_node_ptr.orR && !loaded_original_free_ptr) {
        /* TODO This "when"-clause is a hacky way of allowing the DMA to "remember" what the last free-ptr-addr was in
            previous invocations of the DMA. Ideally, this information would be stored by the CPU instead, which would
            update the DMA's free-ptr metadata address correspondingly.
         */
        axis_data.original_next_free_ptr_addr := next_free_node_ptr
        loaded_original_free_ptr := true.B
      }
    }
    compressed_axis_state_datas.zipWithIndex.foreach { case (axis_data, axisId) =>
      axis_data.use_ll_ptr := is_axis(io.req.bits.axes(axisId), FiberTreeAxis.LinkedList) &&
        align(io.req.bits.metadata_base_addrs(axisId)(axisId)(LinkedListMetadata.next_ptr_buffer_id), ptrPacketWidthInBytes) =/= 0.U
    }
    compressed_axis_state_datas.foreach(_.requested_coord_it := 0.U)
    compressed_axis_state_datas.foreach(_.requested_coord_iterated := false.B)
    compressed_axis_state_datas.foreach(_.prefetched_ptrs.foreach(_.reset()))
    compressed_axis_state_datas.foreach(_.updated_rowlen.foreach(_ := false.B))
    compressed_axis_state_datas.foreach(_.updated_last_node_ptr.foreach(_ := false.B))
    compressed_axis_state_datas.foreach(_.updated_next_node_ptr.foreach(_ := false.B))
    compressed_axis_state_datas.foreach(_.row_id_prefetch_reqs.foreach(_.reset()))
    compressed_axis_state_datas.foreach(_.prefetched_row_id_valids.foreach(_ := false.B))
    compressed_axis_state_datas.foreach(_.prefetched_row_id_data_requested.foreach(_ := false.B))
    compressed_axis_state_datas.foreach(_.prefetched_row_id_written.foreach(_ := false.B))
    when (ChiselUtil.all(io.req.bits.addrs.dropRight(nUselessAxes).map(_ === 0.U))) {
      compressed_axis_state_datas.foreach(_.tiled_outer_dim_id := 0.U)
    }

    if (has_repeated_bias_optimizations) {
      val strides = io.req.bits.data_strides.dropRight(nUselessAxes)
      val axes = io.req.bits.axes.dropRight(nUselessAxes).take(nNonDenseAxes)

      val repeated_bias = !io.req.bits.write &&
        ChiselUtil.all(strides.tail.map(_ === 0.U)) &&
        ChiselUtil.all(axes.map(is_axis(_, FiberTreeAxis.Dense)))

      when (repeated_bias) {
        sram_spans := io.req.bits.spans
        req.spans.tail.foreach(_ := 1.U)
      }.otherwise {
        sram_spans.foreach(_ := 1.U)
      }
    }

    ptr_chaser_first_outer_id_returned := false.B
    ptr_chaser_inner_ids_written := false.B
    ptr_chaser_outer_ids_written := false.B
    ptr_chaser_data_written := false.B

    ptr_chaser_threshold_point_reached := false.B
    ptr_chaser_total_nodes_moved_in := 0.U

    ptr_chaser_last_ptr_packet_addr_captured := false.B
  }

  // Prefetching ptrs (We don't do this in the when-else-statement above, so that pointers waiting to be prefetched can
  //    be prefetched early, even before reaching their stage. However, for simplicity, we currently only prefetch from
  //    one axis at a time. /* TODO remove this limitation */)
  {
    val prefetch_from_current_axis = axis < nNonDenseAxes.U && compressed_axis_state_datas(axis).use_ll_ptr && any(compressed_axis_state_datas(axis).prefetched_ptrs.map(_.valid))
    val axis_to_prefetch_from = Mux(prefetch_from_current_axis, axis, PriorityEncoder(compressed_axis_state_datas.map { state_data =>
      state_data.use_ll_ptr && any(state_data.prefetched_ptrs.map(_.valid))
    }))

    val state_data = compressed_axis_state_datas(axis_to_prefetch_from)
    val should_prefetch_pointers = state_data.use_ll_ptr && any(state_data.prefetched_ptrs.map(_.valid))
    val prefetched_ptrs = state_data.prefetched_ptrs
    val next_cycle_prefetched_ptrs = all_next_cycle_prefetched_ptrs(axis_to_prefetch_from)

    when (should_prefetch_pointers) {
      prefetched_ptrs.zip(next_cycle_prefetched_ptrs).foldLeft((tl_req_valids, false.B, false.B, 0.U, 0.U(log2Up(maxInFlightReqs).W))) { case ((tl_req_port_occupied, prev_req_valid, prev_req_fired, prev_req_addr, prev_req_id), (prefetched_ptr, next_cycle_prefetched_ptr)) =>
        val next_tl_req_port_occupied = WireInit(tl_req_port_occupied)
        val next_prev_req_fired = WireInit(false.B)
        val next_prev_req_id = WireInit(prev_req_id)

        when (prefetched_ptr.valid) {
          // Request
          when (!prefetched_ptr.requested) {
            val can_combine_with_prev_prefetch = prev_req_valid && (prev_req_addr / beatWidthInBytes.U) === (prefetched_ptr.addr / beatWidthInBytes.U)

            when (can_combine_with_prev_prefetch && prev_req_fired) {
              next_cycle_prefetched_ptr.requested := true.B
              next_cycle_prefetched_ptr.id := prev_req_id
              next_cycle_prefetched_ptr.offset := prefetched_ptr.addr - align(prefetched_ptr.addr, beatWidthInBytes)

              next_prev_req_fired := true.B
            }

            when (!can_combine_with_prev_prefetch && !ChiselUtil.all(tl_req_port_occupied)) {
              val free_tl_req_port_id = PriorityEncoder(tl_req_port_occupied.map(!_))
              next_tl_req_port_occupied(free_tl_req_port_id) := true.B

              mk_read_into_dma_req(prefetched_ptr.addr, portId = free_tl_req_port_id)
              prefetch_tl_req_valids(free_tl_req_port_id) := true.B

              when (io.tl_reqs(free_tl_req_port_id).fire) {
                next_cycle_prefetched_ptr.requested := true.B
                next_cycle_prefetched_ptr.id := io.inflight_alloc_ids(free_tl_req_port_id)
                next_cycle_prefetched_ptr.offset := io.inflight_alloc_reqs(free_tl_req_port_id).bits.offset
              }

              next_prev_req_fired := io.tl_reqs(free_tl_req_port_id).ready
              next_prev_req_id := io.inflight_alloc_ids(free_tl_req_port_id)
            }
          }
        }

        (next_tl_req_port_occupied, prefetched_ptr.valid && !prefetched_ptr.requested, next_prev_req_fired, prefetched_ptr.addr, next_prev_req_id)
      }
    }
  }

  // Capture returning ptr prefetches. (We can't do this in the when-else-statement above, because a prefetched pointer
  //   may return in a different axis than the one it was requested in).
  compressed_axis_state_datas.map(_.prefetched_ptrs).zip(all_next_cycle_prefetched_ptrs).foreach { case(prefetched_ptrs, next_cycle_prefetched_ptrs) =>
    prefetched_ptrs.zip(next_cycle_prefetched_ptrs).foreach { case (prefetched_ptr, next_cycle_prefetched_ptr) =>
      when (prefetched_ptr.valid) {
        when(prefetched_ptr.waiting_to_return) {
          io.read_beats.foreach { read_beat =>
            when(read_beat.bits.id === prefetched_ptr.id) {
              read_beat.ready := true.B
            }

            when(read_beat.fire && read_beat.bits.id === prefetched_ptr.id) {
              val byte_offset = align(prefetched_ptr.offset /* read_beat.bits.table_entry.offset */, ptrPacketWidthInBytes) * 8.U
              val ptr_packet = (read_beat.bits.data >> byte_offset).asTypeOf(ptr_packet_t)
              next_cycle_prefetched_ptr.packet.update(ptr_packet, prefetched_ptr.last_node_updated || next_cycle_prefetched_ptr.last_node_updated, prefetched_ptr.next_node_updated || next_cycle_prefetched_ptr.next_node_updated)
              next_cycle_prefetched_ptr.returned := true.B
            }
          }
        }
      }
    }
  }

  // Capture returning row-id prefetches. (We can't do this in the when-else-statement above, because a prefetched row-id may
  //   return in a different axis than the one it was requested in).
  compressed_axis_state_datas.zip(all_next_cycle_prefetched_row_ids).zip(all_next_cycle_prefetched_row_id_valids).foreach { case ((compressed_axis_state_data,next_cycle_prefetched_row_ids),next_cycle_prefetched_row_id_valids) =>
    val row_id_prefetch_reqs = compressed_axis_state_data.row_id_prefetch_reqs

    io.read_beats.foreach { read_beat =>
      val matching_prefetch_reqs = row_id_prefetch_reqs.map(r => r.requested && r.tl_id === read_beat.bits.id)
      val prefetch_req_id = PriorityEncoder(matching_prefetch_reqs)
      when (any(matching_prefetch_reqs)) {
        read_beat.ready := !io.sram_write_req.valid || io.sram_write_req.ready

        when (read_beat.fire) {
          val prefetch_req = row_id_prefetch_reqs(prefetch_req_id)

          next_cycle_prefetched_row_ids.zip(next_cycle_prefetched_row_id_valids).zipWithIndex.foreach { case ((row_id, valid), i) =>
            when (i.U >= prefetch_req.row_id_id && i.U < prefetch_req.row_id_id +& prefetch_req.size) {
              valid := true.B
              row_id := read_beat.bits.data >> ((read_beat.bits.table_entry.offset +& (i.U - prefetch_req.row_id_id) * metadataBytes.U) * 8.U)
            }
          }

          row_id_prefetch_reqs(prefetch_req_id).requested := false.B
        }
      }
    }
  }

  // Hardcoding
  req.axes.drop(nNonDenseAxes).foreach(_ := FiberTreeAxis.Dense)

  req.metadata_base_addrs.zipWithIndex.foreach { case (addrs, outerAxis) =>
    addrs.drop(outerAxis+1).foreach(_ := DontCare)
  }
  req.metadata_base_addrs.foreach(_.drop(nNonDenseAxes).foreach(_ := DontCare))

  for (strides <- Seq(req.metadata_strides, req.metadata_strides_by_value, req.sram_metadata_strides, req.sram_metadata_strides_by_addr))
    strides.zipWithIndex.foreach { case (stridess, outerAxis) =>
      stridess.drop(outerAxis+1).foreach(_.foreach(_ := 0.U))
      stridess.drop(nNonDenseAxes).foreach(_.foreach(_ := 0.U))
    }

  span_iterators.takeRight(nUselessAxes).foreach(_ := 0.U)
  req.spans.takeRight(nUselessAxes).foreach(_ := 1.U)
  req.data_strides.takeRight(nUselessAxes).foreach(_ := 0.U)
  req.metadata_strides.takeRight(nUselessAxes).foreach(_.foreach(_.foreach(_ := 0.U)))
  req.metadata_strides_by_value.takeRight(nUselessAxes).foreach(_.foreach(_.foreach(_ := 0.U)))
  req.sram_metadata_strides.takeRight(nUselessAxes).foreach(_.foreach(_.foreach(_ := 0.U)))
  req.sram_metadata_strides_by_addr.takeRight(nUselessAxes).foreach(_.foreach(_.foreach(_ := 0.U)))
  req.sram_base_addrs.takeRight(nUselessAxes).foreach(_ := 0.U)
  sram_spans.takeRight(nUselessAxes).foreach(_ := 1.U)

  // Assertions
  val stall_counter = RegInit(0.U(32.W))
  when (!req_valid || any(io.tl_reqs.map(_.fire))) {
    stall_counter := 0.U
  }.otherwise {
    stall_counter := stall_counter + 1.U
  }
  assert(stall_counter <= 1000000.U, "dma is stalling")

  // Debugging signals
  val debug_state_cycle_counts = RegInit(VecInit.fill(CompressedStates.all.size)(0.U(32.W)))
  val debug_state_cycle_counts_only_writes = RegInit(VecInit.fill(CompressedStates.all.size)(0.U(32.W)))
  val debug_state_cycle_counts_only_a = RegInit(VecInit.fill(CompressedStates.all.size)(0.U(32.W)))

  val debug_wasted_cycles = RegInit(VecInit.fill(CompressedStates.all.size)(0.U(32.W)))
  val debug_wasted_cycles_only_writes = RegInit(VecInit.fill(CompressedStates.all.size)(0.U(32.W)))
  val debug_wasted_cycles_only_a = RegInit(VecInit.fill(CompressedStates.all.size)(0.U(32.W)))

  when (req_valid && axis_type =/= FiberTreeAxis.Dense) {
    val count = debug_state_cycle_counts(compressed_axis_state_datas(axis).asUInt)
    count := count + 1.U

    val count2 = debug_wasted_cycles(compressed_axis_state_datas(axis).asUInt)
    count2 := count2 +& debug_is_wasted.asUInt

    when (req.write) {
      val count = debug_state_cycle_counts_only_writes(compressed_axis_state_datas(axis).asUInt)
      count := count + 1.U

      val count2 = debug_wasted_cycles_only_writes(compressed_axis_state_datas(axis).asUInt)
      count2 := count2 +& debug_is_wasted.asUInt
    }.elsewhen (req.sram_code === 0.U) {
      val count = debug_state_cycle_counts_only_a(compressed_axis_state_datas(axis).asUInt)
      count := count + 1.U

      val count2 = debug_wasted_cycles_only_a(compressed_axis_state_datas(axis).asUInt)
      count2 := count2 +& debug_is_wasted.asUInt
    }
  }

  val debug_for_firesim = false
  def debug_printf(p: => Printf): Unit = {
    if (debug_for_firesim) {
      midas.targetutils.SynthesizePrintf(p)
    } else {
      p
    }
  }

  when (io.debug_print) {
    debug_printf(printf(p"\nOnly write cycle counts:\n"))
    for (((name, i), count) <- CompressedStates.allNames.zipWithIndex.zip(debug_state_cycle_counts_only_writes)) {
      debug_printf(printf(p"  $name | state: $i | count = $count\n"))
    }

    debug_printf(printf(p"\nOnly read-A cycle counts:\n"))
    for (((name, i), count) <- CompressedStates.allNames.zipWithIndex.zip(debug_state_cycle_counts_only_a)) {
      debug_printf(printf(p"  $name | state: $i | count = $count\n"))
    }

    debug_printf(printf(p"\nOnly write wasted-cycles:\n"))
    for (((name, i), count) <- CompressedStates.allNames.zipWithIndex.zip(debug_wasted_cycles_only_writes)) {
      val relevant = (1 to 4).toSeq :+ 21
      if (relevant.contains(i)) {
        debug_printf(printf(p"  $name | state: $i | count = $count\n"))
      }
    }
    debug_printf(printf(p"\n"))

    debug_state_cycle_counts.foreach(_ := 0.U)
    debug_state_cycle_counts_only_writes.foreach(_ := 0.U)
    debug_state_cycle_counts_only_a.foreach(_ := 0.U)

    debug_wasted_cycles.foreach(_ := 0.U)
    debug_wasted_cycles_only_writes.foreach(_ := 0.U)
    debug_wasted_cycles_only_a.foreach(_ := 0.U)
  }

  /*
  val debug = WireInit(io.tl_reqs.size.U)
  val debug_bool = WireInit(false.B)
  dontTouch(debug); dontTouch(debug_bool)
  io.tl_reqs.zipWithIndex.foreach { case (tl_req, tl_req_id) =>
    val target = "h80005480".U
    val start = tl_req.bits.addr +& tl_req.bits.tableEntry.offset
    val end = start +& minOf(tl_req.bits.tableEntry.len * metadataBytes.U, 1.U << tl_req.bits.logLen)
    when (tl_req.fire && start <= target && end > target && tl_req.bits.write && tl_req.bits.write_from_dma) {
      debug_bool := true.B
      debug := tl_req_id.U
    }
  }
  */
}

object Dma {
  val addrBits: Int = 64
  val spanBits: Int = 32
  val sramCodeBits: Int = 16 // TODO we should get this from the number of SRAMs
}
