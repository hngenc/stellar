package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

import stellar.rtl.ChiselUtil._

class TLReqGenerator[T <: Data](elemT: T, val nBanks: Int, nAdjacents: Int, maxReqLenInBytes: Int, maxInFlightReqs: Int,
                                dmaTLReq: DmaTLReq, sramReadRespT: ChiselSRAMReadResp[T],
                                inflightEntryT: InFlightTableEntry) extends Module {
  // This module allocates TL reqs in the in-flight-table on-the-fly for SRAM->DRAM transfers whose lengths are not
  // known by the Dma. We basically just put this right ahead of the BeatSplitter

  val elemTbytes = elemT.getWidth/8
  val maxReqLenInElems = maxReqLenInBytes / elemTbytes
  val maxInFlightReqsBits = log2Ceil(maxInFlightReqs)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val read_resp = getChiselType(sramReadRespT)
      val tl_req = getChiselType(dmaTLReq)
    }))
    val lookahead_read_resps = Vec(nBanks-1, Flipped(Decoupled(getChiselType(sramReadRespT))))

    val inflight_alloc = new Bundle {
      val req = Decoupled(inflightEntryT)
      val id = Input(UInt(maxInFlightReqsBits.W))
    }

    val out = Decoupled(new Bundle {
      val read_resp = getChiselType(sramReadRespT)
      val tl_req = getChiselType(dmaTLReq)
    })

    val adjacent_offset = Vec(nAdjacents max 1, Flipped(Decoupled(UInt(32.W)))) // TODO magic number

    val pop_tl_req = Output(Bool())

    val bankId = Output(UInt(log2Up(nBanks).W))
  })

  object State extends ChiselEnum {
    val idle, running = Value
  }
  import State._
  val state = RegInit(idle)

  val all_inputs_finished = Reg(Bool())
  val ending = WireInit(false.B)

  val read_resp = Reg(getChiselType(sramReadRespT))
  val read_resp_valid = RegInit(false.B)

  val read_resp_data_size = sramReadRespT.data.size * 2 // We add a little bit of flex space to avoid bubbles with unaligned writes
  val read_resp_data = Reg(Vec(read_resp_data_size, elemT))

  val tl_req = Reg(getChiselType(dmaTLReq))
  val tl_req_valid = RegInit(false.B)
  val made_first_tl_req = Reg(Bool())

  val bankId = RegInit(0.U(log2Up(nBanks).W))
  io.bankId := bankId

  val maxElemsInputted = read_resp_data_size.max(maxReqLenInElems * 2) // We add a little bit of flex space to avoid bubbles with unaligned writes

  val elems_inputted = RegInit(0.U(log2Up(maxElemsInputted+1).W))
  val elems_outputted = RegInit(0.U(log2Up(maxReqLenInElems+1).W))
  val elems_inputted_wire = WireInit(elems_inputted) // This wire is just used to synchronize updates that happen simultaneously to "elems_inputted" by the "io.in.fire" and "io.out.fire" when-clauses

  io.out.bits.read_resp := read_resp
  io.out.bits.tl_req := tl_req
  io.out.bits.read_resp.last := ending
  io.out.bits.read_resp.data.zip(read_resp_data).foreach { case (d,s) => d := s }
  io.out.valid := read_resp_valid && tl_req_valid

  // Because these TL reqs are going to go to writes, there are some InflightTableEntry fields (like "iterator_values")
  // which we can safely ignore
  io.inflight_alloc.req.valid := state === running && !tl_req_valid
  io.inflight_alloc.req.bits := tl_req.tableEntry
  io.inflight_alloc.req.bits.offset := Mux(made_first_tl_req, 0.U, tl_req.tableEntry.offset)
  io.inflight_alloc.req.bits.len := (maxReqLenInBytes / elemTbytes).U // The BeatSplitter doesn't actually look at "len", so it doesn't matter what we set this to

  when (io.inflight_alloc.req.fire) {
    tl_req_valid := true.B
    tl_req.id := io.inflight_alloc.id

    when (made_first_tl_req) {
      tl_req.addr := tl_req.addr +& maxReqLenInBytes.U
    }

    tl_req.tableEntry := io.inflight_alloc.req.bits

    made_first_tl_req := true.B

    assert((1.U << tl_req.logLen) === maxReqLenInBytes.U, "currently, we only allow dynamically-allocating max-size TL reqs")
  }

  val tl_req_len = (1.U << tl_req.logLen).asUInt

  val bytes_inputted = (elems_inputted * elemTbytes.U) +& tl_req.tableEntry.offset
  val bytes_outputted = (elems_outputted * elemTbytes.U) +& tl_req.tableEntry.offset

  val bytes_left_over_in_read_resp_data = WireInit(bytes_inputted - bytes_outputted)
  val elems_left_over_in_read_resp_data = bytes_left_over_in_read_resp_data / elemTbytes.U

  val bytes_to_output_this_cycle = minOf(tl_req_len - bytes_outputted, bytes_inputted - bytes_outputted,
    sramReadRespT.data.size.U * elemTbytes.U)

  io.out.bits.read_resp.spans.head := bytes_to_output_this_cycle / elemTbytes.U
  assert(!io.out.valid || io.out.bits.read_resp.spans.head <= sramReadRespT.data.size.U)

  val (fused_input_read_resp, fused_input_read_resp_data, fused_input_read_resp_span, num_lookaheads) = {
    val main = io.in.bits.read_resp
    val result = WireInit(main)
    val result_data = Wire(getChiselType(read_resp_data))

    val spans = io.lookahead_read_resps.map(r => Mux(r.valid, r.bits.spans.head, maxVal(UInt(32.W)) /* TODO magic number */))
    val cumulative_spans = VecInit(spans.scanLeft(io.in.bits.read_resp.spans.head)(_ +& _))
    assert(!io.in.valid || io.in.bits.tl_req.tableEntry.axis === 0.U)

    val num_lookaheads = MuxCase(0.U, cumulative_spans.zipWithIndex.reverse.map { case (cumul_span, i) =>
      (elems_left_over_in_read_resp_data +& cumul_span <= read_resp_data_size.U &&
        elems_inputted_wire +& cumul_span <= maxElemsInputted.U) -> i.U
    })

    result_data := DontCare
    connectVecs(result_data, main.data)
    io.lookahead_read_resps.zip(cumulative_spans).foreach { case (lookahead_port, cumul_span) =>
      result_data.zipWithIndex.foreach { case (d,i) =>
        when (i.U >= cumul_span) {
          d := lookahead_port.bits.data(i.U - cumul_span)
        }
      }
    }

    if (io.lookahead_read_resps.nonEmpty)
      when (num_lookaheads > 0.U) {
        result.last := io.lookahead_read_resps(num_lookaheads - 1.U).bits.last
        result.last_in_axis := io.lookahead_read_resps(num_lookaheads - 1.U).bits.last_in_axis
      }

    (result, result_data, cumulative_spans(num_lookaheads), num_lookaheads)
  }

  val adjacent_offset = RegInit(0.U(32.W)) // TODO magic number
  val adjacent_offset_set = RegInit(VecInit.fill(nAdjacents max 1)(false.B))
  val adjacent_offset_totally_set = (nAdjacents == 0).B || ChiselUtil.all(adjacent_offset_set)

  io.in.ready := (adjacent_offset_totally_set || !io.in.bits.read_resp.adjacent) && (
    state === idle || !read_resp_valid || ending || (!all_inputs_finished && {
      val span = io.in.bits.read_resp.spans.head; assert(!io.in.valid || io.in.bits.tl_req.tableEntry.axis === 0.U && (ChiselUtil.all(io.in.bits.read_resp.spans.tail.map(_ === 1.U)) || span === 0.U))
      elems_left_over_in_read_resp_data +& span <= read_resp_data_size.U &&
        elems_inputted_wire +& span <= maxElemsInputted.U
    }))
  io.lookahead_read_resps.zipWithIndex.foreach { case (lookahead_port, i) =>
    lookahead_port.ready := io.in.ready && i.U < num_lookaheads
    assert(!lookahead_port.fire || io.in.fire && ChiselUtil.all(io.lookahead_read_resps.take(i).map(_.fire)), "lookahead port fires out-of-sync with other ports")
  }

  io.pop_tl_req := io.in.fire && fused_input_read_resp.last // TODO we should be able to pop during the first `io.in.fire`, but that prevents Top from redirecting the SRAM output to the tl-write-q-generator

  when (io.out.fire) {
    val next_bytes_outputted = bytes_outputted + bytes_to_output_this_cycle
    bytes_left_over_in_read_resp_data := bytes_inputted - next_bytes_outputted
    elems_outputted := elems_outputted + bytes_to_output_this_cycle / elemTbytes.U

    read_resp_data := (read_resp_data.asUInt >> (bytes_to_output_this_cycle * 8.U)).asTypeOf(read_resp_data)

    val tl_req_finished = next_bytes_outputted >= tl_req_len

    when (tl_req_finished || ending) {
      io.inflight_alloc.req.valid := !ending
      tl_req_valid := io.inflight_alloc.req.fire

      elems_inputted_wire := elems_left_over_in_read_resp_data
      elems_inputted := elems_inputted_wire

      elems_outputted := 0.U

      io.out.bits.read_resp.last := true.B
    }

    val clear_read_resp: Bool = bytes_left_over_in_read_resp_data === 0.U
    read_resp_valid := !clear_read_resp

    ending := all_inputs_finished && clear_read_resp
    when (ending) {
      state := idle
    }
  }

  when (io.in.fire) {
    val first = state === idle || ending
    state := running

    read_resp_valid := true.B
    read_resp := fused_input_read_resp
    read_resp_data.zipWithIndex.foreach { case (d, i) =>
      when (i.U >= elems_left_over_in_read_resp_data) {
        d := fused_input_read_resp_data(i.U - elems_left_over_in_read_resp_data)
      }
    }

    when (first) {
      val first_tl_req_already_allocated = (nAdjacents == 0).B || !io.in.bits.read_resp.adjacent

      tl_req := io.in.bits.tl_req
      tl_req_valid := first_tl_req_already_allocated
      made_first_tl_req := first_tl_req_already_allocated

      when (io.in.bits.tl_req.tableEntry.use_running_offset || io.in.bits.read_resp.adjacent) {
        // This is mainly used for OuterSPACE mvouts for now
        assert((1.U << io.in.bits.tl_req.logLen) === maxReqLenInBytes.U)
        val use_running_len = io.in.bits.tl_req.tableEntry.use_running_offset && !((nAdjacents>0).B && io.in.bits.read_resp.adjacent)
        val running_len = Mux(use_running_len, fused_input_read_resp.total_running_len, adjacent_offset)
        val total_offset = io.in.bits.tl_req.tableEntry.offset +& (running_len * elemTbytes.U)
        val offsetted_addr = io.in.bits.tl_req.addr +& total_offset
        tl_req.tableEntry.offset := offsetted_addr % maxReqLenInBytes.U
        tl_req.addr := align(offsetted_addr, maxReqLenInBytes)
      }
    }

    all_inputs_finished := fused_input_read_resp.last

    val span = fused_input_read_resp_span; assert(io.in.bits.tl_req.tableEntry.axis === 0.U && (ChiselUtil.all(fused_input_read_resp.spans.tail.map(_ === 1.U)) || span === 0.U))
    elems_inputted := Mux(first, span, elems_inputted_wire + span)

    val axis = io.in.bits.tl_req.tableEntry.axis
    val last_in_axis = fused_input_read_resp.last_in_axis(axis)
    val last_in_outer_axis = ((fused_input_read_resp.last_in_axis.asUInt >> axis).asUInt & "b11".U) === "b11".U

    when (io.in.bits.read_resp.last || last_in_outer_axis || fused_input_read_resp.adjacent) {
      bankId := 0.U
    }.otherwise {
      bankId := bankId + (num_lookaheads +& last_in_axis)
    }
  }

  io.adjacent_offset.zip(adjacent_offset_set).foreach { case (io_adjacent_offset, set) =>
    when (io_adjacent_offset.fire) {
      set := true.B
    }
    io_adjacent_offset.ready := !set
  }
  adjacent_offset := adjacent_offset + io.adjacent_offset.zip(adjacent_offset_set).map { case (x, y) => Mux(x.fire && !y, x.bits, 0.U) }.reduce(_ +& _)
  when (io.in.fire) {
    adjacent_offset := 0.U
    when (io.in.bits.read_resp.last) {
      adjacent_offset_set.foreach(_ := false.B)

      when (io.in.bits.read_resp.adjacent) {
        assert((nAdjacents == 0).B || adjacent_offset_totally_set)
      }.otherwise {
        assert(!any(adjacent_offset_set))
      }
    }
  }
  // dontTouch(io.in.bits.read_resp.adjacent)
  // dontTouch(io.in.bits.read_resp.compressed_address)

  val debug_all_beats = RegInit(0.U(32.W))
  val debug_total_span = RegInit(0.U(32.W))
  val debug_ideal_beats = WireInit(debug_total_span / 8.U)
  val debug_total_span_padded = RegInit(0.U(32.W))
  val debug_ideal_beats_padded = WireInit(debug_total_span_padded / 8.U)
  val debug_start = RegInit(false.B)
  val debug_wasted_cycles = RegInit(0.U(32.W))

  when (io.in.fire) {
    debug_all_beats := debug_all_beats + 1.U

    debug_total_span := debug_total_span + fused_input_read_resp_span

    when (fused_input_read_resp.last) {
      debug_total_span_padded := align(debug_total_span_padded, maxReqLenInBytes, up=true)
    }.otherwise {
      debug_total_span_padded := debug_total_span_padded + fused_input_read_resp_span
    }

    debug_start := true.B
  }

  when (debug_start && !io.in.valid && io.in.ready) {
    debug_wasted_cycles := debug_wasted_cycles + 1.U
  }

  // dontTouch(debug_all_beats)
  // dontTouch(debug_total_span)
  // dontTouch(debug_ideal_beats)
  // dontTouch(debug_total_span_padded)
  // dontTouch(debug_ideal_beats_padded)
  // dontTouch(debug_start)
  // dontTouch(debug_wasted_cycles)
}
