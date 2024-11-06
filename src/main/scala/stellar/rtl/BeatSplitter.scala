package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._

import ChiselUtil._

class BeatSplitter[T <: Data](elemT: T, beatWidthInBits: Int, maxReqLenInBytes: Int,
                              dmaBeatT: DmaBeat, dmaTLReq: DmaTLReq,
                              sramReadRespT: ChiselSRAMReadResp[T]) extends Module {
  /* TODO Currently, this module waits to buffer the entire TL req internally before sending out beats to the TL port.
          But we should be able to send the beats out even before we've finished reading out everything from the SRAMs.
  */

  val maxReqLenInElems = maxReqLenInBytes / (elemT.getWidth / 8)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val read_resp = getChiselType(sramReadRespT)
      val tl_req = getChiselType(dmaTLReq)
    }))

    val out = Decoupled(new Bundle {
      val dma_beat = getChiselType(dmaBeatT)
      val tl_req = getChiselType(dmaTLReq)
    })
  })

  val byte_counter = RegInit(0.U(log2Up(maxReqLenInBytes).W))

  val in = Reg(getChiselType(io.in.bits))
  val in_valid = RegInit(false.B)

  // TODO "in_data" should be allowed to have fewer than "maxReqLenInElems" elements
  val in_data = Reg(Vec(maxReqLenInElems, getChiselType(elemT)))
  val in_data_len = RegInit(0.U(log2Up(maxReqLenInElems+1).W))
  val in_data_full = RegEnable(io.in.bits.read_resp.last, io.in.fire)

  val len_in_bytes = 1.U << in.tl_req.logLen

  val mask = VecInit((0 until maxReqLenInBytes).map { i =>
    val actual_len_in_bytes = in_data_len * (elemT.getWidth / 8).U
    i.U >= in.tl_req.tableEntry.offset &&
      (i.U < in.tl_req.tableEntry.offset +& actual_len_in_bytes)
  })

  val last = byte_counter +& (beatWidthInBits / 8).U >= len_in_bytes
  val ending = last && io.out.fire

  io.out.valid := in_valid && in_data_full
  io.out.bits.dma_beat.data := (in_data.asUInt << (in.tl_req.tableEntry.offset * 8.U)) >> (byte_counter * 8.U)
  io.out.bits.dma_beat.mask := mask.asUInt >> byte_counter
  io.out.bits.dma_beat.table_entry := in.tl_req.tableEntry
  io.out.bits.dma_beat.first := byte_counter === 0.U
  io.out.bits.dma_beat.last := last
  io.out.bits.dma_beat.id := in.tl_req.id

  io.out.bits.tl_req := in.tl_req

  io.in.ready := !(in_valid && in_data_full) || (last && io.out.ready)

  when (io.out.fire) {
    byte_counter := byte_counter + (beatWidthInBits/8).U

    when (last) {
      in_valid := false.B
      byte_counter := 0.U
      in_data_len := 0.U
    }
  }

  when (io.in.fire) {
    in_valid := true.B
    in := io.in.bits

    val prev_data_len = Mux(ending, 0.U, in_data_len)
    val new_data_len = prev_data_len + io.in.bits.read_resp.spans.head

    in_data_len := new_data_len

    in_data.zipWithIndex.foreach { case (d, i) =>
      when (i.U >= prev_data_len) {
        val offset = i.U - prev_data_len
        d := io.in.bits.read_resp.data(offset)
      }
    }
  }

  assert(!in_valid || (in_data_len <= in.tl_req.tableEntry.len), "too many elements for tl req")
}
