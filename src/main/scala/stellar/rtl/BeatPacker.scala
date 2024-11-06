package stellar.rtl

import chisel3._
import chisel3.util._

import ChiselUtil._

class BeatPacker[T <: Data](elemTs: Seq[T], beatWidthInBits: Int, maxReqLenInBytes: Int, dmaBeatT: DmaBeat,
                            sramWriteReqT: ChiselSRAMWriteReq[T], nSrams: Int,
                            alignedToBytes: Int = 1, has_repeated_bias_optimizations: Boolean = false) extends Module {
  val elemTsBytes = elemTs.map(_.getWidth/8)
  val logElemTsBytes = elemTsBytes.map(log2Ceil(_))

  val elems_per_write = sramWriteReqT.data.size
  val beatWidthInBytes = beatWidthInBits / 8

  val bytesPerWrites = elemTsBytes.map(_ * elems_per_write)
  val beatWidthsInElems = elemTsBytes.map(beatWidthInBytes / _)
  require(beatWidthsInElems.forall(_ > 0), "one beat has to be able to store at least one element")

  val io = IO(new Bundle {
    val dma_beat = Flipped(Decoupled(getChiselType(dmaBeatT))) // TODO add an assertion verifying that this is irrevocable // TODO Wait, is this actually still irrevocable?
    val out = Decoupled(new Bundle {
      val write_req = getChiselType(sramWriteReqT)
      val sram_code = getChiselType(dmaBeatT.table_entry.sram_code)
    })
    val busies = Output(Vec(nSrams, Bool()))
  })

  val byte_counter = RegInit(0.U(log2Up(maxReqLenInBytes).W))

  val write_req_sram_code = Reg(getChiselType(dmaBeatT.table_entry.sram_code))

  val write_req = Reg(getChiselType(io.out.bits.write_req))
  val write_data = Reg(Vec(bytesPerWrites.max, UInt(8.W)))
  val write_req_valid = RegInit(false.B)

  write_req.from_regfile.foreach(_ := false.B)
  write_req.interleave.should_push := false.B
  write_req.interleave.should_pop := false.B
  write_req.should_trail_reads := false.B
  write_req.iteration_strides.foreach(_ := 1.U)

  val capture_dma_beat = io.out.ready || !write_req_valid
  io.dma_beat.ready := false.B

  io.out.valid := write_req_valid
  io.out.bits.write_req := write_req

  io.out.bits.write_req.data := write_data.asTypeOf(Vec(elems_per_write, elemTs.head))
  elemTs.zipWithIndex.foreach { case (elemT, sramCode) =>
    when (write_req_sram_code === sramCode.U) {
      io.out.bits.write_req.data := write_data.asTypeOf(Vec(elems_per_write, elemT))
    }
  }

  io.out.bits.sram_code := write_req_sram_code

  when (io.out.ready) {
    write_req_valid := false.B
  }

  when (io.dma_beat.valid && capture_dma_beat) {
    val dma_data = io.dma_beat.bits.data.asTypeOf(Vec(beatWidthInBytes, UInt(8.W)))
    val table_entry = io.dma_beat.bits.table_entry

    val bytes_per_write = selectFrom(bytesPerWrites.map(_.U), table_entry.sram_code)
    val log_bytes_per_write = selectFrom(bytesPerWrites.map(log2Ceil(_).U), table_entry.sram_code); assert(!write_req_valid || PopCount(bytes_per_write) === 1.U, "not a power-of-2")

    val byte_counter_max_incr = minOf(bytes_per_write, beatWidthInBytes.U); assert(!write_req_valid || bytes_per_write % beatWidthInBytes.U === 0.U || beatWidthInBytes.U % bytes_per_write === 0.U)
    val log_byte_counter_max_incr = selectFrom(bytesPerWrites.map(_ min beatWidthInBytes).map(log2Ceil(_).U), table_entry.sram_code); assert(!write_req_valid || PopCount(byte_counter_max_incr) === 1.U, "not a power-of-2")

    val logElemTBytes = selectFrom(logElemTsBytes.map(_.U), table_entry.sram_code)

    def byte_counter_to_useful_counter(bc: UInt): SInt = {
      (bc.zext -& align(table_entry.offset, alignedToBytes).zext) >> logElemTBytes
    }.asSInt

    val useful_elems_counter = byte_counter_to_useful_counter(byte_counter)

    val end_useful_elems_counter = {
      val max_end_byte_counter = {
        // The code below just calculates:
        //   align(byte_counter, byte_counter_max_incr, up=true)
        // But we made it fancier to deal with the scenario where byte_counter_max_incr is not a literal (by essentially
        // just copying the "alignTo" function and making it work with non-literals)
        byte_counter_max_incr.litOption.map(l => align(byte_counter, l.toInt, up=true)).getOrElse(
          (((byte_counter >> log_byte_counter_max_incr).asUInt +& 1.U) << log_byte_counter_max_incr).asTypeOf(UInt((byte_counter.getWidth + 1).W))
        )
      }

      val max_end = byte_counter_to_useful_counter(max_end_byte_counter)
      val aligned_end = alignS(max_end, elems_per_write) // This is to make sure that we don't pop off so many elements that we end up overwriting some of the data which we were buffering to send off to the SRAMs // TODO is there a more efficient way to make this check?
      val crosses_aligned_point = useful_elems_counter < aligned_end
      Mux(crosses_aligned_point, aligned_end, max_end)
    }

    val last_useful = end_useful_elems_counter >= table_entry.len.zext
    val finished_useful = useful_elems_counter >= table_entry.len.zext

    val out_beat_filled = last_useful || (end_useful_elems_counter > 0.S && end_useful_elems_counter.asUInt % elems_per_write.U === 0.U)
    when (out_beat_filled && !finished_useful) {
      write_req_valid := true.B
    }

    val last = io.dma_beat.bits.last && (finished_useful || last_useful)

    val byte_counter_incr = ((end_useful_elems_counter - useful_elems_counter).asUInt << logElemTBytes).asUInt
    byte_counter := byte_counter + byte_counter_incr
    when (last) {
      byte_counter := 0.U
    }

    val in_beat_fully_captured = (byte_counter +& byte_counter_incr) % beatWidthInBytes.U === 0.U
    when (in_beat_fully_captured || finished_useful || last_useful) {
      io.dma_beat.ready := true.B
    }

    connectVecs(write_req.address, table_entry.iterator_values)
    write_req.address(table_entry.axis) := (table_entry.iterator_values(table_entry.axis).zext +& alignS(floored(useful_elems_counter), elems_per_write)).asUInt
    connectVecs(write_req.data_strides, table_entry.sram_data_strides)
    connectVecOfVecsOfVecs(write_req.metadata_strides, table_entry.sram_metadata_strides)
    connectVecOfVecsOfVecs(write_req.metadata_strides_by_addr, table_entry.sram_metadata_strides_by_addr)
    if (has_repeated_bias_optimizations) {
      connectVecs(write_req.spans, table_entry.lens)
    } else {
      write_req.spans.foreach(_ := 1.U)
    }
    write_req.spans(table_entry.axis) := Mux(!last_useful || table_entry.len % elems_per_write.U === 0.U,
      elems_per_write.U, table_entry.len % elems_per_write.U)
    write_req.is_data := table_entry.is_data
    write_req.axis := table_entry.axis
    write_req.metadata_buffer_id := table_entry.metadata_buffer_id
    write_req.should_trail_reads_coarse_grained := table_entry.sram_should_trail_coarse
    write_req_sram_code := table_entry.sram_code

    // Finally, write out the data from the DMA beats into the "write_data" buffer
    val base_byte_counter = if (beatWidthInBytes == maxReqLenInBytes) 0.U else align(byte_counter, beatWidthInBytes) // Just a little bit of manual const-prop on this line
    val useful_bytes_start = (useful_elems_counter << logElemTBytes).asSInt
    val useful_bytes_end = (end_useful_elems_counter << logElemTBytes).asSInt

    dma_data.zipWithIndex.foreach { case (d, i) =>
      val dma_req_byte_addr = base_byte_counter.zext +& i.S -& align(table_entry.offset, alignedToBytes).zext

      when (dma_req_byte_addr >= 0.S && dma_req_byte_addr >= useful_bytes_start && dma_req_byte_addr < useful_bytes_end) {
        // Previously, this code-block just had these lines:
        //   val write_data_addr = dma_req_byte_addr.asUInt % bytes_per_write.U
        //   write_data(write_data_addr) := d
        // We replaced it with the more complicated, but equivalent, lines below in order to help const-prop with
        // aligned data accesses:
        write_data.zipWithIndex.foreach { case (w, j) =>
          if (i % alignedToBytes == j % alignedToBytes) {
            // when (dma_req_byte_addr.asUInt % bytes_per_write.U === j.U) {
            when ((dma_req_byte_addr.asUInt & ((1.U << log_bytes_per_write).asUInt - 1.U)) === j.U) {
              w := d
            }
          }
        }
      }
    }
  }

  io.busies.zipWithIndex.foreach { case (busy, sramCode) =>
    busy := write_req_valid && write_req_sram_code === sramCode.U
  }
  assert(!write_req_valid || write_req_sram_code < io.busies.size.U, "we're using a one-hot encoding for the busy signal right now")
}
