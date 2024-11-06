package stellar

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.ParallelTestExecution

import stellar.rtl._
import TestUtil._

class DmaTests extends AnyFreeSpec with ChiselScalatestTester with ParallelTestExecution {
  "Dense matrix DMA works" in {
    val dataWidthInBits = 32
    val beatWidthInBits = 128
    
    val dataWidthInBytes = dataWidthInBits / 8
    val elemT = SInt(dataWidthInBits.W)

    val nAxes = 2

    test(new Dma(elemT=elemT, beatWidthInBits=beatWidthInBits, nAxes=nAxes, spanBits=32, maxInFlightReqs=16, logMaxTlReqLen=6, nNonDenseAxes=0, sram_read_req_t = new ChiselSRAMReadReq(2, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers), sram_read_resp_t = new ChiselSRAMReadResp(elemT, 2, nAxes, 32, 32), sram_write_req_t = new ChiselSRAMWriteReq(elemT, 2, nAxes, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers))).withAnnotations(Seq(/*WriteVcdAnnotation*/)) { c =>
      // In this test, we read out the top left 11-by-150 submatrix from a larger 224-by-224 matrix. Matrix is stored in
      // dense row-major format

      val dram_base_addr = 1000
      val dram_rows = 224
      val dram_cols = 224
      val submatrix_rows = 11
      val submatrix_cols = 150

      assert(dram_base_addr % (dataWidthInBits/8) == 0)

      val dram_touched = ArrayBuffer.fill(dram_rows*dram_cols*dataWidthInBits/8)(false)

      resetModule(c)

      c.io.req.valid.poke(true.B)
      c.io.req.bits.axes.foreach(_.poke(FiberTreeAxis.Dense))
      c.io.req.bits.spans(0).poke(submatrix_cols.U)
      c.io.req.bits.spans(1).poke(submatrix_rows.U)
      c.io.req.bits.data_base_addrs(0).poke(dram_base_addr.U)
      c.io.req.bits.data_base_addrs(1).poke(dram_base_addr.U)
      c.io.req.bits.data_strides(1).poke(dram_cols.U)
      c.io.req.bits.write.poke(true.B)

      c.io.inflight_alloc_reqs.head.ready.poke(true.B)

      c.clock.step()
      c.io.req.valid.poke(false.B)
      c.io.tl_reqs.head.ready.poke(true.B)

      while (c.io.busy.peek().litToBoolean) {
        if (c.io.tl_reqs.head.valid.peek().litToBoolean) {
          val tl_req = c.io.tl_reqs.head.bits.peek()
          val start_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue
          val end_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue + tl_req.tableEntry.len.litValue * dataWidthInBytes

          for (i <- start_addr until end_addr) {
            assert(!dram_touched(i.toInt - dram_base_addr), s"Address ${i.toInt} has already been written to")
            dram_touched(i.toInt - dram_base_addr) = true
          }
        }

        c.clock.step()
      }

      val golden_dram_touched = ArrayBuffer.fill(dram_rows*dram_cols*dataWidthInBits/8)(false)
      for (row <- 0 until submatrix_rows)
        for (col <- 0 until submatrix_cols)
          for (byte <- 0 until dataWidthInBytes)
            golden_dram_touched((row * dram_cols + col) * dataWidthInBytes + byte) = true

      assert(dram_touched == golden_dram_touched)
    }
  }

  "CSR matrix DMA works" in {
    val dataWidthInBits = 32
    val metadataWidthInBits = 32
    val beatWidthInBits = 128

    val dataWidthInBytes = dataWidthInBits / 8
    val metadataWidthInBytes = metadataWidthInBits / 8
    val beatWidthInBytes = beatWidthInBits / 8

    val elemT = SInt(dataWidthInBits.W)
    val nAxes = 2

    val n_elems = 1024
    val n_rows = 64

    val data_base_addr = 1000
    val row_id_base_addr = 15000
    val col_id_base_addr = 30000

    val n_dram_bytes = 64000

    {
      assert(data_base_addr < row_id_base_addr && row_id_base_addr < col_id_base_addr,
        "you have to update the assertions below if you change the order of the arrays")
      assert(row_id_base_addr - data_base_addr >= n_elems * dataWidthInBytes)
      assert(col_id_base_addr - row_id_base_addr >= (n_rows+1) * metadataWidthInBytes)
      assert(n_dram_bytes - col_id_base_addr >= n_elems * metadataWidthInBytes)
    }

    val data = Seq.fill(n_elems)(randInt())

    val row_ids = 0 +: Seq.fill(n_rows-1)(randInt(max=n_elems, bias=0)).sorted :+ n_elems

    val col_ids = {
      var row_id = 0
      var last_col = -1
      for (n <- 0 until n_elems) yield {
        var next_row_start = row_ids(row_id + 1)
        while (n >= next_row_start) {
          row_id += 1
          last_col = -1

          next_row_start = row_ids(row_id + 1)
        }

        val new_col = last_col + randInt(max = 20, bias = 1)
        last_col = new_col

        new_col
      }
    }

    // Initialize the DRAM
    val dram = ArrayBuffer.fill(n_dram_bytes)(randInt(255,0))
    val golden_dram_touched = ArrayBuffer.fill(n_dram_bytes)(false)

    val arrays = Seq((row_ids, row_id_base_addr, metadataWidthInBytes),
      (col_ids, col_id_base_addr, metadataWidthInBytes),
      (data, data_base_addr, dataWidthInBytes))

    for ((array, base_addr, byteWidth) <- arrays) {
      for ((d, i) <- array.zipWithIndex) {
        val addr = base_addr + i * byteWidth
        for (b <- 0 until byteWidth) {
          dram(addr + b) = (d >> (b * 8)) & 255
          golden_dram_touched(addr + b) = true
        }
      }
    }

    test(new Dma(elemT=elemT, beatWidthInBits=beatWidthInBits, nAxes=nAxes, spanBits=32, maxInFlightReqs=16, logMaxTlReqLen=6, nNonDenseAxes=nAxes, sram_read_req_t = new ChiselSRAMReadReq(2, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers), sram_read_resp_t = new ChiselSRAMReadResp(elemT, 2, nAxes, 32, 32), sram_write_req_t = new ChiselSRAMWriteReq(elemT, 2, nAxes, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers)))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dram_touched = ArrayBuffer.fill(n_dram_bytes)(false)

      resetModule(c)

      c.io.req.valid.poke(true.B)
      c.io.req.bits.axes(0).poke(FiberTreeAxis.Compressed)
      c.io.req.bits.axes(1).poke(FiberTreeAxis.Dense)
      c.io.req.bits.spans(0).poke(maxVal(c.io.req.bits.spans(0)).U)
      c.io.req.bits.spans(1).poke(n_rows.U)
      c.io.req.bits.data_base_addrs.foreach(_.poke(data_base_addr.U))
      c.io.req.bits.data_strides(0).poke(1.U)
      c.io.req.bits.data_strides(1).poke(0.U)
      c.io.req.bits.metadata_base_addrs.foreach(_(0)(CompressedMetadata.outer_metadata_buffer_id).poke(row_id_base_addr.U))
      c.io.req.bits.metadata_base_addrs.foreach(_(0)(CompressedMetadata.inner_metadata_buffer_id).poke(col_id_base_addr.U))
      c.io.req.bits.metadata_strides(0).foreach(_.foreach(_.poke(1.U)))
      c.io.req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.req.bits.metadata_strides(1)(0)(CompressedMetadata.inner_metadata_buffer_id).poke(0.U)

      c.io.inflight_alloc_reqs.head.ready.poke(true.B)
      c.io.inflight_alloc_ids.head.poke(0.U)
      c.io.inflight_table_is_clearing.valid.poke(true.B)
      c.io.inflight_table_is_clearing.bits.poke(0.U)

      c.clock.step()
      c.io.req.valid.poke(false.B)
      c.io.tl_reqs.head.ready.poke(true.B)

      c.io.read_beats.head.valid.poke(false.B)
      c.io.read_beats.head.bits.first.poke(true.B)
      c.io.read_beats.head.bits.last.poke(true.B)
      c.io.read_beats.head.bits.table_entry.read_into_dma.poke(true.B)

      var row_id_requested = false
      var row_id_req_addr = 0
      var row_id_req_offset = 0

      while (c.io.busy.peek().litToBoolean) {
        c.io.read_beats.head.valid.poke(row_id_requested.B)
        if (row_id_requested) {
          val row_id_requested_start = (row_id_req_addr - row_id_base_addr) / metadataWidthInBytes
          val shift = -row_id_requested_start.min(0)
          val beat = row_ids.drop(row_id_requested_start).take(beatWidthInBytes / metadataWidthInBytes).
            foldRight(BigInt(0)) { (x, acc) =>
              BigInt(x) + (acc << metadataWidthInBits)
            } <<
            (shift * metadataWidthInBits)

          val b = beat & ((BigInt(1) << c.io.read_beats.head.bits.data.getWidth) - 1)
          c.io.read_beats.head.bits.data.poke(b.U)
          c.io.read_beats.head.bits.table_entry.offset.poke(row_id_req_offset.U)
        }

        if (c.io.tl_reqs.head.valid.peek().litToBoolean) {
          val tl_req = c.io.tl_reqs.head.bits.peek()

          if (tl_req.tableEntry.read_into_dma.litToBoolean) {
            row_id_requested = true
            row_id_req_addr = tl_req.addr.litValue.toInt
            row_id_req_offset = tl_req.tableEntry.offset.litValue.toInt
            assert((1 << tl_req.logLen.litValue.toInt) == beatWidthInBytes)
          } else {
            val start_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue
            val end_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue + tl_req.tableEntry.len.litValue * dataWidthInBytes

            for (i <- start_addr until end_addr) {
              dram_touched(i.toInt) = true
            }
          }
        }

        if (row_id_requested && c.io.read_beats.head.ready.peek().litToBoolean) {
          row_id_requested = false
        }

        c.clock.step()
      }

      assert(dram_touched == golden_dram_touched)
    }
  }

  "Software-managed LDL DMA works" in {
    cancel

    val dataWidthInBits = 32
    val metadataWidthInBits = 32
    val beatWidthInBits = 128

    val dataWidthInBytes = dataWidthInBits / 8
    val metadataWidthInBytes = metadataWidthInBits / 8
    val beatWidthInBytes = beatWidthInBits / 8

    val elemT = SInt(dataWidthInBits.W)
    val nAxes = 3

    val nMaxI = 8
    val nK = 16
    val nMaxJ = 32

    val data = Seq.fill(randInt(max=nMaxI, bias=0))(Seq.fill(nK)(Seq.fill(randInt(max=nMaxJ, bias=0))(randInt())))
    val i_coords = Seq.fill(data.size)(randInt(max=30, bias=1)).scanLeft(-1)(_ + _).tail
    val i_row_ids = Seq(0, i_coords.size)
    val j_coords = data.map(axisI => axisI.map(axisK => Seq.fill(axisK.size)(randInt(max=30, bias=1)).scanLeft(-1)(_ + _).tail))
    val j_row_ids = data.map(axisI => axisI.map(_.size).scanLeft(0)(_ + _))

    val i_coords_base_addr = 1000
    val i_row_ids_base_addr = 1500
    val data_base_addrs = data.indices.map(_ * 2000 + i_row_ids_base_addr + 500)
    val j_coord_base_addrs = j_coords.indices.map(_ * 2000 + data_base_addrs.max + 2500)
    val j_row_id_base_addrs = j_row_ids.indices.map(_ * 500 + j_coord_base_addrs.max + 1500)

    val n_dram_bytes = 64000

    {
      assert(i_coords_base_addr < i_row_ids_base_addr &&
        (data_base_addrs.isEmpty || i_row_ids_base_addr < data_base_addrs.min) &&
        (data_base_addrs.isEmpty || j_coord_base_addrs.isEmpty || data_base_addrs.max < j_coord_base_addrs.min) &&
        (j_coord_base_addrs.isEmpty || j_row_id_base_addrs.isEmpty || j_coord_base_addrs.max < j_row_id_base_addrs.min),
        "you have to update the assertions below if you change the order of the arrays")
      assert(i_row_ids_base_addr - i_coords_base_addr >= i_coords.size * dataWidthInBytes)
      assert(data_base_addrs.isEmpty || data_base_addrs.min - i_row_ids_base_addr >= i_row_ids.size * metadataWidthInBytes)
      assert(data_base_addrs.isEmpty || data_base_addrs.zip(data_base_addrs.tail :+ j_coord_base_addrs.min).zip(data).forall { case ((start, end), csr_mat) =>
        end - start >= csr_mat.flatten.size * dataWidthInBytes
      })
      assert(j_coord_base_addrs.isEmpty || j_coord_base_addrs.zip(j_coord_base_addrs.tail :+ j_row_id_base_addrs.min).zip(j_coords).forall { case ((start, end), csr_mat) =>
        end - start >= csr_mat.flatten.size * metadataWidthInBytes
      })
      assert(j_row_id_base_addrs.isEmpty || j_row_id_base_addrs.zip(j_row_id_base_addrs.tail :+ n_dram_bytes).zip(j_row_ids).forall { case ((start, end), row_ids) =>
        end - start >= row_ids.size * metadataWidthInBytes
      })
    }

    // Initialize the DRAM
    val dram = ArrayBuffer.fill(n_dram_bytes)(randInt(255,0))
    val golden_dram_written_to = ArrayBuffer.fill(n_dram_bytes)(false)

    {
      val arrays = Seq(
        (Seq(i_coords), Seq(i_coords_base_addr), metadataWidthInBytes),
        (Seq(i_row_ids), Seq(i_row_ids_base_addr), metadataWidthInBytes),
        (j_row_ids, j_row_id_base_addrs, metadataWidthInBytes),
        (j_coords.map(_.flatten), j_coord_base_addrs, metadataWidthInBytes),
        (data.map(_.flatten), data_base_addrs, dataWidthInBytes),
      )

      for ((mats, base_addrs, byteWidth) <- arrays) {
        for ((mat, base_addr) <- mats.zip(base_addrs)) {
          for ((d, i) <- mat.zipWithIndex) {
            val addr = base_addr + i * byteWidth
            for (b <- 0 until byteWidth) {
              dram(addr + b) = (d >> (b * 8)) & 255
              golden_dram_written_to(addr + b) = true
            }
          }
        }
      }
    }

    test(new Dma(elemT=elemT, beatWidthInBits=beatWidthInBits, nAxes=nAxes, spanBits=32, maxInFlightReqs=16, logMaxTlReqLen=6, nNonDenseAxes=nAxes, sram_read_req_t = new ChiselSRAMReadReq(3, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers), sram_read_resp_t = new ChiselSRAMReadResp(elemT, 2, nAxes, 32, 32), sram_write_req_t = new ChiselSRAMWriteReq(elemT, 2, nAxes, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers)))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dram_written_to = ArrayBuffer.fill(n_dram_bytes)(false)

      resetModule(c)

      // Read out i-coords and i-row-ids
      {
        c.io.req.valid.poke(true.B)
        c.io.req.bits.write.poke(true.B)

        c.io.req.bits.axes(0).poke(FiberTreeAxis.Dense)
        c.io.req.bits.axes(1).poke(FiberTreeAxis.Dense)
        c.io.req.bits.axes(2).poke(FiberTreeAxis.LinkedList)

        c.io.req.bits.spans(0).poke(0)
        c.io.req.bits.spans(1).poke(0)
        pokeMaxVal(c.io.req.bits.spans(2))

        c.io.req.bits.metadata_base_addrs.foreach(_(2)(LinkedListMetadata.head_ptr_buffer_id).poke(i_row_ids_base_addr))
        c.io.req.bits.metadata_base_addrs.foreach(_(2)(LinkedListMetadata.coord_buffer_id).poke(i_coords_base_addr))
        c.io.req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
        c.io.req.bits.metadata_strides(2)(2)(LinkedListMetadata.coord_buffer_id).poke(1)

        c.io.inflight_alloc_reqs.head.ready.poke(true.B)
        c.io.inflight_alloc_ids.head.poke(0.U)
        c.io.inflight_table_is_clearing.valid.poke(true.B)
        c.io.inflight_table_is_clearing.bits.poke(0.U)

        c.clock.step()
        c.io.req.valid.poke(false.B)
        c.io.tl_reqs.head.ready.poke(true.B)

        c.io.read_beats.head.valid.poke(false.B)
        c.io.read_beats.head.bits.first.poke(true.B)
        c.io.read_beats.head.bits.last.poke(true.B)
        c.io.read_beats.head.bits.table_entry.read_into_dma.poke(true.B)

        var row_id_requested = false
        var row_id_req_addr = 0
        var row_id_req_offset = 0

        while (c.io.busy.peek().litToBoolean) {
          c.io.read_beats.head.valid.poke(row_id_requested.B)
          if (row_id_requested) {
            val row_id_requested_start = (row_id_req_addr - i_row_ids_base_addr) / metadataWidthInBytes
            val shift = -row_id_requested_start.min(0)
            val beat = i_row_ids.drop(row_id_requested_start).take(beatWidthInBytes / metadataWidthInBytes).
              foldRight(BigInt(0)) { (x, acc) =>
                BigInt(x) + (acc << metadataWidthInBits)
              } <<
              (shift * metadataWidthInBits)

            val b = beat & ((BigInt(1) << c.io.read_beats.head.bits.data.getWidth) - 1)
            c.io.read_beats.head.bits.data.poke(b.U)
            c.io.read_beats.head.bits.table_entry.offset.poke(row_id_req_offset.U)
          }

          if (c.io.tl_reqs.head.valid.peek().litToBoolean) {
            val tl_req = c.io.tl_reqs.head.bits.peek()

            if (tl_req.tableEntry.read_into_dma.litToBoolean) {
              row_id_requested = true
              row_id_req_addr = tl_req.addr.litValue.toInt
              row_id_req_offset = tl_req.tableEntry.offset.litValue.toInt
              assert((1 << tl_req.logLen.litValue.toInt) == beatWidthInBytes)
              assert(!tl_req.write.litToBoolean)
            } else {
              assert(!tl_req.tableEntry.is_data.litToBoolean)

              val is_row_id = tl_req.tableEntry.metadata_buffer_id.litValue == LinkedListMetadata.head_ptr_buffer_id
              var len = tl_req.tableEntry.len.litValue
              if (!is_row_id) {
                assert(tl_req.tableEntry.iterator_values(2).litValue == 0)
                len = len min i_coords.size
              }

              val start_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue
              val end_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue + len * metadataWidthInBytes

              for (addr <- start_addr until end_addr) {
                dram_written_to(addr.toInt) = true
              }
            }
          }

          if (row_id_requested && c.io.read_beats.head.ready.peek().litToBoolean) {
            row_id_requested = false
          }

          c.clock.step()
        }
      }

      // Loop through i and read out each CSR j-coords and data matrix
      for ((((coord_base_addr, data_base_addr), row_ids_base_addr), i) <- j_coord_base_addrs.zip(data_base_addrs).zip(j_row_id_base_addrs).zipWithIndex) {
        c.io.req.valid.poke(true.B)
        c.io.req.bits.write.poke(true.B)

        c.io.req.bits.axes(0).poke(FiberTreeAxis.LinkedList)
        c.io.req.bits.axes(1).poke(FiberTreeAxis.Dense)
        c.io.req.bits.axes(2).poke(FiberTreeAxis.Dense)

        pokeMaxVal(c.io.req.bits.spans(0))
        c.io.req.bits.spans(1).poke(nK)
        c.io.req.bits.spans(2).poke(1)

        c.io.req.bits.sram_base_addrs.foreach(_.poke(0))
        c.io.req.bits.sram_base_addrs(2).poke(i)

        c.io.req.bits.sram_data_strides.foreach(_.poke(0))
        c.io.req.bits.sram_data_strides(0).poke(1)

        c.io.req.bits.sram_metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
        c.io.req.bits.sram_metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

        c.io.req.bits.metadata_base_addrs.foreach(_(0)(LinkedListMetadata.head_ptr_buffer_id).poke(row_ids_base_addr))
        c.io.req.bits.metadata_base_addrs.foreach(_(0)(LinkedListMetadata.coord_buffer_id).poke(coord_base_addr))

        c.io.req.bits.data_base_addrs.foreach(_.poke(data_base_addr))

        c.io.req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
        c.io.req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

        c.io.req.bits.data_strides.foreach(_.poke(0))
        c.io.req.bits.data_strides(0).poke(1)

        c.io.inflight_alloc_reqs.head.ready.poke(true.B)
        c.io.inflight_alloc_ids.head.poke(0.U)
        c.io.inflight_table_is_clearing.valid.poke(true.B)
        c.io.inflight_table_is_clearing.bits.poke(0.U)

        c.clock.step()
        c.io.req.valid.poke(false.B)
        c.io.tl_reqs.head.ready.poke(true.B)

        c.io.read_beats.head.valid.poke(false.B)
        c.io.read_beats.head.bits.first.poke(true.B)
        c.io.read_beats.head.bits.last.poke(true.B)
        c.io.read_beats.head.bits.table_entry.read_into_dma.poke(true.B)

        var row_id_requested = false
        var row_id_req_addr = 0
        var row_id_req_offset = 0

        while (c.io.busy.peek().litToBoolean) {
          c.io.read_beats.head.valid.poke(row_id_requested.B)
          if (row_id_requested) {
            val row_id_requested_start = (row_id_req_addr - row_ids_base_addr) / metadataWidthInBytes
            val shift = -row_id_requested_start.min(0)
            val beat = j_row_ids(i).drop(row_id_requested_start).take(beatWidthInBytes / metadataWidthInBytes).
              foldRight(BigInt(0)) { (x, acc) =>
                BigInt(x) + (acc << metadataWidthInBits)
              } <<
              (shift * metadataWidthInBits)

            val b = beat & ((BigInt(1) << c.io.read_beats.head.bits.data.getWidth) - 1)
            c.io.read_beats.head.bits.data.poke(b.U)
            c.io.read_beats.head.bits.table_entry.offset.poke(row_id_req_offset.U)
          }

          if (c.io.tl_reqs.head.valid.peek().litToBoolean) {
            val tl_req = c.io.tl_reqs.head.bits.peek()

            if (tl_req.tableEntry.read_into_dma.litToBoolean) {
              row_id_requested = true
              row_id_req_addr = tl_req.addr.litValue.toInt
              row_id_req_offset = tl_req.tableEntry.offset.litValue.toInt
              assert((1 << tl_req.logLen.litValue.toInt) == beatWidthInBytes)
              assert(!tl_req.write.litToBoolean)
            } else {
              val is_row_id = tl_req.tableEntry.metadata_buffer_id.litValue == LinkedListMetadata.head_ptr_buffer_id
              var len = tl_req.tableEntry.len.litValue
              if (!is_row_id) {
                val k = tl_req.tableEntry.iterator_values(1).litValue.toInt
                len = len min j_coords(i)(k).size
              }

              val widthInBytes = if (tl_req.tableEntry.is_data.litToBoolean) dataWidthInBytes else metadataWidthInBytes

              val start_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue
              val end_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue + len * widthInBytes

              for (addr <- start_addr until end_addr) {
                dram_written_to(addr.toInt) = true
              }
            }
          }

          if (row_id_requested && c.io.read_beats.head.ready.peek().litToBoolean) {
            row_id_requested = false
          }

          c.clock.step()
        }
      }

      assert(dram_written_to == golden_dram_written_to)
    }
  }

  def hardwareManagedDllDmaTest(isWrite: Boolean): Unit = {
    val isRead = !isWrite

    val dataWidthInBits = 32
    val metadataWidthInBits = 32
    val beatWidthInBits = 128

    val dataWidthInBytes = dataWidthInBits / 8
    val metadataWidthInBytes = metadataWidthInBits / 8
    val beatWidthInBytes = beatWidthInBits / 8

    val elemT = SInt(dataWidthInBits.W)
    val nAxes = 3

    val density = 0.5

    val nMaxI = 8 // When isWrite = false, "i" is a dense axis
    val nMaxK = 16 // When isWrite = true, "k" is a dense axis
    val nMaxJ = 32

    val aT_coords = Seq.fill(nMaxK)((0 until nMaxI).filter(_ => randBool(density)))
    val aT_row_ids = aT_coords.map(_.size).scanLeft(0)(_ + _)
    val a_coords = (0 until nMaxI).map(i => aT_coords.map(_.contains(i)).zipWithIndex.collect { case (true, k) => k})
    val i_coords = aT_coords.flatten.sorted.distinct; require(i_coords.size <= nMaxI, "too many i-coords")

    val j_coords = a_coords.map(axisK => Seq.fill(axisK.size)((0 until nMaxJ).filter(_ => randBool(density))))
    val data = j_coords.map(axisI => axisI.map(axisK => Seq.fill(axisK.size)(randInt())))
    val j_row_lens = data.map(axisI => axisI.map(axisK => axisK.size))

    val max_data_or_coord_array_size = 200

    val aT_coords_base_addr = 1000
    val aT_row_ids_base_addr = 2000
    val ik_list_ptrs_base_addr = 2512 // We choose this weird number because ik-list-ptrs' addresses need to be aligned to their size
    val data_base_addrs_min = 5000
    val data_base_addrs = data.indices.map(i => data(i).indices.map(k => data_base_addrs_min + (i * nMaxK + k) * max_data_or_coord_array_size))
    val j_coord_base_addrs_min = 40000
    val j_coord_base_addrs = j_coords.indices.map(i => j_coords(i).indices.map(k => j_coord_base_addrs_min + (i * nMaxK + k) * max_data_or_coord_array_size)) // Both "data" and "coords" need to have the same offsets from their bases

    val n_dram_bytes = 128000

    {
      def isSorted(xs: Seq[Int]) = xs == xs.sorted
      require(isSorted(Seq(aT_coords_base_addr, aT_row_ids_base_addr, ik_list_ptrs_base_addr, data_base_addrs_min,
        j_coord_base_addrs_min)), "you have to update the assertions below if you change the order of the arrays")

      require(aT_row_ids_base_addr - aT_coords_base_addr >= aT_coords.flatten.size * metadataWidthInBytes)
      require(ik_list_ptrs_base_addr - aT_row_ids_base_addr >= aT_row_ids.size * metadataWidthInBytes)
      require(data_base_addrs_min - ik_list_ptrs_base_addr >= a_coords.flatten.size * metadataWidthInBytes * 4 /* TODO magic number for size of ik-list pointer */)

      require(data_base_addrs.isEmpty || j_coord_base_addrs_min >= data_base_addrs.flatten.maxOption.getOrElse(data_base_addrs_min), s"$j_coord_base_addrs_min | ${data_base_addrs.flatten.max}")

      require({
        val addrs_and_arrays = data_base_addrs.flatten.zip(data.flatten)
        addrs_and_arrays.zip(addrs_and_arrays.drop(1).map(_._1) :+ j_coord_base_addrs_min).forall { case ((base_addr, array), end_addr) =>
          end_addr - base_addr >= array.size * dataWidthInBytes
        }
      })

      require({
        val addrs_and_arrays = j_coord_base_addrs.flatten.zip(j_coords.flatten)
        addrs_and_arrays.zip(addrs_and_arrays.drop(1).map(_._1) :+ n_dram_bytes).forall { case ((base_addr, array), end_addr) =>
          end_addr - base_addr >= array.size * metadataWidthInBytes
        }
      })
    }

    // Initialize the DRAM
    val dram = ArrayBuffer.fill(n_dram_bytes)(randInt(255,0))
    val golden_dram_touched = ArrayBuffer.fill(n_dram_bytes)(false) // This only tracks reads/writes that are meant to go into the SRAMs

    val ik_ptr_packet_size = 4
    val next_ptr_field_id = 2

    val ik_ptr_list = {
      val head_ptrs = (0 until nMaxI).map { i =>
        val valid = data_base_addrs(i).nonEmpty

        val rowlen = if (isWrite || !valid) -1 else data(i)(0).size; require(metadataWidthInBytes == 4)

        val data_offset = if (valid) (data_base_addrs(i)(0) - data_base_addrs_min) / dataWidthInBytes else 0
        val coord_offset = if (valid) (j_coord_base_addrs(i)(0) - j_coord_base_addrs_min) / metadataWidthInBytes else 0
        assert(data_offset == coord_offset)

        val next_ptr_offset = -1; require(metadataWidthInBytes == 4) // For reads, we set the 'next_ptr_offset' later in the code while we're calculating the 'non_head_ptrs'
        val last_ptr_offset = -1; require(metadataWidthInBytes == 4) // The 'last' pointer is only used during writes, so for now, I don't bother setting it for reads

        val result = ArrayBuffer(rowlen, data_offset, next_ptr_offset, last_ptr_offset)
        assert(result.size == ik_ptr_packet_size && result(next_ptr_field_id) == next_ptr_offset)
        result
      }

      import scala.math.Ordering.Implicits._
      val non_head_ptrs_with_order = j_coords.zipWithIndex.flatMap { case (axisK, i_expanded) => axisK.indices.drop(1).map { k_compressed =>
        val rowlen = if (isWrite) -1 else data(i_expanded)(k_compressed).size; require(metadataWidthInBytes == 4)

        val data_offset = (data_base_addrs(i_expanded)(k_compressed) - data_base_addrs_min) / dataWidthInBytes
        val coord_offset = (j_coord_base_addrs(i_expanded)(k_compressed) - j_coord_base_addrs_min) / metadataWidthInBytes
        assert(data_offset == coord_offset)

        val next_ptr_offset = -1; require(metadataWidthInBytes == 4)
        val last_ptr_offset = -1; require(metadataWidthInBytes == 4)

        val result = ArrayBuffer(rowlen, data_offset, next_ptr_offset, last_ptr_offset)
        assert(result.size == ik_ptr_packet_size && result(next_ptr_field_id) == next_ptr_offset)

        val k_expanded = a_coords(i_expanded)(k_compressed)
        val i_compressed = aT_coords(k_expanded).indexOf(i_expanded)
        val order = Seq(k_expanded, i_compressed)

        (order, result)
      }}.sortBy(_._1)

      if (isRead) {
        // Over here, we are basically setting the 'next_ptr' in the same way that the write-DMA would
        for (k_expanded <- 0 until nMaxK) {
          for ((i_expanded, i_compressed) <- aT_coords(k_expanded).zipWithIndex) {
            val k_compressed = a_coords(i_expanded).indexOf(k_expanded)

            val is_head = k_compressed == 0
            if (!is_head) {
              val node_pos = non_head_ptrs_with_order.indexWhere(_._1 == Seq(k_expanded, i_compressed))
              assert(node_pos >= 0)

              val prev_is_head = k_compressed == 1
              if (prev_is_head) {
                head_ptrs(i_expanded)(next_ptr_field_id) = (nMaxI - i_expanded) + node_pos
              } else {
                val prev_node_order = non_head_ptrs_with_order.indexWhere { case (Seq(k_expanded2, i_compressed2), _) =>
                  val i_expanded2 = aT_coords(k_expanded2)(i_compressed2)
                  val k_compressed2 = a_coords(i_expanded2).indexOf(k_expanded2)
                  i_expanded2 == i_expanded && k_compressed2 + 1 == k_compressed
                }
                assert(node_pos > prev_node_order && prev_node_order >= 0, s"$node_pos")
                non_head_ptrs_with_order(prev_node_order)._2(next_ptr_field_id) = node_pos - prev_node_order
              }
            }
          }
        }
      }

      val non_head_ptrs = non_head_ptrs_with_order.map(_._2) // We sort them so that their order matches the order that the Dma will access them in

      head_ptrs ++ non_head_ptrs
    }.map(_.toSeq)

    {
      case class DmaTouchDetails(row:Seq[Int], base_addr: Int, byteWidth: Int, fillInValues: Boolean, dmaWillTouch: Boolean, mask: Option[Int => Boolean])

      val aT_touches = if (isWrite) Seq(DmaTouchDetails(aT_coords.flatten, aT_coords_base_addr, metadataWidthInBytes, true, false, None),
        DmaTouchDetails(aT_row_ids, aT_row_ids_base_addr, metadataWidthInBytes, true, false, None)) else Seq.empty

      val dmaWritesAndTouches: Seq[DmaTouchDetails] = aT_touches ++ Seq(
        DmaTouchDetails(ik_ptr_list.flatten, ik_list_ptrs_base_addr, metadataWidthInBytes, true, false, None),
        DmaTouchDetails(ik_ptr_list.flatten, ik_list_ptrs_base_addr, metadataWidthInBytes, false, true, Some({ id =>
          // indicate which rowlens will be touched
          val ik_ptr_id = id / ik_ptr_packet_size
          val is_head_ptr = ik_ptr_id < nMaxI
          (!is_head_ptr || i_coords.contains(ik_ptr_id) || isRead) &&
            id % ik_ptr_packet_size == 0 })),
      ) ++ (data zip j_coords).zipWithIndex.flatMap { case (axisK, i_expanded) =>
        val (data_, j_coords_) = axisK
        data_.indices.flatMap { k_compressed =>
          Seq(DmaTouchDetails(data_(k_compressed), data_base_addrs(i_expanded)(k_compressed), dataWidthInBytes, false, true, None),
            DmaTouchDetails(j_coords_(k_compressed), j_coord_base_addrs(i_expanded)(k_compressed), metadataWidthInBytes, false, true, None))
      }}

      for (DmaTouchDetails(row, base_addr, byteWidth, fillInValues, dmaWillTouch, mask) <- dmaWritesAndTouches)
        for ((d, i) <- row.zipWithIndex) {
          if (mask.forall(foo => foo(i))) {
            val addr = base_addr + i * byteWidth
            for (b <- 0 until byteWidth) {
              if (fillInValues)
                dram(addr + b) = (d >> (b * 8)) & 255

              if (dmaWillTouch) {
                golden_dram_touched(addr + b) = true
              }
            }
          }
        }
    }

    test(new Dma(elemT=SInt(dataWidthInBits.W), beatWidthInBits=beatWidthInBits, nAxes=nAxes, spanBits=32, maxInFlightReqs=16, logMaxTlReqLen=6, nNonDenseAxes=nAxes, sram_read_req_t = new ChiselSRAMReadReq(3, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers), sram_read_resp_t = new ChiselSRAMReadResp(elemT, 2, nAxes, 32, 32), sram_write_req_t = new ChiselSRAMWriteReq(elemT, 2, nAxes, 32, 32, FiberTreeAxisMetadata.n_metadata_buffers))).withAnnotations(Seq(/*WriteVcdAnnotation*/)) { c =>
      val dram_touched = ArrayBuffer.fill(n_dram_bytes)(false)

      resetModule(c)

      c.io.req.valid.poke(true)
      c.io.req.bits.write.poke(isWrite)

      c.io.req.bits.axes(0).poke(FiberTreeAxis.LinkedList)
      c.io.req.bits.axes(1).poke(FiberTreeAxis.LinkedList)
      c.io.req.bits.axes(2).poke(FiberTreeAxis.Dense)

      pokeMaxVal(c.io.req.bits.spans(0))
      pokeMaxVal(c.io.req.bits.spans(1))
      c.io.req.bits.spans(2).poke(if (isWrite) nMaxK else nMaxI)

      c.io.req.bits.data_base_addrs.foreach(_.poke(data_base_addrs_min))

      c.io.req.bits.data_strides.foreach(_.poke(0))
      c.io.req.bits.data_strides(0).poke(1)

      c.io.req.bits.metadata_base_addrs.foreach(_(0)(LinkedListMetadata.coord_buffer_id).poke(j_coord_base_addrs_min))

      if (isWrite) {
        c.io.req.bits.metadata_base_addrs.foreach(_(1)(LinkedListMetadata.head_ptr_buffer_id).poke(aT_row_ids_base_addr))
        c.io.req.bits.metadata_base_addrs.foreach(_(1)(LinkedListMetadata.coord_buffer_id).poke(aT_coords_base_addr))
      }

      c.io.req.bits.metadata_base_addrs.foreach(_(0)(LinkedListMetadata.next_ptr_buffer_id).poke(ik_list_ptrs_base_addr))
      if (isRead)
        c.io.req.bits.metadata_base_addrs.foreach(_(1)(LinkedListMetadata.next_ptr_buffer_id).poke(ik_list_ptrs_base_addr))

      if (isWrite)
        c.io.req.bits.metadata_base_addrs.foreach(_(0)(LinkedListMetadata.last_node_len_buffer_id).poke(ik_list_ptrs_base_addr + nMaxI * ik_ptr_packet_size * metadataWidthInBytes))

      c.io.req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
      c.io.req.bits.metadata_strides_by_value.foreach(_.foreach(_.foreach(_.poke(0))))

      if (isWrite)
        c.io.req.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

      if (isWrite)
        c.io.req.bits.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id).poke(1)
      c.io.req.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id).poke(1)

      if (isRead) {
        c.io.req.bits.metadata_strides(2)(1)(LinkedListMetadata.next_ptr_buffer_id).poke(1 * ik_ptr_packet_size)
        c.io.req.bits.metadata_strides_by_value(1)(0)(LinkedListMetadata.next_ptr_buffer_id).poke(1)
      } else
        c.io.req.bits.metadata_strides_by_value(1)(0)(LinkedListMetadata.next_ptr_buffer_id).poke(1 * ik_ptr_packet_size)

      c.io.inflight_alloc_reqs.head.ready.poke(true.B)
      c.io.inflight_alloc_ids.head.poke(0.U)
      c.io.inflight_table_is_clearing.valid.poke(true.B)
      c.io.inflight_table_is_clearing.bits.poke(0.U)

      c.clock.step()
      c.io.req.valid.poke(false)
      c.io.tl_reqs.head.ready.poke(true)

      c.io.read_beats.head.valid.poke(false)
      c.io.read_beats.head.bits.first.poke(true)
      c.io.read_beats.head.bits.last.poke(true)
      c.io.read_beats.head.bits.table_entry.read_into_dma.poke(true)

      c.io.sram_read_req.ready.poke(true)

      var dram_data_requested = false
      var dram_data_req_addr = 0
      var dram_data_req_offset = 0

      while (c.io.busy.peekBoolean()) {
        c.io.read_beats.head.valid.poke(dram_data_requested)
        if (dram_data_requested) {
          val beat = dram.slice(dram_data_req_addr, dram_data_req_addr + beatWidthInBytes).foldRight(BigInt(0)) { (x, acc) =>
            x | (acc << 8)
          }

          c.io.read_beats.head.bits.data.poke(beat)
          c.io.read_beats.head.bits.table_entry.offset.poke(dram_data_req_offset)
        }

        if (c.io.tl_reqs.head.valid.peekBoolean()) {
          val tl_req = c.io.tl_reqs.head.bits.peek()

          val is_infinite = tl_req.write.litToBoolean && tl_req.tableEntry.len.litValue > 1e9.toInt

          val metadata_buffer_id = tl_req.tableEntry.metadata_buffer_id.litValue.toInt
          val axis = tl_req.tableEntry.axis.litValue.toInt

          if (tl_req.tableEntry.read_into_dma.litToBoolean) {
            assert(tl_req.tableEntry.axis.litValue.toInt <= 1, s"${tl_req.tableEntry.axis.litValue}")
            assert((1 << tl_req.logLen.litValue.toInt) == beatWidthInBytes)
            assert(!dram_data_requested, "this unit test doesn't yet support multiple dma reqs from dram happening simultaneously")

            dram_data_requested = true
            dram_data_req_addr = tl_req.addr.litValue.toInt
            dram_data_req_offset = tl_req.tableEntry.offset.litValue.toInt
          } else if (is_infinite) {
            assert(tl_req.tableEntry.axis.litValue == 0)
            assert(tl_req.tableEntry.iterator_values.drop(3).forall(_.litValue == 0))
            val Seq(0 /* j should be 0 */, i_compressed, k_expanded) = tl_req.tableEntry.iterator_values.take(3).map(_.litValue.toInt)

            val i_expanded = aT_coords(k_expanded)(i_compressed)
            val k_compressed = a_coords(i_expanded).indexOf(k_expanded)
            val len = data(i_expanded)(k_compressed).size * (if (tl_req.tableEntry.is_data.litToBoolean) dataWidthInBytes else metadataWidthInBytes)

            val start_addr = tl_req.addr.litValue.toInt + tl_req.tableEntry.offset.litValue.toInt

            if (tl_req.tableEntry.is_data.litToBoolean) {
              assert(start_addr == data_base_addrs(i_expanded)(k_compressed), s"correct addr = ${data_base_addrs(i_expanded)(k_compressed)}")
            }

            for (n <- start_addr until (start_addr + len))
              dram_touched(n) = true
          } else {
            val start_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue
            val end_addr = tl_req.addr.litValue + tl_req.tableEntry.offset.litValue + tl_req.tableEntry.len.litValue * dataWidthInBytes

            for (n <- start_addr until end_addr) {
              if (tl_req.write_from_dma.litToBoolean) {
                val mask = tl_req.mask.litValue >> tl_req.tableEntry.offset.litValue.toInt
                val wdata = tl_req.data.litValue >> (tl_req.tableEntry.offset.litValue.toInt * 8)
                val off = (n - start_addr).toInt
                if (((mask >> off) & 1).toInt == 1) {
                  dram(n.toInt) = ((wdata >> (off * 8)) & 255).toInt
                }
              } else if (axis == 0 && metadata_buffer_id == LinkedListMetadata.head_ptr_buffer_id && tl_req.write.litToBoolean) {
                // We assume here that we're writing the rowlen

                val (i_expanded, k_compressed) = if (isWrite) {
                  val Seq(_, i_compressed, k_expanded) = tl_req.tableEntry.iterator_values.take(3).map(_.litValue.toInt)

                  val i_expanded = aT_coords(k_expanded)(i_compressed)
                  val k_compressed = a_coords(i_expanded).indexOf(k_expanded)
                  (i_expanded, k_compressed)
                } else {
                  val Seq(_, k_compressed, i_expanded) = tl_req.tableEntry.iterator_values.take(3).map(_.litValue.toInt)
                  (i_expanded, k_compressed)
                }

                val rowlen = data(i_expanded)(k_compressed).size; assert(tl_req.tableEntry.len.litValue == 1, "for now, we assume that we're writing out only a single metadata value")

                val off = (n - start_addr).toInt
                if (off > 0) {
                  dram(n.toInt) = (rowlen >> (off * 8)) & 255
                }
              }

              if (!tl_req.write_from_dma.litToBoolean) {
                // For now, this unit test only tracks writes from SRAMs. In the future, we may expand it also check
                // writes that come directly from the DMA
                dram_touched(n.toInt) = true
              }
            }
          }
        }

        if (c.io.read_beats.head.ready.peek().litToBoolean) {
          dram_data_requested = false
        }

        if (c.io.sram_read_req.valid.peekBoolean())
          assert(c.io.sram_read_req.bits.reset_running_state.peekBoolean(), "we should only be making direct sram read reqs in order to reset the sram")

        c.clock.step()
      }

      assert(dram_touched == golden_dram_touched)
    }
  }

  "Hardware-managed DLL DMA writes work" in {
    /* This is for the OuterSpace-like accelerator's "matmul" phase
        for k:
          for i:
            Read expanded_i = A_coords[k][i] // TODO ideally, this would come from the SRAMs rather than from DRAM
              * A_coords[k] is the 'row_id' address of axis-i
              * A_coords[k][i] is the 'coords' address of axis-i

            Read pointer to beginning of (i,k) linked-list in DRAM
              * This is the 'next_ptr' address of axis-j, for lack of a better alternative

            for j:
              Write data[k][i][j] to DRAM
              Write coords[k][i][j] to DRAM
              Write row-len to DRAM
     */
    hardwareManagedDllDmaTest(isWrite = true)
  }

  "Hardware-managed DLL DMA reads work" in {
    /* This is for the OuterSpace-like accelerator's "merge" phase
        for i:
          for k:
            Read expanded_i = A_coords[k][i] // TODO ideally, this would come from the SRAMs rather than from DRAM
              * A_coords[k] is the 'row_id' address of axis-i
              * A_coords[k][i] is the 'coords' address of axis-i

            Read pointer to beginning of (i,k) linked-list in DRAM
              * This is the 'next_ptr' address of axis-j, for lack of a better alternative

            for j:
              Write data[i][k][j] to SRAM
              Write coords[i][k][j] to SRAM
     */
    hardwareManagedDllDmaTest(isWrite = false)
  }

  "Beat packer works" in {
    val dataWidthInBits = 32
    val beatWidthInBytes = 16
    val maxReqLenInBytes = 64
    val nAxes = 2
    val elemsPerReadWrite = 2
    val axisIteratorBits = 16
    val spanBits = 16

    val elemsInReq = 7
    val offsetInElems = 2
    /*
        Input beats: _ _ X X | X X X X | X _ _ _
        Output beats: X X X X | X X X _
    */

    assert((elemsInReq + offsetInElems) * dataWidthInBits <= maxReqLenInBytes * 8)

    val tableEntryT = new InFlightTableEntry(nAxes = nAxes, elemWidthInBytes = dataWidthInBits/8, maxReqLenInBytes = maxReqLenInBytes)
    val dmaBeatT = new DmaBeat(tableEntryT = tableEntryT, beatWidthInBits = beatWidthInBytes*8, maxInFlightReqsBits = 16)
    val writeReqT = new ChiselSRAMWriteReq(elemT = SInt(dataWidthInBits.W), elemsPerWrite = elemsPerReadWrite, nAxes = nAxes, axisIteratorBits = axisIteratorBits, spanBits = spanBits, nMetadataBuffers = FiberTreeAxisMetadata.n_metadata_buffers)

    test(new BeatPacker(elemT = SInt(dataWidthInBits.W), beatWidthInBits=beatWidthInBytes*8, maxReqLenInBytes=maxReqLenInBytes,
      dmaBeatT=dmaBeatT, sramWriteReqT=writeReqT))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dataBytes = Seq.fill(maxReqLenInBytes)(randInt(max=255, bias=0))

      val golden = dataBytes.drop(offsetInElems * dataWidthInBits/8).take(elemsInReq * dataWidthInBits/8)
        .grouped(dataWidthInBits / 8).map(_.reverse.reduce((acc,x) => (acc << 8) | x)).toSeq

      resetModule(c)

      val in_beats = dataBytes.grouped(beatWidthInBytes).toSeq
      var in_beat_counter = 0

      val n_out_beats = Math.ceil(elemsInReq.toDouble / elemsPerReadWrite).toInt
      var out_beat_counter = 0
      val result = ArrayBuffer.fill(elemsInReq)(0)

      c.io.out.ready.poke(true.B)
      while (out_beat_counter < n_out_beats || in_beat_counter < in_beats.size) {
        // Feed beats into beat-packer
        val in_beat = in_beats(in_beat_counter % in_beats.size)
        c.io.dma_beat.valid.poke((in_beat_counter < in_beats.size).B)
        c.io.dma_beat.bits.data.poke(in_beat.map(i => BigInt(i)).reverse.reduce((acc, x) => (acc << 8) | x).U)
        c.io.dma_beat.bits.last.poke((in_beat_counter == in_beats.size-1).B)
        c.io.dma_beat.bits.table_entry.offset.poke((offsetInElems * dataWidthInBits/8).U)
        c.io.dma_beat.bits.table_entry.axis.poke(0.U)
        c.io.dma_beat.bits.table_entry.is_data.poke(true.B)
        c.io.dma_beat.bits.table_entry.len.poke(elemsInReq.U)

        if (in_beat_counter < in_beats.size && c.io.dma_beat.ready.peek().litToBoolean) {
          in_beat_counter += 1
        }

        // Read beats out of beat-packer
        if (c.io.out.valid.peek().litToBoolean) {
          val span = c.io.out.bits.write_req.spans.head.peek().litValue.toInt

          for ((elem, i) <- c.io.out.bits.write_req.data.take(span).zipWithIndex) {
            result(i + out_beat_counter) = elem.peek().litValue.toInt
          }

          out_beat_counter += span
        }

        c.clock.step()
      }

      assert(result == golden)
    }
  }
}
