package stellar

import scala.collection.mutable.{ArrayBuffer, ArrayStack}
import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.ParallelTestExecution
import stellar.rtl._
import TestUtil._
import stellar.RfEntryExitOption.{Anywhere, Incrementing, PerpendicularEdge}

class MemoryTests extends AnyFreeSpec with ChiselScalatestTester with ParallelTestExecution {
  "Reg-file works" in {
    val design = new RegFile(nElems=4, nIOCoords=2, nUpdatePorts=0, nDomainCoords=3)
    test(design.toChiselModule(nInPorts=2, nOutPorts=2)).withAnnotations(Seq(/*WriteVcdAnnotation*/)) { c =>
      val matrix = Seq(
        Seq(1, 0, 3),
        Seq(10, 22, 0),
      )

      val rows = 2
      val cols = 2

      assert(matrix.size == rows)
      assert(matrix.forall(_.count(_ != 0) == cols))

      resetModule(c)

      c.io.last_in.poke(false.B)
      c.io.last_out.poke(false.B)

      // Write values into register file
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val expandedCol = matrix(row).zipWithIndex.filter(_._1 != 0)(col)._2

          val in = c.io.ins(col)

          in.valid.poke(true.B)
          in.ready.expect(true)

          in.bits.coords(0).poke(row.S)
          in.bits.coords(1).poke(expandedCol.S)

          in.bits.element.data.poke(matrix(row)(expandedCol).S)

          in.bits.op_count.bits.poke(0.U)
        }

        if (row == rows-1)
          c.io.last_in.poke(true.B)

        c.clock.step()

        c.io.ins.foreach(_.valid.poke(false.B))
      }

      c.io.last_in.poke(false.B)

      // Read values out of register file
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val expandedCol = matrix(row).zipWithIndex.filter(_._1 != 0)(col)._2

          val out = c.io.outs(col)

          out.valid.poke(true.B)

          out.coords(0).poke(row.S)
          out.coords(1).poke(expandedCol.S)

          out.op_count.bits.poke(0.U)

          out.data.expect(matrix(row)(expandedCol).S)
          out.found.expect(true.B)
        }

        if (row == rows-1)
          c.io.last_out.poke(true.B)

        c.clock.step()

        c.io.outs.foreach(_.valid.poke(false.B))
      }

      c.io.last_out.poke(false.B)
    }
  }

  "Pipelined regfile for dense matmul works" in {
    val size = 2
    val design = new RegFile(nElems=size*size, nIOCoords=2, nUpdatePorts=0, nDomainCoords=3,
      entryOption = RfEntryExitOption.Edge, exitOption = RfEntryExitOption.Edge,
      constantCoordsForInputs = (0 until size).map(inPortId => Seq(None, Some(inPortId))), incrementingCoordsForInputs = Set((0, true)),
      maxOutCoordOpt = Some(size), coordsToIgnoreForOutputs = Set(0,1))
    test(design.toChiselModule(nInPorts=size, nOutPorts=size)).withAnnotations(Seq(/*WriteVcdAnnotation*/)) { c =>
      val matrix = Seq(
        Seq(1, 3),
        Seq(10, 22),
      )

      val rows = size
      val cols = size

      assert(matrix.size == rows)
      assert(matrix.forall(_.size == cols))

      resetModule(c)

      // Write values into register file
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val in = c.io.ins(col)

          in.valid.poke(true.B)
          in.ready.expect(true)

          in.bits.coords(0).poke(row.S)
          in.bits.coords(1).poke(col.S)

          in.bits.element.data.poke(matrix(row)(col).S)

          in.bits.op_count.bits.poke(0.U)
        }

        if (row == rows-1)
          c.io.last_in.poke(true.B)

        c.clock.step()

        c.io.ins.foreach(_.valid.poke(false.B))
      }

      c.io.last_in.poke(false.B)

      // Read values out of register file
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val out = c.io.outs(col)

          out.valid.poke(true.B)

          out.coords(0).poke(row.S)
          out.coords(1).poke(col.S)

          out.op_count.bits.poke(0.U)

          out.pop.valid.poke(true)

          out.data.expect(matrix(row)(col).S)
          out.found.expect(true.B)
        }

        if (row == rows-1)
          c.io.last_out.poke(true.B)

        c.clock.step()

        c.io.outs.foreach(_.valid.poke(false.B))
      }

      c.io.last_out.poke(false.B)
    }
  }

  "Transposed pipelined regfile for dense matmul works" in {
    val size = 2
    val design = new RegFile(nElems=size*size, nIOCoords=2, nUpdatePorts=0, nDomainCoords=3,
      entryOption = RfEntryExitOption.Edge, exitOption = RfEntryExitOption.Edge, transposingPipelines = true,
      constantCoordsForInputs = (0 until size).map(inPortId => Seq(None, Some(inPortId))), incrementingCoordsForInputs = Set((0, true)),
      maxOutCoordOpt = Some(size), coordsToIgnoreForOutputs = Set(0,1))
    test(design.toChiselModule(nInPorts=size, nOutPorts=size)).withAnnotations(Seq(/*WriteVcdAnnotation*/)) { c =>
      val matrix = Seq(
        Seq(1, 3),
        Seq(10, 22),
      )

      val rows = size
      val cols = size

      assert(matrix.size == rows)
      assert(matrix.forall(_.size == cols))

      resetModule(c)

      // Write values into register file
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val in = c.io.ins(col)

          in.valid.poke(true.B)
          in.ready.expect(true)

          in.bits.coords(0).poke(row.S)
          in.bits.coords(1).poke(col.S)

          in.bits.element.data.poke(matrix(row)(col).S)

          in.bits.op_count.bits.poke(0.U)
        }

        if (row == rows-1)
          c.io.last_in.poke(true.B)

        c.clock.step()

        c.io.ins.foreach(_.valid.poke(false.B))
      }

      c.io.last_in.poke(false.B)

      // Read values out of register file
      for (col <- 0 until cols) {
        for (row <- 0 until rows) {
          val out = c.io.outs(row)

          out.valid.poke(true.B)

          out.coords(0).poke(row.S)
          out.coords(1).poke(col.S)

          out.op_count.bits.poke(0.U)

          out.pop.valid.poke(true)

          out.data.expect(matrix(row)(col).S)
          out.found.expect(true.B)
        }

        if (col == cols-1)
          c.io.last_out.poke(true.B)

        c.clock.step()

        c.io.outs.foreach(_.valid.poke(false.B))
      }

      c.io.last_out.poke(false.B)
    }
  }

  "Queued regfile for scattered matrix merging works" in {
    {
      val seed = System.currentTimeMillis
      scala.util.Random.setSeed(seed)

      val fileWriter = new java.io.FileWriter("queued-rf-seed.txt")
      fileWriter.write(s"$seed")
      fileWriter.close()
    }

    for (exitOption <- Seq(PerpendicularEdge, Anywhere)) {
      val size = 4
      val sparsity = 0.5

      val nI = size
      val maxK = size
      val maxJ = size * 4

      val coords = Seq.fill(nI)((0 until maxK).filter(_ => randBool(sparsity)).map { k =>
        val js = (0 until maxJ).filter(_ => randBool(sparsity))
        (k, js)
      })
      val data = coords.map { axisK => axisK.map(_._2).map(axisJ => Seq.fill(axisJ.size)(randInt())) }

      val design = new RegFile(nElems = size * size, nUpdatePorts = 0, nIOCoords = 3, nDomainCoords = 3,
        subPipelineOpt = Some((size, 1, true, false)),
        entryOption = Incrementing, exitOption = exitOption
      )
      test(design.toChiselModule(nInPorts = size, nOutPorts = size)).withAnnotations(Seq(/*WriteVcdAnnotation*/)) { c =>
        c.clock.setTimeout((nI * maxK * maxJ) / size * 2 + reset_cycles)

        resetModule(c)

        var write_i = 0
        var write_k = 0
        var write_j = 0

        var read_i = 0
        val read_k = 0 // We assume that read_k will always be 0
        var read_j = 0

        while (read_i < nI) {
          c.io.ins.foreach(_.valid.poke(false))
          c.io.outs.foreach(_.valid.poke(false))
          c.io.outs.foreach(_.pop.valid.poke(false))
          c.io.last_in.poke(false)
          c.io.last_out.poke(false)

          // Write values into reg-file
          if (write_i < nI) {
            val nJ = if (coords(write_i).nonEmpty) coords(write_i)(write_k)._2.size else 0
            val n_elems = (nJ - write_j).max(0).min(size)

            val ready = c.io.ins.take(n_elems).forall(_.ready.peekBoolean())

            for (elem_id <- 0 until n_elems) {
              val in = c.io.ins(elem_id)

              in.valid.poke(ready)

              in.bits.coords(0).poke(write_i)
              in.bits.coords(1).poke(coords(write_i)(write_k)._2(write_j + elem_id))
              in.bits.coords(2).poke(coords(write_i)(write_k)._1)

              in.bits.element.domainCoords(0).poke(write_j + elem_id)
              in.bits.element.domainCoords(1).poke(write_k)
              in.bits.element.domainCoords(2).poke(write_i)

              in.bits.element.data.poke(data(write_i)(write_k)(write_j + elem_id))
            }

            if (ready) {
              write_k += 1
              if (write_k >= coords(write_i).size) {
                write_k = 0
                write_j += size
                if (write_j >= data(write_i).map(_.size).maxOption.getOrElse(0)) {
                  write_j = 0
                  write_i += 1
                  if (write_i >= nI) {
                    c.io.last_in.poke(true)
                  }
                }
              }
            }

          }

          // Read values from reg-file
          {
            // In this example, we assume that we have a coordLookup that can tell us when any value is unavailable, so we
            // only request data that we know to exist
            val nK = coords(read_i).size
            val n_elems = (nK - read_k).min(size)

            val out_is_valid = (0 until n_elems).map(elem_id => data(read_i)(read_k + elem_id).size > read_j)

            for (elem_id <- 0 until n_elems) {
              if (out_is_valid(elem_id)) {
                val out = c.io.outs(elem_id)

                out.coords(0).poke(read_i)
                out.coords(1).poke(coords(read_i)(read_k + elem_id)._2(read_j))
                out.coords(2).poke(coords(read_i)(read_k + elem_id)._1)
              }
            }

            val found = c.io.outs.zip(out_is_valid).collect { case (out, true) => out }.forall(_.found.peekBoolean())

            for (elem_id <- 0 until n_elems) {
              if (out_is_valid(elem_id) && found) {
                val out = c.io.outs(elem_id)

                out.valid.poke(true)
                out.pop.valid.poke(true)
                out.data.expect(data(read_i)(read_k + elem_id)(read_j))
                out.unavailable.expect(false)
              }
            }

            if (found) {
              assert(read_k == 0 && nK <= size, "we don't bother incrementing read_k because we assume it'll always be 0 anyways")
              read_j += 1
              val nJ = data(read_i).map(_.size).maxOption.getOrElse(0)
              if (read_j >= nJ) {
                read_j = 0
                read_i += 1
                if (read_i >= nI) {
                  c.io.last_out.poke(true)
                }
              }
            }
          }

          c.clock.step()
        }
      }
    }
  }

  "2D Dense SRAM works" in {
    val elemT = SInt(32.W)
    val nElems = 1024
    val elemsPerRead = 16
    val elemsPerWrite = 16

    test(new ChiselSRAM(elemT=elemT, nElems=nElems, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
      axes=Seq.fill(2)(FiberTreeAxis.Dense), metadatas=Seq.fill(2)(DenseMetadata))) { c =>
      val nRows = 16
      val nCols = 16

      resetModule(c)

      // Write matrix into SRAM
      val golden = Seq.fill(nRows)(Seq.fill(nCols)(randInt()))

      c.io.write.bits.from_regfile.foreach(_.poke(false.B))

      c.io.write.valid.poke(true.B)
      for (row <- 0 until nRows) {
        for (col <- 0 until nCols by elemsPerRead) {
          val n_elems_read = (nCols - col).min(elemsPerRead)
          c.io.write.bits.address(0).poke(col.U)
          c.io.write.bits.address(1).poke(row.U)

          c.io.write.bits.spans(0).poke(n_elems_read.U)
          c.io.write.bits.spans(1).poke(1.U)

          c.io.write.bits.data_strides(0).poke(1.U)
          c.io.write.bits.data_strides(1).poke(nCols.U)

          c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

          for (i <- 0 until n_elems_read)
            c.io.write.bits.data(i).poke(golden(row)(col+i).S)

          c.clock.step()
        }
      }
      c.io.write.valid.poke(false.B)

      // Read matrix out of SRAM
      val result = Seq.fill(nRows)(ArrayBuffer.fill(nCols)(0))

      c.io.read_req.valid.poke(true.B)
      c.io.read_req.bits.address.foreach(_.poke(0.U))
      c.io.read_req.bits.spans(0).poke(nCols.U)
      c.io.read_req.bits.spans(1).poke(nRows.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(nCols.U)
      c.io.read_req.bits.iteration_strides(0).poke(1.U)
      c.io.read_req.bits.iteration_strides(1).poke(1.U)

      c.clock.step()

      c.io.read_req.valid.poke(false.B)
      c.io.read_resp.ready.poke(true.B)
      while (c.io.busy.peek().litToBoolean) {
        if (c.io.read_resp.valid.peek().litToBoolean) {
          val col = c.io.read_resp.bits.compressed_address(0).peek().litValue.toInt
          val row = c.io.read_resp.bits.compressed_address(1).peek().litValue.toInt

          val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
          val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)

          val data = c.io.read_resp.bits.data
          for (c <- 0 until cols)
            result(row)(col + c) = data(c).peek().litValue.toInt
        }

        c.clock.step()
      }

      assert(result == golden, "aligned read failed")

      // Now, we attempt an unaligned read
      val nColsReadOffset = nCols / 2; assert(nCols % 2 == 0, "unaligned test expects even number of cols")

      val golden_unaligned = golden.flatten.drop(nColsReadOffset).dropRight(nColsReadOffset).grouped(nCols).toSeq
      val nRowsRead = golden_unaligned.size

      val result_unaligned = Seq.fill(nRowsRead)(ArrayBuffer.fill(nCols)(0))

      c.io.read_req.valid.poke(true.B)
      c.io.read_req.bits.address(0).poke(nColsReadOffset.U)
      c.io.read_req.bits.address(1).poke(0.U)
      c.io.read_req.bits.spans(0).poke(nCols.U)
      c.io.read_req.bits.spans(1).poke(nRowsRead.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(nCols.U)
      c.io.read_req.bits.iteration_strides(0).poke(1.U)
      c.io.read_req.bits.iteration_strides(1).poke(1.U)

      c.clock.step()

      c.io.read_req.valid.poke(false.B)
      c.io.read_resp.ready.poke(true.B)
      while (c.io.busy.peek().litToBoolean) {
        if (c.io.read_resp.valid.peek().litToBoolean) {
          val col = c.io.read_resp.bits.compressed_address(0).peek().litValue.toInt
          val row = c.io.read_resp.bits.compressed_address(1).peek().litValue.toInt

          val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
          val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)
          assert(cols == elemsPerRead, "results were read out piecemeal")

          val data = c.io.read_resp.bits.data
          for (c <- 0 until cols)
            result_unaligned(row)(col + c - nColsReadOffset) = data(c).peek().litValue.toInt
        }

        c.clock.step()
      }

      assert(golden_unaligned == result_unaligned, "unaligned read fails")
    }
  }

  "Balanced (ELL-like) 2D CSR matrix SRAM works" in {
    val elemT = SInt(32.W)

    val nElems = 1024
    val nRows = 16

    val elemsPerRead = 16
    val elemsPerWrite = 16

    test(new ChiselSRAM(elemT=elemT, nElems=nElems, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
      axes=Seq(FiberTreeAxis.Compressed, FiberTreeAxis.Dense),
      metadatas=Seq(CompressedMetadata(nOuter = nRows+1, nInnerOpt = CompressedMetadata.InnerBuffer(Some(nElems))), DenseMetadata)))
      .withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/))
    { c =>
      val nCompressedCols = 16

      resetModule(c)

      // Initialize data and coord matrices
      val golden_data = Seq.fill(nRows)(Seq.fill(nCompressedCols)(randInt()))
      val golden_coords = {
        val coords = Seq.fill(nRows)(ArrayBuffer.empty[Int])
        for (row <- 0 until nRows) {
          coords(row) += randInt(max=10, bias=1)
          for (_ <- 0 until nCompressedCols-1) {
            coords(row) += coords(row).last + randInt(max=10, bias=1)
          }
        }
        coords
      }

      // Write row-coords into SRAM
      c.io.write.valid.poke(true.B)
      c.io.write.bits.is_data.poke(false.B)
      c.io.write.bits.axis.poke(0.U)
      c.io.write.bits.metadata_buffer_id.poke(CompressedMetadata.outer_metadata_buffer_id.U)
      c.io.write.bits.spans.foreach(_.poke(1.U))
      c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

      var row_coord = 0
      for (row <- 0 until nRows+1) {
        c.io.write.bits.address(0).poke(0.U)
        c.io.write.bits.address(1).poke(row.U)

        c.io.write.bits.data(0).poke(row_coord.S)

        c.clock.step()

        if (row < golden_data.size)
          row_coord += golden_data(row).size
      }
      c.io.write.valid.poke(false.B)

      // Write col-coords into SRAM
      c.io.write.valid.poke(true.B)
      c.io.write.bits.is_data.poke(false.B)
      c.io.write.bits.axis.poke(0.U)
      c.io.write.bits.metadata_buffer_id.poke(CompressedMetadata.inner_metadata_buffer_id.U)
      c.io.write.bits.spans.tail.foreach(_.poke(1.U))
      c.io.write.bits.data_strides.foreach(_.poke(0.U))
      c.io.write.bits.data_strides(0).poke(1.U)
      c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

      for (row <- 0 until nRows) {
        var col = 0
        while (col < nCompressedCols) {
          val n_elems_read = (nCompressedCols - col).min(elemsPerRead)
          c.io.write.bits.address(0).poke(col.U)
          c.io.write.bits.address(1).poke(row.U)

          c.io.write.bits.spans(0).poke(n_elems_read.U)

          for (i <- 0 until n_elems_read)
            c.io.write.bits.data(i).poke(golden_coords(row)(col+i).S)

          if (c.io.write.ready.peekBoolean())
            col += elemsPerRead

          c.clock.step()
        }
      }
      c.io.write.valid.poke(false.B)

      // Write data matrix into SRAM
      c.io.write.valid.poke(true.B)
      c.io.write.bits.is_data.poke(true.B)
      c.io.write.bits.axis.poke(0.U)
      c.io.write.bits.spans(1).poke(1.U)
      c.io.write.bits.data_strides.foreach(_.poke(0.U))
      c.io.write.bits.data_strides(0).poke(1.U)
      c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

      for (row <- 0 until nRows) {
        var col = 0
        while (col < nCompressedCols) {
          val n_elems_read = (nCompressedCols - col).min(elemsPerRead)
          c.io.write.bits.address(0).poke(col.U)
          c.io.write.bits.address(1).poke(row.U)

          c.io.write.bits.spans(0).poke(n_elems_read.U)

          for (i <- 0 until n_elems_read)
            c.io.write.bits.data(i).poke(golden_data(row)(col+i).S)

          if (c.io.write.ready.peekBoolean())
            col += elemsPerRead

          c.clock.step()
        }
      }
      c.io.write.valid.poke(false.B)

      // Read out CSR matrix data and expanded coords
      val result_data = Seq.fill(nRows)(ArrayBuffer.fill(nCompressedCols)(0))
      val result_coords = Seq.fill(nRows)(ArrayBuffer.fill(nCompressedCols)(0))

      c.io.read_req.valid.poke(true.B)
      c.io.read_req.bits.address.foreach(_.poke(0.U))
      c.io.read_req.bits.spans(0).poke(nCompressedCols.U)
      c.io.read_req.bits.spans(1).poke(nRows.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(0.U)
      c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.read_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
      c.io.read_req.bits.is_data.poke(true.B)
      c.io.read_req.bits.to_regfile.poke(true.B)

      c.clock.step()

      c.io.read_req.valid.poke(false.B)
      c.io.read_resp.ready.poke(true.B)

      while (c.io.busy.peek().litToBoolean) {
        if (c.io.read_resp.valid.peek().litToBoolean) {
          val col = c.io.read_resp.bits.compressed_address(0).peek().litValue.toInt
          val row = c.io.read_resp.bits.compressed_address(1).peek().litValue.toInt

          val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
          val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)

          val expanded_coords = c.io.read_resp.bits.expanded_addresses

          val data = c.io.read_resp.bits.data
          for (c <- 0 until cols) {
            result_data(row)(col + c) = data(c).peek().litValue.toInt
          }

          for (c <- 0 until cols) {
            assert(expanded_coords(c)(1).peek().litValue == row, s"c=$c")
            result_coords(row)(col + c) = expanded_coords(c)(0).peek().litValue.toInt
          }
        }

        c.clock.step()
      }

      assert(result_data == golden_data, "data did not match")
      assert(result_coords == golden_coords, "coords did not match")

      // Read out inner-dim metadata (i.e. COL_ID in CSR) from matrix
      val golden_inner_dim_metadata = golden_coords
      val result_inner_dim_metadata = Seq.fill(nRows)(ArrayBuffer.fill(nCompressedCols)(0))

      c.io.read_req.valid.poke(true.B)
      c.io.read_req.bits.address.foreach(_.poke(0.U))
      c.io.read_req.bits.spans(0).poke(nCompressedCols.U)
      c.io.read_req.bits.spans(1).poke(nRows.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(0.U)
      c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.read_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
      c.io.read_req.bits.is_data.poke(false.B)
      c.io.read_req.bits.axis.poke(0.U)
      c.io.read_req.bits.metadata_buffer_id.poke(CompressedMetadata.inner_metadata_buffer_id.U)

      c.clock.step()

      c.io.read_req.valid.poke(false.B)
      c.io.read_resp.ready.poke(true.B)

      while (c.io.busy.peek().litToBoolean) {
        if (c.io.read_resp.valid.peek().litToBoolean) {
          val col = c.io.read_resp.bits.compressed_address(0).peek().litValue.toInt
          val row = c.io.read_resp.bits.compressed_address(1).peek().litValue.toInt

          val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
          val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)

          val data = c.io.read_resp.bits.data
          for (c <- 0 until cols) {
            result_inner_dim_metadata(row)(col + c) = data(c).peek().litValue.toInt
          }
        }

        c.clock.step()
      }

      assert(result_inner_dim_metadata == golden_inner_dim_metadata)

      // Read out outer-dim metadata (ROW_ID for CSR)
      val golden_outer_dim_metadata = ArrayBuffer(0)
      for (row <- golden_coords.init)
        golden_outer_dim_metadata += golden_outer_dim_metadata.last + row.size

      val result_outer_dim_metadata = ArrayBuffer.fill(nRows)(0)

      c.io.read_req.valid.poke(true.B)
      c.io.read_req.bits.address.foreach(_.poke(0.U))
      c.io.read_req.bits.spans(0).poke(nCompressedCols.U)
      c.io.read_req.bits.spans(1).poke(nRows.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(0.U)
      c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.read_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
      c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
      c.io.read_req.bits.is_data.poke(false.B)
      c.io.read_req.bits.axis.poke(0.U)
      c.io.read_req.bits.metadata_buffer_id.poke(CompressedMetadata.outer_metadata_buffer_id.U)

      c.clock.step()

      c.io.read_req.valid.poke(false.B)
      c.io.read_resp.ready.poke(true.B)

      while (c.io.busy.peek().litToBoolean) {
        if (c.io.read_resp.valid.peek().litToBoolean) {
          val row = c.io.read_resp.bits.compressed_address(1).peek().litValue.toInt
          val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)

          val data = c.io.read_resp.bits.data
          result_outer_dim_metadata(row) = data.head.peek().litValue.toInt
        }

        c.clock.step()
      }

      assert(result_outer_dim_metadata == golden_outer_dim_metadata)
    }
  }

  for (nBranches <- Seq(1, 2)) {
    ((if (nBranches > 1) "Branched u" else "U") +
    "nbalanced 2D CSR matrix SRAM works") in {
      val elemT = SInt(32.W)

      val nElems = 1024
      val nRows = if (nBranches > 1) nBranches else 128

      val elemsPerRead = 16
      val elemsPerWrite = 16

      val data = Seq.fill(nElems)(randInt())

      val max_row_size = if (nBranches > 1) 100 else 10
      val row_ids = Seq.fill(nRows-1)(randInt(max=max_row_size, bias=0)).scanLeft(0)(_ + _).map(_.min(nElems)) :+ nElems

      val col_ids = {
        var row_id = 0
        var last_col = -1
        for (n <- 0 until nElems) yield {
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

      val golden = {
        var row_id = 0
        for (n <- 0 until nElems) yield {
          var next_row_start = row_ids(row_id + 1)
          while (n >= next_row_start) {
            row_id += 1
            next_row_start = row_ids(row_id + 1)
          }

          (data(n), (row_id, col_ids(n)))
        }
      }

      test(new ChiselSRAM(elemT=elemT, nElems=nElems, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
        axes=Seq(FiberTreeAxis.Compressed, FiberTreeAxis.Dense),
        metadatas=Seq(CompressedMetadata(nOuter = nRows+1, nInnerOpt = CompressedMetadata.InnerBuffer(Some(nElems))), DenseMetadata),
        branchSizes=Seq(nBranches)))
        .withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
        resetModule(c)

        // Write row-coords into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.metadata_buffer_id.poke(CompressedMetadata.outer_metadata_buffer_id.U)
        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        for (row <- 0 until nRows+1) {
          c.io.write.bits.address(0).poke(0.U)
          c.io.write.bits.address(1).poke(row.U)

          c.io.write.bits.data(0).poke(row_ids(row).S)

          assert(c.io.write.ready.peek().litToBoolean)
          c.clock.step()
        }
        c.io.write.valid.poke(false.B)

        // Write col-coords into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.metadata_buffer_id.poke(CompressedMetadata.inner_metadata_buffer_id.U)
        c.io.write.bits.spans.tail.foreach(_.poke(1.U))
        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(0).poke(1.U)
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        for (row <- 0 until nRows) {
          val nCompressedCols = row_ids(row+1) - row_ids(row)

          var col = 0
          while (col < nCompressedCols) {
            val n_elems_written = (nCompressedCols - col).min(elemsPerWrite)
            c.io.write.bits.address(0).poke(col.U)
            c.io.write.bits.address(1).poke(row.U)

            c.io.write.bits.spans(0).poke(n_elems_written.U)

            for (i <- 0 until n_elems_written)
              c.io.write.bits.data(i).poke(col_ids(row_ids(row) + col + i).S)

            if(c.io.write.ready.peek().litToBoolean)
              col += elemsPerWrite

            c.clock.step()
          }
        }
        c.io.write.valid.poke(false.B)

        // Write data into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.is_data.poke(true.B)
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.spans(1).poke(1.U)
        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(0).poke(1.U)
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        for (row <- 0 until nRows) {
          val nCompressedCols = row_ids(row+1) - row_ids(row)

          var col = 0
          while (col < nCompressedCols) {
            val n_elems_written = (nCompressedCols - col).min(elemsPerRead)
            c.io.write.bits.address(0).poke(col.U)
            c.io.write.bits.address(1).poke(row.U)

            c.io.write.bits.spans(0).poke(n_elems_written.U)

            for (i <- 0 until n_elems_written)
              c.io.write.bits.data(i).poke(data(row_ids(row) + col + i).S)

            if(c.io.write.ready.peek().litToBoolean)
              col += elemsPerWrite

            c.clock.step()
          }
        }
        c.io.write.valid.poke(false.B)

        // Read out CSR matrix data and expanded coords
        val result = ArrayBuffer.empty[(Int, (Int, Int))]

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.address.foreach(_.poke(0.U))
        c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
        c.io.read_req.bits.spans(1).poke(nRows.U)
        c.io.read_req.bits.data_strides(0).poke(1.U)
        c.io.read_req.bits.data_strides(1).poke(0.U)
        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
        c.io.read_req.bits.is_data.poke(true.B)
        c.io.read_req.bits.to_regfile.poke(true.B)

        assert(c.io.read_req.ready.peek().litToBoolean)
        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
            val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
            assert(rows == 1)

            val data = c.io.read_resp.bits.data
            val expanded_coords = c.io.read_resp.bits.expanded_addresses

            for (c <- 0 until cols) {
              result += ((
                data(c).peek().litValue.toInt,
                (expanded_coords(c)(1).peek().litValue.toInt,
                  expanded_coords(c)(0).peek().litValue.toInt)
              ))
            }
          }

          c.clock.step()
        }

        if (nBranches == 1)
          assert(result == golden, "data and expanded coords did not match")
        else
          assert(result.toSet == golden.toSet, "data and expanded coords did not match")

        if (nBranches == 1) {
          // TODO Right now, we don't bother testing the branched SRAM outputs that are meant for the DMA, because the
          // branched SRAMs can return data out-of-order (interleaved across branches). But in the future, we should
          // test this as well, because users might want to write data from branched SRAMs into main memory

          // Read out inner-dim metadata (i.e. COL_ID in CSR) from matrix
          val result_inner_dim_metadata = ArrayBuffer.empty[Int]

          c.io.read_req.valid.poke(true.B)
          c.io.read_req.bits.address.foreach(_.poke(0.U))
          c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
          c.io.read_req.bits.spans(1).poke(nRows.U)
          c.io.read_req.bits.data_strides(0).poke(1.U)
          c.io.read_req.bits.data_strides(1).poke(0.U)
          c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          c.io.read_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
          c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
          c.io.read_req.bits.is_data.poke(false.B)
          c.io.read_req.bits.axis.poke(0.U)
          c.io.read_req.bits.metadata_buffer_id.poke(CompressedMetadata.inner_metadata_buffer_id.U)
          c.io.read_req.bits.to_regfile.poke(false.B)

          assert(c.io.read_req.ready.peek().litToBoolean)
          c.clock.step()

          c.io.read_req.valid.poke(false.B)
          c.io.read_resp.ready.poke(true.B)

          while (c.io.busy.peek().litToBoolean) {
            if (c.io.read_resp.valid.peek().litToBoolean) {
              val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
              val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
              assert(rows == 1)

              val data = c.io.read_resp.bits.data
              for (c <- 0 until cols) {
                result_inner_dim_metadata += data(c).peek().litValue.toInt
              }
            }

            c.clock.step()
          }

          assert(result_inner_dim_metadata == col_ids, "col ids are incorrect")

          // Read out outer-dim metadata (ROW_ID for CSR)
          val result_outer_dim_metadata = ArrayBuffer.empty[Int]

          c.io.read_req.valid.poke(true.B)
          c.io.read_req.bits.address.foreach(_.poke(0.U))
          c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
          c.io.read_req.bits.spans(1).poke((nRows + 1).U)
          c.io.read_req.bits.data_strides(0).poke(1.U)
          c.io.read_req.bits.data_strides(1).poke(0.U)
          c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          c.io.read_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
          c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
          c.io.read_req.bits.is_data.poke(false.B)
          c.io.read_req.bits.axis.poke(0.U)
          c.io.read_req.bits.metadata_buffer_id.poke(CompressedMetadata.outer_metadata_buffer_id.U)
          c.io.read_req.bits.to_regfile.poke(false.B)

          assert(c.io.read_req.ready.peek().litToBoolean)
          c.clock.step()

          c.io.read_req.valid.poke(false.B)
          c.io.read_resp.ready.poke(true.B)

          while (c.io.busy.peek().litToBoolean) {
            if (c.io.read_resp.valid.peek().litToBoolean) {
              val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
              assert(rows == 1)

              val data = c.io.read_resp.bits.data
              result_outer_dim_metadata += data.head.peek().litValue.toInt
            }

            c.clock.step()
          }

          assert(result_outer_dim_metadata == row_ids, "row ids are incorrect")
        }
      }
    }
  }

  "Linked-list SRAM works" in {
    val elemT = SInt(32.W)

    val nRows = 64
    val maxNCols = 128

    val elemsPerRead = 16
    val elemsPerWrite = 16

    val node_size = 16

    val data = Seq.fill(nRows)(Seq.fill(randInt(max=maxNCols, bias=0))(randInt()))
    val coords = data.map(row => Seq.fill(row.size)(randInt(max=30, bias=1)).scanLeft(-1)(_ + _).tail)

    val row_ids = coords.map(_.size).scanLeft(0)(_ + _)

    val nSramElems = nRows*maxNCols.max(node_size)

    test(new ChiselSRAM(elemT=elemT, nElems=nSramElems, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
      axes=Seq(FiberTreeAxis.LinkedList, FiberTreeAxis.Dense),
      metadatas=Seq(LinkedListMetadata(nRows, (nRows*maxNCols.max(node_size))/node_size, node_size), DenseMetadata)))
      .withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/))
      { c =>
        c.clock.setTimeout(50000)
        resetModule(c)

        // Write head pointers into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.spans(1).poke(nRows.U)
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        assert(c.io.write.ready.peek().litToBoolean)
        c.clock.step()
        c.io.write.valid.poke(false.B)

        // Write data and coords into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.spans(1).poke(1.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(0).poke(1.U)

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        for ((mat, is_data) <- Seq((data, true), (coords, false))) {
          c.io.write.bits.is_data.poke(is_data.B)

          for (row <- 0 until nRows) {
            val nCompressedCols = mat(row).size

            var col = 0
            while (col < nCompressedCols) {
              val n_elems_written = (nCompressedCols - col).min(elemsPerWrite)
              c.io.write.bits.address(0).poke(col.U)
              c.io.write.bits.address(1).poke(row.U)

              c.io.write.bits.spans(0).poke(n_elems_written.U)

              for (i <- 0 until n_elems_written)
                c.io.write.bits.data(i).poke(mat(row)(col+i).S)

              if(c.io.write.ready.peek().litToBoolean)
                col += elemsPerWrite

              c.clock.step()
            }
          }
        }

        c.io.write.valid.poke(false.B)

        // Wait for writes to complete
        while (c.io.busy.peek().litToBoolean) {
          c.clock.step()
        }

        // Read coords and row_ids out of SRAM
        val result_coords = Seq.fill(data.size)(ArrayBuffer.empty[Int])
        val result_row_ids = ArrayBuffer.fill(data.size+1)(-1)

        c.io.read_req.bits.axis.poke(0.U)
        c.io.read_req.bits.is_data.poke(false.B)
        c.io.read_req.bits.to_regfile.poke(false.B)
        c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
        c.io.read_req.bits.spans(1).poke(1.U)
        c.io.read_req.bits.address(0).poke(0.U)
        c.io.read_req.bits.data_strides(0).poke(1.U)
        c.io.read_req.bits.data_strides(1).poke(0.U)
        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.read_resp.ready.poke(true.B)

        var req_row = 0
        var requested_row_id = false
        while (req_row < nRows || req_row == nRows && !requested_row_id || c.io.busy.peek().litToBoolean) {
          c.io.read_req.valid.poke((req_row < nRows || req_row == nRows && !requested_row_id).B)
          c.io.read_req.bits.address(1).poke(req_row.U)
          if (requested_row_id)
            c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
          else
            c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

          if (c.io.read_req.ready.peek().litToBoolean) {
            if (requested_row_id) {
              req_row += 1
              requested_row_id = false
            } else
              requested_row_id = true
          }

          if (c.io.read_resp.valid.peek().litToBoolean) {
            if (c.io.read_resp.bits.metadata_buffer_id.peek().litValue == LinkedListMetadata.coord_buffer_id) {
              val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
              val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
              assert(rows == 1)

              val data = c.io.read_resp.bits.data
              val expanded_coords = c.io.read_resp.bits.expanded_addresses

              for (c <- 0 until cols) {
                val row = expanded_coords(c)(1).peek().litValue.toInt
                result_coords(row) += data(c).peek().litValue.toInt
              }
            } else {
              val row = c.io.read_resp.bits.compressed_address(1).peek().litValue
              assert(c.io.read_resp.bits.spans.peek().forall(_.litValue == 1))
              result_row_ids(row.toInt) = c.io.read_resp.bits.data.head.peek().litValue.toInt
            }
          }

          c.clock.step()
        }

        // Read data out of SRAM
        val result_data = Seq.fill(data.size)(ArrayBuffer.empty[Int])

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.is_data.poke(true.B)
        c.io.read_req.bits.address.foreach(_.poke(0.U))
        c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
        c.io.read_req.bits.spans(1).poke(nRows.U)
        c.io.read_req.bits.data_strides(0).poke(1.U)
        c.io.read_req.bits.data_strides(1).poke(0.U)
        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        assert(c.io.read_req.ready.peek().litToBoolean)
        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
            val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
            assert(rows == 1)

            val data = c.io.read_resp.bits.data
            val expanded_coords = c.io.read_resp.bits.expanded_addresses

            for (c <- 0 until cols) {
              val row = expanded_coords(c)(1).peek().litValue.toInt
              result_data(row) += data(c).peek().litValue.toInt
            }
          }

          c.clock.step()
        }

        assert(result_data == data, "data doesn't match")
        assert(result_coords == coords, "expanded coords don't match")
        assert(result_row_ids == row_ids, "row-ids don't match")
      }
  }

  "Branched linked-list sram writes from regfile correctly" in {
    val elemT = SInt(32.W)

    val elemsPerRead = 16
    val elemsPerWrite = 16

    val node_size = 16

    val nSramElems = 8 * 1024
    val nBranches = 2
    val nMaxCols = 128

    val data = Seq.fill(nBranches)(Seq.fill(randInt(max=nMaxCols, bias=0))(randInt()))
    val coords = data.map(_.size).map { nCols =>
      Seq.fill(nCols)(randInt(max=128, bias=1)).scanLeft(-1)(_ + _).tail
    }

    val row_ids = coords.map(_.size).scanLeft(0)(_ + _)

    test(new ChiselSRAM(elemT=elemT, nElems=nSramElems, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
      axes=Seq(FiberTreeAxis.LinkedList, FiberTreeAxis.Dense),
      metadatas=Seq(LinkedListMetadata(nHeads=nSramElems, nNodes=nSramElems/node_size, nodeSize=node_size), DenseMetadata), // TODO we don't need nearly this much metadata storage
      branchSizes=Seq(nBranches),
    )).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/))
    { c =>
      resetModule(c)

      // Write head pointers into SRAM
      c.io.write.valid.poke(true.B)
      c.io.write.bits.is_data.poke(false.B)
      c.io.write.bits.address.foreach(_.poke(0.U))
      c.io.write.bits.axis.poke(0.U)
      c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
      c.io.write.bits.spans.foreach(_.poke(1.U))
      c.io.write.bits.spans(1).poke(nBranches.U)
      c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
      c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

      c.io.write.ready.expect(true.B)
      c.clock.step()
      c.io.write.valid.poke(false.B)

      // Make request to write from reg-file into SRAM
      c.io.write.valid.poke(true.B)
      c.io.write.bits.is_data.poke(true.B)
      c.io.write.bits.from_regfile(0).poke(true.B)
      c.io.write.bits.from_regfile_last_axis.poke(1.U)
      c.io.write.bits.address.foreach(_.poke(0.U))
      c.io.write.bits.spans(0).poke(maxVal(c.io.write.bits.spans(0)).U)
      c.io.write.bits.spans(1).poke(nBranches.U)

      c.io.write.bits.data_strides(0).poke(1.U)
      c.io.write.bits.data_strides.tail.foreach(_.poke(0.U))

      c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)

      c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

      c.io.write.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false.B)))
      c.io.write.bits.from_regfile_metadata.head(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
      c.io.write.bits.from_regfile_metadata.head(LinkedListMetadata.coord_buffer_id).coord.poke(0.U)

      while (!c.io.write.ready.peek().litToBoolean)
        c.clock.step()
      c.clock.step()
      c.io.write.valid.poke(false.B)

      // Gradually write reg-file entries into SRAM
      val writtenCols = ArrayBuffer.fill(nBranches)(0)
      var last_rf_access = false
      while (c.io.busy.peek().litToBoolean) {
        c.io.write_from_regfile_reqs.head.ready.poke(false.B)

        if (c.io.write_from_regfile_reqs.head.valid.peek().litToBoolean) {
          assert(!last_rf_access, "making more rf accesses after the last one")

          val col = c.io.write_from_regfile_reqs.head.bits.req.address(0).peek().litValue.toInt
          val row = c.io.write_from_regfile_reqs.head.bits.req.address(1).peek().litValue.toInt

          assert(col == writtenCols(row), "in this test, we're assuming that each row is written out sequentially")

          val notRunningAhead = writtenCols.zip(data.map(_.size)).forall { case (written, size) =>
            written >= writtenCols(row) || written >= size
          }

          if (notRunningAhead) {
            val nCols = c.io.write_from_regfile_reqs.head.bits.req.spans.head.peek().litValue.toInt.min(data(row).size - col)

            c.io.write_from_regfile_resps.head.found.foreach(_.poke(false.B))
            for (coff <- 0 until nCols) {
              c.io.write_from_regfile_resps.head.found(coff).poke(true.B)
              c.io.write_from_regfile_resps.head.data(coff).poke(data(row)(col + coff).S)
              c.io.write_from_regfile_resps.head.metadata(LinkedListMetadata.coord_buffer_id)(coff).poke(coords(row)(col + coff).U)
            }

            writtenCols(row) += nCols

            c.io.write_from_regfile_reqs.head.ready.poke(true.B)

            last_rf_access = c.io.write_from_regfile_reqs.head.bits.last.peek().litToBoolean
          }
        }

        c.clock.step()
      }

      // Read coords and row_ids out of SRAM
      val result_coords = Seq.fill(data.size)(ArrayBuffer.empty[Int])
      val result_row_ids = ArrayBuffer.fill(data.size+1)(-1)

      c.io.read_req.bits.axis.poke(0.U)
      c.io.read_req.bits.is_data.poke(false.B)
      c.io.read_req.bits.to_regfile.poke(false.B)
      c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
      c.io.read_req.bits.spans(1).poke(1.U)
      c.io.read_req.bits.address(0).poke(0.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(0.U)
      c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
      c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

      c.io.read_resp.ready.poke(true.B)

      var req_row = 0
      var requested_row_id = false
      while (req_row < nBranches || req_row == nBranches && !requested_row_id || c.io.busy.peek().litToBoolean) {
        c.io.read_req.valid.poke((req_row < nBranches || req_row == nBranches && !requested_row_id).B)
        c.io.read_req.bits.address(1).poke(req_row.U)
        if (requested_row_id)
          c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
        else
          c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

        if (c.io.read_req.ready.peek().litToBoolean) {
          if (requested_row_id) {
            req_row += 1
            requested_row_id = false
          } else
            requested_row_id = true
        }

        if (c.io.read_resp.valid.peek().litToBoolean) {
          if (c.io.read_resp.bits.metadata_buffer_id.peek().litValue == LinkedListMetadata.coord_buffer_id) {
            val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
            val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
            assert(rows == 1)

            val data = c.io.read_resp.bits.data
            val expanded_coords = c.io.read_resp.bits.expanded_addresses

            for (c <- 0 until cols) {
              val row = expanded_coords(c)(1).peek().litValue.toInt
              result_coords(row) += data(c).peek().litValue.toInt
            }
          } else {
            val row = c.io.read_resp.bits.compressed_address(1).peek().litValue
            assert(c.io.read_resp.bits.spans.peek().forall(_.litValue == 1), s"Spans were: ${c.io.read_resp.bits.spans.map(_.peekInt())}")
            result_row_ids(row.toInt) = c.io.read_resp.bits.data.head.peek().litValue.toInt
          }
        }

        c.clock.step()
      }

      // Read data and coords out of SRAM
      val result_data = Seq.fill(data.size)(ArrayBuffer.empty[Int])

      c.io.read_req.valid.poke(true.B)
      c.io.read_req.bits.is_data.poke(true.B)
      c.io.read_req.bits.axis.poke(0.U)
      c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
      c.io.read_req.bits.to_regfile.poke(false.B)
      c.io.read_req.bits.address.foreach(_.poke(0.U))
      c.io.read_req.bits.spans(0).poke(maxVal(c.io.read_req.bits.spans(0)).U)
      c.io.read_req.bits.spans(1).poke(nBranches.U)
      c.io.read_req.bits.data_strides(0).poke(1.U)
      c.io.read_req.bits.data_strides(1).poke(0.U)
      c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
      c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
      c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

      c.io.read_req.ready.expect(true.B)
      c.clock.step()

      c.io.read_req.valid.poke(false.B)
      c.io.read_resp.ready.poke(true.B)

      while (c.io.busy.peek().litToBoolean) {
        if (c.io.read_resp.valid.peek().litToBoolean) {
          val cols = c.io.read_resp.bits.spans(0).peek().litValue.toInt
          val rows = c.io.read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)

          val data = c.io.read_resp.bits.data
          val expanded_coords = c.io.read_resp.bits.expanded_addresses

          for (c <- 0 until cols) {
            val row = expanded_coords(c)(1).peek().litValue.toInt
            result_data(row) += data(c).peek().litValue.toInt
          }
        }

        c.clock.step()
      }

      assert(result_row_ids == row_ids, "row-ids don't match")
      assert(result_coords == coords, "coords don't match")
      assert(result_data == data, "data doesn't match")
    }
  }

  "LDL sram works" in {
    // This SRAM is meant to store the outputs of an outer-space like matmul array. The outputs are indexed as
    //   [i'][k][j'], with j being the innermost element, where i=f(i',k) and j=g(j',k).
    val elemT = SInt(32.W)

    val nMaxI = 32
    val nK = 8
    val nMaxJ = 32

    val elemsPerRead = 16
    val elemsPerWrite = 16

    val node_size = 16

    val data = Seq.fill(randInt(max=nMaxI, bias=0))(Seq.fill(nK)(Seq.fill(randInt(max=nMaxJ, bias=0))(randInt())))
    val i_coords = Seq.fill(data.size)(randInt(max=30, bias=1)).scanLeft(-1)(_ + _).tail
    val j_coords = data.map(axisI => axisI.map(axisK => Seq.fill(axisK.size)(randInt(max=30, bias=1)).scanLeft(-1)(_ + _).tail))
    val j_row_ids = data.map(axisI => axisI.map(_.size).scanLeft(0)(_ + _))

    val nSramElems = nMaxI*nK*nMaxJ.max(node_size)

    test(new ChiselSRAM(elemT=elemT, nElems=nSramElems, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
      axes=Seq(FiberTreeAxis.LinkedList, FiberTreeAxis.Dense, FiberTreeAxis.LinkedList),
      metadatas=Seq(
        LinkedListMetadata(nMaxI*nK, (nMaxI*nK*nMaxJ.max(node_size))/node_size, node_size),
        DenseMetadata,
        LinkedListMetadata(1, nMaxI.max(node_size)/node_size, node_size, nCoordsOpt = CompressedMetadata.InnerBuffer(Some(nMaxI))))
    )).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
      c.clock.setTimeout(50000)

      resetModule(c)

      // Write i coords
      {
        // Write head pointers into SRAM
        /* TODO Since there's really just one head-pointer, this step shouldn't be necessary. This head-pointer should
                always just be set to 0 */
        c.io.write.valid.poke(true.B)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.from_regfile.foreach(_.poke(false.B))
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(2.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        assert(c.io.write.ready.peek().litToBoolean)
        c.clock.step()
        c.io.write.valid.poke(false.B)

        // Write data and coords into SRAM
        c.io.write.valid.poke(true.B)

        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(2).poke(1.U)

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.write.bits.is_data.poke(false.B)

        var i = 0
        while (i < i_coords.size) {
          val n_elems_written = (i_coords.size - i).min(elemsPerWrite)
          c.io.write.bits.address(2).poke(i.U)
          c.io.write.bits.spans(2).poke(n_elems_written.U)

          for ((elem, port) <- i_coords.drop(i).zip(c.io.write.bits.data))
            port.poke(elem.S)

          if (c.io.write.ready.peek().litToBoolean)
            i += elemsPerWrite

          c.clock.step()
        }

        c.io.write.valid.poke(false.B)

        // Wait for writes to complete
        while (c.io.busy.peek().litToBoolean) {
          c.clock.step()
        }
      }

      // Write data and j_coords
      {
        // Write head pointers into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.spans(1).poke(nK.U)
        pokeMaxVal(c.io.write.bits.spans(2))

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(nK.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.write.ready.expect(true.B)
        c.clock.step()
        c.io.write.valid.poke(false.B)

        // Write data and coords into SRAM
        c.io.write.valid.poke(true.B)
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(0).poke(1.U)

        for ((mat, is_data) <- Seq((data, true), (j_coords, false))) {
          c.io.write.bits.is_data.poke(is_data.B)

          for (i <- i_coords.indices) {
            for (k <- 0 until nK) {
              val nJ = mat(i)(k).size

              var j = 0
              while (j < nJ) {
                val n_elems_written = (nJ - j).min(elemsPerWrite)
                c.io.write.bits.address(0).poke(j.U)
                c.io.write.bits.address(1).poke(k.U)
                c.io.write.bits.address(2).poke(i.U)

                c.io.write.bits.spans(0).poke(n_elems_written.U)

                for (elem_id <- 0 until n_elems_written)
                  c.io.write.bits.data(elem_id).poke(mat(i)(k)(j+elem_id).S)

                if (c.io.write.ready.peek().litToBoolean) {
                  j += elemsPerWrite
                }

                c.clock.step()
              }
            }
          }
        }

        c.io.write.valid.poke(false.B)

        // Wait for writes to complete
        while (c.io.busy.peek().litToBoolean) {
          c.clock.step()
        }
      }

      // Read out i coords
      {
        // Read out the coord
        val result_coords = ArrayBuffer.fill(i_coords.size)(-1)

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.axis.poke(2.U)
        c.io.read_req.bits.is_data.poke(false.B)
        c.io.read_req.bits.to_regfile.poke(false.B)
        c.io.read_req.bits.spans.foreach(_.poke(1.U))
        pokeMaxVal(c.io.read_req.bits.spans(2))
        c.io.read_req.bits.address.foreach(_.poke(0.U))
        c.io.read_req.bits.data_strides.foreach(_.poke(0.U))
        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))
        c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

        c.io.read_req.ready.expect(true.B)

        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            val n_elems = c.io.read_resp.bits.spans(2).peek().litValue.toInt
            assert(c.io.read_resp.bits.spans.init.forall(_.peek().litValue.toInt <= 1), "not reading out a 1d vector")

            val data = c.io.read_resp.bits.data
            val addr = c.io.read_resp.bits.compressed_address(2).peek().litValue.toInt

            for (elem_id <- 0 until n_elems) {
              result_coords(addr + elem_id) = data(elem_id).peek().litValue.toInt
            }
          }

          c.clock.step()
        }

        c.io.read_resp.ready.poke(true.B)

        assert(result_coords == i_coords, "i_coords aren't correct")

        // Read out the length of the i-coords
        var result_coord_len = -1
        var result_coord_len_written = false

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.spans(2).poke(1)
        c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            assert(!result_coord_len_written)
            assert(c.io.read_resp.bits.spans(2).peek().litValue.toInt == 1)
            result_coord_len = c.io.read_resp.bits.data.head.peek().litValue.toInt
            result_coord_len_written = true
          }

          c.clock.step()
        }

        c.io.read_resp.ready.poke(false.B)

        assert(result_coord_len == i_coords.size, "length of i_coords is incorrect")
      }

      // Read out j_coords and j_row_ids
      {
        val result_coords = j_coords.map(_.map(x => ArrayBuffer.fill(x.size)(-1)))
        val result_row_ids = j_row_ids.map(x => ArrayBuffer.fill(x.size)(-1))

        c.io.read_req.bits.axis.poke(0.U)
        c.io.read_req.bits.is_data.poke(false.B)
        c.io.read_req.bits.to_regfile.poke(false.B)

        pokeMaxVal(c.io.read_req.bits.spans(0))
        c.io.read_req.bits.spans(1).poke(1.U)
        c.io.read_req.bits.spans(2).poke(1.U)

        c.io.read_req.bits.data_strides.foreach(_.poke(0.U))
        c.io.read_req.bits.data_strides(0).poke(1.U)

        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.read_req.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(nK.U)

        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.read_req.bits.reset_running_state.poke(false)

        c.io.read_resp.ready.poke(true.B)

        var i = 0
        var k = 0
        var requested_row_id = false
        var resetted_running_len = true
        while (i < i_coords.size || c.io.busy.peek().litToBoolean) {
          c.io.read_req.valid.poke((i < i_coords.size).B)
          c.io.read_req.bits.address(0).poke(0.U)
          c.io.read_req.bits.address(1).poke(k.U)
          c.io.read_req.bits.address(2).poke(i.U)
          if (requested_row_id)
            c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
          else
            c.io.read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
          c.io.read_req.bits.reset_running_state.poke(!resetted_running_len)

          if (c.io.read_req.ready.peek().litToBoolean) {
            if (!resetted_running_len) {
              resetted_running_len = true
            } else if (requested_row_id) {
              k += 1
              requested_row_id = false
            } else {
              assert(k <= nK)
              if (k == nK) {
                k = 0
                i += 1
                resetted_running_len = false
              } else {
                requested_row_id = true
              }
            }
          }

          if (c.io.read_resp.valid.peek().litToBoolean) {
            val data = c.io.read_resp.bits.data.map(_.peek().litValue.toInt)
            val addr = c.io.read_resp.bits.compressed_address.map(_.peek().litValue.toInt)
            val spans = c.io.read_resp.bits.spans.map(_.peek().litValue.toInt)
            val Seq(j_, k_, i_) = addr

            if (c.io.read_resp.bits.metadata_buffer_id.peek().litValue == LinkedListMetadata.coord_buffer_id) {
              val n_elems = spans.head
              assert(spans.tail.forall(_ == 1), s"spans are $spans")

              for (elem_id <- 0 until n_elems)
                result_coords(i_)(k_)(j_ + elem_id) = data(elem_id)
            } else {
              assert(spans.forall(_ == 1), s"spans are $spans")
              result_row_ids(i_)(k_) = data.head
            }
          }

          c.clock.step()
        }

        assert(result_coords == j_coords, "j_coords are incorrect")
        assert(result_row_ids == j_row_ids, "j_row_ids are incorrect")
      }

      // Read out data to regfile
      {
        val result_data = data.map(_.map(x => ArrayBuffer.fill(x.size)(-1000000)))
        val result_coords = j_coords.map(_.map(x => ArrayBuffer.fill(x.size)(Seq.empty[Int])))

        val golden_coords: Seq[Seq[Seq[Seq[Int]]]] = for ((i, i_comp) <- i_coords.zipWithIndex) yield
          for (k <- 0 until nK) yield
            for (j <- j_coords(i_comp)(k)) yield
              Seq(i, k, j)

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.to_regfile.poke(true)
        c.io.read_req.bits.is_data.poke(true.B)
        c.io.read_req.bits.address.foreach(_.poke(0.U))

        pokeMaxVal(c.io.read_req.bits.spans(0))
        c.io.read_req.bits.spans(1).poke(nK)
        pokeMaxVal(c.io.read_req.bits.spans(2))

        c.io.read_req.bits.data_strides.foreach(_.poke(0.U))
        c.io.read_req.bits.data_strides(0).poke(1.U)

        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.read_req.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(nK.U)

        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.read_req.ready.expect(true)
        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            val spans = c.io.read_resp.bits.spans.map(_.peekInt().toInt)
            val n_elems = spans.head

            val data = c.io.read_resp.bits.data
            val expanded_coords = c.io.read_resp.bits.expanded_addresses

            val Seq(j, k, i) = c.io.read_resp.bits.compressed_address.map(_.peekInt().toInt)

            assert(spans.tail.forall(_ == 1), s"spans = $spans")

            for (elem_id <- 0 until n_elems) {
              result_data(i)(k)(j + elem_id) = data(elem_id).peekInt().toInt
              result_coords(i)(k)(j + elem_id) = expanded_coords(elem_id).map(_.peekInt().toInt).reverse
            }
          }

          c.clock.step()
        }

        assert(result_data == data, "data doesn't match")
        assert(result_coords == golden_coords, "expanded coords don't match")
      }
    }
  }

  "LDL SRAM writes from regfile correctly" in {
    {
      val seed = System.currentTimeMillis
      scala.util.Random.setSeed(seed)

      val fileWriter = new java.io.FileWriter("ldl-sram-from-rf-seed.txt")
      fileWriter.write(s"$seed")
      fileWriter.close()
    }

    val elemT = SInt(32.W)

    val nMaxI = 32
    val nK = 8
    val nMaxJ = 32

    val elemsPerRead = 16
    val elemsPerWrite = 16

    val node_size = 16

    val data = Seq.fill(randInt(max = nMaxI, bias = 0))(Seq.fill(nK)(Seq.fill(randInt(max = nMaxJ, bias = 0))(randInt())))
    val i_coords = Seq.fill(data.size)(randInt(max = 30, bias = 1)).scanLeft(-1)(_ + _).tail
    val j_coords = data.map(axisI => axisI.map(axisK => Seq.fill(axisK.size)(randInt(max = 30, bias = 1)).scanLeft(-1)(_ + _).tail))
    val j_row_ids = data.map(axisI => axisI.map(_.size).scanLeft(0)(_ + _))

    val golden_coords: Seq[Seq[Seq[Seq[Int]]]] = for ((i, i_comp) <- i_coords.zipWithIndex) yield
      for (k <- 0 until nK) yield
        for (j <- j_coords(i_comp)(k)) yield
          Seq(i, k, j)

    val nSramElems = nMaxI * nK * nMaxJ.max(node_size)

    test(new ChiselSRAM(elemT = elemT, nElems = nSramElems, elemsPerRead = elemsPerRead, elemsPerWrite = elemsPerWrite,
      axes = Seq(FiberTreeAxis.LinkedList, FiberTreeAxis.Dense, FiberTreeAxis.LinkedList),
      metadatas = Seq(
        LinkedListMetadata(nMaxI * nK, (nMaxI * nK * nMaxJ.max(node_size)) / node_size, node_size),
        DenseMetadata,
        LinkedListMetadata(1, nMaxI.max(node_size) / node_size, node_size, nCoordsOpt = CompressedMetadata.InnerBuffer(Some(nMaxI)))),
    )).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
      c.clock.setTimeout(50000)

      resetModule(c)

      // Initialize SRAM (from dma)
      {
        // Init i head pointer
        def init_i_head_pointer(): Unit = {
          /* TODO Since there's really just one head-pointer, this step shouldn't be necessary. This head-pointer should
                always just be set to 0 */
          c.io.write.valid.poke(true.B)
          c.io.write.bits.is_data.poke(false.B)
          c.io.write.bits.from_regfile.foreach(_.poke(false.B))
          c.io.write.bits.reset_running_state.poke(false)
          c.io.write.bits.address.foreach(_.poke(0.U))
          c.io.write.bits.axis.poke(2.U)
          c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
          c.io.write.bits.spans.foreach(_.poke(1.U))
          c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
          c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

          while (!c.io.write.ready.peekBoolean())
            c.clock.step()
          c.clock.step()
          c.io.write.valid.poke(false.B)
        }

        init_i_head_pointer()

        // Write in single coord value (can be garbage)
        c.io.write.valid.poke(true)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.from_regfile.foreach(_.poke(false.B))
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(2.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.write.ready.expect(true)
        c.clock.step()
        c.io.write.valid.poke(false.B)

        // Init all j head pointers
        c.io.write.valid.poke(true)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.spans(1).poke(nMaxI * nK)

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.write.ready.expect(true)
        c.clock.step()
        c.io.write.valid.poke(false)

        // Reset i head pointer
        c.io.write.valid.poke(true)
        c.io.write.bits.is_data.poke(false)
        c.io.write.bits.from_regfile.foreach(_.poke(false))
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id)
        c.io.write.bits.axis.poke(2.U)
        c.io.write.bits.reset_running_state.poke(true)
        c.io.write.bits.address.foreach(_.poke(0))
        c.io.write.bits.spans.foreach(_.poke(1))
        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        while (!c.io.write.ready.peekBoolean())
          c.clock.step()
        c.clock.step()
        c.io.write.valid.poke(false)

        init_i_head_pointer()
      }

      // Write data (from regfile)
      {
        c.io.write.valid.poke(true.B)
        c.io.write.bits.axis.poke(0)
        c.io.write.bits.is_data.poke(true.B)

        c.io.write.bits.from_regfile.foreach(_.poke(false))
        c.io.write.bits.from_regfile(0).poke(true)
        c.io.write.bits.from_regfile(2).poke(true)
        c.io.write.bits.from_regfile_last_axis.poke(2.U)

        c.io.write.bits.address.foreach(_.poke(0.U))

        pokeMaxVal(c.io.write.bits.spans(0))
        c.io.write.bits.spans(1).poke(nK)
        pokeMaxVal(c.io.write.bits.spans(2))

        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(0).poke(1.U)

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(nK.U)

        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.write.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false.B)))
        c.io.write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
        c.io.write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).coord.poke(2.U)
        c.io.write.bits.from_regfile_metadata(2)(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
        c.io.write.bits.from_regfile_metadata(2)(LinkedListMetadata.coord_buffer_id).coord.poke(0.U)

        while (!c.io.write.ready.peekBoolean())
          c.clock.step()
        c.io.write.ready.expect(true)
        c.clock.step()
        c.io.write.valid.poke(false.B)

        var last_in_axis_signal_recieved = Set.empty[Int]

        while (c.io.busy.peekBoolean()) {
          for ((portId, coords, nCoords) <- Seq((2, Seq(Seq(i_coords.map(Seq(_)))), 1), (0, golden_coords, 3))) {
            val req = c.io.write_from_regfile_reqs(portId)
            val resp = c.io.write_from_regfile_resps(portId)

            req.ready.poke(randBool())
            resp.found.foreach(_.poke(false))

            val axis = req.bits.req.axis.peekInt().toInt
            val req_coords = (req.bits.req.address.drop(axis).take(nCoords).map(_.peekInt().toInt) ++ Seq.fill(3 - nCoords)(0)).reverse
            val span = req.bits.req.spans(axis).peekInt().toInt

            def access[T](mat: Seq[Seq[Seq[T]]], coords_ : Seq[Int]): Option[T] = {
              try {
                Some(mat(coords_(0))(coords_(1))(coords_(2)))
              } catch {
                case _: IndexOutOfBoundsException => None
              }
            }

            for (elem_id <- 0 until span) {
              val compressed_coords = req_coords.init :+ (req_coords.last + elem_id)
              val expanded_coords_opt = access(coords, compressed_coords)

              access(data, compressed_coords).foreach { d =>
                resp.data(elem_id).poke(d)
              }

              expanded_coords_opt.foreach { expanded_coords =>
                req.bits.req.from_regfile_metadata(portId).zip(resp.metadata).foreach { case (req_metadata, resp_metadata) =>
                  if (req_metadata.valid.peekBoolean()) {
                    val coordId = req_metadata.coord.peekInt().toInt
                    resp_metadata(elem_id).poke(expanded_coords(coordId))
                  }
                }

                resp.found(elem_id).poke(true)
              }
            }

            if (req.valid.peekBoolean()) {
              assert(!last_in_axis_signal_recieved.contains(portId), "assert the last-signal more than once for a given port")
              if (req.bits.last.peekBoolean()) {
                last_in_axis_signal_recieved = last_in_axis_signal_recieved + portId
              }
            }
          }

          c.clock.step()
        }

        assert(last_in_axis_signal_recieved == Set(0, 2), "did not recieve the two last-signals required")
      }

      // Read out data to regfile
      {
        val result_data = data.map(_.map(x => ArrayBuffer.fill(x.size)(-1000000)))
        val result_coords = j_coords.map(_.map(x => ArrayBuffer.fill(x.size)(Seq.empty[Int])))

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.to_regfile.poke(true)
        c.io.read_req.bits.is_data.poke(true.B)
        c.io.read_req.bits.address.foreach(_.poke(0.U))

        pokeMaxVal(c.io.read_req.bits.spans(0))
        c.io.read_req.bits.spans(1).poke(nK)
        pokeMaxVal(c.io.read_req.bits.spans(2))

        c.io.read_req.bits.data_strides.foreach(_.poke(0.U))
        c.io.read_req.bits.data_strides(0).poke(1.U)

        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.read_req.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(nK.U)

        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.read_req.ready.expect(true)
        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            val spans = c.io.read_resp.bits.spans.map(_.peekInt().toInt)
            val n_elems = spans.head

            val data = c.io.read_resp.bits.data
            val expanded_coords = c.io.read_resp.bits.expanded_addresses

            val Seq(j, k, i) = c.io.read_resp.bits.compressed_address.map(_.peekInt().toInt)

            assert(spans.tail.forall(_ == 1) || n_elems == 0 && spans.tail.forall(_ <= 1), s"spans = $spans")

            for (elem_id <- 0 until n_elems) {
              result_data(i)(k)(j + elem_id) = data(elem_id).peekInt().toInt
              result_coords(i)(k)(j + elem_id) = expanded_coords(elem_id).map(_.peekInt().toInt).reverse
            }
          }

          c.clock.step()
        }

        assert(result_data == data, "data doesn't match")
        assert(result_coords == golden_coords, "expanded coords don't match")
      }
    }
  }

  "Branched DLL SRAM writes from regfile correctly" in {
    {
      val seed = System.currentTimeMillis
      scala.util.Random.setSeed(seed)

      val fileWriter = new java.io.FileWriter("branched-dll-seed.txt")
      fileWriter.write(s"$seed")
      fileWriter.close()
    }

    val elemT = SInt(32.W)

    // Data is stored in k -> i -> j format, from outermost to innermost axes.
    val nK = 8
    val nMaxI = 16
    val nMaxJ = 64

    val elemsPerRead = 16
    val elemsPerWrite = 16

    val node_size = 16

    val nSramElems = nMaxI.max(node_size) * nK * nMaxJ.max(node_size)

    val branch_size = 16
    require(branch_size >= nMaxI)

    val data = Seq.fill(nK)(Seq.fill(randInt(max = nMaxI, bias = 0))(Seq.fill(randInt(max = nMaxJ, bias = 0))(randInt())))
    val i_coords = data.map(axisK => Seq.fill(axisK.size)(randInt(max = 30, bias = 1)).scanLeft(-1)(_ + _).tail)
    val j_coords = data.map(axisK => axisK.map(axisI => Seq.fill(axisI.size)(randInt(max = 30, bias = 1)).scanLeft(-1)(_ + _).tail))
    val j_row_ids = data.map(axisK => axisK.map(_.size).scanLeft(0)(_ + _))

    val ik_coords: Seq[Seq[Seq[Int]]] = {
      for (k <- 0 until nK) yield
        for (i <- i_coords(k)) yield
          Seq(i, k)
    }

    val golden_coords: Seq[Seq[Seq[Seq[Int]]]] = {
      for (k <- 0 until nK) yield
        for ((i, i_comp) <- i_coords(k).zipWithIndex) yield
          for (j <- j_coords(k)(i_comp)) yield
            Seq(i, j, k)
    }

    test(new ChiselSRAM(elemT = elemT, nElems = nSramElems, elemsPerRead = elemsPerRead, elemsPerWrite = elemsPerWrite,
      axes = Seq(FiberTreeAxis.LinkedList, FiberTreeAxis.LinkedList, FiberTreeAxis.Dense),
      metadatas = Seq(
        LinkedListMetadata(nK * nMaxI.max(node_size), (nK * nMaxI.max(node_size) * nMaxJ.max(node_size)) / node_size, node_size),
        LinkedListMetadata(nK, nK * nMaxI.max(node_size) / node_size, node_size),
        DenseMetadata),
      branchSizes = Seq(branch_size)
    )).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
      c.clock.setTimeout(10000)

      resetModule(c)

      // Initialize SRAM (from dma)
      {
        // Init i head pointers
        def init_i_head_pointers(): Unit = {
          c.io.write.valid.poke(true.B)
          c.io.write.bits.is_data.poke(false.B)
          c.io.write.bits.reset_running_state.poke(false)
          c.io.write.bits.from_regfile.foreach(_.poke(false.B))
          c.io.write.bits.address.foreach(_.poke(0.U))
          c.io.write.bits.axis.poke(1.U)
          c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
          c.io.write.bits.spans.foreach(_.poke(1.U))
          c.io.write.bits.spans(2).poke(nK)
          c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          c.io.write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
          c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

          while (!c.io.write.ready.peekBoolean())
            c.clock.step()
          c.clock.step()
          c.io.write.valid.poke(false.B)
        }

        init_i_head_pointers()

        // Write in 'nMaxI' coord values for each i-row (can be garbage)
        c.io.write.valid.poke(true)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.from_regfile.foreach(_.poke(false.B))
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(1.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.spans(1).poke(nMaxI)
        c.io.write.bits.spans(2).poke(nK)
        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        while (!c.io.write.ready.peekBoolean())
          c.clock.step()
        c.clock.step()
        c.io.write.valid.poke(false.B)

        // Init all j head pointers
        c.io.write.valid.poke(true)
        c.io.write.bits.is_data.poke(false.B)
        c.io.write.bits.address.foreach(_.poke(0.U))
        c.io.write.bits.axis.poke(0.U)
        c.io.write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

        c.io.write.bits.spans.foreach(_.poke(1.U))
        c.io.write.bits.spans(1).poke(nMaxI)
        c.io.write.bits.spans(2).poke(nK)

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        c.io.write.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        while (!c.io.write.ready.peekBoolean())
          c.clock.step()
        c.clock.step()
        c.io.write.valid.poke(false)

        // Reset i head pointers
        c.io.write.valid.poke(true)
        c.io.write.bits.reset_running_state.poke(true)
        c.io.write.bits.is_data.poke(false)
        c.io.write.bits.from_regfile.foreach(_.poke(false))
        c.io.write.bits.axis.poke(1)
        c.io.read_req.bits.spans.foreach(_.poke(1))
        c.io.read_req.bits.spans(2).poke(nK)
        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        while (!c.io.write.ready.peekBoolean())
          c.clock.step()
        c.clock.step()
        c.io.write.valid.poke(false)
        c.io.write.bits.reset_running_state.poke(false)

        init_i_head_pointers()
      }

      // Write data (from regfile)
      {
        c.io.write.valid.poke(true.B)
        c.io.write.bits.axis.poke(0)
        c.io.write.bits.is_data.poke(true.B)

        c.io.write.bits.from_regfile.foreach(_.poke(false))
        c.io.write.bits.from_regfile(0).poke(true)
        c.io.write.bits.from_regfile(1).poke(true)
        c.io.write.bits.from_regfile_last_axis.poke(2.U)

        c.io.write.bits.address.foreach(_.poke(0.U))

        c.io.write.bits.spans(0).poke(nMaxJ)
        c.io.write.bits.spans(1).poke(nMaxI)
        c.io.write.bits.spans(2).poke(nK)

        c.io.write.bits.data_strides.foreach(_.poke(0.U))
        c.io.write.bits.data_strides(0).poke(1.U)

        c.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1)
        c.io.write.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

        c.io.write.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.write.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false.B)))
        c.io.write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
        c.io.write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).coord.poke(1.U)
        c.io.write.bits.from_regfile_metadata(1)(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
        c.io.write.bits.from_regfile_metadata(1)(LinkedListMetadata.coord_buffer_id).coord.poke(0.U)

        while (!c.io.write.ready.peekBoolean())
          c.clock.step()
        c.clock.step()
        c.io.write.valid.poke(false.B)

        var last_in_axis_signal_received = Set.empty[Int]

        while (c.io.busy.peekBoolean()) {
          for ((portId, nCoords, coords_mat) <- Seq((1, 2, ik_coords), (0, 3, golden_coords))) {
            val req = c.io.write_from_regfile_reqs(portId)
            val resp = c.io.write_from_regfile_resps(portId)
            val is_ready = randBool()

            req.ready.poke(is_ready)
            resp.found.foreach(_.poke(false))

            val axis = req.bits.req.axis.peekInt().toInt
            val req_coords = req.bits.req.address.drop(axis).take(nCoords).map(_.peekInt().toInt).reverse
            val span = req.bits.req.spans(axis).peekInt().toInt

            def access[T](mat: Seq[Any], coords: Seq[Int]): Option[T] = {
               try {
                 val elem = mat(coords.head)
                 if (coords.size == 1)
                   Some(elem.asInstanceOf[T])
                 else {
                   assert(coords.nonEmpty)
                   access(elem.asInstanceOf[Seq[Any]], coords.tail)
                 }
               } catch {
                 case _: IndexOutOfBoundsException => None
                 case _: java.lang.ClassCastException => None
               }
            }

            for (elem_id <- 0 until span) {
              val compressed_coords = req_coords.init :+ (req_coords.last + elem_id)
              val expanded_coords_opt = access[Seq[Int]](coords_mat, compressed_coords)

              val evidence: Seq[Seq[Seq[Int]]] = data // If this line fails to commit, then we may have to change the if-condition below because it will mean that "data" does not have 3 dimensions
              if (nCoords == 3) {
                access[Int](data, compressed_coords).foreach { d =>
                  resp.data(elem_id).poke(d)
                }
              }

              expanded_coords_opt.foreach { expanded_coords =>
                req.bits.req.from_regfile_metadata(portId).zip(resp.metadata).foreach { case (req_metadata, resp_metadata) =>
                  if (req_metadata.valid.peekBoolean()) {
                    val coordId = req_metadata.coord.peekInt().toInt
                    resp_metadata(elem_id).poke(expanded_coords(coordId))
                  }
                }

                resp.found(elem_id).poke(true)
              }
            }

            if (req.valid.peekBoolean()) {
              assert(!last_in_axis_signal_received.contains(portId), "accessing reg-file after last-signal was asserted")
              if (req.bits.last.peekBoolean()) {
                last_in_axis_signal_received = last_in_axis_signal_received + portId
              }
            }
          }

          c.clock.step()
        }

        assert(last_in_axis_signal_received == Set(0, 1), "did not receive the two last-signals required")
      }

      // Read out data to regfile
      {
        val result_data = data.map(_.map(x => ArrayBuffer.fill(x.size)(-1000000)))
        val result_coords = j_coords.map(_.map(x => ArrayBuffer.fill(x.size)(Seq.empty[Int])))

        c.io.read_req.valid.poke(true.B)
        c.io.read_req.bits.to_regfile.poke(true)
        c.io.read_req.bits.is_data.poke(true.B)
        c.io.read_req.bits.address.foreach(_.poke(0.U))

        pokeMaxVal(c.io.read_req.bits.spans(0))
        pokeMaxVal(c.io.read_req.bits.spans(1))
        c.io.read_req.bits.spans(2).poke(nK)

        c.io.read_req.bits.data_strides.foreach(_.poke(0.U))
        c.io.read_req.bits.data_strides(0).poke(1.U)

        c.io.read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        c.io.read_req.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1)
        c.io.read_req.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

        c.io.read_req.bits.iteration_strides.foreach(_.poke(1.U))

        c.io.read_req.ready.expect(true)
        c.clock.step()

        c.io.read_req.valid.poke(false.B)
        c.io.read_resp.ready.poke(true.B)

        while (c.io.busy.peek().litToBoolean) {
          if (c.io.read_resp.valid.peek().litToBoolean) {
            val spans = c.io.read_resp.bits.spans.map(_.peekInt().toInt)
            val n_elems = spans.head

            val data = c.io.read_resp.bits.data
            val expanded_coords = c.io.read_resp.bits.expanded_addresses

            val Seq(j, i, k) = c.io.read_resp.bits.compressed_address.map(_.peekInt().toInt)

            assert(spans.tail.forall(_ <= 1), s"spans = $spans")

            for (elem_id <- 0 until n_elems) {
              result_data(k)(i)(j + elem_id) = data(elem_id).peekInt().toInt
              result_coords(k)(i)(j + elem_id) = {
                val Seq(j_exp, i_exp, k_exp) = expanded_coords(elem_id).map(_.peekInt().toInt)
                Seq(i_exp, j_exp, k_exp)
              }
            }
          }

          c.clock.step()
        }

        assert(result_data == data, "data doesn't match")
        assert(result_coords == golden_coords, "expanded coords don't match")
      }
    }
  }
}
