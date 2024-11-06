package stellar

import java.util.Random
import scala.collection.mutable.ArrayBuffer
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.ParallelTestExecution
import chisel3._
import chiseltest._
import MatrixUtil._
import TestUtil._

class AcceleratorWithMemoryTests extends AnyFreeSpec with ChiselScalatestTester with ParallelTestExecution {
  "Dense matmul works with memory and spatial arrays connected" in {
    cancel
    class DenseMatmulAccelerator extends Accelerator {
      val matmul = new DenseMatmul
      matmul.elaborate(shouldPrint = false, shouldRender = false, shouldUnroll = false, emitVerilog = false)

      val dim = matmul.block.upperBound
      val nAxes = 2

      val regA = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=3)
      val regB = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=3)
      val regC = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=3)

      val sramA = new SRAM(elemT=SInt(32.W), nElems=dim*dim, elemsPerRead=dim, elemsPerWrite=dim,
        axes=Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(nAxes)(DenseMetadata))
      val sramB = new SRAM(elemT=SInt(32.W), nElems=dim*dim, elemsPerRead=dim, elemsPerWrite=dim,
        axes=Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(nAxes)(DenseMetadata))
      val sramC = new SRAM(elemT=SInt(32.W), nElems=dim*dim, elemsPerRead=dim, elemsPerWrite=dim,
        axes=Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(nAxes)(DenseMetadata))

      connectVarToRegFile(matmul.A, regA)
      connectVarToRegFile(matmul.B, regB)
      connectVarToRegFile(matmul.C, regC)

      connectSRAMtoRegFile(sramA, regA)
      connectSRAMtoRegFile(sramB, regB)
      connectRegFileToSRAM(regC, sramC)

      registerSpatialArray(matmul)
    }

    val design = new DenseMatmulAccelerator
    val dim = design.dim

    val matA = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))
    val matB = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))

    val golden = matmul(matA, matB)

    val result = ArrayBuffer.fill(dim)(ArrayBuffer.fill(dim)(0))

    test (design.toChiselModule).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
      resetAccelerator(c)

      val sramAwritePort = c.getWritePortForSram(design.sramA)
      val sramBwritePort = c.getWritePortForSram(design.sramB)

      // Feed in A and B, row-by-row
      val mats = Seq((matA, sramAwritePort), (matB, sramBwritePort))

      for ((mat, port) <- mats) {
        for (row <- 0 until dim) {
          port.valid.poke(true.B)

          port.bits.address(0).poke(0.U)
          port.bits.address(1).poke(row.U)

          port.bits.spans(0).poke(dim.U)
          port.bits.spans(1).poke(1.U)

          port.bits.data_strides(0).poke(1.U)
          port.bits.data_strides(1).poke(dim.U)

          port.bits.iteration_strides(0).poke(1.U)
          port.bits.iteration_strides(1).poke(1.U)

          for (col <- 0 until dim)
            port.bits.data(col).poke(mat(row)(col).S)

          do {
            c.clock.step()
          } while (!port.ready.peek().litToBoolean)

          port.valid.poke(false.B)
        }
      }

      waitWhileBusy(c)

      // Feed A and B into their regfiles
      val sramAreadReqPort = c.getReadReqPortForSram(design.sramA)
      val sramBreadReqPort = c.getReadReqPortForSram(design.sramB)

      val read_req_ports = Seq(sramAreadReqPort, sramBreadReqPort)

      for (read_req <- read_req_ports) {
        read_req.valid.poke(true.B)
        read_req.bits.to_regfile.poke(true.B)
        read_req.bits.to_regfile_last_axis.poke(1.U)
        read_req.bits.address.foreach(_.poke(0.U))
        read_req.bits.spans.foreach(_.poke(dim.U))
        read_req.bits.data_strides(0).poke(1.U)
        read_req.bits.data_strides(1).poke(dim.U)
        read_req.bits.iteration_strides.foreach(_.poke(1.U))

        assert(read_req.ready.peek().litToBoolean)
      }

      c.clock.step()

      for (read_req <- read_req_ports) {
        read_req.valid.poke(false.B)
      }

      waitWhileBusy(c)

      // Write matmul result into sram
      val sramCwritePort = c.getWritePortForSram(design.sramC)

      sramCwritePort.valid.poke(true.B)
      sramCwritePort.bits.from_regfile(0).poke(true.B)
      sramCwritePort.bits.from_regfile_last_axis.poke(1.U)
      sramCwritePort.bits.address.foreach(_.poke(0.U))
      sramCwritePort.bits.spans.foreach(_.poke(dim.U))
      sramCwritePort.bits.data_strides(0).poke(1.U)
      sramCwritePort.bits.data_strides(1).poke(dim.U)
      sramCwritePort.bits.iteration_strides.foreach(_.poke(1.U))

      assert(sramCwritePort.ready.peek().litToBoolean)

      c.clock.step()

      sramCwritePort.valid.poke(false.B)

      waitWhileBusy(c)

      // Read matmul result out of SRAM
      val sramCreadReqPort = c.getReadReqPortForSram(design.sramC)
      val sramCreadRespPort = c.getReadRespPortsForSram(design.sramC)

      sramCreadReqPort.valid.poke(true.B)
      sramCreadReqPort.bits.to_regfile.poke(false.B)
      sramCreadReqPort.bits.address.foreach(_.poke(0.U))
      sramCreadReqPort.bits.spans.foreach(_.poke(dim.U))
      sramCreadReqPort.bits.data_strides(0).poke(1.U)
      sramCreadReqPort.bits.data_strides(1).poke(dim.U)
      sramCreadReqPort.bits.iteration_strides.foreach(_.poke(1.U))

      assert(sramCreadReqPort.ready.peek().litToBoolean)

      c.clock.step()

      sramCreadReqPort.valid.poke(false.B)

      sramCreadRespPort.ready.poke(true.B)
      while (c.io.busy.peek().litToBoolean) {
        if (sramCreadRespPort.valid.peek().litToBoolean) {
          val resp = sramCreadRespPort.bits

          val row = resp.compressed_address(1).peek().litValue
          val col = resp.compressed_address(0).peek().litValue

          val cols = resp.spans(0).peek().litValue
          assert(resp.spans.tail.forall(_.peek().litValue == 1))

          for (coff <- 0 until cols.toInt) {
            result(row.toInt)((col + coff).toInt) = resp.data(coff).peek().litValue.toInt
          }
        }

        c.clock.step()
      }

      assert(golden == result.toSeq)
    }
  }

  "Dense overlapped matmuls work with memory and spatial arrays connected" in {
    cancel
    /*
    Over here, we perform a sequence of matmuls (A * B = C)

    Each C matrix generated is added to a D matrix already resident in sramC (C + D = E)

    Afterwards, each E matrix is stored in the accumulator
     */

    class DenseMatmulAccelerator extends Accelerator {
      val size = 8

      val matmul = new DenseMatmul(size)
      matmul.elaborate(shouldPrint = false, shouldRender = false, shouldUnroll = false, emitVerilog = false)
      registerSpatialArray(matmul)

      val matadder = new MatrixAdder(size)
      matadder.elaborate(shouldPrint = false, shouldRender = false, shouldUnroll = false, emitVerilog = false)
      registerSpatialArray(matadder)

      val dim = matmul.block.upperBound
      val nAxes = 3
      val nMatmuls = 8

      val regA = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=3)
      val regB = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=3)
      val regC = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=3)
      val regD = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=2)
      val regE = new RegFile(nElems=dim*dim, nUpdatePorts=0, nIOCoords=2, nDomainCoords=2)

      val sramA = new SRAM(elemT=SInt(32.W), nElems=dim*dim*nMatmuls, elemsPerRead=dim, elemsPerWrite=dim,
        axes=Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(nAxes)(DenseMetadata))
      val sramB = new SRAM(elemT=SInt(32.W), nElems=dim*dim*nMatmuls, elemsPerRead=dim, elemsPerWrite=dim,
        axes=Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(nAxes)(DenseMetadata))
      val sramC = new SRAM(elemT=SInt(32.W), nElems=dim*dim, elemsPerRead=dim, elemsPerWrite=dim,
        axes=Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(nAxes)(DenseMetadata))

      connectVarToRegFile(matmul.A, regA)
      connectVarToRegFile(matmul.B, regB)
      connectVarToRegFile(matmul.C, regC)
      connectVarToRegFile(matadder.C, regC)
      connectVarToRegFile(matadder.D, regD)
      connectVarToRegFile(matadder.E, regE)

      connectSRAMtoRegFile(sramA, regA)
      connectSRAMtoRegFile(sramB, regB)
      connectSRAMtoRegFile(sramC, regD)
      connectRegFileToSRAM(regE, sramC)
    }

    val design = new DenseMatmulAccelerator
    val dim = design.dim
    val nMatmuls = design.nMatmuls

    val matA = Seq.fill(dim)(Seq.fill(nMatmuls*dim)(scala.util.Random.nextInt(100) - 50))
    val matB = Seq.fill(nMatmuls*dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))
    val matD = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))

    val golden = matadd(matmul(matA, matB), matD)

    val result = Seq.fill(dim)(ArrayBuffer.fill(dim)(0))

    test (design.toChiselModule).withAnnotations(Seq(/*WriteVcdAnnotation,*/ VcsBackendAnnotation)) { c =>
      resetAccelerator(c)

      val sramAwritePort = c.getWritePortForSram(design.sramA)
      val sramBwritePort = c.getWritePortForSram(design.sramB)
      val sramDwritePort = c.getWritePortForSram(design.sramC)

      // Feed in A, B, and D row-by-row
      val mats = Seq((matA, sramAwritePort), (matB, sramBwritePort), (matD, sramDwritePort))

      for ((mat, port) <- mats) {
        val nRows = mat.size
        val nCols = mat.head.size

        port.valid.poke(true.B)

        for (row <- 0 until nRows) {
          for (col <- 0 until nCols by dim) {
            port.bits.address(0).poke(col.U)
            port.bits.address(1).poke(row.U)
            port.bits.address.drop(2).foreach(_.poke(0.U))

            port.bits.spans(0).poke(dim.U)
            port.bits.spans.tail.foreach(_.poke(1.U))

            port.bits.data_strides(0).poke(1.U)
            port.bits.data_strides(1).poke(nCols.U)
            port.bits.data_strides.drop(2).foreach(_.poke(0.U))

            port.bits.iteration_strides.foreach(_.poke(1.U))

            for (c <- 0 until dim)
              port.bits.data(c).poke(mat(row)(c+col).S)

            assert(port.ready.peek().litToBoolean)
            c.clock.step()
          }
        }

        port.valid.poke(false.B)
      }

      waitWhileBusy(c)

      // Feed A, B, and D into their regfiles
      val sramAreadReqPort = c.getReadReqPortForSram(design.sramA)
      val sramBreadReqPort = c.getReadReqPortForSram(design.sramB)
      val sramDreadReqPort = c.getReadReqPortForSram(design.sramC)

      val read_req_ports = Seq(
        (sramAreadReqPort, Seq(nMatmuls * dim, dim), false),
        (sramBreadReqPort, Seq(dim, dim*dim), false),
        (sramDreadReqPort, Seq(dim, 0), true),
      )

      /*
      A:
        for k_outer
          for i
            for k_inner
              k = k_outer * dim + k_inner

              i * n_matmuls * dim + k
              i * n_matmuls * dim + k_outer * dim + k_inner
              k_outer * dim + i * n_matmuls * dim + k_inner

       B:
        for k_outer
          for k_inner
            for j
              k = k_outer * dim + k_inner

              k * dim + j
              (k_outer * dim + k_inner) * dim + j

       C:
        for k_outer
          for i
            for j
              i * dim + j
       */

      for ((read_req, strides, should_interleave) <- read_req_ports) {
        read_req.valid.poke(true.B)

        read_req.bits.to_regfile.poke(true.B)
        read_req.bits.to_regfile_last_axis.poke(1.U)

        read_req.bits.interleave.should_push.poke(false.B)
        read_req.bits.interleave.should_pop.poke(should_interleave.B)
        read_req.bits.interleave.axis.poke(1.U)

        read_req.bits.address.foreach(_.poke(0.U))

        read_req.bits.spans(0).poke(dim.U)
        read_req.bits.spans(1).poke(dim.U)
        read_req.bits.spans(2).poke(nMatmuls.U)

        read_req.bits.data_strides(0).poke(1.U)
        read_req.bits.data_strides.tail.zip(strides).foreach { case (p, s) =>
          p.poke(s.U)
        }

        read_req.bits.iteration_strides.foreach(_.poke(1.U))

        assert(read_req.ready.peek().litToBoolean)
      }

      // Write matmul result into sram
      val sramCwritePort = c.getWritePortForSram(design.sramC)

      sramCwritePort.valid.poke(true.B)
      sramCwritePort.bits.from_regfile(0).poke(true.B)
      sramCwritePort.bits.from_regfile_last_axis.poke(1.U)

      sramCwritePort.bits.interleave.should_push.poke(true.B)
      sramCwritePort.bits.interleave.should_pop.poke(false.B)
      sramCwritePort.bits.interleave.axis.poke(1.U)
      sramCwritePort.bits.should_trail_reads.poke(true.B)

      sramCwritePort.bits.address.foreach(_.poke(0.U))
      sramCwritePort.bits.spans(0).poke(dim.U)
      sramCwritePort.bits.spans(1).poke(dim.U)
      sramCwritePort.bits.spans(2).poke(nMatmuls.U)
      sramCwritePort.bits.data_strides(0).poke(1.U)
      sramCwritePort.bits.data_strides(1).poke(dim.U)
      sramCwritePort.bits.data_strides(2).poke(0.U)
      sramCwritePort.bits.iteration_strides.foreach(_.poke(1.U))

      assert(sramCwritePort.ready.peek().litToBoolean)

      c.clock.step()

      // Wait for matmul to finish
      sramCwritePort.valid.poke(false.B)
      for ((read_req, _, _) <- read_req_ports) {
        read_req.valid.poke(false.B)
      }

      waitWhileBusy(c)

      // Read matmul result out of SRAM
      val sramCreadReqPort = c.getReadReqPortForSram(design.sramC)
      val sramCreadRespPort = c.getReadRespPortsForSram(design.sramC)

      sramCreadReqPort.valid.poke(true.B)
      sramCreadReqPort.bits.to_regfile.poke(false.B)
      sramCreadReqPort.bits.interleave.should_push.poke(false.B)
      sramCreadReqPort.bits.interleave.should_pop.poke(false.B)
      sramCreadReqPort.bits.address.foreach(_.poke(0.U))
      sramCreadReqPort.bits.spans(0).poke(dim.U)
      sramCreadReqPort.bits.spans(1).poke(dim.U)
      sramCreadReqPort.bits.spans(2).poke(1.U)
      sramCreadReqPort.bits.data_strides(0).poke(1.U)
      sramCreadReqPort.bits.data_strides(1).poke(dim.U)
      sramCreadReqPort.bits.data_strides(2).poke(0.U)
      sramCreadReqPort.bits.iteration_strides.foreach(_.poke(1.U))

      assert(sramCreadReqPort.ready.peek().litToBoolean)

      c.clock.step()

      sramCreadReqPort.valid.poke(false.B)

      sramCreadRespPort.ready.poke(true.B)
      while (c.io.busy.peek().litToBoolean) {
        if (sramCreadRespPort.valid.peek().litToBoolean) {
          val resp = sramCreadRespPort.bits

          val row = resp.compressed_address(1).peek().litValue
          val col = resp.compressed_address(0).peek().litValue

          val cols = resp.spans(0).peek().litValue
          assert(resp.spans.tail.forall(_.peek().litValue == 1))

          for (coff <- 0 until cols.toInt) {
            result(row.toInt)((col + coff).toInt) = resp.data(coff).peek().litValue.toInt
          }
        }

        c.clock.step()
      }

      assert(golden == result.toSeq)
    }
  }

  for (isLoadBalanced <- Seq(false, true)) {
    ((if (isLoadBalanced) "Load-balanced i" else "I") +
    "nput-stationary sparse-dense matmul works with memory and spatial arrays connected") in {
      cancel
      /*
        Currently, this multiplies a dense matrix A with a CSR matrix B to compute a CSR matrix C
          A (dense) * B (csr) = C (csr)

         A has size (iTiles * dim) x (kTiles * dim)
         B has size (kTiles * dim) x dim
         C has size (iTiles * dim) x dim

        Each C matrix generated is added to a D matrix (csr) already resident in sramC:
          C (csr) + D (csr) = E (csr)

        Afterwards, each E matrix is stored in the accumulator.

        Loop order (from outermost to innermost) is i -> k -> j
       */

      implicit val rand = {
        /* TODO Failing seeds:
             Unbalanced:
               * 1693026917997
               * 1693621736822
               * 1695174455522

             Load balanced:
               * 1682624093111: To fix this, we need to support "sub-spatial arrays", which permit independent groupings
                                of connected PEs to continue execution even when other groupings have stalled.
               * 1688450763407
               * 1694807137583
         */
        val seed = System.currentTimeMillis
        val rand: java.util.Random = new Random()
        rand.setSeed(seed)
        val fileWriter = new java.io.FileWriter((if (isLoadBalanced) "load-balanced-" else "") + "sparse-dense-matmul-seed.txt")
        fileWriter.write(s"$seed")
        fileWriter.close()
        rand
      }

      val iTiles = 16
      val jTiles = 4
      val kTiles = if (isLoadBalanced) 1 else 8 // TODO "kTiles" should be the same whether or not load-balancing is enabled. Right now, that doesn't seem to pass, but that's probably just due to some as-yet-unknown bug in how we set the loadBalancer config below that I do not have time to fix right now

      val design = new SpDenseMMAccelerator(iTiles, jTiles, kTiles, isLoadBalanced, false)
      val dim = design.matmulDimSize

      val matA = Seq.fill(iTiles*dim, kTiles*dim)(randInt())

      // TODO Increase elaboration speed so that we don't need to keep matB_coords constant across the kTiles dimension. (We only do that to limit the time it takes to elaborate the adder)
      val matB_coords = {
        var nCols = Seq.fill(dim)(randInt(max=(jTiles*dim)+1, bias=0))

        while (isLoadBalanced && !nCols.grouped(2).exists {
          case Seq(x, y) => x/dim - y/dim > 1
          case Seq(_) => false
        }) {
          nCols = Seq.fill(dim)(randInt(max=(jTiles*dim)+1, bias=0))
        }

        val coords = nCols.map(nCol => Seq.fill(nCol)(randInt(bias=1))).map(_.scanLeft(-1)(_ + _).tail)
        Seq.fill(kTiles)(coords).flatten
      }
      assert(matB_coords.flatten.distinct.size <= jTiles * dim * dim,
      "For now, we can't have more than (jTiles * dim * dim) output-coords, or the adder takes too long to elaborate")
      val matB_data = matB_coords.map(_.size).map(Seq.fill(_)(randInt()))

      val (golden_data, golden_coords, golden_row_ids) = {
        val expandedCols = matB_coords.flatten.maxOption.getOrElse(-1) + 1
        val matB_expanded = matB_data.zip(matB_coords).map { case (data, coords) =>
          val row = ArrayBuffer.fill(expandedCols)(0)
          for ((d,c) <- data.zip(coords))
            row(c) = d
          row.toSeq
        }

        val expanded = matmul(matA, matB_expanded)

        val coords = Seq.fill(iTiles * dim)(matB_coords.flatten.distinct.sorted)
        val data = expanded.map(_.zipWithIndex.collect { case (d,c) if coords.head.contains(c) => d })
        val row_ids = coords.map(_.size).scanLeft(0)(_ + _)

        (data, coords, row_ids)
      }

      val result_data = golden_data.map(_.size).map(len => ArrayBuffer.fill(len)(0))
      val result_coords = golden_coords.map(_.size).map(len => ArrayBuffer.fill(len)(-1))
      val result_row_ids = ArrayBuffer.fill(golden_row_ids.size)(-1)

      test(design.toChiselModule).withAnnotations(Seq(WriteVcdAnnotation, VcsBackendAnnotation)) { c =>
        resetAccelerator(c)
        c.clock.setTimeout(50000)

        // Initializing C/D
        {
          val nRows = iTiles * design.matmulDimSize
          val sram_write_port = c.getWritePortForSram(design.sramC)

          sram_write_port.valid.poke(true.B)
          sram_write_port.bits.is_data.poke(false.B)
          sram_write_port.bits.address.foreach(_.poke(0.U))
          sram_write_port.bits.axis.poke(0.U)
          sram_write_port.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
          sram_write_port.bits.spans.foreach(_.poke(1.U))
          sram_write_port.bits.spans(1).poke(nRows.U)
          sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
          sram_write_port.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
          sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

          sram_write_port.ready.expect(true.B)
          c.clock.step()
          sram_write_port.valid.poke(false.B)
        }

        // Feed row-ids of B into its SRAM
        {
          val nRows = kTiles*design.matmulDimSize
          val sram_write_port = c.getWritePortForSram(design.sramB)

          // Write row-coords into sram
          sram_write_port.valid.poke(true.B)
          sram_write_port.bits.is_data.poke(false.B)
          sram_write_port.bits.from_regfile(0).poke(false.B)
          sram_write_port.bits.axis.poke(0.U)
          sram_write_port.bits.metadata_buffer_id.poke(0.U)

          sram_write_port.bits.spans.foreach(_.poke(1.U))

          sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          sram_write_port.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

          sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

          var row_coord = 0
          for (row <- 0 until nRows+1) {
            sram_write_port.bits.address(0).poke(0.U)
            sram_write_port.bits.address(1).poke(row.U)
            sram_write_port.bits.address.drop(2).foreach(_.poke(0.U))

            sram_write_port.bits.data(0).poke(row_coord.S)

            assert(sram_write_port.ready.peek().litToBoolean, "Write port was not ready")
            c.clock.step()

            if (row < nRows)
              row_coord += matB_data(row).size
          }

          sram_write_port.valid.poke(false.B)
        }

        // Feed col-ids of B into its SRAM
        {
          val colCoords = matB_coords

          val nRows = colCoords.size
          val elemsPerWrite = design.matmulDimSize

          val sram_write_port = c.getWritePortForSram(design.sramB)

          // Write col-coords into sram
          sram_write_port.valid.poke(true.B)
          sram_write_port.bits.is_data.poke(false.B)
          sram_write_port.bits.from_regfile(0).poke(false.B)
          sram_write_port.bits.axis.poke(0.U)
          sram_write_port.bits.metadata_buffer_id.poke(1.U)

          sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          sram_write_port.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

          sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

          for (row <- 0 until nRows) {
            val nCols = colCoords(row).size

            var col = 0
            while (col < nCols) {
              val n_elems_written = (nCols - col).min(elemsPerWrite)

              sram_write_port.bits.address(0).poke(col.U)
              sram_write_port.bits.address(1).poke(row.U)
              sram_write_port.bits.address.drop(2).foreach(_.poke(0.U))

              sram_write_port.bits.spans(0).poke(n_elems_written.U)
              sram_write_port.bits.spans.tail.foreach(_.poke(1.U))

              for (coff <- 0 until n_elems_written)
                sram_write_port.bits.data(coff).poke(colCoords(row)(col+coff).S)

              if (sram_write_port.ready.peek().litToBoolean)
                col += elemsPerWrite
              c.clock.step()
            }
          }

          sram_write_port.valid.poke(false.B)
        }

        // Feed A and B read_data into their SRAMs
        for ((sram, data, isCompressed) <- Seq((design.sramA, matA, false), (design.sramB, matB_data, true))) {
          val nRows = data.size
          val elemsPerRead = design.matmulDimSize

          val sram_write_port = c.getWritePortForSram(sram)

          sram_write_port.valid.poke(true.B)
          sram_write_port.bits.is_data.poke(true.B)
          sram_write_port.bits.from_regfile(0).poke(false.B)
          sram_write_port.bits.axis.poke(0.U)

          sram_write_port.bits.data_strides(0).poke(1.U)
          sram_write_port.bits.data_strides.drop(2).foreach(_.poke(0.U))

          sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          if (sram_write_port.bits.metadata_strides(1)(0).nonEmpty)
            sram_write_port.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

          sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

          for (row <- 0 until nRows) {
            val nCols = data(row).size
            sram_write_port.bits.data_strides(1).poke({if (isCompressed) 0 else nCols}.U)

            var col = 0
            while (col < nCols) {
              val n_elems_read = (nCols - col).min(elemsPerRead)

              sram_write_port.bits.address(0).poke(col.U)
              sram_write_port.bits.address(1).poke(row.U)
              sram_write_port.bits.address.drop(2).foreach(_.poke(0.U))

              sram_write_port.bits.spans(0).poke(n_elems_read.U)
              sram_write_port.bits.spans.tail.foreach(_.poke(1.U))

              for (coff <- 0 until n_elems_read)
                sram_write_port.bits.data(coff).poke(data(row)(col+coff).S)

              if (sram_write_port.ready.peek().litToBoolean)
                col += elemsPerRead
              c.clock.step()
            }
          }

          sram_write_port.valid.poke(false.B)
        }

        waitWhileBusy(c)

        // Configure the load-balancer
        if (isLoadBalanced) {
          /*
          for i_outer
            for k_outer
              for j_outer'
                j = j_outer' * dim
                C[i_outer*dim][j][k_outer*dim] = A[i_outer*dim][k_outer*dim] * B[k_outer*dim][j]
           */

          val inVars = c.loadBalancersSorted.head.inVars
          val indices = c.loadBalancersSorted.head.indices

          val Seq(jOuterInd, kOuterInd, iOuterInd) = 0 until 3

          c.io.loadBalancerConfigs.head.valid.poke(true.B)

          val config = c.io.loadBalancerConfigs.head.bits

          config.axis_sizes(jOuterInd).poke(1.U)
          config.axis_sizes(kOuterInd).poke(kTiles.U)
          config.axis_sizes(iOuterInd).poke(iTiles.U)

          config.rf_in_strides.flatten.flatten.foreach(_.poke(0.U))

          config.rf_in_strides(inVars.indexOf(design.matmul.A))(0)(iOuterInd).poke(dim.U)
          config.rf_in_strides(inVars.indexOf(design.matmul.A))(1)(kOuterInd).poke(dim.U)

          config.rf_in_strides(inVars.indexOf(design.matmul.B))(0)(kOuterInd).poke(dim.U)

          config.rf_in_strides(inVars.indexOf(design.matmul.Bj))(0)(kOuterInd).poke(dim.U)

          config.cl_in_strides.flatten.foreach(_.poke(0.U))

          for ((index, outerInd) <- Seq((design.matmul.i, iOuterInd), (design.matmul.j, jOuterInd), (design.matmul.k, kOuterInd))) {
            config.cl_in_strides(indices.indexOf(index))(outerInd).poke(dim.U)
          }

          c.clock.step()
          c.io.loadBalancerConfigs.head.valid.poke(false.B)
        }

        // Feed A, B, and D into their regfiles
        {
          /*
          A:
          for i_outer
            for k_outer
              for j_outer
                for i_inner
                  for k_inner
                    k = k_outer * dim + k_inner
                    i = i_outer * dim + i_inner

                    i * (kTiles * dim) + k
                    (i_outer * dim + i_inner) * kTiles * dim + k_outer * dim + k_inner
                    (i_outer * kTiles * dim^2) + (k_outer * dim) + (j_outer * 0) + (i_inner * kTiles * dim) + k_inner

         B:
          for i_outer
            for k_outer
              for j_outer
                for k_inner
                  for j_inner
                    j = j_outer * dim + j_inner
                    k = k_outer * dim + k_inner

                    Row address is:
                    k
                    (i_outer * 0) + (k_outer * dim) + (j_outer * 0) + k_inner + (j_inner * 0)

                    Col address is:
                    0, because we go over the entire J dimension in just one indefinitely-large iteration

         D:
          for i_outer
            for k_outer
              for j_outer
                for i_inner
                  for j_inner
                    j = j_outer * dim + j_inner
                    i = i_outer * dim + i_inner

                    Row address is:
                    i
                    i_outer * dim + i_inner
                    (i_outer * dim) + (k_outer * 0) + (j_outer * 0) + i_inner + (j_inner * 0)

                    Col address is:
                    0, because we go over the entire J dimension in just one indefinitely-large iteration
         */

          val srams = Seq(
            (design.sramA, Some(dim), Seq(kTiles*dim, 0, dim, kTiles*dim*dim), Seq(0, 0, 0), false),
            (design.sramB, None, Seq(0, 0, 0, 0), Seq(1, 0, dim, 0), false),
            (design.sramC, None, Seq(0, 0, 0, 0), Seq(1, 0, 0, dim), true),
          )
          for ((sram, nColsOpt, dataStrides, metadataStrides, should_interleave) <- srams) {
            val read_req = c.getReadReqPortForSram(sram)

            read_req.valid.poke(true.B)
            read_req.bits.is_data.poke(true.B)
            read_req.bits.to_regfile.poke(true.B)
            read_req.bits.to_regfile_last_axis.poke(1.U)
            read_req.bits.address.foreach(_.poke(0.U))
            read_req.bits.spans(0).poke(nColsOpt.map(BigInt.apply).getOrElse(maxVal(read_req.bits.spans(0))).U)
            read_req.bits.spans(1).poke(dim.U)
            read_req.bits.spans(2).poke(1.U)
            read_req.bits.spans(3).poke(kTiles.U)
            read_req.bits.spans(4).poke(iTiles.U)

            read_req.bits.data_strides(0).poke(1.U)
            read_req.bits.data_strides.tail.zip(dataStrides).foreach { case (p, s) => p.poke(s.U) }

            read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
            read_req.bits.metadata_strides.tail.map(_(0)).zip(metadataStrides).foreach { case (p, s) =>
              assert(CompressedMetadata.outer_metadata_buffer_id == 0 && LinkedListMetadata.head_ptr_buffer_id == 0, "if this assertion fails, then the line below needs to change")
              if (p.nonEmpty)
                p(0).poke(s.U)
            }

            read_req.bits.iteration_strides.foreach(_.poke(1.U))

            read_req.bits.interleave.should_push.poke(false.B)
            read_req.bits.interleave.should_pop.poke(should_interleave.B)
            read_req.bits.interleave.axis.poke(3.U)

            assert(read_req.ready.peek().litToBoolean)
          }
        }

        // Write merged matmul result into sram
        {
          val sramCwritePort = c.getWritePortForSram(design.sramC)

          sramCwritePort.valid.poke(true.B)
          sramCwritePort.bits.is_data.poke(true.B)
          sramCwritePort.bits.from_regfile(0).poke(true.B)
          sramCwritePort.bits.from_regfile_last_axis.poke(1.U)
          sramCwritePort.bits.address.foreach(_.poke(0.U))
          sramCwritePort.bits.spans(0).poke(maxVal(sramCwritePort.bits.spans(0)).U)
          sramCwritePort.bits.spans(1).poke(dim.U)
          sramCwritePort.bits.spans(2).poke(1.U)
          sramCwritePort.bits.spans(3).poke(kTiles.U)
          sramCwritePort.bits.spans(4).poke(iTiles.U)

          sramCwritePort.bits.data_strides(0).poke(1.U)
          sramCwritePort.bits.data_strides.tail.foreach(_.poke(0.U))

          sramCwritePort.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          sramCwritePort.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
          sramCwritePort.bits.metadata_strides(4)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(dim.U)

          sramCwritePort.bits.iteration_strides.foreach(_.poke(1.U))

          sramCwritePort.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false.B)))
          sramCwritePort.bits.from_regfile_metadata.head(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
          sramCwritePort.bits.from_regfile_metadata.head(LinkedListMetadata.coord_buffer_id).coord.poke(1.U)

          sramCwritePort.bits.interleave.should_push.poke(true.B)
          sramCwritePort.bits.interleave.should_pop.poke(false.B)
          sramCwritePort.bits.interleave.axis.poke(3.U)
          sramCwritePort.bits.should_trail_reads.poke(true.B)

          assert(sramCwritePort.ready.peek().litToBoolean)
        }

        // Wait for matmuls and merges to complete, with their final results stored in SRAMs
        {
          c.clock.step()

          for (sram <- Seq(design.sramA, design.sramB, design.sramC)) {
            c.getReadReqPortForSram(sram).valid.poke(false.B)
            c.getWritePortForSram(sram).valid.poke(false.B)
          }

          waitWhileBusy(c)
        }

        // Read coords and row-ids out of SRAM
        {
          val nRows = iTiles * dim

          val sramCreadReqPort = c.getReadReqPortForSram(design.sramC)
          val sramCreadRespPort = c.getReadRespPortsForSram(design.sramC)

          sramCreadReqPort.bits.axis.poke(0.U)
          sramCreadReqPort.bits.is_data.poke(false.B)
          sramCreadReqPort.bits.to_regfile.poke(false.B)
          sramCreadReqPort.bits.spans(0).poke(maxVal(sramCreadReqPort.bits.spans(0)).U)
          sramCreadReqPort.bits.spans.drop(1).foreach(_.poke(1.U))
          sramCreadReqPort.bits.address.foreach(_.poke(0.U))

          sramCreadReqPort.bits.data_strides(0).poke(1.U)
          sramCreadReqPort.bits.data_strides.tail.foreach(_.poke(0.U))

          sramCreadReqPort.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          sramCreadReqPort.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)

          sramCreadReqPort.bits.iteration_strides.foreach(_.poke(1.U))

          sramCreadRespPort.ready.poke(true.B)

          var req_row = 0
          var requested_row_id = false
          while (req_row < nRows || req_row == nRows && !requested_row_id || c.io.busy.peek().litToBoolean) {
            sramCreadReqPort.valid.poke((req_row < nRows || req_row == nRows && !requested_row_id).B)
            sramCreadReqPort.bits.address(1).poke(req_row.U)
            if (requested_row_id)
              sramCreadReqPort.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
            else
              sramCreadReqPort.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

            if (sramCreadReqPort.ready.peek().litToBoolean) {
              if (requested_row_id) {
                req_row += 1
                requested_row_id = false
              } else
                requested_row_id = true
            }

            if (sramCreadRespPort.valid.peek().litToBoolean) {
              if (sramCreadRespPort.bits.metadata_buffer_id.peek().litValue == LinkedListMetadata.coord_buffer_id) {
                val cols = sramCreadRespPort.bits.spans(0).peek().litValue.toInt
                val rows = sramCreadRespPort.bits.spans(1).peek().litValue.toInt
                assert(rows == 1)

                val data = sramCreadRespPort.bits.data

                val row = sramCreadRespPort.bits.compressed_address(1).peek().litValue.toInt
                val col = sramCreadRespPort.bits.compressed_address(0).peek().litValue.toInt

                for (c <- 0 until cols) {
                  result_coords(row)(col + c) = data(c).peek().litValue.toInt
                }
              } else {
                val row = sramCreadRespPort.bits.compressed_address(1).peek().litValue
                assert(sramCreadRespPort.bits.spans.peek().forall(_.litValue == 1))
                result_row_ids(row.toInt) = sramCreadRespPort.bits.data.head.peek().litValue.toInt
              }
            }

            c.clock.step()
          }

          sramCreadReqPort.valid.poke(false.B)
          sramCreadRespPort.ready.poke(false.B)
        }

        // Read matmul read_data out of SRAM
        {
          val sramCreadReqPort = c.getReadReqPortForSram(design.sramC)
          val sramCreadRespPort = c.getReadRespPortsForSram(design.sramC)

          // Read out read_data
          sramCreadReqPort.valid.poke(true.B)
          sramCreadReqPort.bits.to_regfile.poke(false.B)
          sramCreadReqPort.bits.address.foreach(_.poke(0.U))
          sramCreadReqPort.bits.spans(0).poke(maxVal(sramCreadReqPort.bits.spans(0)).U)
          sramCreadReqPort.bits.spans(1).poke((iTiles*dim).U)
          sramCreadReqPort.bits.spans.drop(2).foreach(_.poke(1.U))

          sramCreadReqPort.bits.data_strides(0).poke(1.U)
          sramCreadReqPort.bits.data_strides.tail.foreach(_.poke(0.U))

          sramCreadReqPort.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          sramCreadReqPort.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)

          sramCreadReqPort.bits.iteration_strides.foreach(_.poke(1.U))

          sramCreadReqPort.bits.is_data.poke(true.B)

          sramCreadReqPort.bits.interleave.should_push.poke(false.B)
          sramCreadReqPort.bits.interleave.should_pop.poke(false.B)

          assert(sramCreadReqPort.ready.peek().litToBoolean)

          c.clock.step()

          sramCreadReqPort.valid.poke(false.B)

          sramCreadRespPort.ready.poke(true.B)
          while (c.io.busy.peek().litToBoolean) {
            if (sramCreadRespPort.valid.peek().litToBoolean) {
              val resp = sramCreadRespPort.bits

              val row = resp.compressed_address(1).peek().litValue
              val col = resp.compressed_address(0).peek().litValue

              val cols = resp.spans(0).peek().litValue
              assert(resp.spans.tail.forall(_.peek().litValue == 1))

              for (coff <- 0 until cols.toInt) {
                val data = resp.data(coff).peek().litValue.toInt
                result_data(row.toInt)((col + coff).toInt) = data
              }
            }

            c.clock.step()
          }
        }
      }

      assert(result_data == golden_data, s"read_data doesn't match")
      assert(result_coords == golden_coords, s"coords don't match")
      assert(result_row_ids == golden_row_ids, s"row-ids does't match")
    }
  }

  "Output-stationary sparse-dense matmul works with memory and spatial arrays connected" in {
    cancel
    val iTiles = 16
    val jTiles = 4
    val kTiles = 8

    val design = new SpDenseMMAccelerator(iTiles, jTiles, kTiles, false, true)
    val dim = design.matmulDimSize

    // TODO Finish implementing this test
    test(design.toChiselModule).withAnnotations(Seq(/*WriteVcdAnnotation,*/ VcsBackendAnnotation)) { c =>

    }
  }

  "Dynamic-len matrix addition works" in {
    cancel
    /* TODO Failing seeds:
          1686471205837
          1692994817838
          1693008272071
          1693171891107
     */
    val seed = System.currentTimeMillis
    implicit val rand: java.util.Random = new Random()
    rand.setSeed(seed)

    {
      val fileWriter = new java.io.FileWriter("dynamic-len-seed.txt")
      fileWriter.write(s"$seed")
      fileWriter.close()
    }

    val nRows = 32
    val maxACols = 8
    val maxBCols = 128

    val matAData = Seq.fill(nRows)(Seq.fill(randInt(max=maxACols, bias=0))(randInt()))
    val matBData = Seq.fill(nRows)(Seq.fill(randInt(max=maxBCols, bias=0))(randInt()))

    val matAColIds = matAData.map { row =>
      Seq.fill(row.size)(randInt(max=100, bias=1)).scanLeft(-1)(_ + _).tail
    }
    val matBColIds = matBData.map { row =>
      Seq.fill(row.size)(randInt(max=100, bias=1)).scanLeft(-1)(_ + _).tail
    }

    val matARowIds = matAData.map(_.size).scanLeft(0)(_ + _)
    val matBRowIds = matBData.map(_.size).scanLeft(0)(_ + _)

    val (goldenData, goldenColIds) = {
      val maxAColId = matAColIds.flatten.max
      val maxBColId = matBColIds.flatten.max

      val maxCColId = maxAColId.max(maxBColId)

      val result = for (row <- 0 until nRows) yield {
        (for (col <- 0 to maxCColId) yield {
          val aInd = matAColIds(row).indexOf(col)
          val bInd = matBColIds(row).indexOf(col)

          val a = if (aInd >= 0) matAData(row)(aInd) else 0
          val b = if (bInd >= 0) matBData(row)(bInd) else 0

          ((a+b, col), aInd >= 0 || bInd >= 0)
        }).filter(_._2).map(_._1)
      }

      (result.map(_.map(_._1)), result.map(_.map(_._2)))
    }

    val goldenRowIds = goldenData.map(_.size).scanLeft(0)(_ + _)

//    println(s"matAData = $matAData")
//    println(s"matBData = $matBData")
//    println()
//    println(s"matAColIds = $matAColIds")
//    println(s"matBColIds = $matBColIds")
//    println()
//    println(s"matARowIds = $matARowIds")
//    println(s"matBRowIds = $matBRowIds")
//    println()
//    println(s"goldenData = $goldenData")
//    println(s"goldenColIds = $goldenColIds")
//    println(s"goldenRowIds = $goldenRowIds")
//    println()

    val design = new CsrMatrixAdderAccelerator
    assert(design.Acols >= maxACols && design.Bcols >= maxBCols, "this test doesn't tile over the innermost vector dimension")

    test(design.toChiselModule).withAnnotations(Seq(/*WriteVcdAnnotation,*/ VcsBackendAnnotation)) { c =>
      resetAccelerator(c)
      c.clock.setTimeout(100000)

      // Write A's row-coords into it's SRAM
      {
        val sram_write_port = c.getWritePortForSram(design.sramA)

        sram_write_port.valid.poke(true.B)
        sram_write_port.bits.is_data.poke(false.B)
        sram_write_port.bits.from_regfile(0).poke(false.B)
        sram_write_port.bits.axis.poke(0.U)
        sram_write_port.bits.metadata_buffer_id.poke(0.U)

        sram_write_port.bits.spans.foreach(_.poke(1.U))

        sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        sram_write_port.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

        sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

        for ((row_coord, row) <- matARowIds.zipWithIndex) {
          sram_write_port.bits.address(0).poke(0.U)
          sram_write_port.bits.address(1).poke(row.U)
          sram_write_port.bits.address.drop(2).foreach(_.poke(0.U))

          sram_write_port.bits.data(0).poke(row_coord.S)

          assert(sram_write_port.ready.peek().litToBoolean, "Write port was not ready")
          c.clock.step()
        }

        sram_write_port.valid.poke(false.B)
      }

      // Write A's read_data and col-coords into it's SRAM
      for ((mat, isData) <- Seq((matAData, true), (matAColIds, false))) {
        val sram = design.sramA
        val sram_write_port = c.getWritePortForSram(design.sramA)

        sram_write_port.valid.poke(true.B)
        sram_write_port.bits.is_data.poke(isData.B)
        sram_write_port.bits.from_regfile(0).poke(false.B)
        sram_write_port.bits.axis.poke(0.U)
        sram_write_port.bits.metadata_buffer_id.poke(CompressedMetadata.inner_metadata_buffer_id.U)

        sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        sram_write_port.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

        sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

        for (row <- 0 until nRows) {
          val nCols = mat(row).size

          var col = 0
          while (col < nCols) {
            val n_elems_written = (nCols - col).min(sram.elemsPerWrite)

            sram_write_port.bits.address(0).poke(col.U)
            sram_write_port.bits.address(1).poke(row.U)
            sram_write_port.bits.address.drop(2).foreach(_.poke(0.U))

            sram_write_port.bits.spans(0).poke(n_elems_written.U)
            sram_write_port.bits.spans.tail.foreach(_.poke(1.U))

            for (coff <- 0 until n_elems_written)
              sram_write_port.bits.data(coff).poke(mat(row)(col+coff).S)

            if (sram_write_port.ready.peek().litToBoolean)
              col += sram.elemsPerWrite
            c.clock.step()
          }
        }

        sram_write_port.valid.poke(false.B)
      }

      // Write B into SRAM
      {
        // Initialize head pointers
        val sram_write_port = c.getWritePortForSram(design.sramC)

        sram_write_port.valid.poke(true.B)
        sram_write_port.bits.is_data.poke(false.B)
        sram_write_port.bits.address.foreach(_.poke(0.U))
        sram_write_port.bits.axis.poke(0.U)
        sram_write_port.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
        sram_write_port.bits.spans.foreach(_.poke(1.U))
        sram_write_port.bits.spans(1).poke(nRows.U)
        sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        sram_write_port.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
        sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

        assert(sram_write_port.ready.peek().litToBoolean)
        c.clock.step()
        sram_write_port.valid.poke(false.B)

        // Write in read_data and col-ids
        sram_write_port.valid.poke(true.B)
        sram_write_port.bits.axis.poke(0.U)
        sram_write_port.bits.spans(1).poke(1.U)
        sram_write_port.bits.axis.poke(0.U)
        sram_write_port.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

        sram_write_port.bits.data_strides.foreach(_.poke(0.U))
        sram_write_port.bits.data_strides(0).poke(1.U)

        sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        sram_write_port.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)

        sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

        for ((mat, is_data) <- Seq((matBData, true), (matBColIds, false))) {
          sram_write_port.bits.is_data.poke(is_data.B)

          for (row <- 0 until nRows) {
            val nCompressedCols = mat(row).size
            val elemsPerWrite = design.sramC.elemsPerWrite

            var col = 0
            while (col < nCompressedCols) {
              val n_elems_written = (nCompressedCols - col).min(elemsPerWrite)
              sram_write_port.bits.address(0).poke(col.U)
              sram_write_port.bits.address(1).poke(row.U)

              sram_write_port.bits.spans(0).poke(n_elems_written.U)

              for (i <- 0 until n_elems_written)
                sram_write_port.bits.data(i).poke(mat(row)(col+i).S)

              if(sram_write_port.ready.peek().litToBoolean)
                col += elemsPerWrite

              c.clock.step()
            }
          }
        }

        sram_write_port.valid.poke(false.B)
      }

      // Wait for writes to complete
      waitWhileBusy(c)

      // Write A and B into their reg-files
      for (sram <- Seq(design.sramA, design.sramC)) {
        val read_req = c.getReadReqPortForSram(sram)

        read_req.valid.poke(true.B)
        read_req.bits.is_data.poke(true.B)
        read_req.bits.to_regfile.poke(true.B)
        read_req.bits.to_regfile_last_axis.poke(0.U)
        read_req.bits.address.foreach(_.poke(0.U))
        read_req.bits.spans(0).poke(maxVal(read_req.bits.spans(0)).U)
        read_req.bits.spans(1).poke(nRows.U)

        read_req.bits.data_strides.foreach(_.poke(0.U))
        read_req.bits.data_strides(0).poke(1.U)

        read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        read_req.bits.metadata_strides(1)(0)(0).poke(1.U); assert(CompressedMetadata.outer_metadata_buffer_id == 0 && LinkedListMetadata.head_ptr_buffer_id == 0, "if this assertion fails, then this line of code needs to be changed")

        read_req.bits.iteration_strides.foreach(_.poke(1.U))

        read_req.bits.interleave.should_push.poke(false.B)
        read_req.bits.interleave.should_pop.poke(false.B)

        read_req.ready.expect(true.B)
        c.clock.step()

        read_req.valid.poke(false.B)
      }

      // Write C into the SRAM
      {
        val write_req = c.getWritePortForSram(design.sramC)

        write_req.valid.poke(true.B)
        write_req.bits.from_regfile(0).poke(true.B)
        write_req.bits.from_regfile_last_axis.poke(0.U)
        write_req.bits.address.foreach(_.poke(0.U))
        write_req.bits.spans(0).poke(maxVal(write_req.bits.spans(0)).U)
        write_req.bits.spans(1).poke(nRows.U)

        write_req.bits.data_strides(0).poke(1.U)
        write_req.bits.data_strides.tail.foreach(_.poke(0.U))

        write_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        write_req.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

        write_req.bits.iteration_strides.foreach(_.poke(1.U))

        write_req.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false.B)))
        write_req.bits.from_regfile_metadata.head(CompressedMetadata.inner_metadata_buffer_id).valid.poke(true.B)
        write_req.bits.from_regfile_metadata.head(CompressedMetadata.inner_metadata_buffer_id).coord.poke(0.U)

        write_req.bits.interleave.should_push.poke(false.B)
        write_req.bits.interleave.should_pop.poke(false.B)
        write_req.bits.should_trail_reads.poke(true.B)

        write_req.ready.expect(true.B)
        c.clock.step()

        write_req.valid.poke(false.B)
      }

      waitWhileBusy(c)

      // Read C out of SRAM
      val result_data = Seq.fill(goldenData.size)(ArrayBuffer.empty[Int])
      val result_coords = Seq.fill(goldenData.size)(ArrayBuffer.empty[Int])

      for ((is_data, result) <- Seq((false, result_coords), (true, result_data))) {
        val read_req = c.getReadReqPortForSram(design.sramC)
        val read_resp = c.getReadRespPortsForSram(design.sramC)

        read_req.valid.poke(true.B)
        read_req.bits.to_regfile.poke(false.B)
        read_req.bits.address.foreach(_.poke(0.U))
        read_req.bits.spans(0).poke(maxVal(read_req.bits.spans(0)).U)
        read_req.bits.spans(1).poke(nRows.U)
        read_req.bits.spans.drop(2).foreach(_.poke(1.U))

        read_req.bits.data_strides(0).poke(1.U)
        read_req.bits.data_strides.tail.foreach(_.poke(0.U))

        read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
        read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)

        read_req.bits.iteration_strides.foreach(_.poke(1.U))

        read_req.bits.is_data.poke(is_data.B)
        read_req.bits.axis.poke(0.U)
        read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

        read_req.bits.interleave.should_push.poke(false.B)
        read_req.bits.interleave.should_pop.poke(false.B)

        read_req.ready.expect(true.B)

        c.clock.step()

        read_req.valid.poke(false.B)

        read_resp.ready.poke(true.B)
        while (c.io.busy.peek().litToBoolean) {
          if (read_resp.valid.peek().litToBoolean) {
            val resp = read_resp.bits

            val row = resp.compressed_address(1).peek().litValue

            val cols = resp.spans(0).peek().litValue
            assert(resp.spans.tail.forall(_.peek().litValue == 1))

            for (coff <- 0 until cols.toInt) {
              val data = resp.data(coff).peek().litValue.toInt
              result(row.toInt) += data
            }
          }

          c.clock.step()
        }
      }

//      println()
//      println(s"result_data = $result_data")
//      println(s"result_coords = $result_coords")
//      println()

      assert(result_coords == goldenColIds, s"coords don't match (seed = $seed)")
      assert(result_data == goldenData, s"read_data doesn't match (seed = $seed)")
    }
  }

  for (phase <- Seq("matmul", "merge"))
    s"OuterSpace-like $phase accelerator works" in {
      val seed = System.currentTimeMillis
      implicit val rand: java.util.Random = new Random()
      rand.setSeed(seed)

      {
        val fileWriter = new java.io.FileWriter(s"outerspace-$phase-seed.txt")
        fileWriter.write(s"$seed")
        fileWriter.close()
      }

      val size = 2

      val maxI = if (phase == "merge") size else (size * 4)
      val maxJ = size * 16
      val maxK = if (phase == "matmul") size else (size * 4)

      val iTiles = (maxI / size) + (if (maxI % size != 0) 1 else 0)
      val kTiles = (maxK / size) + (if (maxK % size != 0) 1 else 0)

      val sparsity = 0.5

      val aT_coords = Seq.fill(maxK)((0 until maxI).filter(_ => randBool(sparsity)))
      val aT_data = aT_coords.map(row => Seq.fill(row.size)(randInt()))

      val b_coords = Seq.fill(maxK)((0 until maxJ).filter(_ => randBool(sparsity)))
      val b_data = b_coords.map(row => Seq.fill(row.size)(randInt()))

      case class Result(data: Int, i: Int, j: Int, k: Int = -1)

      val scattered_result = (for (k <- 0 until maxK) yield {
        for ((a,i) <- aT_data(k) zip aT_coords(k)) yield {
          for ((b,j) <- b_data(k) zip b_coords(k)) yield {
            Result(a * b, i, j, k)
          }
        }
      }).flatten.flatten

      val merged_result = (for (i <- 0 until maxI) yield {
        val scattered_i = scattered_result.filter(_.i == i)
        for (j <- 0 until maxJ) yield {
          val scattered_ij = scattered_i.filter(_.j == j)
          if (scattered_ij.isEmpty) None
          else Some(Result(scattered_ij.map(_.data).sum, i, j))
        }
      }).flatten.flatten

      val scattered_data = (0 until maxI).map { i =>
        val groupedByI = scattered_result.groupBy(_.i).getOrElse(i, Seq.empty)
        val groupedByK = groupedByI.groupBy(_.k)
        (0 until maxK).map(k => groupedByK.getOrElse(k, Seq.empty[Result]).map(_.data))
      }
      val scattered_j_coords = (0 until maxI).map { i =>
        val groupedByI = scattered_result.groupBy(_.i).getOrElse(i, Seq.empty)
        val groupedByK = groupedByI.groupBy(_.k)
        (0 until maxK).map(k => groupedByK.getOrElse(k, Seq.empty[Result]).map(_.j))
      }
      val scattered_j_rowlens = (0 until maxI).map { i =>
        val groupedByI = scattered_result.groupBy(_.i).getOrElse(i, Seq.empty)
        val groupedByK = groupedByI.groupBy(_.k)
        (0 until maxK).map(k => groupedByK.getOrElse(k, Seq.empty[Result]).size)
      }
      val scattered_k_coords = (0 until maxI).map { i =>
        val groupedByI = scattered_result.groupBy(_.i).getOrElse(i, Seq.empty)
        groupedByI.map(_.k).distinct.sorted
      }

      val merged_data = (0 until maxI).map { i =>
        val groupedByI = merged_result.groupBy(_.i).getOrElse(i, Seq.empty)
        groupedByI.sortBy(_.j).map(_.data)
      }
      val merged_coords = (0 until maxI).map { i =>
        val groupedByI = merged_result.groupBy(_.i).getOrElse(i, Seq.empty)
        groupedByI.sortBy(_.j).map(_.j)
      }
      val merged_row_ids = merged_coords.scanLeft(0)(_ + _.size)

      println()
      println(s"$phase aT_coords = $aT_coords")
      println(s"$phase aT_data = $aT_data")
      println()
      println(s"$phase b_coords = $b_coords")
      println(s"$phase b_data = $b_data")
      println()
      println(s"$phase scattered_result = $scattered_result")
      println(s"$phase merged_result = $merged_result")
      println()
      println(s"$phase scattered_data = $scattered_data")
      println(s"$phase scattered_j_coords = $scattered_j_coords")
      println(s"$phase scattered_j_rowlens = $scattered_j_rowlens")
      println(s"$phase scattered_k_coords = $scattered_k_coords")
      println()
      println(s"$phase merged_data = $merged_data")
      println(s"$phase merged_coords = $merged_coords")
      println(s"$phase merged_row_ids = $merged_row_ids")
      println()

      val design = new OuterSpace(size = size, withAsserts = size <= 2)
      test(design.toChiselModule).withAnnotations(Seq(WriteVcdAnnotation, VcsBackendAnnotation)) { c =>
        println("Resetting accelerator")
        resetAccelerator(c)
        // c.clock.setTimeout(500)

        def init_head_pointers(sram: SRAM, nHeads: Int, axis: Int, baseHead: Int = 0): Unit = {
          val write = c.getWritePortForSram(sram)

          write.valid.poke(true.B)
          write.bits.is_data.poke(false.B)
          write.bits.reset_running_state.poke(false)
          write.bits.from_regfile.foreach(_.poke(false.B))
          write.bits.address.foreach(_.poke(0.U))
          write.bits.address(axis+1).poke(baseHead)
          write.bits.axis.poke(axis)
          write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
          write.bits.spans.foreach(_.poke(1.U))
          write.bits.spans(axis+1).poke(nHeads)
          write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          write.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0.U))))
          write.bits.metadata_strides(axis+1)(axis)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
          write.bits.iteration_strides.foreach(_.poke(1.U))

          waitUntilReady(c.clock, write.ready)
          c.clock.step()
          write.valid.poke(false.B)
        }

        def init_sram_scattered_c_j_head_pointers(spans: Seq[Int] = Seq(maxI, maxK)): Unit = {
          val write = c.getWritePortForSram(design.sramScatteredC)

          write.valid.poke(true)
          write.bits.is_data.poke(false.B)
          write.bits.address.foreach(_.poke(0.U))
          write.bits.axis.poke(0.U)
          write.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)

          require(spans.size == 2)
          write.bits.spans.foreach(_.poke(1.U))
          write.bits.spans.tail.zip(spans).foreach { case (x, y) => x.poke(y) }

          write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          write.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0.U))))
          write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)
          write.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1)

          write.bits.iteration_strides.foreach(_.poke(1.U))

          waitUntilReady(c.clock, write.ready)
          c.clock.step()
          write.valid.poke(false)
        }

        def write_2d_mat_into_sram(sram: SRAM, mat: Seq[Seq[Int]], is_data: Boolean, axis: Int): Unit = {
          val nRows = mat.size

          val sram_write_port = c.getWritePortForSram(sram)

          sram_write_port.valid.poke(true.B)
          sram_write_port.bits.is_data.poke(is_data)
          sram_write_port.bits.from_regfile(0).poke(false.B)
          sram_write_port.bits.axis.poke(axis.U)
          sram_write_port.bits.metadata_buffer_id.poke({
            assert(CompressedMetadata.inner_metadata_buffer_id == LinkedListMetadata.coord_buffer_id)
            CompressedMetadata.inner_metadata_buffer_id
          })

          sram_write_port.bits.data_strides.foreach(_.poke(0.U))
          sram_write_port.bits.data_strides(0).poke(1.U)

          sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
          sram_write_port.bits.metadata_strides(axis + 1)(axis)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)
          sram_write_port.bits.metadata_strides(axis)(axis)(CompressedMetadata.inner_metadata_buffer_id).poke(1.U)

          sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

          for (row <- 0 until nRows) {
            val nCols = mat(row).size

            var col = 0
            while (col < nCols) {
              val n_elems_written = (nCols - col).min(sram.elemsPerWrite)

              sram_write_port.bits.address.foreach(_.poke(0.U))
              sram_write_port.bits.address(axis).poke(col.U)
              sram_write_port.bits.address(axis + 1).poke(row.U)

              sram_write_port.bits.spans.foreach(_.poke(1.U))
              sram_write_port.bits.spans(axis).poke(n_elems_written.U)

              for (coff <- 0 until n_elems_written)
                sram_write_port.bits.data(coff).poke(mat(row)(col + coff).S)

              if (sram_write_port.ready.peekBoolean())
                col += sram.elemsPerWrite
              c.clock.step()
            }
          }

          sram_write_port.valid.poke(false.B)
        }

        // Matmul phase
        if (phase == "matmul") {
          println("Matmul phase")
          assert(maxK <= size)

          // Initialize the ScatteredC SRAM's i head pointers
          println(s"$phase Initialize the ScatteredC SRAM's i head pointers")
          init_head_pointers(design.sramScatteredC, maxK, 1)

          // Write A and B from DRAM into the SRAM
          println(s"$phase Write A and B from DRAM into the SRAM");
          {
            // Write A and B row-ids into SRAM
            println(s"$phase Write A and B row-ids into SRAM")
            for ((sram, data) <- Seq((design.sramA, aT_data), (design.sramB, b_data))) {
              val nRows = data.size
              val sram_write_port = c.getWritePortForSram(sram)

              sram_write_port.valid.poke(true.B)
              sram_write_port.bits.is_data.poke(false.B)
              sram_write_port.bits.from_regfile(0).poke(false.B)
              sram_write_port.bits.axis.poke(0.U)
              sram_write_port.bits.metadata_buffer_id.poke(0.U)

              sram_write_port.bits.spans.foreach(_.poke(1.U))

              sram_write_port.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
              sram_write_port.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id).poke(1.U)

              sram_write_port.bits.iteration_strides.foreach(_.poke(1.U))

              var row_coord = 0
              for (row <- 0 until nRows+1) {
                sram_write_port.bits.address(0).poke(0.U)
                sram_write_port.bits.address(1).poke(row.U)
                sram_write_port.bits.address.drop(2).foreach(_.poke(0.U))

                sram_write_port.bits.data(0).poke(row_coord.S)

                assert(sram_write_port.ready.peek().litToBoolean, "Write port was not ready")
                c.clock.step()

                if (row < maxK)
                  row_coord += data(row).size
              }

              sram_write_port.valid.poke(false.B)
            }

            // Write data and col-coords into SRAM
            println(s"$phase Write data and col-coords into SRAM");
            for ((sram, mat, is_data, axis) <- Seq(
              (design.sramA, aT_coords, false, 0), (design.sramA, aT_data, true, 0),
              (design.sramB, b_coords, false, 0), (design.sramB, b_data, true, 0),
              (design.sramScatteredC, aT_coords, false, 1),
            )) {
              write_2d_mat_into_sram(sram, mat, is_data, axis)
            }
          }

          // Initialize the ScatteredC SRAM's j-head pointers
          println(s"$phase Initialize the ScatteredC SRAM's j-head pointers");
          init_sram_scattered_c_j_head_pointers()

          println(s"$phase Waiting for all DRAM to SRAM writes to finish")
          waitWhileBusy(c)

          // Read A and B from SRAM into regfiles
          println(s"$phase Read A and B from SRAM into regfiles");
          {
            /*
            A:
            for i_outer
              for k_inner
                for i_inner
                  k = k_inner
                  i = i_outer * dim + i_inner

                  Row (k) address is:
                  k
                  (i_outer * 0) + k_inner + (i_inner * 0)

                  Col (i) address is:
                  i
                  (i_outer * dim) + (k_inner * 0) + i_inner

            B:
            for i_outer
              for k_inner
                for j_inner
                  j = j_inner
                  k = k_inner

                  Row address is:
                  k
                  (i_outer * 0) + k_inner + (j_inner * 0)

                  Col address is:
                  j
                  (i_outer * 0) + (k_inner * 0) + j_inner
            */

            val srams = Seq(
              (design.sramA, Some(size), Seq(0, size), Seq(1, 0)),
              (design.sramB, None, Seq(0, 0), Seq(1, 0)),
            )
            for ((sram, nColsOpt, dataStrides, metadataStrides) <- srams) {
              val read_req = c.getReadReqPortForSram(sram)

              read_req.valid.poke(true)
              read_req.bits.is_data.poke(true)
              read_req.bits.to_regfile.poke(true)
              read_req.bits.to_regfile_last_axis.poke(1)
              read_req.bits.address.foreach(_.poke(0))

              read_req.bits.spans.foreach(_.poke(1))
              read_req.bits.spans(0).poke(nColsOpt.map(BigInt.apply).getOrElse(maxVal(read_req.bits.spans(0))))
              read_req.bits.spans(1).poke(size)
              read_req.bits.spans(2).poke(iTiles)

              read_req.bits.data_strides.foreach(_.poke(0))
              read_req.bits.data_strides(0).poke(1)
              read_req.bits.data_strides.tail.zip(dataStrides).foreach { case (p, s) => p.poke(s) }

              read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
              read_req.bits.metadata_strides.tail.map(_(0)).zip(dataStrides).zip(metadataStrides).foreach {
                case ((p, ds), ms) =>
                  p(CompressedMetadata.outer_metadata_buffer_id).poke(ms)
                  p(CompressedMetadata.inner_metadata_buffer_id).poke(ds)
              }

              read_req.bits.iteration_strides.foreach(_.poke(1))

              read_req.bits.interleave.should_push.poke(false)
              read_req.bits.interleave.should_pop.poke(false)

              read_req.ready.expect(true)
            }
          }

          // Write ScatteredC from regfiles into SRAM
          println(s"$phase Write ScatteredC from regfiles into SRAM");
          {
            /*
            ScatteredC:
            for i_outer
              for k_inner
                for i_inner
                  for j_inner
                    i = i_outer * dim + i_inner
                    k = k_inner
                    j = j_inner

                    I-row-address is:
                    k

                    I-col-address is:
                    i
                    (i_outer * dim) + (k_inner * 0) + i_inner + (j_inner * 0)

                    J-row-address is:
                    i-i_addr

                    J-col-address is:
                    j
                    (i_outer * 0) + (k_inner * 0) + (i_inner * 0) + j_inner
            */

            val write = c.getWritePortForSram(design.sramScatteredC)

            write.valid.poke(true)
            write.bits.axis.poke(0)
            write.bits.is_data.poke(true)
            write.bits.from_regfile(0).poke(true)
            write.bits.from_regfile_last_axis.poke(2.U)
            write.bits.address.foreach(_.poke(0.U))

            write.bits.spans(0).poke(maxJ)
            write.bits.spans(1).poke(size)
            write.bits.spans(2).poke(maxK)
            write.bits.spans(3).poke(iTiles)

            write.bits.data_strides.foreach(_.poke(0))
            write.bits.data_strides(0).poke(1)

            write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
            write.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0))))
            write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // I-row address
            write.bits.metadata_strides(3)(1)(LinkedListMetadata.coord_buffer_id).poke(size) // I-col address
            write.bits.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id).poke(1) // I-col address
            write.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // J-row address
            write.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id).poke(1) // J-col address

            write.bits.iteration_strides.foreach(_.poke(1.U))

            write.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false.B)))
            write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
            write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).coord.poke(1.U)
            write.bits.from_regfile_metadata(1)(LinkedListMetadata.coord_buffer_id).valid.poke(true.B)
            write.bits.from_regfile_metadata(1)(LinkedListMetadata.coord_buffer_id).coord.poke(0.U)

            write.bits.interleave.should_push.poke(false)
            write.bits.interleave.should_pop.poke(false)
            write.bits.should_trail_reads.poke(false)

            write.ready.expect(true)
          }

          // Wait for matmul to finish
          println(s"$phase Wait for matmul to finish");
          {
            c.clock.step()
            design.sramCodes.keys.foreach { sram =>
              c.getReadReqPortForSram(sram).valid.poke(false)
              c.getWritePortForSram(sram).valid.poke(false)
            }
            waitWhileBusy(c)
          }

          // Read ScatteredC from SRAM into DRAM
          println(s"$phase Read ScatteredC from SRAM into DRAM");
          {
            val read_req = c.getReadReqPortForSram(design.sramScatteredC)
            val read_resp = c.getReadRespPortsForSram(design.sramScatteredC)

            // Read out i coords
            println(s"$phase Read out i coords");
            {
              val result_coords = aT_coords.map(row => ArrayBuffer.fill(row.size)(-1))

              read_req.valid.poke(true.B)
              read_req.bits.axis.poke(1.U)
              read_req.bits.is_data.poke(false.B)
              read_req.bits.to_regfile.poke(false.B)
              read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

              read_req.bits.spans.foreach(_.poke(1.U))
              pokeMaxVal(read_req.bits.spans(1))
              read_req.bits.spans(2).poke(maxK)

              read_req.bits.address.foreach(_.poke(0.U))

              read_req.bits.data_strides.foreach(_.poke(0))

              read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
              read_req.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0))))
              read_req.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // I-row address
              read_req.bits.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id).poke(1) // I-col address

              read_req.bits.iteration_strides.foreach(_.poke(1.U))

              read_req.ready.expect(true.B)

              c.clock.step()

              read_req.valid.poke(false.B)
              read_resp.ready.poke(true.B)

              while (c.io.busy.peek().litToBoolean) {
                if (read_resp.valid.peekBoolean()) {
                  val i_axis = 1
                  val k_axis = 2

                  val n_elems = read_resp.bits.spans(i_axis).peek().litValue.toInt
                  assert(read_resp.bits.spans.patch(i_axis, Nil, 1).forall(_.peek().litValue.toInt <= 1), "not reading out a 1d vector")

                  val data = read_resp.bits.data
                  val i_addr = read_resp.bits.compressed_address(i_axis).peekInt().toInt
                  val k_addr = read_resp.bits.compressed_address(k_axis).peekInt().toInt

                  println(s"\t$phase Writing $n_elems elems to $k_addr, $i_addr")

                  for (elem_id <- 0 until n_elems) {
                    println(s"\t\t$phase Writing ${data(elem_id).peekInt()} to $k_addr, ${i_addr + elem_id}")
                    result_coords(k_addr)(i_addr + elem_id) = data(elem_id).peekInt().toInt
                  }
                }

                c.clock.step()
              }

              read_resp.ready.poke(false)

              println(s"$phase result_coords = $result_coords")
              assert(result_coords == aT_coords)
            }

            // Read out data, j_coords, and j_row_ids
            println(s"$phase Read out data, j_coords, and j_row_ids");
            {
              val result_rowlens = scattered_j_rowlens.map(x => ArrayBuffer.fill(x.size)(0))
              val result_coords = scattered_j_coords.map(_.map(x => ArrayBuffer.fill(x.size)(-1)))
              val result_data = scattered_data.map(_.map(x => ArrayBuffer.fill(x.size)(-1)))

              read_req.valid.poke(true)
              read_req.bits.axis.poke(0)
              read_req.bits.is_data.poke(false)
              read_req.bits.to_regfile.poke(false)

              read_req.bits.spans.foreach(_.poke(1))
              pokeMaxVal(read_req.bits.spans(0))

              read_req.bits.data_strides.foreach(_.poke(0.U))
              read_req.bits.data_strides(0).poke(1.U)

              read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
              read_req.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0))))
              read_req.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // I-row address
              read_req.bits.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id).poke(1) // I-col address
              read_req.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // J-row address
              read_req.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id).poke(1) // J-col address

              read_req.bits.iteration_strides.foreach(_.poke(1.U))

              read_resp.ready.poke(true.B)

              var k = 0
              var i = 0
              var resetted_lens = false
              var requested_coords = false
              var requested_data = false
              while (k < maxK || c.io.busy.peek().litToBoolean) {
                /*
                Step 1: Reset lens
                Step 2: Request coords
                Step 3: Request read_data
                Step 4: Request row-len
                */
                read_req.valid.poke(k < maxK && aT_coords(k).nonEmpty)
                read_req.bits.address(0).poke(0)
                read_req.bits.address(1).poke(i.U)
                read_req.bits.address(2).poke(k.U)
                read_req.bits.reset_running_state.poke(!resetted_lens)
                read_req.bits.is_data.poke(requested_coords && !requested_data)
                if (requested_coords)
                  read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
                else /* if (resetted_lens) */
                  read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)

                if (read_req.ready.peekBoolean() || k < maxK && aT_coords(k).isEmpty) {
                  if (requested_data || k < maxK && aT_coords(k).isEmpty) {
                    i += 1

                    if (k < maxK && i >= aT_coords(k).size) {
                      i = 0
                      k += 1
                    }

                    resetted_lens = false
                    requested_coords = false
                    requested_data = false
                  } else if (requested_coords) {
                    requested_data = true
                  } else if (resetted_lens) {
                    requested_coords = true
                  } else {
                    resetted_lens = true
                  }
                }

                if (read_resp.valid.peek().litToBoolean) {
                  val read_data = read_resp.bits.data.map(_.peek().litValue.toInt)
                  val addr = read_resp.bits.compressed_address.map(_.peek().litValue.toInt)
                  val spans = read_resp.bits.spans.map(_.peek().litValue.toInt)
                  val metadata_buffer_id = read_resp.bits.metadata_buffer_id.peekInt()
                  val is_data = read_resp.bits.is_data.peekBoolean()

                  val Seq(j_, i_, k_) = addr.take(3)
                  println(s"\t$phase j_ = $j_ | i_ = $i_ | k_ = $k_")
                  println(s"\t\t$phase metadata_buffer_id = $metadata_buffer_id | is_data = $is_data | spans = $spans")
                  val i__ = aT_coords(k_)(i_)
                  println(s"\t\t$phase i__ = $i__")

                  if (is_data || metadata_buffer_id == LinkedListMetadata.coord_buffer_id) {
                    val n_elems = spans.head
                    assert(spans.tail.forall(_ == 1), s"spans are $spans")

                    for (elem_id <- 0 until n_elems) {
                      if (is_data) {
                        println(s"\t\t\t$phase Writing data=${read_data(elem_id)} to result_data_${Seq(i__,k_,j_ + elem_id).mkString("_")}")
                        result_data(i__)(k_)(j_ + elem_id) = read_data(elem_id)
                      } else {
                        println(s"\t\t\t$phase Writing coord=${read_data(elem_id)} to result_coords_${Seq(i__,k_,j_ + elem_id).mkString("_")}")
                        result_coords(i__)(k_)(j_ + elem_id) = read_data(elem_id)
                      }
                    }
                  } else {
                    assert(spans.forall(_ == 1), s"spans are $spans")
                    println(s"\t\t\t$phase Writing rowlen=${read_data.head} to result_rowlens_${Seq(i__,k_).mkString("_")}")
                    result_rowlens(i__)(k_) = read_data.head
                  }
                }

                c.clock.step()
              }

              read_req.valid.poke(false)
              read_resp.ready.poke(false)

              println(s"$phase result_rowlens = $result_rowlens")
              println(s"$phase result_coords = $result_coords")
              println(s"$phase result_data = $result_data")
              assert(result_rowlens == scattered_j_rowlens, "j_rowlens are incorrect")
              assert(result_coords == scattered_j_coords, "j_coords are incorrect")
              assert(result_data == scattered_data, "read_data are incorrect")
            }
          }
        }

        // Merge phase
        else if (phase == "merge") {
          println("Merge phase")
          assert(maxI <= size)

          // Reset SramScatteredC
          println(s"$phase Reset SramScatteredC");
          {
            val write = c.getWritePortForSram(design.sramScatteredC)

            // Reset j head pointers
            println(s"\t$phase Reset j head pointers")
            write.valid.poke(true)
            write.bits.axis.poke(0)
            write.bits.reset_running_state.poke(true)
            write.bits.from_regfile.foreach(_.poke(false))
            write.bits.should_trail_reads.poke(false)
            write.bits.address.foreach(_.poke(0))
            write.bits.iteration_strides.foreach(_.poke(1))

            write.bits.spans.foreach(_.poke(1))

            waitUntilReady(c.clock, write.ready)
            c.clock.step()
            write.valid.poke(false)

            // Reset i head pointers
            println(s"\t$phase Reset i head pointers")
            write.valid.poke(true)
            write.bits.axis.poke(1)

            waitUntilReady(c.clock, write.ready)
            c.clock.step()
            write.valid.poke(false)
            write.bits.reset_running_state.poke(false)
          }

          // Write from DRAM into SramScatteredC
          println(s"$phase Write from DRAM into SramScatteredC");
          {
            // Init k head pointers
            println(s"\t$phase Init k head pointers")
            init_head_pointers(design.sramScatteredC, maxI, 1)

            // Write k coords
            println(s"\t$phase Write k coords");
            write_2d_mat_into_sram(design.sramScatteredC, scattered_k_coords, false, 1)

            // Init j-head pointers
            println(s"\t$phase Init j-head pointers")
            init_sram_scattered_c_j_head_pointers(Seq(maxK, maxI))

            // Write j-coords
            println(s"\t$phase Write j-coords");
            {
              val write = c.getWritePortForSram(design.sramScatteredC)
              val elemsPerWrite = design.sramScatteredC.elemsPerWrite

              write.valid.poke(true.B)
              write.bits.from_regfile.foreach(_.poke(false.B))
              write.bits.axis.poke(0.U)
              write.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id)

              write.bits.data_strides.foreach(_.poke(0.U))
              write.bits.data_strides(0).poke(1.U)

              write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
              write.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0.U))))
              write.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // K-row address
              write.bits.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id).poke(1) // K-col address
              write.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // J-row address
              write.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id).poke(1) // J-col address

              write.bits.iteration_strides.foreach(_.poke(1.U))

              for ((mat, is_data) <- Seq((scattered_data, true), (scattered_j_coords, false))) {
                write.bits.is_data.poke(is_data)

                for (i <- 0 until maxI) {
                  for ((k_exp, k_comp) <- scattered_k_coords(i).zipWithIndex) {
                    val nJ = mat(i)(k_exp).size
                    var j = 0

                    while (j < nJ) {
                      val n_elems_written = (nJ - j).min(elemsPerWrite)

                      write.bits.address.foreach(_.poke(0))
                      write.bits.address(0).poke(j)
                      write.bits.address(1).poke(k_comp)
                      write.bits.address(2).poke(i)

                      write.bits.spans.foreach(_.poke(1.U))
                      write.bits.spans(0).poke(n_elems_written.U)

                      for (off <- 0 until n_elems_written)
                        write.bits.data(off).poke(mat(i)(k_exp)(j + off).S)

                      if (write.ready.peekBoolean())
                        j += elemsPerWrite
                      c.clock.step()
                    }
                  }
                }
              }

              write.valid.poke(false.B)
            }
          }

          // Initialize SramMergedC
          println(s"$phase Initialize SramMergedC")
          for (baseHead <- Seq(0, design.sramMergedC_nHeads/2))
            init_head_pointers(design.sramMergedC, maxI, 0, baseHead)

          // Wait for all writes to finish
          println(s"$phase Wait for all writes to finish")
          waitWhileBusy(c)

          // Read out from SramScatteredC and SramMergedC into regfiles
          println(s"$phase Read out from SramScatteredC and SramMergedC into regfiles");
          {
            /*
            ScatteredC:
            for k_outer
              for i
                for k_inner
                  for j
                    k = k_outer * dim + k_inner

                    K-row-address is:
                    i
                    (k_outer * 0) + i + (k_inner * 0) + (j * 0)

                    K-col-address is:
                    k
                    (k_outer * dim) + (i * 0) + k_inner + (j * 0)

                    J-row-address is:
                    k_addr

                    J-col-address is:
                    j
                    (k_outer * 0) + (i * 0) + (k_inner * 0) + j

            SramMergedC:
            for k_outer
              for i
                for j
                  J-row-address is:
                  k_outer * (nHeads / 2) + i

                  J-col-address is:
                  j
            */
            for ((sram, has_inner_k, should_interleave) <- Seq((design.sramScatteredC, true, false), (design.sramMergedC, false, true))) {
              val read_req = c.getReadReqPortForSram(sram)

              val Seq(i_axis, k_outer_axis) = if (has_inner_k) Seq(2, 3) else Seq(1, 2)

              read_req.valid.poke(true)
              read_req.bits.axis.poke(0)
              read_req.bits.is_data.poke(true)
              read_req.bits.to_regfile.poke(true)
              read_req.bits.to_regfile_last_axis.poke(if (has_inner_k) 2 else 1); assert(iTiles <= 1)
              read_req.bits.address.foreach(_.poke(0))

              read_req.bits.spans.foreach(_.poke(1))
              pokeMaxVal(read_req.bits.spans(0))
              read_req.bits.spans(1).poke(size)
              read_req.bits.spans(i_axis).poke(maxI); assert(iTiles <= 1)
              read_req.bits.spans(k_outer_axis).poke(kTiles)

              read_req.bits.data_strides.foreach(_.poke(0))
              read_req.bits.data_strides(0).poke(1)

              read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
              read_req.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0))))
              if (has_inner_k) {
                read_req.bits.metadata_strides(2)(1)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // K-row address
                read_req.bits.metadata_strides(3)(1)(LinkedListMetadata.coord_buffer_id).poke(size) // K-col address
                read_req.bits.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id).poke(1) // K-col address
                read_req.bits.metadata_strides_by_addr(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // J-row address
              } else {
                read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // J-row address
                read_req.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(design.sramMergedC_nHeads / 2) // J-row address
              }
              read_req.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id).poke(1) // J-col address

              read_req.bits.iteration_strides.foreach(_.poke(1))

              read_req.bits.interleave.should_push.poke(false)
              read_req.bits.interleave.should_pop.poke(should_interleave)
              read_req.bits.interleave.axis.poke(2)
            }
          }

          // Write from regfiles into SramMergedC
          println(s"$phase Write from regfiles into SramMergedC");
          {
            /*
            MergedC:
            for k_outer
              for i
                for j
                  J-row-address is:
                  (k_outer + 1) * (nHeads / 2) + i

                  J-col-address is:
                  j
            */

            val write = c.getWritePortForSram(design.sramMergedC)

            write.valid.poke(true)
            write.bits.axis.poke(0)
            write.bits.is_data.poke(true)
            write.bits.address.foreach(_.poke(0))
            write.bits.address(2).poke(1)
            write.bits.iteration_strides.foreach(_.poke(1.U))

            write.bits.from_regfile.foreach(_.poke(false))
            write.bits.from_regfile(0).poke(true)
            write.bits.from_regfile_last_axis.poke(1); assert(iTiles <= 1)

            write.bits.spans.foreach(_.poke(1))
            pokeMaxVal(write.bits.spans(0))
            write.bits.spans(1).poke(maxI)
            write.bits.spans(2).poke(kTiles)

            write.bits.data_strides.foreach(_.poke(0))
            write.bits.data_strides(0).poke(1)

            write.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0))))
            write.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0))))
            write.bits.metadata_strides(2)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(design.sramMergedC_nHeads / 2) // J-row address
            write.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1) // J-row address
            write.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id).poke(1) // J-col address

            write.bits.from_regfile_metadata.foreach(_.foreach(_.valid.poke(false)))
            write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).valid.poke(true)
            write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).coord.poke(1)

            write.bits.interleave.should_push.poke(true)
            write.bits.interleave.should_pop.poke(false)
            write.bits.interleave.axis.poke(2)
          }

          // Wait for merge to finish
          println(s"$phase Wait for merge to finish");
          {
            val ports = Seq(c.getReadReqPortForSram(design.sramScatteredC), c.getReadReqPortForSram(design.sramMergedC),
              c.getWritePortForSram(design.sramMergedC))
            for (port <- ports)
              waitUntilReady(c.clock, port.ready)
            c.clock.step()
            for (port <- ports)
              port.valid.poke(false)

            waitWhileBusy(c)
          }

          // Read out from SramMergedC into DRAM
          println(s"$phase Read out from SramMergedC into DRAM");
          {
            val result_data = merged_data.map(x => ArrayBuffer.fill(x.size)(-1))
            val result_coords = merged_coords.map(x => ArrayBuffer.fill(x.size)(-1))
            val result_row_ids = ArrayBuffer.fill(merged_row_ids.size)(-1)

            val baseHead = if (kTiles % 2 == 0) 0 else (design.sramMergedC_nHeads / 2)
            println(s"\t$phase baseHead = $baseHead")
            read_out_2d_csr_matrix(c, design.sramMergedC, result_data, result_coords, result_row_ids, baseHead)

            println()
            println(s"$phase result_data = $result_data")
            println(s"$phase result_coords = $result_coords")
            println(s"$phase result_row_ids = $result_row_ids")
            println()

            assert(result_row_ids == merged_row_ids, "merged data is incorrect")
            assert(result_coords == merged_coords, "merged data is incorrect")
            assert(result_data == merged_data, "merged data is incorrect")
          }
        }

        else {
          assert(false, s"unknown phase for outer-space-like test: $phase")
        }
      }
    }
}
