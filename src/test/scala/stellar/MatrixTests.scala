package stellar

import scala.collection.mutable.ArrayBuffer
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.ParallelTestExecution

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._

import TestUtil._

class MatrixTests extends AnyFreeSpec with ChiselScalatestTester with ParallelTestExecution {
  "Dense outer-product matmul works" in {
    cancel
    val design = new DenseMatmul
    test (design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val m1 = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))
      val m2 = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))
      val golden = MatrixUtil.matmul(m1, m2)

      val result = ArrayBuffer.fill(dim)(ArrayBuffer.fill(dim)(-1000))

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        val inputMats = Seq((m1, design.A), (m2, design.B))

        // Poke in the inputs
        for ((mat, inputVar) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val data = mat(coords(0))(coords(1))
              inPort.data.poke(data.S)
            }
          }
        }

        // Peek out the outputs
        val outPorts = c.getOutPortsForVar(design.C)
        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt
            result(coords(0))(coords(1)) = data
          }
        }

        c.clock.step()
      }

      assert(result == golden)
    }
  }

  "Sparse Gustavson matmul works" in {
    cancel
    val design = new GustavsonsMatmul
    test (design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val mat_a = Seq(
        Seq(-1, 0, 0, 5),
        Seq(2, -1, 0, 0),
      )

      val mat_b = Seq(
        Seq(9, 0, 3),
        Seq(-22, 44, 0),
        Seq(0, 0, 4),
        Seq(0, 8, 10),
      )

      val golden_c = MatrixUtil.matmul(mat_a, mat_b)

      val result_c = ArrayBuffer.fill(golden_c.size)(ArrayBuffer.fill(golden_c.head.size)(0))

      val compressed_a = mat_a.map(_.filter(_ != 0))
      assert(compressed_a.size == dim)
      assert(compressed_a.forall(_.size == dim))

      def get_indices(dense_mat: Seq[Seq[Int]]) = dense_mat.zipWithIndex.map { case (row, rowId) =>
        row.zipWithIndex.collect { case (col, colId) if col != 0 => (rowId, colId) }
      }

      val compressed_a_indices = get_indices(mat_a)
      val compressed_b_indices = get_indices(mat_b)

      def get_k(i: Int, k_compressed: Int) = try {
        compressed_a_indices(i)(k_compressed)._2
      } catch {
        case _: IndexOutOfBoundsException => -100
      }

      def get_j(i: Int, k_compressed: Int, j_compressed: Int) = {
        val k = get_k(i, k_compressed)
        try {
          compressed_b_indices(k)(j_compressed)._2
        } catch {
          case _: IndexOutOfBoundsException => -200
        }
      }

      val macs = compressed_a_indices.flatten.map { case (_, col_a) =>
        compressed_b_indices.flatten.count(_._1 == col_a)
      }.sum

      assert(macs == dim*dim*dim)

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        // Right now, the systolic array still requests data using the coordinates of the expanded matrices. In the
        // future, we might want to update this to use the coordinates of the compressed matrices instead
        val inputMats = Seq((mat_a, design.A), (mat_b, design.B))

        // Poke in the inputs
        c.io.compressed2ExpandedMappings.foreach { ioMappings =>
          val i = ioMappings.compressed(0).peek().litValue.toInt
          val j_compressed = ioMappings.compressed(1).peek().litValue.toInt
          val k_compressed = ioMappings.compressed(2).peek().litValue.toInt

          val j_expanded = get_j(i, k_compressed, j_compressed)
          val k_expanded = get_k(i, k_compressed)

          ioMappings.expanded(1).poke(j_expanded.S)
          ioMappings.expanded(2).poke(k_expanded.S)
        }

        for ((mat, inputVar) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val data = mat(coords(0))(coords(1))
              inPort.data.poke(data.S)
            }
          }
        }

        // Peek out the outputs
        val outPorts = c.getOutPortsForVar(design.C)
        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt
            result_c(coords(0))(coords(1)) += data
          }
        }

        c.clock.step()
      }

      assert(golden_c == result_c)
    }
  }

  "Sparse load-balanced outer-product matmul works" in {
    val design = new OuterMatmul(isUnbounded = false) // TODO The load-balanced OuterMatmul doesn't seem to work when it is unbounded
    test (design.toChiselModule(true)).withAnnotations(Seq(/*VcsBackendAnnotation, WriteVcdAnnotation*/)) { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val mat_a = Seq(
        Seq(-1, 0),
        Seq(1, 0),
        Seq(15, 0),
        Seq(0, 0),
        Seq(51, 0),
      )

      val mat_b = Seq(
        Seq(9, 0, 4),
        Seq(0, 1, 0),
      )

      val golden_c = MatrixUtil.matmul(mat_a, mat_b)

      val result_c = ArrayBuffer.fill(golden_c.size)(ArrayBuffer.fill(golden_c.head.size)(0))

      def get_indices(dense_mat: Seq[Seq[Int]]) = dense_mat.zipWithIndex.map { case (row, rowId) =>
        row.zipWithIndex.collect { case (col, colId) if col != 0 => (rowId, colId) }
      }

      val compressed_a_indices = get_indices(mat_a.transpose).map(_.map(t => (t._2, t._1)))
      val compressed_b_indices = get_indices(mat_b)

      def get_i(i_compressed: Int, k: Int): Int = {
        try {
          compressed_a_indices(k)(i_compressed)._1
        } catch {
          case _: IndexOutOfBoundsException => Int.MaxValue
        }
      }

      def get_j(j_compressed: Int, k: Int): Int = {
        try {
          compressed_b_indices(k)(j_compressed)._2
        } catch {
          case _: IndexOutOfBoundsException => Int.MaxValue
        }
      }

      val macs = compressed_a_indices.flatten.map { case (_, col_a) =>
        compressed_b_indices.flatten.count(_._1 == col_a)
      }.sum

      assert(macs == dim*dim*dim, s"macs = $macs")

      assert(c.io.loadBalancingMappings.forall(_.configs.size == 1))
      c.io.loadBalancingMappings.foreach(_.configs.map { config =>
        config.valid.poke(true.B)
        config.bits.poke(true.B)
      })
      resetSpatialArray(c)
      startSpatialArray(c)

      while (c.io.busy.peekBoolean()) {
        // Right now, the systolic array still requests data using the coordinates of the expanded matrices. In the
        // future, we might want to update this to use the coordinates of the compressed matrices instead
        val inputMats = Seq((mat_a, design.A), (mat_b, design.B),
          (mat_a.zipWithIndex.map { case (row, i) => Seq.fill(row.size)(i) }, design.Ai),
          (mat_b.map(row => row.indices), design.Bj))

        // Poke in the inputs
        c.io.compressed2ExpandedMappings.foreach { ioMappings =>
          val i_compressed = ioMappings.compressed(0).peek().litValue.toInt
          val j_compressed = ioMappings.compressed(1).peek().litValue.toInt
          val k = ioMappings.compressed(2).peek().litValue.toInt

          val i_expanded = get_i(i_compressed, k)
          val j_expanded = get_j(j_compressed, k)

          ioMappings.expanded(0).poke(i_expanded.S)
          ioMappings.expanded(1).poke(j_expanded.S)
        }

        for ((mat, inputVar) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val data = try {
                mat(coords(0))(coords(1))
              } catch { case _: IndexOutOfBoundsException =>
                Int.MaxValue
              }
              inPort.data.poke(data.S)
            }
          }
        }

        // Peek out the outputs
        val outPorts = c.getOutPortsForVar(design.C)
        for (outPort <- outPorts) {
          if (outPort.valid.peekBoolean()) {
            val Seq(i, j, k) = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt
            result_c(i)(j) += data
          }
        }

        c.clock.step()
      }

      assert(golden_c == result_c)
    }
  }

  "A100 sparse matmul work" in {
    cancel

    val design = new A100Test
    test (design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val expandedDim = 4

      val mat_a = Seq(
        Seq(1, 0, 0, 20, 3, 4, 0, 0),
        Seq(-1, 0, -10, 0, 0, 9, 5, 0),
      )

      val mat_b = Seq(
        Seq(3, 4),
        Seq(5, 6),
        Seq(7, 8),
        Seq(9, 1),
        Seq(-9, -1),
        Seq(-7, -8),
        Seq(-5, -6),
        Seq(-3, -4),
      )

      val golden_c = MatrixUtil.matmul(mat_a, mat_b)
      val result_c = ArrayBuffer.fill(golden_c.size)(ArrayBuffer.fill(golden_c.head.size)(0))

      val compressed_a_indices = mat_a.map(_.grouped(expandedDim).map(_.zipWithIndex.collect { case (e, i) if e != 0 => i }).toSeq)

      val macs = {
        var m = 0

        for (i <- 0 until mat_a.size)
          for (j <- 0 until mat_b.head.size)
            for (k <- 0 until mat_b.size)
              if (mat_a(i)(k) == 0) m += 1

        m
      }

      assert(macs == dim*dim*dim*dim)

      def get_ki(i: Int, ko: Int, ki_compressed: Int): Int = {
        try {
          compressed_a_indices(i)(ko)(ki_compressed)
        } catch {
          case _: IndexOutOfBoundsException => -100
        }
      }

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        // Poke in the inputs
        val inputMats = Seq((mat_a, design.A), (mat_b, design.B))

        c.io.compressed2ExpandedMappings.foreach { ioMappings =>
          val i = ioMappings.compressed(0).peek().litValue.toInt
          val j = ioMappings.compressed(1).peek().litValue.toInt
          val ko = ioMappings.compressed(2).peek().litValue.toInt
          val ki_compressed = ioMappings.compressed(3).peek().litValue.toInt

          val ki_expanded = get_ki(i, ko, ki_compressed)

          ioMappings.expanded(0).poke(i.S)
          ioMappings.expanded(1).poke(j.S)
          ioMappings.expanded(2).poke(ko.S)
          ioMappings.expanded(3).poke(ki_expanded.S)
        }

        for ((mat, inputVar) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val data = mat(coords(0))(coords(1))
              inPort.data.poke(data.S)
            }
          }
        }

        c.getOptSkipBufferIns.foreach { case (ioVar, inPorts, comp2ExpMappings) =>
          val i = comp2ExpMappings.expanded(0).peek().litValue.toInt
          val j = comp2ExpMappings.expanded(1).peek().litValue.toInt
          val ko = comp2ExpMappings.expanded(2).peek().litValue.toInt

          if (ioVar == design.A) {
            inPorts.zipWithIndex.foreach { case (inPort, id) =>
              try {
                inPort.data.poke(mat_a(i)(ko * 4 + id).S)
              } catch {
                case _: IndexOutOfBoundsException =>
              }
              inPort.coords(0).poke(id.S)
              assert(inPort.coords.size == 1)
            }
          } else if (ioVar == design.B) {
            inPorts.zipWithIndex.foreach { case (inPort, id) =>
              try {
                inPort.data.poke(mat_b(ko * 4 + id)(j).S)
              } catch {
                case _: IndexOutOfBoundsException =>
              }
              inPort.coords(0).poke(id.S)
              assert(inPort.coords.size == 1)
            }
          } else {
            assert(false)
          }
        }

        // Peek out the outputs
        // TODO we don't check the ioOutputSkipBuffers in this test, which means that this test will only work for OS dataflows
        val outPorts = c.getOutPortsForVar(design.C)
        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt
            result_c(coords(0))(coords(1)) = data
          }
        }

        c.clock.step()
      }

      assert(golden_c == result_c)
    }
  }

  "Imbalanced sparse-dense matmul works" in {
    cancel
    val design = new SparseDenseMatmul(isLoadBalanced = false)
    test (design.toChiselModule(true)) { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val matA = List(List(32, 0), List(26, 39))

      val matB_data = List(List(), List(35, 3))
      val matB_coords = List(List(), List(34, 72))

      val expandedCols = matB_coords.flatten.maxOption.getOrElse(-1) + 1
      val matB_data_expanded = Seq.fill(dim)(ArrayBuffer.fill(expandedCols)(0))
      val matB_coords_expanded = Seq.fill(dim)(ArrayBuffer.fill(expandedCols)(-1000))
      matB_data.zip(matB_coords).zipWithIndex.foreach { case ((data, coords), k) =>
        for ((d,c) <- data.zip(coords)) {
          matB_data_expanded(k)(c) = d
          matB_coords_expanded(k)(c) = c
        }
      }

      val golden = {
        for (i <- 0 until dim) yield
          for (j <- 0 until expandedCols) yield
            (for (k <- 0 until dim) yield {
              val exists = matB_coords(k).contains(j)
              (matA(i)(k) * matB_data_expanded(k)(j), (i,j,k), exists)
            }).collect { case (d, c, true) => (d,c) }
      }.flatten.flatten.toSet.toSeq

      val result = ArrayBuffer.empty[(Int, (Int, Int, Int))]

      def get_j(k: Int, j_compressed: Int) = try {
        matB_coords(k)(j_compressed)
      } catch {
        case _: IndexOutOfBoundsException => -10000
      }

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        val inputMats = Seq((matA, design.A, 0), (matB_data_expanded, design.B, 0),
          (matB_coords_expanded, design.Bj, 1000000000)) // TODO this should be some automatic maximum value

        // Poke in the inputs
        c.io.compressed2ExpandedMappings.foreach { ioMappings =>
          val j_compressed = ioMappings.compressed(1).peek().litValue.toInt
          val k = ioMappings.compressed(2).peek().litValue.toInt

          val j_expanded = get_j(k, j_compressed)

          ioMappings.expanded(1).poke(j_expanded.S)
        }

        for ((mat, inputVar, fallback) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val data = try {
                mat(coords(0))(coords(1))
              } catch {
                case _: IndexOutOfBoundsException => fallback
              }
              inPort.data.poke(data.S)
            }
          }
        }

        // Peek out the outputs
        val outPorts = c.getOutPortsForVar(design.C)
        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt

            val i = coords(0)
            val j_expanded = coords(1)
            val k = coords(2)

            val packet = (data, (i, j_expanded, k))
            assert(!result.contains(packet), "overwriting existing result")
            result += packet
          }
        }

        c.clock.step()
      }

      assert(result == golden)
    }
  }

  "Dense matmul addition works" in {
    cancel
    val design = new MatrixAdder
    test (design.toChiselModule(true)) { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val m1 = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))
      val m2 = Seq.fill(dim)(Seq.fill(dim)(scala.util.Random.nextInt(100) - 50))

      val golden = MatrixUtil.matadd(m1, m2)
      val result = Seq.fill(dim)(ArrayBuffer.fill(dim)(0))

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        val inputMats = Seq((m1, design.C), (m2, design.D))

        // Poke in the inputs
        for ((mat, inputVar) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val data = mat(coords(0))(coords(1))
              inPort.data.poke(data.S)
            }
          }
        }

        // Peek out the outputs
        val outPorts = c.getOutPortsForVar(design.E)
        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt
            result(coords(0))(coords(1)) = data
          }
        }

        c.clock.step()
      }

      assert(golden.toSeq == result.toSeq)
    }
  }

  "Dense matmul with inputs gradually becoming available works" in {
    cancel
    val design = new DenseMatmul
    test (design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val m1 = Seq.fill(dim)(Seq.fill(dim)(randInt()))
      val m2 = Seq.fill(dim)(Seq.fill(dim)(randInt()))
      val golden = MatrixUtil.matmul(m1, m2)

      val result = Seq.fill(dim)(ArrayBuffer.fill(dim)(0))

      resetSpatialArray(c)
      startSpatialArray(c)

      val m1_available_at = Seq.fill(dim)(Seq.fill(dim)(randInt(max=50, bias=0)))
      val m2_available_at = Seq.fill(dim)(Seq.fill(dim)(randInt(max=50, bias=0)))

      var cycle = 0
      while (c.io.busy.peek().litToBoolean) {
        val inputMats = Seq((m1, m1_available_at, design.A), (m2, m2_available_at, design.B))

        // Poke in the inputs
        for ((mat, available_at, inputVar) <- inputMats) {
          val inPorts = c.getInPortsForVar(inputVar)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = inPort.coords.map(_.peek().litValue.toInt)
              val available = available_at(coords(0))(coords(1)) <= cycle
              val data = if (available) mat(coords(0))(coords(1)) else -100

              inPort.data.poke(data.S)
              inPort.found.poke(available.B)
            }
          }
        }

        // Peek out the outputs
        val outPorts = c.getOutPortsForVar(design.C)
        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val data = outPort.bits.element.data.peek().litValue.toInt
            result(coords(0))(coords(1)) = data
          }
        }

        c.clock.step()
        cycle += 1
      }

      assert(result == golden)
    }
  }

  "Dense SCNN multiplier array works" in {
    cancel

    val maxI = 4
    val maxF = 4
    val maxWt = 3
    val maxHt = 3

    val design = new SCNNMultArray(maxI=maxI, maxF=maxF, maxWt=maxWt, maxHt=maxHt)
    test (design.toChiselModule(true)).withAnnotations(Seq(WriteVcdAnnotation, VcsBackendAnnotation)) { c =>
      val image_width = 6
      val image_height = 6
      assert((image_width * image_height) / (maxWt * maxHt) <= maxI)

      val weight_out_channels = 3
      val weight_width = 3
      val weight_height = 3

      val tile_width = (image_width + (maxWt - 1)) / maxWt
      val tile_height = (image_height + (maxHt - 1)) / maxHt
      assert(tile_width * tile_height <= maxI)

      val image = Seq.fill(image_width, image_height)(randInt())

      val weights = for (k <- 0 until weight_out_channels) yield {
        for (r <- 0 until weight_width) yield {
          for (s <- 0 until weight_height) yield {
            (randInt(), Seq(k,r,s))
          }
        }
      }

      val allF = weights.flatten.flatten.size

      val Seq(golden, result) = Seq.fill(2)(ArrayBuffer.fill(maxWt, maxHt, weight_out_channels, tile_width + 2*weight_width - 2, tile_height + 2*weight_height - 2)(0))
      for (wt <- 0 until maxWt) {
        for (ht <- 0 until maxHt) {
          val in_slice = image.slice(wt * tile_width, (wt + 1) * tile_width).flatMap(_.slice(ht * tile_height, (ht + 1) * tile_height))
          val w_slice = weights.flatten.flatten

          for (i <- 0 until maxI) {
            val image_w_it: Int = i / tile_height
            val image_h_it: Int = i % tile_width

            for (f <- 0 until allF) {
              val (weight_val, Seq(weight_out_channel_it, weight_w_it, weight_h_it)) = w_slice(f)

              val psum = in_slice(i) * weight_val

              try {
                golden(wt)(ht)(weight_out_channel_it)(image_w_it - weight_w_it + 1 + weight_width-1)(image_h_it - weight_h_it + 1 + weight_height-1) += psum
              } catch {
                case _: IndexOutOfBoundsException =>
              }
            }
          }
        }
      }

      println(s"image = $image")
      println(s"weights = $weights")
      println(s"golden = $golden")

      resetSpatialArray(c)
      startSpatialArray(c)

      var ended = false
      var cycle = 0
      while (!ended) {
        println(s"cycle = $cycle")
        cycle += 1

        c.io.ins.foreach(_.found.poke(false.B))

        {
          // Write in image and weight inputs
          def getImageVal(coords: Seq[Int]): Int = {
            val Seq(wt, ht, i) = coords
            val in_slice = image.slice(wt * tile_width, (wt + 1) * tile_width).flatMap(_.slice(ht * tile_height, (ht + 1) * tile_height))
            in_slice(i)
          }
          def getImageW(coords: Seq[Int]): Int = {
            val Seq(wt, ht, i) = coords
            val image_w_it = i / tile_height
            image_w_it
          }
          def getImageH(coords: Seq[Int]): Int = {
            val Seq(wt, ht, i) = coords
            val image_h_it = i % tile_height
            image_h_it
          }
          def getFilterVal(coords: Seq[Int]): Int = {
            val Seq(f) = coords
            val w_slice = weights.flatten.flatten
            val (weight_val, Seq(weight_out_channel_it, weight_w_it, weight_h_it)) = w_slice(f)
            weight_val
          }
          def getFilterW(coords: Seq[Int]): Int = {
            val Seq(f) = coords
            val w_slice = weights.flatten.flatten
            val (weight_val, Seq(weight_out_channel_it, weight_w_it, weight_h_it)) = w_slice(f)
            weight_w_it
          }
          def getFilterH(coords: Seq[Int]): Int = {
            val Seq(f) = coords
            val w_slice = weights.flatten.flatten
            val (weight_val, Seq(weight_out_channel_it, weight_w_it, weight_h_it)) = w_slice(f)
            weight_h_it
          }
          def getFilterC(coords: Seq[Int]): Int = {
            val Seq(f) = coords
            val w_slice = weights.flatten.flatten
            val (weight_val, Seq(weight_out_channel_it, weight_w_it, weight_h_it)) = w_slice(f)
            weight_out_channel_it
          }
          val inputGetters = Seq((design.Image, getImageVal(_)), (design.ImageWCoord, getImageW(_)), (design.ImageHCoord, getImageH(_)),
            (design.Filter, getFilterVal(_)), (design.FilterWCoord, getFilterW(_)), (design.FilterHCoord, getFilterH(_)), (design.FilterOutChannelCoord, getFilterC(_)))

          for ((inputVar, getter) <- inputGetters) {
            val inPorts = c.getInPortsForVar(inputVar)
            for (inPort <- inPorts) {
              if (inPort.valid.peekBoolean()) {
                val coords = inPort.coords.map(_.peekInt().toInt)
                val data = try {
                  getter(coords)
                } catch { case _: IndexOutOfBoundsException =>
                  Int.MaxValue
                }
                inPort.data.poke(data.S)
                inPort.found.poke(true.B)
              }
            }
          }
        }

        {
          // Read out partial sums
          val outVars = Seq(design.PartialSums/*, design.PartialSumChannelCoord, design.PartialSumWCoord, design.PartialSumHCoord*/)
          for (outVar <- outVars) {
            val outPorts = c.getOutPortsForVar(outVar)
            for (outPort <- outPorts) {
              if (outPort.valid.peekBoolean()) {
                val Seq(wt, ht, out_channel, w, h) = outPort.bits.coords.map(_.peekInt().toInt)
                result(wt)(ht)(out_channel)(w + weight_width-1)(h + weight_height-1) += outPort.bits.element.data.peekInt().toInt
              }
            }
          }
        }

        if (c.io.ending.peekBoolean())
          ended = true

        c.clock.step()
      }

      println(s"result = $result")
      assert(result == golden)
    }
  }
}
