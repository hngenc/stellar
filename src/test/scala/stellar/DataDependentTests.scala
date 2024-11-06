package stellar

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.collection.mutable.ArrayBuffer

import TestUtil._

class DataDependentTests extends AnyFreeSpec with ChiselScalatestTester {
  "Key merging works" in {
    val design = new KeyMatcher
    test(design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val keys = Seq(
        ((1 to dim).map(_ * 10), (1 to (dim * 2) by 2).map(_ * 10)),
        ((1 to dim), (dim + 1 to 2 * dim)),
        ((1 to dim), (1 to dim))
      )

      for ((keys1, keys2) <- keys) {
        resetSpatialArray(c)
        startSpatialArray(c)

        val golden = keys1.intersect(keys2)

        val resultM = ArrayBuffer.fill(dim)(0)
        val resultC = ArrayBuffer.fill(dim)(0)

        for (timeStep <- 0 until timeSteps) {
          // Poke in the inputs
          val inputKeys = Seq((keys1, design.A), (keys2, design.B))

          for ((keys, inputVar) <- inputKeys) {
            val inPorts = c.getInPortsForVar(inputVar)
            for (inPort <- inPorts) {
              if (inPort.valid.peek().litToBoolean) {
                val coords = inPort.coords.map(_.peek().litValue.toInt)
                val data = keys(coords(0))
                inPort.data.poke(data.S)
              }
            }
          }

          // Poke the outputs
          val outputResults = Seq((resultM, design.M), (resultC, design.C))

          for ((resultArray, outputVar) <- outputResults) {
            val outPorts = c.getOutPortsForVar(outputVar)
            for (outPort <- outPorts) {
              if (outPort.valid.peek().litToBoolean) {
                val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
                val data = outPort.bits.element.data.peek().litValue.toInt
                resultArray(coords(0)) = data
              }
            }
          }

          c.clock.step()
        }

        val result = resultC.zip(resultM).collect { case (c, 1) => c }

        assert(result == golden)
      }
    }
  }

  "Sorted CSR matrix merging works" in {
    val nRows = 2
    val design = new SortedCsrMatricesMerger(rows = nRows, smallCompressedCols = 2, largeCompressedCols = 4)
    test(design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val matmulResultData = Seq(
        Seq(5, 2),
        Seq(10, 2),
      )

      val existingMatrixData = Seq(
        Seq(1, 20, 3, 9),
        Seq(3, 1, 12, 2),
      )

      val matmulResultColCoords = Seq(
        Seq(0, 20),
        Seq(5, 35),
      )

      val existingMatrixColCoords = Seq(
        Seq(0, 20, 21, 25),
        Seq(6, 35, 40, 100),
      )

      val goldenData = Seq(
        Seq(6, 22, 3, 9),
        Seq(10, 3, 3, 12, 2),
      )

      val goldenCoords = Seq(
        Seq(0, 20, 21, 25),
        Seq(5, 6, 35, 40, 100),
      )

      val resultData = Seq(
        ArrayBuffer.fill(4)(-1000),
        ArrayBuffer.fill(5)(-1000),
      )

      val resultCoords = Seq(
        ArrayBuffer.fill(4)(-1000),
        ArrayBuffer.fill(5)(-1000),
      )

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        val inputs = Seq((matmulResultData, design.MatmulResult), (matmulResultColCoords, design.MatmulResultCoords),
          (existingMatrixData, design.ExistingMatrix), (existingMatrixColCoords, design.ExistingMatrixCoords))

        for (_ <- 0 until 2) {
          // The reason why we perform this loops twice is that some of the IO inputs depend combinationally on some
          // other IO inputs. By running this loop twice, we make sure that we've set each IO input to its final
          // required value

          for ((values, variable) <- inputs) {
            val inPorts = c.getInPortsForVar(variable)
            for (inPort <- inPorts) {
              if (inPort.valid.peek().litToBoolean) {
                val row = inPort.coords(0).peek().litValue.toInt
                val col = inPort.coords(1).peek().litValue.toInt

                val value = try {
                  values(row)(col)
                } catch {
                  case _: IndexOutOfBoundsException => -1
                }

                inPort.data.poke(value.S)
              }
            }
          }
        }

        val outPorts = c.getOutPortsForVar(design.OutputMatrix)

        for (outPort <- outPorts) {
          val output_is_valid = outPort.valid.peek().litToBoolean

          if (output_is_valid) {
            val data = outPort.bits.element.data.peek().litValue.toInt
            val row = outPort.bits.coords(0).peek().litValue.toInt
            val expanded_col = outPort.bits.coords(1).peek().litValue.toInt
            val compressed_col = goldenCoords(row).indexOf(expanded_col)

            assert(compressed_col >= 0, s"Output coordinates are not correct: ($row, $expanded_col)")

            resultData(row)(compressed_col) = data
            resultCoords(row)(compressed_col) = expanded_col
          }
        }

        c.clock.step()
      }

      assert(resultData == goldenData)
      assert(resultCoords == goldenCoords)
    }
  }

  "Scattered matrix merging into CSR format works" in {
    for (hasSubArrays <- Seq(false, true)) {
      for (oneDimensional <- Seq(false, true)) {
        for (expandI <- Seq(false, true)) {
          val design = new CsrMatrixSorter(hasSubArrays = hasSubArrays, oneDimensional = oneDimensional, expandI = expandI)
          test(design.toChiselModule(true)).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
            val dim = design.block.upperBound
            val timeSteps = design.block.timeStepsOpt.get.map(_ + 1).product

            val coords = Seq(
              Seq(1, 10),
              Seq(2, 10),
            )

            val data = Seq(Seq(
              Seq(-1, -2),
              Seq(-3, -4),
            ), Seq(
              Seq(-10, -20),
              Seq(-30, -40),
            ))

            val golden_coords = Seq(
              Seq(1, 2, 10),
              Seq(1, 2, 10),
            )
            val golden_data = Seq(
              Seq(-1, -3, -6),
              Seq(-10, -30, -60),
            )

            val result_coords = Seq(
              ArrayBuffer(-200, -200, -200),
              ArrayBuffer(-200, -200, -200),
            )
            val result_data = Seq(
              ArrayBuffer(-1000, -1000, -1000),
              ArrayBuffer(-1000, -1000, -1000),
            )

            resetSpatialArray(c)
            startSpatialArray(c)

            while (c.io.busy.peekBoolean()) {
              val inputs = Seq(
                {
                  def access(_coords: Seq[Int]) = {
                    assert(_coords.size == 3)
                    data(_coords(0))(_coords(2))(_coords(1))
                  }
                  (access(_), design.ScatteredMatrix)
                },
                {
                  def access(_coords: Seq[Int]) = {
                    assert(_coords.size == 3)
                    if (!hasSubArrays && !oneDimensional && !expandI)
                      assert(_coords(0) == 0)
                    else if (_coords(0) >= dim)
                      throw new IndexOutOfBoundsException
                    coords(_coords(2))(_coords(1))
                  }
                  (access(_), design.ScatteredMatrixJ)
                }
              ) ++ design.ExpandedIOpt.map { ExpandedI =>
                def access(_coords: Seq[Int]) = {
                  assert(_coords.size == 3 && _coords.tail.forall(_ == 0))
                  if (_coords(0) >= dim)
                    throw new IndexOutOfBoundsException
                  _coords(0) // TODO for now, we just assume that the expandedI is identical to the compressedI. We should probably add a test for when this isn't the case
                }
                (access(_), ExpandedI)
              }

              for (_ <- 0 until 2) {
                // The reason why we perform this loops twice is that some of the IO inputs depend combinationally on some
                // other IO inputs. By running this loop twice, we make sure that we've set each IO input to its final
                // required value

                for ((values, variable) <- inputs) {
                  val inPorts = c.getInPortsForVar(variable)
                  for (inPort <- inPorts) {
                    if (inPort.valid.peek().litToBoolean) {
                      val coords = inPort.coords.map(_.peek().litValue.toInt)

                      try {
                        inPort.data.poke(values(coords))
                        inPort.unavailable.poke(false)
                      } catch {
                        case _: IndexOutOfBoundsException =>
                          inPort.data.poke(design.maximum_value + 1)
                          inPort.unavailable.poke(true)
                      }
                    }
                  }
                }
              }

              val outPorts = c.getOutPortsForVar(design.MergedMatrix)

              for (outPort <- outPorts) {
                if (outPort.valid.peekBoolean()) {
                  val data = outPort.bits.element.data.peek().litValue.toInt
                  val row = outPort.bits.coords(0).peek().litValue.toInt
                  val expanded_col = outPort.bits.coords(1).peek().litValue.toInt
                  val compressed_col = golden_coords(row).indexOf(expanded_col)

                  assert(compressed_col >= 0, s"$row, $expanded_col")

                  result_data(row)(compressed_col) = data
                  result_coords(row)(compressed_col) = expanded_col
                }
              }

              c.clock.step()
            }

            assert(golden_data == result_data)
            assert(golden_coords == result_coords)
          }
        }
      }
    }
  }

  "CSR matrix adding works" in {
    val design = new CsrMatrixAdder()
    test(design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val a_data = Seq.fill(dim)(Seq.fill(dim)(randInt()))
      val b_data = Seq.fill(dim)(Seq.fill(dim)(randInt()))

      def gen_coords() = {
        for (_ <- 0 until dim) yield {
          var base = 0
          for (_ <- 0 until dim) yield {
            base += randInt(max=10, bias=1)
            base
          }
        }
      }

      val a_coords = gen_coords()
      val b_coords = gen_coords()

      val golden = {
        for (i <- 0 until dim) yield {
          var aj = 0
          var bj = 0

          (for (_ <- 0 until dim * dim) yield {
            val ac = if (aj < dim) a_coords(i)(aj) else 2000000000
            val bc = if (bj < dim) b_coords(i)(bj) else 2000000000

            val ad = if (aj < dim) a_data(i)(aj) else -500
            val bd = if (bj < dim) b_data(i)(bj) else -600

            val cc = {
              if (aj < dim && bj < dim) ac.min(bc)
              else if (aj < dim) ac
              else bc
            }

            val cd = {
              if (aj < dim && bj < dim) {
                if (ac == bc) ad + bd
                else if (ac < bc) ad
                else bd
              } else if (aj < dim)
                ad
              else
                bd
            }

            val valid = aj < dim || bj < dim

            val aj_ = aj
            if (bj >= dim || ac == cc)
              aj += 1
            if (aj_ >= dim || bc == cc)
              bj += 1

            if (valid) Some(cd, cc)
            else None
          }).flatten
        }
      }

      val golden_data = golden.map(_.map(_._1))
      val golden_coords = golden.map(_.map(_._2))

      val result_data = golden_data.map(g => ArrayBuffer.fill(g.size)(-1))
      val result_coords = golden_coords.map(g => ArrayBuffer.fill(g.size)(-1))

      resetSpatialArray(c)
      startSpatialArray(c)

      for (timeStep <- 0 until timeSteps) {
        val inputs = Seq((a_data, design.Adata), (b_data, design.Bdata), (a_coords, design.Acoords),
          (b_coords, design.Bcoords))

        for (_ <- 0 until 2) {
          // The reason why we perform this loops twice is that some of the IO inputs depend combinationally on some
          // other IO inputs. By running this loop several times, we make sure that we've set each IO input to its final
          // required value

          for ((values, variable) <- inputs) {
            val inPorts = c.getInPortsForVar(variable)
            for (inPort <- inPorts) {
              if (inPort.valid.peek().litToBoolean) {
                val coords = inPort.coords.map(_.peek().litValue.toInt)

                val value = try {
                  values(coords(0))(coords(1))
                } catch {
                  case _: IndexOutOfBoundsException => 2000000000
                }

                inPort.data.poke(value.S)
              }
            }
          }
        }

        val outPorts = c.getOutPortsForVar(design.C)

        for (outPort <- outPorts) {
          val output_is_valid = outPort.valid.peek().litToBoolean

          if (output_is_valid) {
            val data = outPort.bits.element.data.peek().litValue.toInt
            val row = outPort.bits.coords(0).peek().litValue.toInt
            val expanded_col = outPort.bits.coords(1).peek().litValue.toInt
            val compressed_col = golden_coords(row).indexOf(expanded_col)

            assert(compressed_col >= 0, s"$row, $expanded_col")

            result_data(row)(compressed_col) = data
            result_coords(row)(compressed_col) = expanded_col
          }
        }

        c.clock.step()
      }

      assert(result_coords == golden_coords)
      assert(result_data == golden_data)
    }
  }

  "Dense matrix filtering works" in {
    // In this test, A is dense and B is CSC

    val size_I = 2
    val size_J = 2
    val size_K = 16

    val design = new Filter(max_I = size_I, max_J = size_J, max_K = 4.min(size_K))
    test(design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val a_data = Seq.fill(size_I)(Seq.fill(size_K)(randInt()))
      val b_coords = Seq.fill(size_J)((0 until size_K).filter(_ => randBool(0.1)))

      val golden_coords = b_coords.flatten.distinct.sorted
      val golden_out = a_data.zipWithIndex.flatMap { case (row, i) => golden_coords.map(k => (Seq(i,k), row(k))) }

      val result_out = ArrayBuffer.empty[(Seq[Int], Int)]

      resetSpatialArray(c)
      startSpatialArray(c)

      while (c.io.busy.peek().litToBoolean) {
        val inputs = Seq((a_data, design.A, false), (b_coords, design.Bk, true))

        for ((values, variable, reverse) <- inputs) {
          val inPorts = c.getInPortsForVar(variable)
          for (inPort <- inPorts) {
            if (inPort.valid.peek().litToBoolean) {
              val coords = {
                val result = inPort.coords.map(_.peek().litValue.toInt)
                if (reverse) result.reverse else result
              }

              val value = try {
                values(coords(0))(coords(1))
              } catch {
                case _: IndexOutOfBoundsException => 2000000000
              }

              inPort.data.poke(value.S)
            }
          }
        }

        val outPorts = c.getOutPortsForVar(design.Out)

        for (outPort <- outPorts) {
          if (outPort.valid.peek().litToBoolean) {
            val data = outPort.bits.element.data.peek().litValue.toInt
            val coords = outPort.bits.coords.map(_.peek().litValue.toInt)
            val tup = (coords, data)
            assert(!result_out.contains(tup))
            result_out += tup
          }
        }

        c.clock.step()
      }

      assert(result_out.toSet == golden_out.toSet)
    }
  }

  "I-sorter works" in {
    val design = new OuterISorter

    test(design.toChiselModule(true))/*.withAnnotations(Seq(WriteVcdAnnotation))*/ { c =>
      val dim = design.block.upperBound
      val timeSteps = design.block.timeStepsOpt.get.product + 1

      val unsorted = Seq.fill(dim)(Seq.fill(dim)(randInt(bias=1)).scanLeft(-1)(_ + _).tail)
      val golden = unsorted.flatten.distinct.sorted

      val result = ArrayBuffer.fill(golden.size)(-1)

      val popped = scala.collection.mutable.Set.empty[Seq[Int]]

      resetSpatialArray(c)
      startSpatialArray(c)

      while (c.io.busy.peek().litToBoolean) {
        for (port <- c.getInPortsForVar(design.ExpandedI)) {
          if (port.valid.peekBoolean()) {
            val coords = port.coords.map(_.peek().litValue.toInt)
            assert(coords.size == 3 && coords(1) == 0)

            try {
              if (popped.contains(coords)) throw new IndexOutOfBoundsException // This is a somewhat hacky way of jumping to the "catch" code block
              port.data.poke(unsorted(coords(2))(coords(0)))
              port.unavailable.poke(false)
            } catch { case _: IndexOutOfBoundsException =>
              port.data.poke(design.maxVal+1)
              port.unavailable.poke(true)
            }

            if (port.pop.valid.peekBoolean())
              popped += coords
          }
        }

        for (port <- c.getOutPortsForVar(design.SortedI)) {
          if (port.valid.peekBoolean()) {
            val compressed_coord = port.bits.element.domainCoords(0).peekInt().toInt
            val expanded_coord = port.bits.coords(0).peekInt().toInt
            result(compressed_coord) = expanded_coord
          }
        }

        c.clock.step()
      }

      assert(result == golden)
    }
  }
}
