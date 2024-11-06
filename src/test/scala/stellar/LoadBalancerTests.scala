package stellar

import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Implicits._

import org.scalatest.freespec.AnyFreeSpec

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._

import stellar.rtl._
import stellar.Util.SMap

import TestUtil._

class LoadBalancerTests extends AnyFreeSpec with ChiselScalatestTester {
  case class RfOrClUpdate(varOrInd: Either[Input, Index], from: (Int, Seq[Int]), to: (Int, Seq[Int])) {
    // Note: for "from" and "to", the first element in the tuple in the op-count, and the second element is the coords
  }

  case class C2EMappingT(expanded: Int, compressed: Seq[Int], opCount: Int)

  def popUpdates(c: ChiselLoadBalancer, updates: ArrayBuffer[RfOrClUpdate]): Unit = {
    (c.io.rfUpdates.zip(c.inVars.map(Left(_))) ++ c.io.clUpdates.zip(c.indices.map(Right(_)))).foreach { case (updatePorts, inVarOrIndex) =>
      updatePorts.foreach { updatePort =>
        if (updatePort.valid.peek().litToBoolean) {
          val update = RfOrClUpdate(inVarOrIndex,
            from = (updatePort.bits.from.op_count.peek().litValue.toInt,
              updatePort.bits.from.coords.map(_.peek().litValue.toInt)),
            to = (updatePort.bits.to.op_count.peek().litValue.toInt,
              updatePort.bits.to.coords.map(_.peek().litValue.toInt))
          )
          val updateIndex = updates.indexOf(update)
          assert(updateIndex >= 0, s"update is unaccounted for: $update")
          updates.remove(updateIndex)
        }
      }
    }
  }

  def inputIoConns(c: ChiselLoadBalancer, ioConns: Iterable[(IOConn, Int)], poker: PartialFunction[(Expr, Int), Int] = PartialFunction.empty, updates: ArrayBuffer[RfOrClUpdate] = ArrayBuffer.empty): Unit = {
    c.io.rfIns.flatten.foreach(_.valid.poke(false.B))
    c.io.rf_last_ins.foreach(_.poke(false.B))

    val inVars = ioConns.map(_._1.ioVar).collect { case in: stellar.Input => in }
    for (inVar <- inVars) {
      val (rfIns, last_in, _) = c.getRfInsForInVar(inVar)

      val ioConns_ = ioConns.filter(_._1.ioVar == inVar).toSeq.sortBy(_._1.time.reverse).grouped(rfIns.size).toSeq

      for ((iocs, i) <- ioConns_.zipWithIndex) {
        rfIns.foreach(_.valid.poke(false.B))

        assert(iocs.size <= rfIns.size)
        iocs.zip(rfIns).foreach { case ((ioc, opCount), rfIn) =>
          rfIn.valid.poke(true.B)
          rfIn.ready.expect(true.B, s"Cannot input $ioc at op-count=$opCount because rf-snooping port is not ready")
          rfIn.bits.coords.zip(ioc.ioIndex).foreach {
            case (coord, expr) if poker.isDefinedAt(expr, opCount) => coord.poke(poker(expr, opCount).S)
            case (coord, Const(c)) => coord.poke(c.S)
            case _ => throw new Exception("UNREACHABLE")
          }
          rfIn.bits.op_count.bits.poke(opCount.U)
        }

        last_in.poke((i == ioConns_.size-1).B)

        popUpdates(c, updates)
        c.clock.step()
      }

      rfIns.foreach(_.valid.poke(false.B))
      last_in.poke(false.B)
    }

    c.io.rfIns.flatten.foreach(_.valid.poke(false.B))
    c.io.rf_last_ins.foreach(_.poke(false.B))
  }

  def inputComp2ExpMappings(c: ChiselLoadBalancer, c2eMappings: SMap[Index, Seq[C2EMappingT]], updates: ArrayBuffer[RfOrClUpdate] = ArrayBuffer.empty): Unit = {
    c.io.clIns.flatten.foreach(_.valid.poke(false.B))
    c.io.cl_last_ins.foreach(_.poke(false.B))

    c.io.clIns.zip(c.io.cl_last_ins).zip(c.indices).collect { case ((clIns, last_in), index) if c2eMappings.contains(index) =>
      c2eMappings(index).groupBy(_.opCount).mapValues(_.grouped(clIns.size).toSeq).toSeq.sortBy(_._1).foreach { case (opCount, c2eMaps) =>
        c2eMaps.zipWithIndex.foreach { case (c2es, i) =>
          clIns.zip(c2es).foreach { case (clIn, C2EMappingT(exp, comps, _)) =>
            clIn.valid.poke(true.B)
            clIn.compressed.zip(comps).foreach { case (c, comp) => c.poke(comp.S) }
            clIn.expanded(c.indices.indexOf(index)).poke(exp.S)
            clIn.op_count.bits.poke(opCount.U)
            clIn.ready.foreach(_.expect(true.B))
          }

          last_in.poke((i == c2eMaps.size-1).B)

          popUpdates(c, updates)
          c.clock.step()

          c.io.clIns.flatten.foreach(_.valid.poke(false.B))
          c.io.cl_last_ins.foreach(_.poke(false.B))
        }
      }
    }
  }

  def waitWhileUpdating(c: ChiselLoadBalancer, opCount: Int, updates: ArrayBuffer[RfOrClUpdate] = ArrayBuffer.empty): Unit = {
    // Note: "updates" have form Seq((in_regfile, updated)). Updates are popped from the ArrayBuffer as they occur
    c.io.mappings.foreach { mapping =>
      mapping.opCount.bits.poke(opCount.U)
      mapping.configs.map(_.ready.poke(true.B))
    }

    while (!c.io.mappings.flatMap(_.configs.map(_.valid.peek().litToBoolean)).reduce(_ && _)) {
      popUpdates(c, updates)
      c.clock.step()
    }

    c.io.mappings.foreach(_.configs.foreach(_.ready.poke(false.B)))
  }

  "Dense load-balancing works" in {
    class LoadBalancedDenseMatmul extends DenseMatmul(size=2, hasTransform=false) {
      Transform(
        1, 0, 0,
        0, 1, 0,
        1, 1, 1,
      )
      Map(i, j.upperBound->, k) to (i+1, (j.lowerBound+1) -> j.upperBound, k)
    }

    val spatialArray = new LoadBalancedDenseMatmul
    spatialArray.elaborate(shouldPrint=false, shouldRender=false, shouldUnroll=false, emitVerilog=false)

    val dim = spatialArray.block.upperBound

    val its = spatialArray.block.rangeSpaceItsOpt.get
    assert(its.pointsMappings.size == 1, "right now, this test assumes that there is only one potential load-balancing mapping")

    val transform = spatialArray.block.transform.get

    val axes = Seq(spatialArray.k, spatialArray.j, spatialArray.i)

    def rfInPortT = new SpatialArrayOutPort(nIOCoords = 2, nDomainCoords = 3)
    def rfUpdatePortT = new RegfileUpdatePort(nCoords = 2)

    test(new ChiselLoadBalancer(nAxes = 3, nOpCounts=2, nOutputPorts=1,
      rfInPorts=Seq(spatialArray.A, spatialArray.B).map(_ -> (rfInPortT,1)).toMap,
      rfUpdatePorts=Seq(spatialArray.A, spatialArray.B).map(_ -> (rfUpdatePortT,1)).toMap,
      nClInPortsPerIndex=1, nClUpdatePortsPerIndex=1, its=its, transform=transform)).withAnnotations(Seq(/*WriteVcdAnnotation, VcsBackendAnnotation*/)) { c =>
      resetModule(c)

      // Set up loop bounds and strides
      c.io.config.valid.poke(true.B)

      c.io.config.bits.axis_sizes.foreach(_.poke(1.U))
      c.io.config.bits.axis_sizes(axes.indexOf(spatialArray.j)).poke(3.U)

      c.io.config.bits.rf_in_strides.flatten.flatten.foreach(_.poke(0.U))
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.A))(0)(axes.indexOf(spatialArray.i)).poke(dim.U)
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.A))(1)(axes.indexOf(spatialArray.k)).poke(dim.U)
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.B))(0)(axes.indexOf(spatialArray.k)).poke(dim.U)
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.B))(1)(axes.indexOf(spatialArray.j)).poke(dim.U)

      c.clock.step()
      c.io.config.valid.poke(false.B)

      // No load-balancing
      inputIoConns(c, its.ioConns.filter(_.mapping.isEmpty).map((_, 0)))

      waitWhileUpdating(c, 0)
      c.io.mappings.flatMap(_.configs).foreach { m =>
        m.valid.expect(true.B)
        m.bits.expect(false.B)
      }

      c.io.last_out.poke(true.B)
      c.clock.step()
      c.io.last_out.poke(false.B)

      // With load-balancing
      val (ioConns, updates) = {
        val withRepeated = its.ioConns.filter(_.ioVar != spatialArray.C).filter { ioc =>
          val inverse = spatialArray.block.transform.get.inverse
          val spacetime = ioc.point.coords ++ ioc.time
          val originalPoint = MatrixUtil.matvec(inverse, spacetime).toSeq
          val canBeRemapped = its.pointsMappings.flatMap(_.tos).map(_.coords).contains(originalPoint)
          !canBeRemapped || ioc.mapping.nonEmpty
        }

        val withoutRepeated = withRepeated.filter { ioc =>
          ioc.mapping.isEmpty || !withRepeated.filter(_.mapping.isEmpty).map(_.ioIndex).contains(ioc.ioIndex)
        }

        val result = withoutRepeated.map { ioc =>
          val coords = ioc.ioIndex.map { case Const(c) => Const(c % dim) }

          val (opCount, update) = if (ioc.mapping.nonEmpty) {
            val update_ = RfOrClUpdate(Left(ioc.ioVar.asInstanceOf[Input]),
              from = (2, ioc.ioIndex.map { case Const(c) => c % dim }),
              to = (1, ioc.ioIndex.map { case Const(c) => c }),
            )

            (2, Some(update_))
          } else {
            (1, None)
          }

          ((ioc.copy(ioIndex = coords), opCount), update)
        }

        (result.map(_._1), result.flatMap(_._2))
      }

      val updateBuffer = ArrayBuffer.from(updates)

      inputIoConns(c, ioConns, updates=updateBuffer)
      waitWhileUpdating(c, 1, updateBuffer)

      c.io.mappings.flatMap(_.configs).foreach { m =>
        m.valid.expect(true.B)
        m.bits.expect(true.B)
      }
      assert(updateBuffer.isEmpty, s"Remaining updates: $updateBuffer")
    }
  }

  "Sparse outer-product matmul load-balancing works" in {
    val spatialArray = new OuterMatmul(isUnbounded = false) // TODO this test currently fails if isUnbounded = true; we should fix that
    spatialArray.elaborate(shouldPrint=false, shouldRender=false, shouldUnroll=false, emitVerilog=false)

    val dim = spatialArray.block.upperBound

    val its = spatialArray.block.rangeSpaceItsOpt.get
    assert(its.pointsMappings.size == 1, "right now, this test assumes that there is only one potential load-balancing mapping")

    val transform = spatialArray.block.transform.get

    val axes = Seq(spatialArray.k, spatialArray.j, spatialArray.i)

    def rfInPortT = new SpatialArrayOutPort(nIOCoords = 2, nDomainCoords = 3)
    def rfUpdatePortT = new RegfileUpdatePort(nCoords = 2)

    test(new ChiselLoadBalancer(nAxes = 3, nOpCounts=2, nOutputPorts=1,
      rfInPorts=Seq(spatialArray.A, spatialArray.B, spatialArray.Ai, spatialArray.Bj).map(_ -> (rfInPortT,1)).toMap,
      rfUpdatePorts=Seq(spatialArray.A, spatialArray.B, spatialArray.Ai, spatialArray.Bj).map(_ -> (rfUpdatePortT,1)).toMap,
      nClInPortsPerIndex=1, nClUpdatePortsPerIndex=1, its=its, transform=transform)).withAnnotations(Seq(VcsBackendAnnotation/*, WriteVcdAnnotation*/)) { c =>
      resetModule(c)

      def c2eMap(matATCoords: Seq[Seq[Int]], matBCoords: Seq[Seq[Int]]) = Seq(
        spatialArray.i -> matATCoords.zipWithIndex.flatMap { case (iCoords, k) =>
          iCoords.zipWithIndex.map { case (iExp, iComp) => C2EMappingT(iExp, Seq(iComp % dim, 0, k), iComp / dim) }
        },
        spatialArray.j -> matBCoords.zipWithIndex.flatMap { case (jCoords, k) =>
          jCoords.zipWithIndex.map { case (jExp, jComp) => C2EMappingT(jExp, Seq(0, jComp, k), 0) }
        }
      ).toMap

      // Set up loop bounds and strides
      c.io.config.valid.poke(true.B)

      c.io.config.bits.axis_sizes.foreach(_.poke(1.U))
      c.io.config.bits.axis_sizes(axes.indexOf(spatialArray.i)).poke(1.U)

      c.io.config.bits.rf_in_strides.flatten.flatten.foreach(_.poke(0.U))
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.A))(1)(axes.indexOf(spatialArray.k)).poke(dim.U)
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.Ai))(1)(axes.indexOf(spatialArray.k)).poke(dim.U)
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.B))(0)(axes.indexOf(spatialArray.k)).poke(dim.U)
      c.io.config.bits.rf_in_strides(c.inVars.indexOf(spatialArray.Bj))(0)(axes.indexOf(spatialArray.k)).poke(dim.U)

      c.io.config.bits.cl_in_strides.flatten.foreach(_.poke(0.U))
      c.io.config.bits.cl_in_strides(c.indices.indexOf(spatialArray.i))(axes.indexOf(spatialArray.i)).poke(dim.U)
      c.io.config.bits.cl_in_strides(c.indices.indexOf(spatialArray.j))(axes.indexOf(spatialArray.j)).poke(dim.U)
      c.io.config.bits.cl_in_strides(c.indices.indexOf(spatialArray.k))(axes.indexOf(spatialArray.k)).poke(dim.U)

      c.clock.step()
      c.io.config.valid.poke(false.B)

      {
        // No load-balancing
        c.getRfInsForInVar(spatialArray.A)._1.foreach(_.ready.expect(false.B))
        c.getRfInsForInVar(spatialArray.Ai)._1.foreach(_.ready.expect(false.B))

        val matATCoords = Seq(
          Seq(1, 2),
          Seq(10),
        )

        val matBCoords = Seq(
          Seq(0),
          Seq(4, 10),
        )

        inputComp2ExpMappings(c, c2eMap(matATCoords, matBCoords))

        val ioConns = its.ioConns.filter {
          case IOConn(_, Seq(IndexSkipFunc(spatialArray.i, Const(iComp), _, _), Const(k)), _, spatialArray.a | spatialArray.ai, _, None) =>
            matATCoords(k).size > iComp

          case IOConn(_, Seq(Const(k), IndexSkipFunc(spatialArray.j, Const(jComp), _, _)), _, spatialArray.b | spatialArray.bj, _, None) =>
            matBCoords(k).size > jComp

          case _ => false
        }

        inputIoConns(c, ioConns.map((_,0)), {
          case (IndexSkipFunc(spatialArray.i, Const(compressed_i), Seq(Const(k)), _), _) => matATCoords(k)(compressed_i)
          case (IndexSkipFunc(spatialArray.j, Const(compressed_j), Seq(Const(k)), _), _) => matBCoords(k)(compressed_j)
        })

        waitWhileUpdating(c, 0)
        c.io.mappings.flatMap(_.configs).foreach { m =>
          m.valid.expect(true.B)
          m.bits.expect(false.B)
        }
      }

      c.io.last_out.poke(true.B)
      c.clock.step()
      c.io.last_out.poke(false.B)

      c.io.config.valid.poke(true.B)
      c.io.config.bits.axis_sizes(axes.indexOf(spatialArray.i)).poke(2.U)
      c.clock.step()
      c.io.config.valid.poke(false.B)

      {
        // Make sure we still choose the default load-balancing if all options are available
        val matATCoords = Seq(
          Seq(1, 2, 3, 4),
          Seq(10, 11, 12, 13),
        )

        val matBCoords = Seq(
          Seq(0, 10, 20, 30),
          Seq(4, 10, 100, 1000),
        )

        inputComp2ExpMappings(c, c2eMap(matATCoords, matBCoords))

        val ioConns = its.ioConns.collect {
          case ioc @ IOConn(_, Seq(IndexSkipFunc(spatialArray.i, Const(iComp), _, false), Const(k)), _, spatialArray.a | spatialArray.ai, _, _) =>
            ioc.copy(ioIndex = Seq(IndexSkipFunc(spatialArray.i, Const(iComp % dim), Seq(Const(k % dim)), false), Const(k)))

          case ioc @ IOConn(_, Seq(Const(k), IndexSkipFunc(spatialArray.j, Const(jComp), _, false)), _, spatialArray.b | spatialArray.bj, _, _) =>
            ioc.copy(ioIndex = Seq(Const(k % dim), IndexSkipFunc(spatialArray.j, Const(jComp % dim), Seq(Const(k % dim)), false)))
        }.map { ioc =>
          val opCount = if (ioc.mapping.isEmpty) 1 else 2
          (ioc, opCount)
        }

        inputIoConns(c, ioConns, {
          case (IndexSkipFunc(spatialArray.i, Const(compressed_i), Seq(Const(k)), _), _) => matATCoords(k)(compressed_i)
          case (IndexSkipFunc(spatialArray.j, Const(compressed_j), Seq(Const(k)), _), _) => matBCoords(k)(compressed_j)
        })

        waitWhileUpdating(c, 1)
        c.io.mappings.flatMap(_.configs).foreach { m =>
          m.valid.expect(true.B)
          m.bits.expect(false.B)
        }
      }

      resetModule(c)

      {
        // With load-balancing
        val matATCoords = Seq(
          Seq(0, 1, 2, 4),
          Seq(),
        )

        val matBCoords = Seq(
          Seq(0, 2),
          Seq(1),
        )

        val c2eMappings = c2eMap(matATCoords, matBCoords)
        val clUpdates = c2eMappings.flatMap { case (index, mappings) =>
          mappings.collect { case C2EMappingT(_, compressed, opCount) if opCount > 0 =>
            assert(opCount == 1)

            val newCompressed = ArrayBuffer.from(compressed)
            newCompressed(c.indices.indexOf(index)) += opCount * dim

            RfOrClUpdate(Right(index),
              from = (opCount, compressed),
              to = (0, newCompressed.toSeq)
            )
          }
        }

        val updateBuffer = ArrayBuffer.from(clUpdates)

        inputComp2ExpMappings(c, c2eMappings, updates = updateBuffer)

        val ioConns = {
          val withRepeated = its.ioConns.filter { ioc =>
            val inverse = spatialArray.block.transform.get.inverse
            val spacetime = ioc.point.coords ++ ioc.time
            val originalPoint = MatrixUtil.matvec(inverse, spacetime).toSeq
            val canBeRemapped = its.pointsMappings.flatMap(_.tos).map(_.coords).contains(originalPoint)
            !canBeRemapped || ioc.mapping.nonEmpty
          }.filter {
            case IOConn(_, Seq(IndexSkipFunc(spatialArray.i, Const(iComp), _, _), Const(k)), _, spatialArray.a | spatialArray.ai, _, _) =>
              matATCoords(k).size > iComp

            case IOConn(_, Seq(Const(k), IndexSkipFunc(spatialArray.j, Const(jComp), _, _)), _, spatialArray.b | spatialArray.bj, _, _) =>
              matBCoords(k).size > jComp

            case _ => false
          }

          withRepeated.filter { ioc =>
            ioc.mapping.isEmpty || !withRepeated.filter(_.mapping.isEmpty).map(_.ioIndex).contains(ioc.ioIndex)
          }.map { ioc =>
            val opCount = if (ioc.mapping.nonEmpty) 1 else 0
            val coords = ioc.ioIndex.map {
              case Const(c) => Const(c % dim)
              case IndexSkipFunc(index, Const(c1), Seq(Const(c2)), isSync) => IndexSkipFunc(index, Const(c1%dim), Seq(Const(c2%dim)), isSync)
            }
            (ioc.copy(ioIndex = coords), opCount, ioc)
          }
        }

        val expander: PartialFunction[(Expr, Int), Int] = {
          case (IndexSkipFunc(spatialArray.i, Const(compressed_i), Seq(Const(k)), _), opCount) => matATCoords(k)(opCount * dim + compressed_i)
          case (IndexSkipFunc(spatialArray.j, Const(compressed_j), Seq(Const(k)), _), _) => matBCoords(k)(compressed_j)
        }

        val rfUpdates = ioConns.collect { case (ioc, 1, origIoc) =>
          RfOrClUpdate(Left(ioc.ioVar.asInstanceOf[Input]),
            from = (1, ioc.ioIndex.map {
              case Const(c) => c
              case expr if expander.isDefinedAt(expr, 1) => expander(expr, 1)
            }),
            to = (0, origIoc.ioIndex.map {
              case Const(c) => c
              case expr if expander.isDefinedAt(expr, 0) => expander(expr, 0)
            })
          )
        }

        updateBuffer ++= rfUpdates

        inputIoConns(c, ioConns.map(t => (t._1, t._2)), expander, updates = updateBuffer)
        waitWhileUpdating(c, 0, updateBuffer)

        c.io.mappings.flatMap(_.configs).foreach { m =>
          m.valid.expect(true.B)
          m.bits.expect(true.B)
        }
        assert(updateBuffer.isEmpty)
      }
    }
  }
}
