package stellar.rtl

import scala.math.Ordering.Implicits._
import chisel3._
import chisel3.util._
import stellar._
import stellar.Util.{SMap, without}
import stellar.RfEntryExitOption.Edge
import ChiselUtil._
import CombPathBreaker._

class ChiselAccelerator(spatialArrayGens: Iterable[SpatialArray],
                        regFiles: Iterable[RegFile], coordLookups: Iterable[CoordLookup],
                        loadBalancers: Iterable[LoadBalancer], loadBalancerCodes: SMap[LoadBalancer, Int],
                        srams: Seq[SRAM], sramCodes: SMap[SRAM, Int],

                        var2RegFileConnections: Seq[(stellar.Variable, RegFile, Mem2MemConnType, Option[(Int, Option[Int])], Option[Int], Option[Int])],
                        sram2RegFileConnections: Iterable[(SRAM, RegFile, Seq[Int])],
                        regFile2SRAMConnections: Iterable[RegFile2SRAMConn],

                        coordLookupsFromSramsOrVars: SMap[(Either[(SRAM, Int), Variable], CoordLookup), CoordLookupFromSramOrIo],
                        coordLookupsFromSpatialArrayIndices: SMap[(CoordLookup, SpatialArray), CoordLookupFromSpatialArrayIndex],

                        sram2CoordLookupFills: Iterable[(SRAM, CoordLookup, Seq[Int])],
                        var2CoordLookupFills: Iterable[Var2CoordLookupFill],

                        coordLookupEmpties: SMap[CoordLookup, Variable],

                        dataWidthBits: Int, withAsserts: Boolean = true) extends Module
{
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  val debug_cycle = RegInit(0.U(32.W))
  debug_cycle := debug_cycle + 1.U

  // Generate the inner modules
  val spatialArray2Module = spatialArrayGens.map { gen =>
    println(s"DEBUG generating spatial array: ${gen.name}")
    val mod = CombPathBreaker(gen.toChiselModule(gen.dataWidthBitsOpt getOrElse dataWidthBits, withAsserts), name = gen.name.getOrElse("spatial_array") + "_Wrapper")
      {_.io}({ x => x.outs.map { out =>
        CombPathBreaker.DelayPort(out, true, stages = gen.outputDelay, onReset = Some({ y: Data =>
            y.asInstanceOf[SpatialArrayOutPort].last_in_axis.foreach(_ := false.B)
          }))
        }.toSeq
    }).suggestName(gen.name.getOrElse("spatial_array"))

    mod.io.compressed2ExpandedMappings.zip(mod.inner.io.compressed2ExpandedMappings).foreach { case (outer, inner) =>
      outer.relevantIndices = inner.relevantIndices
    }

    mod.io.ins.foreach { in =>
      in.axis_spans.foreach(_.valid := false.B)
      in.axis_spans.foreach(_.bits := DontCare)
    }

    println(s"\tDEBUG finished: ${gen.name}")

    (gen, mod)
  }.toMap
  val spatialArrays = spatialArray2Module.toSeq.map(_._2)
  println(s"DEBUG finished generating spatial arrays")

  val sramsSorted = srams.toSeq.sortBy(s => sramCodes(s))
  val sram2Module = srams.map { sram =>
    val name = sram.name.getOrElse(s"SRAM_${sramCodes(sram)}")
    println(s"DEBUG generating SRAM: $name")
    val mod = CombPathBreaker(sram.toChiselModule, name = s"${name}Wrapper"){_.io}({ x =>
      if (sram.maxElemsInRf.isEmpty) Seq((x.read_resps, true)) else Seq.empty
    }, postProcess = sram.postProcess.toSeq).suggestName(name)
    mod.io.n_elems_in_axis_lookups.foreach(_.nElems := DontCare)
    println(s"\tDEBUG done generating $name")

    (sram, mod)
  }.toMap

  val regFile2Module = regFiles.map { rf =>
    val name = rf.nameOpt.getOrElse {
      val inVarName = var2RegFileConnections.toSeq.collectFirst { case (v: Input, `rf`, _, _, _, _) => v.name }.getOrElse("X")
      val outVarName = var2RegFileConnections.toSeq.collectFirst { case (v: Output, `rf`, _, _, _, _) => v.name }.getOrElse("X")

      s"RegFile_${inVarName}_$outVarName"
    }

    val nInPorts = {
      val fromSpArrays: Int = var2RegFileConnections.toSeq.collect {
        case (outVar: stellar.Output, `rf`, _, None, _, _) => spatialArrays.map(_.inner.getOutPortsForVar(outVar).size).sum
        case (outVar: stellar.Output, `rf`, _, Some((maxOutPorts, _)), _, _) => spatialArrays.map(_.inner.getOutPortsForVar(outVar).size.min(maxOutPorts)).sum
      }.sum

      val fromSrams = sram2RegFileConnections.toSeq.collect {
        case (sram, `rf`, _) => sram2Module(sram).io.read_resps.map(_.bits.data.size).sum
      }.maxOption.getOrElse(0)

      fromSpArrays + fromSrams
    }

    val nOutPorts = {
      val toSpArrays = var2RegFileConnections.toSeq.collect {
        case (inVar: stellar.Input, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrays.map(_.inner.getInPortsForVar(inVar).size).sum
      }.sum

      val toSrams = regFile2SRAMConnections.toSeq.collect {
        case RegFile2SRAMConn(`rf`, sram, sramAxisId, _, _, _) => sram2Module(sram).io.write_from_regfile_resps(sramAxisId).flatMap(_.data).size
      }.maxOption.getOrElse(0)

      toSpArrays + toSrams
    }

    val nElemsLookupPorts = sram2RegFileConnections.collect { case (sram, `rf`, _) if sram.maxElemsInRf.nonEmpty => sram2Module(sram).io.n_elems_in_axis_lookups.size }.sum +
      var2RegFileConnections.collect { case (inVar: stellar.Input, `rf`, NElemLookupConn, _, _, _) => spatialArrays.map(_.inner.getInPortsForVar(inVar).size).sum }.sum

    val constantCoordsForOutputs = {
      // TODO expand this so we can take advantage of hardcoded coords in other cases as well
      val toSpArrays = var2RegFileConnections.collect {
        case (inVar: stellar.Input, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(inVar))
      }.toSet.size
      val toSrams = regFile2SRAMConnections.toSeq.count {
        case RegFile2SRAMConn(`rf`, _, _, _, _, _) => true
        case _ => false
      }
      val expandsIndices = var2RegFileConnections.exists {
        case (inVar: stellar.Input, `rf`, _, _, _, _) => coordLookupsFromSramsOrVars.keys.exists(k => (k._1: Either[_, Variable]) == Right(inVar))
        case _ => false
      }
      val onlyReadFromOneSpArrayWithoutExpandingIndices = toSpArrays == 1 && toSrams == 0 && !expandsIndices
      val onlyReadFromOneSram = toSpArrays == 0 && toSrams == 1

      if (onlyReadFromOneSpArrayWithoutExpandingIndices) {
        // Note: we need to be very careful here that the order of sp-array-ports here matches the order they are connected in later in this file
        val dstSpArrayPorts = var2RegFileConnections.toSeq.collect {
          case (inVar: stellar.Input, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrays.flatMap(_.inner.getInPortsForVar(inVar))
        }.flatten
        dstSpArrayPorts.map(_.hardCodedCoords.toSeq)
      } else if (onlyReadFromOneSram) {
        // We check here for independent scattering in the innermost bank
        val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => (sram2Module(dst), reorderSramCoords) }.toSeq
        if (dst_srams.size == 1) {
          val (sram, reorderCoords) = dst_srams.head
          if (reorderCoords.sorted == reorderCoords) {
            val alwaysScatters = sram.inner.write_stages.head.forall(_.inner.alwaysScatters)
            val readsFromIndependentBanks = sram.inner.independentBanks
            if (alwaysScatters && readsFromIndependentBanks) {
              val result = Seq.tabulate(sram.inner.nBanks)(bankId => Seq.fill(rf.nIOCoords)(None: Option[Int]).updated(rf.nIOCoords-2, Some(bankId)))
              if (result.size == nOutPorts)
                result
              else
                Seq.empty
            } else
            Seq.empty
          } else
            Seq.empty
        } else
          Seq.empty
      } else {
        Seq.empty
      }
    }

    val constantCoordsForNElemsLookups = {
      val onlyLookedUpByOneSpArrayWithoutExpandingIndices = {
        val toSpArrays = var2RegFileConnections.collect {
          case (inVar: stellar.Input, `rf`, NElemLookupConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(inVar))
        }.toSet.size
        val toSrams = sram2RegFileConnections.toSeq.count {
          case (sram, `rf`, _) => sram.maxElemsInRf.nonEmpty
          case _ => false
        }
        toSpArrays == 1 && toSrams == 0
      }

      val for_cheap_sparch_row_id_rf = rf.nameOpt.exists(_.startsWith("sparch_row_id_rf_"))

      if (onlyLookedUpByOneSpArrayWithoutExpandingIndices) {
        // Note: we need to be very careful here that the order of sp-array-ports here matches the order they are connected in later in this file
        val nElemsLookupPorts = var2RegFileConnections.toSeq.collect {
          case (inVar: stellar.Input, `rf`, NElemLookupConn, _, _, _) => spatialArrays.flatMap(_.inner.getInPortsForVar(inVar))
        }.flatten
        nElemsLookupPorts.map(_.hardCodedCoords.toSeq)
      } else if (for_cheap_sparch_row_id_rf) {
        // This if-statement shouldn't change the actual generated hardware
        // (after synthesis tools have const-propped everything they can), but
        // it helps us avoid generating a bunch of intermediate lines of
        // FIRRTL which make Verilog-elaboration slower and more likely to break
        // the 2GB-of-FIRRTL limit.
        val result = Seq.tabulate(rf.lastInAxisFlags.map(_._1).product)(i => Seq(Some(i), None))
        assert(result.size == nElemsLookupPorts && rf.nIOCoords == result.head.size,
          s"rf = ${rf.nameOpt} | result.size = ${result.size} | nPipelines = ${rf.nPipelinesOpt} | nInPorts = $nInPorts | lastInAxisFlags = ${rf.lastInAxisFlags.size} | nIOCoords = ${rf.nIOCoords} | nElemsLookupPorts = $nElemsLookupPorts\n\tresult = $result")
        result
      } else
        Seq.empty
    }

    val coordsThatNeedExpanding = (0 until rf.nIOCoords) /* TODO This code doesn't really work anymore with CoordOf .filter { coordId =>
      val neededForSpArray = var2RegFileConnections.exists {
        case (_, `rf`, CoordConn(`coordId`), _) => true
        case _ => false
      }

      val neededForSram = regFile2SRAMConnections.exists(x => (x.src: RegFile) == rf) // TODO By looking at hardcoded SRAM params, we should be able to determine whether or not an SRAM actually needs the expanded coords from the regfile. For now, since we don't yet check that, we just default to assuming that any of the expanded coords may be necessary if this regfile writes to an SRAM

      neededForSpArray || neededForSram
    } */ .toSet

    val canBeEdgeToEdge = if (rf.automaticallyOptimize) {
      /* Regfiles can be edge-to-edge if entries are consumed by the accelerator in the same order that they're produced
         in */
      val srcSpArrays = var2RegFileConnections.collect {
        case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
      }.flatten.toSet

      val dstSpArrays = var2RegFileConnections.collect {
        case (inVar: stellar.Input, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(inVar))
      }.flatten.toSet

      val variables = var2RegFileConnections.collect {
        case (variable, `rf`, DataConn /* | _: CoordConn */, _, _, _) => variable
      }.toSet

      val reordered_src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords != reorderCoords.sorted => (src, reorderCoords) }.toSeq
      val unordered_src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords == reorderCoords.sorted => src }.toSeq

      val reordered_dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) if reorderSramCoords != reorderSramCoords.sorted => dst }

      if ((dstSpArrays.size == 1 || srcSpArrays.size == 1) && (reordered_src_srams.isEmpty || unordered_src_srams.isEmpty && reordered_src_srams.map(_._2).toSet.size == 1)) {
        val spArray = spatialArray2Module((if (dstSpArrays.nonEmpty) dstSpArrays else srcSpArrays).head)
        val variable = variables.find(spArray.inner.ioVars.contains(_)).get

        val its = spArray.inner.its
        val ioConns = its.ioConns.filter(_.ioVar == variable)

        val allIndicesKnownAtElaborationTime = ioConns.forall(_.ioIndex.forall(_.isInstanceOf[Const]))
        val multiDimensionalTime = ioConns.exists(_.time.size != 1)

        if (allIndicesKnownAtElaborationTime && !multiDimensionalTime && ioConns.forall(_.mapping.isEmpty)) {
          // For any particular PE, make sure that it only pops elements in-order
          ioConns.map(_.point).forall { point =>
            val iocs = ioConns.filter(_.point == point)
            val timeSteps = iocs.map(_.time).toSeq.sorted
            var previousIoConn: Option[Seq[Int]] = None
            timeSteps.forall { timeStep =>
              val ioConnsHere = iocs.filter(_.time == timeStep).map(_.ioIndex.map(_.asInstanceOf[Const].const))

              if (ioConnsHere.isEmpty) {
                true
              } else if (ioConnsHere.size > 1) {
                false
              } else {
                val outermostId = if (reordered_src_srams.isEmpty) 0 else if (reordered_src_srams.nonEmpty) {
                  val reorderCoords = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords != reorderCoords.sorted => reorderCoords }.toSet
                  assert(reorderCoords.size == 1)
                  val index = reorderCoords.head.indexOf(0)
                  if (index < 0) rf.nIOCoords-1 else index
                } else
                  0

                val ioConnHere = ioConnsHere.head

                val result = previousIoConn.isEmpty || without(ioConnHere, outermostId).zip(without(previousIoConn.get, outermostId)).forall(t => t._1 == t._2) &&
                  (ioConnHere(outermostId) == previousIoConn.get(outermostId) || ioConnHere(outermostId) == previousIoConn.get(outermostId) + 1)

                previousIoConn = Some(ioConnHere)

                result
              }
            }
          }
        } else if (!multiDimensionalTime && ioConns.forall(_.mapping.isEmpty) && dstSpArrays.isEmpty && reordered_src_srams.isEmpty && reordered_dst_srams.isEmpty) {
          (0 until rf.nIOCoords).count { coordId =>
            ioConns.map(_.point).forall { point =>
              val ioCoords = ioConns.filter(_.point == point).map(_.ioIndex)
              ioCoords.map(_(coordId)).toSet.size > 1
            }
          } == 1
        } else
          false
      } else
        false
    } else
      false

    val (canBeKrsteStyleTranspose, krsteStyleTransposeInnermostId) = if (rf.automaticallyOptimize && !canBeEdgeToEdge) {
      val outVars = var2RegFileConnections.collect {
        case (outVar, `rf`, DataConn | _: CoordConn, _, _, _) => outVar
      }

      val srcSpArrays = var2RegFileConnections.collect {
        case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
      }.flatten.map(spatialArray2Module(_)).toSet

      val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => (dst, reorderSramCoords) }

      if (srcSpArrays.size == 1 && dst_srams.size == 1) {
        val (inputFromEdge, inputCoordId) = {
          val spArray = srcSpArrays.head
          val its = spArray.inner.its
          val ioConns = its.ioConns.filter(ioc => outVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar))
          val points = ioConns.map(_.point)

          val unchangingDomainCoords = rf.domainCoordsToUseForOutputs.toSeq.flatMap { case (rfOutCoordId, domainCoordId) =>
            val domainCoordIsStationary = {
              val onlyAppearsInOneSpaceAxis = spArray.inner.transform.spaceTr.count(_(domainCoordId) != 0) == 1
              val soleDomainCoordInThatAxis = spArray.inner.transform.spaceTr.exists(axis => axis(domainCoordId) != 0 || axis.count(_ != 0) == 1)
              onlyAppearsInOneSpaceAxis && soleDomainCoordInThatAxis
            }
            Option.when(domainCoordIsStationary)(rfOutCoordId)
          }.toSet

          val changingCoords = (0 until rf.nIOCoords).filter { coordId =>
            !unchangingDomainCoords.contains(coordId) && points.forall { point =>
              val ioCoords = ioConns.filter(_.point == point).map(_.ioIndex)
              ioCoords.map(_(coordId)).toSet.size > 1
            }
          }

          if (changingCoords.size == 1) (true, changingCoords.head) else (false, -1)
        }

        val outputPerpendicularly = {
          val (dst_sram, reorderCoords) = dst_srams.head
          val innermostCoordId = {
            val result = reorderCoords.indexOf(0)
            if (result < 0) 0 else result
          }
          innermostCoordId == inputCoordId
        }

        (inputFromEdge && outputPerpendicularly, inputCoordId)
      } else
        (false, -1)
    } else
      (false, -1)

    val (canBeExplicitPipelineTransposed, nExplicitPipelineTransposedPipelines, explicitPipeliningInnermostId) = if (rf.automaticallyOptimize && !canBeEdgeToEdge && !canBeKrsteStyleTranspose) {
      val src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => src }.toSeq

      val dstSpArrays = var2RegFileConnections.collect {
        case (inVar: stellar.Input, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(inVar))
      }.flatten.toSet

      val variables = var2RegFileConnections.collect {
        case (variable: stellar.Input, `rf`, DataConn /* | _: CoordConn */, _, _, _) => variable
      }.toSet

      if (src_srams.size == 1 && dstSpArrays.size == 1) {
        val src_sram = src_srams.head

        if (src_sram.nBanks > 1) {
          val spArray = spatialArray2Module(dstSpArrays.head)
          val variable = variables.find(spArray.inner.ioVars.contains(_)).get

          val its = spArray.inner.its
          val ioConns = its.ioConns.filter(_.ioVar == variable)
          val points = ioConns.map(_.point)

          val changingCoords = (0 until rf.nIOCoords).filter { coordId =>
            points.forall { point =>
              val ioCoords = ioConns.filter(_.point == point).map(_.ioIndex.map {
                case Add(IndexLowerBound(_), Const(c)) => Const(c - 1) // We sometimes get exprs here where the lowerbounds haven't yet been removed // TODO figure out why that is
                case expr => expr
              })
              ioCoords.map(_(coordId)).toSet.size > 1
            }
          }.toSet

          val spArrayReadsFromEdge = changingCoords.size == 1

          if (spArrayReadsFromEdge) {
            // val nEdgePorts = spArray.inner.getInPortsForVar(variable).size
            (true, src_sram.nBanks, changingCoords.head)
          } else {
            (false, -1, -1)
          }
        } else
          (false, -1, -1)
      } else
        (false, -1, -1)
    } else
      (false, -1, -1)

    val triangularOpt = if (rf.automaticallyOptimize) {
      if (canBeEdgeToEdge) {
        val src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => src }.toSeq
        val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => dst }

        val inVars = var2RegFileConnections.collect { case (inVar: stellar.Input, `rf`, _, _, _, _) => inVar }.toSet
        val dstSpArrays = spatialArrayGens.filter(x => inVars.exists(x.variables.contains(_)))

        val outVars = var2RegFileConnections.collect { case (outVar: stellar.Output, `rf`, _, _, _, _) => outVar }.toSet
        val srcSpArrays = spatialArrayGens.filter(x => outVars.exists(x.variables.contains(_)))

        if (src_srams.nonEmpty && src_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense))) {
          if (dstSpArrays.nonEmpty && dstSpArrays.forall(x => x.transform.nTimeAxes == 1 && x.transform.timeTr.forall(_.forall(_ > 0))))
            Some(Some(true))
          else
            None
        } else if (dst_srams.nonEmpty && dst_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense))) {
          if (srcSpArrays.nonEmpty && srcSpArrays.forall(x => x.transform.nTimeAxes == 1 && x.transform.timeTr.forall(_.forall(_ > 0))))
            Some(Some(false))
          else
            None
        } else if (src_srams.isEmpty && dst_srams.isEmpty) {
          if (dstSpArrays.nonEmpty && dstSpArrays.forall(x => x.transform.nTimeAxes == 1 && x.transform.timeTr.forall(_.forall(_ >= 0))) && srcSpArrays.nonEmpty && srcSpArrays.forall(x => x.transform.nTimeAxes == 1 && x.transform.timeTr.forall(_.forall(_ > 0))))
            Some(Some(false))
          else
            None
        } else
          None
      } else
        None
    } else
      None

    val canBeInLockStepped = if (rf.automaticallyOptimize && triangularOpt.isEmpty) {
      val reordered_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords != reorderCoords.sorted => src }.toSeq ++
        regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, true/*isWrite*/, DataConn | _: CoordConn) if reorderSramCoords != reorderSramCoords.sorted => dst }.toSeq
      val unreordered_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords == reorderCoords.sorted => src }.toSeq ++
        regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, true/*isWrite*/, DataConn | _: CoordConn) if reorderSramCoords == reorderSramCoords.sorted => dst }.toSeq

      canBeEdgeToEdge && reordered_srams.isEmpty && unreordered_srams.nonEmpty && unreordered_srams.forall(_.elemsPerRead >= nInPorts) && unreordered_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense))
    } else
      false

    val constantCoordsForInputsOpt = if (rf.automaticallyOptimize) {
      val reordered_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords != reorderCoords.sorted => (src, reorderCoords) }.toSeq
      val unreordered_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords == reorderCoords.sorted => src }.toSeq

      val srcSpArrays = var2RegFileConnections.collect {
        case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
      }.flatten.map(spatialArray2Module(_))

      if (srcSpArrays.size == 1 && reordered_srams.isEmpty && unreordered_srams.isEmpty) {
        val spArray = srcSpArrays.head

        val srcSpArrayPorts = var2RegFileConnections.toSeq.collect {
          case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spArray.inner.getOutPortsForVar(outVar)
        }.flatten

        val hardcodedCoords = rf.domainCoordsToUseForOutputs.toSeq.foldLeft(srcSpArrayPorts.map(_.bits.hardCodedCoords.toSeq).toSeq) { case (acc, (rfOutCoordId, domainCoordId)) =>
          acc.zip(srcSpArrayPorts).map { case (accc, port) => accc.updated(rfOutCoordId, port.bits.hardCodedDomainCoords(domainCoordId)) }
        }

        // Some(srcSpArrayPorts.map(_.bits.hardCodedCoords.toSeq).toSeq)
        Some(hardcodedCoords.toSeq)
      } else if (canBeEdgeToEdge && srcSpArrays.isEmpty && reordered_srams.isEmpty && unreordered_srams.nonEmpty && unreordered_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense))) {
        val nPorts = unreordered_srams.map(_.elemsPerRead).min
        val nCoords = rf.nIOCoords
        Some(Seq.tabulate(nPorts)(inPortId => Seq.fill(nCoords - 1)(None) :+ Some(inPortId)))
      } else if (canBeExplicitPipelineTransposed && (reordered_srams.isEmpty || reordered_srams.size == 1 && unreordered_srams.isEmpty)) {
        val reorderCoords = if (reordered_srams.isEmpty) Seq.empty else reordered_srams.head._2
        val coordForOutermostBank = rf.nIOCoords - 1 - reorderCoords.lift(rf.nIOCoords-1).getOrElse(rf.nIOCoords-1)
        Some((reordered_srams.map(_._1) ++ unreordered_srams).flatten { sram =>
          Seq.tabulate(sram.nBanks, sram.elemsPerRead)((n,_) => Seq.fill(rf.nIOCoords)(None).updated(coordForOutermostBank,Some(n))).flatten
        })
      } else
        None
    } else
      None

    val incrementingCoordsForInputsOpt = if (rf.automaticallyOptimize) {
      val reordered_src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords != reorderCoords.sorted => src }.toSeq
      val unreordered_src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) if reorderCoords == reorderCoords.sorted => src }.toSeq

      val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) if reorderSramCoords.sorted == reorderSramCoords => dst }

      val srcSpArrays = var2RegFileConnections.collect {
        case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
      }.flatten.map(spatialArray2Module(_))

      if (canBeEdgeToEdge && reordered_src_srams.isEmpty && unreordered_src_srams.nonEmpty && unreordered_src_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense))) {
        val nPorts = unreordered_src_srams.map(_.elemsPerRead).min
        val nCoords = rf.nIOCoords
        Some(Set.tabulate(nCoords-1)((_,false)))
      } else if (canBeEdgeToEdge && reordered_src_srams.isEmpty && unreordered_src_srams.isEmpty && dst_srams.nonEmpty && dst_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense))) {
        Some(Set.tabulate(rf.nIOCoords-1)((_,false)))
      } else if (canBeEdgeToEdge && srcSpArrays.nonEmpty) {
        val outVars = var2RegFileConnections.collect {
          case (outVar, `rf`, DataConn | _: CoordConn, _, _, _) => outVar
        }

        val canIncrement = srcSpArrays.forall { srcSpArray =>
          val ioConns = srcSpArray.inner.its.ioConns.filter(ioc => outVars.contains(ioc.ioVar))

          ioConns.map(_.point).forall { point =>
            val iocs = ioConns.filter(_.point == point)
            val timeSteps = iocs.map(_.time).toSeq.sorted
            var previousIoConn: Option[Seq[Int]] = None
            timeSteps.forall { timeStep =>
              val ioConnsHere = iocs.filter(_.time == timeStep)
              if (ioConnsHere.forall(_.ioIndex.forall(_.isInstanceOf[Const]))) {
                val ioCoordsHere = iocs.filter(_.time == timeStep).map(_.ioIndex.map(_.asInstanceOf[Const].const))

                if (ioCoordsHere.isEmpty)
                  true
                else if (ioCoordsHere.size > 1)
                  false
                else {
                  val ioConnHere = ioCoordsHere.head
                  val result = previousIoConn.isEmpty || ioConnHere.tail.zip(previousIoConn.get.tail).forall(t => t._1 == t._2) &&
                    (ioConnHere.head == previousIoConn.get.head || ioConnHere.head == previousIoConn.get.head + 1)

                  previousIoConn = Some(ioConnHere)

                  result
                }
              } else
                false
            }
          }
        }

        if (canIncrement)
          Some(Set((0,false)))
        else
          None
      } else
        None
    } else
      None

    val maxOutCoordOptOpt: Option[Option[Int]] = if (rf.automaticallyOptimize) {
      val variables = var2RegFileConnections.collect { case (variable, `rf`, _, _, _, _) => variable }.toSet
      val src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => src }.toSeq
      if (variables.nonEmpty && src_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense) /* Sparse axes can produce unpredictable coordinates sometimes */)) {
        val coords = variables.flatMap { variable =>
          spatialArrayGens.filter(_.variables.contains(variable)).map(spatialArray2Module(_)).map(_.inner.its).flatMap(_.ioConns.filter(_.ioVar == variable).map(_.ioIndex)).flatten.toSet
        }

        if (coords.forall(_.isInstanceOf[Const])) {
          val maximum = coords.map { case Const(c) => c }.max + 1
          if (isPow2(maximum))
            Some(Some(maximum))
          else
            Some(Some(1 << log2Up(maximum)))
        } else
          None
      } else
        None
    } else
      None

    val coordsToIgnoreForOutputsOpt = if (rf.automaticallyOptimize) {
      val src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => src }.toSeq
      val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) if reorderSramCoords.sorted == reorderSramCoords => dst }

      val inVars = var2RegFileConnections.collect { case (inVar: stellar.Input, `rf`, _, _, _, _) => inVar }.toSet
      val dstSpArrays = spatialArrayGens.filter(x => inVars.exists(x.variables.contains(_)))
      val dstIoConns = dstSpArrays.map(spatialArray2Module(_)).flatMap(_.inner.its.ioConns.filter(ioc => inVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar)))

      if (canBeEdgeToEdge && src_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense)) && dst_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense)) && dstIoConns.forall(_.ioIndex.forall(_.isInstanceOf[Const])))
        Some((0 until rf.nIOCoords).toSet)
      else if (canBeEdgeToEdge && dstSpArrays.isEmpty && dst_srams.size == 1) {
        val innermostId = rf.nIOCoords - 1
        Some(Set(innermostId))
      } else if (canBeKrsteStyleTranspose)
        Some(Set(krsteStyleTransposeInnermostId))
      else if (canBeExplicitPipelineTransposed)
        Some(Set(explicitPipeliningInnermostId))
      else
        None
    } else
      None

    val lockstepOutsOpt = if (rf.automaticallyOptimize && triangularOpt.isEmpty) {
      if (canBeEdgeToEdge && canBeInLockStepped) {
        val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => dst }

        val inVars = var2RegFileConnections.collect { case (inVar: stellar.Input, `rf`, _, _, _, _) => inVar }.toSet
        val dstSpArrays = spatialArrayGens.filter(x => inVars.exists(x.variables.contains(_)))

        if (dst_srams.nonEmpty && dst_srams.forall(_.axes.forall(_ == FiberTreeAxis.Dense)))
          Some((nOutPorts, 1))
        else if (dstSpArrays.size == 1) {
          val dstSpArray = spatialArray2Module(dstSpArrays.head)
          val ioConns = dstSpArray.inner.its.ioConns.filter(ioc => inVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar))
          val points = ioConns.map(_.point)
          val timeSteps = ioConns.map(_.time)
          if (timeSteps.forall(timeStep => ioConns.filter(_.time == timeStep).map(_.point) == points))
            Some((nOutPorts, 1))
          else
            None
        } else
          None
      } else if (canBeKrsteStyleTranspose) {
        val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => dst }
        val nBanks = dst_srams.map(_.nBanks).sum
        val bankSize = dst_srams.map(_.elemsPerWrite).maxOption.getOrElse(1)
        Some((nBanks, bankSize))
      } else
        None
    } else
      None

    val lastInAxisFlagsOpt = if (rf.automaticallyOptimize) {
      val src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => (src, reorderCoords) }.toSeq
      val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => (dst, reorderSramCoords) }

      val inVars = var2RegFileConnections.collect { case (inVar: stellar.Input, `rf`, _, _, _, _) => inVar }.toSet
      val dstSpArrays = spatialArrayGens.filter(x => inVars.exists(x.variables.contains(_))).toSet

      val outVars = var2RegFileConnections.collect { case (outVar: stellar.Output, `rf`, _, _, _, _) => outVar }.toSet
      val srcSpArrays = spatialArrayGens.filter(x => outVars.exists(x.variables.contains(_)))

      if (src_srams.size == 1 && dstSpArrays.nonEmpty) {
        val (src_sram, reorderCoords) = src_srams.head
        val nInnermostNonDenseAxes = src_sram.axes.takeWhile(_ != FiberTreeAxis.Dense).size

        val result = Seq.tabulate(nInnermostNonDenseAxes) { axisId =>
          val outerAxisId = axisId + 1
          val outerCoordId = rf.nIOCoords - 1 - reorderCoords.lift(outerAxisId).getOrElse(outerAxisId)

          if (outerCoordId >= 0 && outerCoordId < rf.nIOCoords) {
            val ioConns = dstSpArrays.map(spatialArray2Module(_)).flatMap(_.inner.its.ioConns.filter(ioc => inVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar)))
            val ioConnCoords = ioConns.map(_.ioIndex(outerCoordId) match {
              case Add(IndexLowerBound(_), Const(c)) => Const(c - 1) // We sometimes get exprs here where the lowerbounds haven't yet been removed // TODO figure out why that is
              case expr => expr
            })
            val allKnownAtElaborationTime = ioConnCoords.forall(_.isInstanceOf[Const])
            Option.when(allKnownAtElaborationTime)((ioConnCoords.map(_.asInstanceOf[Const].const).max + 1, outerCoordId))
          } else
            None
        }

        Option.when(result.forall(_.nonEmpty))(result.map(_.get))
      } else if (canBeEdgeToEdge && srcSpArrays.size == 1 && dst_srams.size == 1) {
        val (sram, reorder_coords: Seq[Int]) = dst_srams.head
        Option.when(reorder_coords == reorder_coords.sorted && rf.nIOCoords == 2)(Seq((sram.elemsPerWrite, 0)))
      } else if (canBeKrsteStyleTranspose) {
        val outVars = var2RegFileConnections.collect {
          case (outVar, `rf`, DataConn | _: CoordConn, _, _, _) => outVar
        }

        val srcSpArrays = var2RegFileConnections.collect {
          case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
        }.flatten.map(spatialArray2Module(_)).toSet

        if (srcSpArrays.size == 1) {
          val spArray = srcSpArrays.head
          val its = spArray.inner.its
          val ioConns = its.ioConns.filter(ioc => outVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar))
          val points = ioConns.map(_.point)

          val unchangingDomainCoords = rf.domainCoordsToUseForOutputs.toSeq.flatMap { case (rfOutCoordId, domainCoordId) =>
            val domainCoordIsStationary = {
              val onlyAppearsInOneSpaceAxis = spArray.inner.transform.spaceTr.count(_(domainCoordId) != 0) == 1
              val soleDomainCoordInThatAxis = spArray.inner.transform.spaceTr.exists(axis => axis(domainCoordId) != 0 || axis.count(_ != 0) == 1)
              onlyAppearsInOneSpaceAxis && soleDomainCoordInThatAxis
            }
            Option.when(domainCoordIsStationary)((rfOutCoordId, domainCoordId))
          }.toSet

          val unchangingCoords = (0 until rf.nIOCoords).filter { coordId =>
            unchangingDomainCoords.map(_._1).contains(coordId) || points.forall { point =>
              val ioCoords = ioConns.filter(_.point == point).map(_.ioIndex)
              ioCoords.map(_(coordId)).toSet.size == 1
            }
          }

          val spans = unchangingCoords.map { coordId =>
            val size = if (unchangingDomainCoords.map(_._1).contains(coordId)) {
              val domainCoord = unchangingDomainCoords.find(_._1 == coordId).get._2
              its.base.points.map(_.coords(domainCoord)).toSet.size
            } else
              ioConns.map(_.ioIndex(coordId)).toSet.size
            (size, coordId)
          }

          Some(spans)
        } else
          None
      } else
        None
    } else
      None

    val subPipelineOptOpt = if (rf.automaticallyOptimize && !canBeEdgeToEdge) {
      val src_srams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => (src, reorderCoords) }.toSeq

      val inVars = var2RegFileConnections.collect { case (inVar: stellar.Input, `rf`, _, _, _, _) => inVar }.toSet
      val dstSpArrays = spatialArrayGens.filter(x => inVars.exists(x.variables.contains(_))).toSet

      if (src_srams.size == 1 && dstSpArrays.size == 1) {
        val (src_sram, reorderCoords) = src_srams.head
        val nInnermostNonDenseAxes = src_sram.axes.takeWhile(_ != FiberTreeAxis.Dense).size

        if (reorderCoords == reorderCoords.sorted || reorderCoords.forall(_ < nInnermostNonDenseAxes)) {
          if (nInnermostNonDenseAxes == 1) {
            val ioConns = dstSpArrays.map(spatialArray2Module(_)).flatMap(_.inner.its.ioConns.filter(ioc => inVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar)))
            val outermostCoordId = 0
            val ioConnCoords = ioConns.map(_.ioIndex(outermostCoordId))
            val allKnownAtElaborationTime = ioConnCoords.forall(_.isInstanceOf[Const])
            Option.when(allKnownAtElaborationTime)(Some(ioConnCoords.map(_.asInstanceOf[Const].const).max + 1, outermostCoordId, false, true))
          } else if (canBeExplicitPipelineTransposed) {
            val ioConns = dstSpArrays.map(spatialArray2Module(_)).flatMap(_.inner.its.ioConns.filter(ioc => inVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar)))
            val innermostCoordId = rf.nIOCoords - 1
            val ioConnCoords = ioConns.map(_.ioIndex(innermostCoordId) match {
              case Add(IndexLowerBound(_), Const(c)) => Const(c - 1) // We sometimes get exprs here where the lowerbounds haven't yet been removed // TODO figure out why that is
              case expr => expr
            })
            val allKnownAtElaborationTime = ioConnCoords.forall(_.isInstanceOf[Const])
            Option.when(allKnownAtElaborationTime)(Some(ioConnCoords.map(_.asInstanceOf[Const].const).max + 1, innermostCoordId, false, true))
          } else
            None
        } else
          None
      } else {
        None
      }
    } else
      None

    val getLastInFromInOpCountsOpt = if (rf.automaticallyOptimize) {
      if (canBeKrsteStyleTranspose) {
        val outVars = var2RegFileConnections.collect {
          case (outVar, `rf`, DataConn | _: CoordConn, _, _, _) => outVar
        }

        val srcSpArrays = var2RegFileConnections.collect {
          case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
        }.flatten.map(spatialArray2Module(_)).toSet

        val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => dst }

        Option.when(dst_srams.exists(_.nBanks > 1) && srcSpArrays.exists(_.inner.hasSubArrays))(true)
      } else
        None
    } else
      None

    val getTravellingOpCountFromInPortsOpt = if (rf.automaticallyOptimize) {
      if (canBeEdgeToEdge) {
        val outVars = var2RegFileConnections.collect {
          case (outVar, `rf`, DataConn | _: CoordConn, _, _, _) => outVar
        }

        val srcSpArrays = var2RegFileConnections.collect {
          case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
        }.flatten.map(spatialArray2Module(_)).toSet

        if (srcSpArrays.size == 1) {
          val spArray = srcSpArrays.head
          val its = spArray.inner.its
          val ioConns = its.ioConns.filter(ioc => outVars.map(_.asInstanceOf[Variable]).contains(ioc.ioVar)).map(_.ioIndex)
          val subArraysFinishAtUnpredictableTimes = spArray.inner.hasSubArrays && !ioConns.forall(_.isInstanceOf[Const])
          Option.when(subArraysFinishAtUnpredictableTimes)(true)
        } else
          None
      } else if (canBeKrsteStyleTranspose) {
        val outVars = var2RegFileConnections.collect {
          case (outVar, `rf`, DataConn | _: CoordConn, _, _, _) => outVar
        }

        val srcSpArrays = var2RegFileConnections.collect {
          case (outVar: stellar.Output, `rf`, DataConn | _: CoordConn, _, _, _) => spatialArrayGens.filter(_.variables.contains(outVar))
        }.flatten.map(spatialArray2Module(_)).toSet

        val dst_srams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, 0, reorderSramCoords, /*isWrite*/ true, DataConn | _: CoordConn) => dst }

        Option.when(dst_srams.exists(_.nBanks > 1) && srcSpArrays.exists(_.inner.hasSubArrays))(true)
      } else if (canBeExplicitPipelineTransposed)
        Option.when(sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => src }.forall(_.nBanks > 1))(true)
      else
        None
    } else
      None

    /*
    println(s"DEBUG Generating $name\n\tnElems = ${rf.nElems} | nOutPorts = $nOutPorts | nInPorts = $nInPorts | nElemsLookupPorts = $nElemsLookupPorts\n\tconstantCoordsForOutputs = $constantCoordsForOutputs\n\tconstantCoordsForNElemsLookups = $constantCoordsForNElemsLookups\n\tcanBeEdgeToEdge = $canBeEdgeToEdge")
    println(s"\tcanBeKrsteStyleTranspose = $canBeKrsteStyleTranspose")
    println(s"\tcanBeExplicitPipelineTransposed = $canBeExplicitPipelineTransposed | nExplicitPipelineTransposedPipelines = $nExplicitPipelineTransposedPipelines")
    println(s"\tcanBeInLockStepped = $canBeInLockStepped")
    println(s"\tconstantCoordsForInputsOpt = $constantCoordsForInputsOpt")
    println(s"\tincrementingCoordsForInputsOpt = $incrementingCoordsForInputsOpt")
    println(s"\tmaxOutCoordOptOpt = $maxOutCoordOptOpt")
    println(s"\tcoordsToIgnoreForOutputsOpt = $coordsToIgnoreForOutputsOpt")
    println(s"\ttriangularOpt = $triangularOpt")
    println(s"\tlockstepOutsOpt = $lockstepOutsOpt")
    println(s"\tlastInAxisFlagsOpt = $lastInAxisFlagsOpt")
    println(s"\tsubPipelineOptOpt = $subPipelineOptOpt")
    println(s"\tgetLastInFromInOpCountsOpt = $getLastInFromInOpCountsOpt")
    println(s"\tgetTravellingOpCountFromInPortsOpt = $getTravellingOpCountFromInPortsOpt")
    */

    val mod = Module(rf.toChiselModule(nInPorts.max(1), nOutPorts.max(1), dataWidthBits = dataWidthBits, nElemsLookupPorts = nElemsLookupPorts, constantCoordsForOutputs = constantCoordsForOutputs, constantCoordsForNElemsLookups = constantCoordsForNElemsLookups, coordsThatNeedExpanding = coordsThatNeedExpanding, withAsserts = withAsserts,

      entryOptionOpt = Option.when(rf.automaticallyOptimize && (canBeEdgeToEdge || canBeKrsteStyleTranspose || canBeExplicitPipelineTransposed))(Edge(shiftIfAnyFreeSpot = true)),
      exitOptionOpt = Option.when(rf.automaticallyOptimize && (canBeEdgeToEdge || canBeExplicitPipelineTransposed))(Edge()),
      lockstepInsOpt = Option.when(rf.automaticallyOptimize && canBeInLockStepped)(true),
      constantCoordsForInputsOpt = if (rf.automaticallyOptimize) constantCoordsForInputsOpt else None,
      incrementingCoordsForInputsOpt = if (rf.automaticallyOptimize) incrementingCoordsForInputsOpt else None,
      maxOutCoordOptOpt = if (rf.automaticallyOptimize) maxOutCoordOptOpt else None,
      coordsToIgnoreForOutputsOpt = if (rf.automaticallyOptimize) coordsToIgnoreForOutputsOpt else None,
      triangularOpt = if (rf.automaticallyOptimize) triangularOpt else None,
      lockstepOutsOpt = if (rf.automaticallyOptimize) lockstepOutsOpt else None,
      lastInAxisFlagsOpt = if (rf.automaticallyOptimize) lastInAxisFlagsOpt else None,
      subPipelineOptOpt = if (rf.automaticallyOptimize) subPipelineOptOpt else None,
      getLastInFromInOpCountsOpt = if (rf.automaticallyOptimize) getLastInFromInOpCountsOpt else None,
      getTravellingOpCountFromInPortsOpt = if (rf.automaticallyOptimize) getTravellingOpCountFromInPortsOpt else None,
      nPipelinesOptOpt = Option.when(rf.automaticallyOptimize && canBeExplicitPipelineTransposed)(Some(nExplicitPipelineTransposedPipelines)),
      separateLastInAxisFlagsForEachPipelineOpt = Option.when(rf.automaticallyOptimize && canBeExplicitPipelineTransposed && getTravellingOpCountFromInPortsOpt.contains(true))(true),
      resetLastInAxisFlagsBasedOnTravellingOpCountsOpt = Option.when(rf.automaticallyOptimize && canBeExplicitPipelineTransposed && getTravellingOpCountFromInPortsOpt.contains(true))(true),

    )).suggestName(name)
    println(s"\tDEBUG finished generating $name")

    mod.io.ins.foreach(_.valid := false.B)
    mod.io.ins.foreach(_.bits := DontCare)
    mod.io.outs.foreach(_.valid := false.B)
    mod.io.outs.foreach(_.coords := DontCare)
    mod.io.outs.foreach(_.op_count := DontCare)
    mod.io.outs.foreach(_.pop := DontCare)
    mod.io.outs.foreach(_.last_in_axis.foreach(_ := false.B))
    mod.io.last_in := false.B
    mod.io.last_out := false.B
    mod.io.n_elems_in_axis_lookups.foreach(_.address := DontCare)

    /*
    mod.io.ins.foreach(_.bits.valid_copy := DontCare)
    mod.io.ins.foreach(_.bits.increment_sticky_coord := DontCare)
    mod.io.outs.foreach(_.last_in_axis.foreach(_ := false.B))
    */

    (rf, mod)
  }.toMap
  println(s"DEBUG Finished generating regfiles")

  def coordLookupIsNeededForC2EMapping(spatialArrayMapping: Compressed2ExpandedMapping, relevantIndices: Set[Index]) =
    spatialArrayMapping.relevantIndices.intersect(relevantIndices).nonEmpty

  val coordLookup2Module = coordLookups.toSeq.map { coordLookup =>
    val name = coordLookup.name.getOrElse {
      val fillSrc = var2CoordLookupFills.find(_.coordLookup == coordLookup).map(_.outVar) match {
        case Some(outVar) => s"Var_$outVar"
        case _ =>
          val sram = sram2CoordLookupFills.find(_._2 == coordLookup).get._1
          sram.name.getOrElse(s"Sram_${sramCodes(sram)}")
      }
      s"CoordLookup_Filled_By_$fillSrc"
    }

    val nInports = {
      val fromSrams = sram2CoordLookupFills.toSeq.collect {
        case (sram, `coordLookup`, _) => sram2Module(sram).io.read_resps.map(_.bits.data.size).sum
      }.sum

      val fromVars = var2CoordLookupFills.toSeq.collect {
        case Var2CoordLookupFill(outVar, `coordLookup`, _) =>
          spatialArrays.map(_.inner.getOutPortsForVar(outVar).size).sum
      }.sum

      fromSrams + fromVars
    }

    val nLookupPorts = {
      val fromSramsOrVars = coordLookupsFromSramsOrVars.keys.toSeq.collect {
        case (Left((sram, axisId)), `coordLookup`) => sram2Module(sram).io.write_from_regfile_resps(axisId).flatMap(_.data).size
        case (Right(inVar: stellar.Input), `coordLookup`) => spatialArrays.map(_.inner.getInPortsForVar(inVar).size).sum
        case (Right(outVar: stellar.Output), `coordLookup`) => spatialArrays.map(_.inner.getOutPortsForVar(outVar).size).sum
      }.sum

      val fromSpArrayIndices = coordLookupsFromSpatialArrayIndices.toSeq.collect { case ((`coordLookup`, spatialArray), CoordLookupFromSpatialArrayIndex(_, indicesToInputIntoSpatialArray)) =>
        spatialArray2Module(spatialArray).io.compressed2ExpandedMappings.count(coordLookupIsNeededForC2EMapping(_, indicesToInputIntoSpatialArray.map(_._1).toSet))
      }.sum

      fromSramsOrVars + fromSpArrayIndices
    }

    val mod = Module(coordLookup.toChiselModule(nInports, nLookupPorts, withAsserts)).suggestName(name)
    mod.io.n_elems_in_axis_lookup.address := DontCare

    (coordLookup, mod)
  }.toMap

  val loadBalancerForRf = collection.mutable.Map.empty[RegFile, Seq[(LoadBalancer, stellar.Input)]]
  val loadBalancerForCl = collection.mutable.Map.empty[CoordLookup, Seq[(LoadBalancer, Index)]]
  val loadBalancer2Module = loadBalancers.toSeq.map { loadBalancer =>
    val inVars = loadBalancer.spatialArray.block.ioVariables.collect {
      case inVar: stellar.Input => inVar
    }
    val rfInPorts = inVars.map { inVar =>
      inVar -> var2RegFileConnections.toSeq.collectFirst { case (`inVar`, rf, _, _, _, _) =>
        val ports = regFile2Module(rf).io.ins
        (getChiselType(ports.head.bits), ports.size)
      }.get
    }.toMap
    val rfUpdatePorts = inVars.map { inVar =>
      inVar -> var2RegFileConnections.toSeq.collectFirst { case (`inVar`, rf, _, _, _, _) =>
        val ports = regFile2Module(rf).io.updates
        assert(ports.nonEmpty, s"${rf.nameOpt.getOrElse(s"Regfile for $inVar")} has no update ports")
        (getChiselType(ports.head.bits), ports.size)
      }.get
    }.toMap

    loadBalancerForRf ++= inVars.flatMap { inVar =>
      var2RegFileConnections.toSeq.collect { case (`inVar`, rf, _, _, _, _) => (rf, inVar) }
    }.groupBy(_._1).view.mapValues(_.map(loadBalancer -> _._2)).toMap

    val nClInPortsPerIndex = {
      val sizes = coordLookupsFromSpatialArrayIndices.toSeq.collect {
        case ((coordLookup, loadBalancer.spatialArray), _) =>
          coordLookup2Module(coordLookup).io.insert.size
      }.distinct
      assert(sizes.size == 1)
      sizes.head
    }
    val nClUpdatePortsPerIndex = {
      val sizes = coordLookupsFromSpatialArrayIndices.toSeq.collect {
        case ((coordLookup, loadBalancer.spatialArray), _) =>
          coordLookup2Module(coordLookup).io.updates.size
      }.distinct
      assert(sizes.size == 1)
      sizes.head
    }

    loadBalancerForCl ++= coordLookupsFromSpatialArrayIndices.toSeq.collect {
      case ((coordLookup, loadBalancer.spatialArray), CoordLookupFromSpatialArrayIndex(_, indicesToInputIntoSpatialArray)) =>
        indicesToInputIntoSpatialArray.map { case (index, _) =>
          coordLookup -> (loadBalancer, index)
        }
    }.flatten.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    val unsupportedVars = coordLookupsFromSramsOrVars.collect { case ((Right(variable), _), _) if inVars.contains(variable) => variable }.toSeq
    assert(unsupportedVars.isEmpty, s"i'm not yet sure how to support load-balancing for coord-lookups from these vars: $unsupportedVars")

    val nOutputPorts = spatialArray2Module(loadBalancer.spatialArray).io.loadBalancingMappings.size

    println(s"DEBUG Generating load balancer")
    val mod = Module(loadBalancer.toChiselModule(rfInPorts = rfInPorts, rfUpdatePorts = rfUpdatePorts, nOutputPorts = nOutputPorts, nClInPortsPerIndex = nClInPortsPerIndex, nClUpdatePortsPerIndex = nClUpdatePortsPerIndex))
    mod.io.clIns.flatten.foreach { clIn =>
      clIn.valid := false.B
      clIn.compressed := DontCare
      clIn.expanded := DontCare
      clIn.op_count := DontCare
    }
    mod.io.cl_last_ins.foreach(_ := false.B)
    (mod.io.rf_lasts_in_axes ++ mod.io.cl_lasts_in_axes).foreach(_ := false.B)
    println(s"\tDEBUG done generating load balancer")

    (loadBalancer, mod)
  }.toMap
  val loadBalancersSorted = loadBalancerCodes.toSeq.sortBy(_._2).map(_._1).map(loadBalancer2Module.apply)

  val coordLookupLookupsConnected = collection.mutable.Map() ++ coordLookups.map(c => (c, 0)).toMap
  val rfOutPortsConnected = collection.mutable.Map() ++ regFiles.map(rf => (rf, 0)).toMap
  val rfNElemsLookupPortsConnected = collection.mutable.Map() ++ regFiles.map(rf => (rf, 0)).toMap

  // Instantiate the outer
  val io = IO(new Bundle {
    val read_reqs = MixedVec(sramsSorted.map(sram => getChiselType(sram2Module(sram).io.read_reqs)))
    val read_resps = MixedVec(sramsSorted.map(sram => getChiselType(sram2Module(sram).io.read_resps)))

    val writes = MixedVec(sramsSorted.map(sram => Flipped(getChiselType(sram2Module(sram).io.write))))

    val loadBalancerConfigs = MixedVec(loadBalancersSorted.map(lb => Flipped(getChiselType(lb.io.config))))

    val busy = chisel3.Output(Bool())
    val read_busies = chisel3.Output(Vec(srams.size, Bool()))
    val write_busies = chisel3.Output(Vec(srams.size, Bool()))

    val incoming_reads = chisel3.Output(Vec(srams.size, Bool()))
    val incoming_writes = chisel3.Output(Vec(srams.size, Bool()))
  })

  def getReadReqPortIdForSramCode(sramCode: Int) = {
    val swapped = sramCodes.map(_.swap)
    val sram = swapped(sramCode)
    sramsSorted.indexOf(sram)
  }

  def getReadRespPortIdForSramCode(sramCode: Int) = {
    val swapped = sramCodes.map(_.swap)
    val sram = swapped(sramCode)
    sramsSorted.indexOf(sram)
  }

  def getWritePortIdForSramCode(sramCode: Int) = {
    val swapped = sramCodes.map(_.swap)
    val sram = swapped(sramCode)
    sramsSorted.indexOf(sram)
  }

  def getReadReqPortsForSramCode(sramCode: Int) = io.read_reqs(getReadReqPortIdForSramCode(sramCode))
  def getReadRespPortsForSramCode(sramCode: Int) = io.read_resps(getReadRespPortIdForSramCode(sramCode))
  def getWritePortForSramCode(sramCode: Int) = io.writes(getWritePortIdForSramCode(sramCode))

  def getReadReqPortsForSram(sram: SRAM) = getReadReqPortsForSramCode(sramCodes(sram))
  def getReadRespPortsForSram(sram: SRAM) = getReadRespPortsForSramCode(sramCodes(sram))
  def getWritePortForSram(sram: SRAM) = getWritePortForSramCode(sramCodes(sram))

  // val busy = any(sram2Module.values.map(_.io.busy) ++ spatialArrays.map(_.io.busy))
  val busy = any(sram2Module.values.map(_.io.busy))
  // val busy = any(sram2Module.values.map(_.io.busy) ++ regFile2Module.values.map(_.io.busy))
  io.busy := busy || RegNext(busy) // TODO we currently add this RegNext because there are sometimes one-cycle delays between the end of one spatial array execution, and the beginning of the next execution in the pipeline, during which the busy signal may be low for a cycle. We should fix this so that the RegNext isn't necessary // TODO do we still need the regnext if we don't check if spatialArrays are busy?
  io.read_busies := srams.toSeq.map(sram2Module(_).io.read_busy)
  io.write_busies := srams.toSeq.map(sram2Module(_).io.write_busy)

  io.incoming_writes := io.writes.map(_.valid).toSeq
  io.incoming_reads := io.read_reqs.map(x => any(x.map(_.valid))).toSeq

  // Create a Vec of ready signals for SRAM read resps. Some SRAMs will be able to output their read-resps as soon as
  // outer-IO is ready; others will have to wait for regfiles or coord-lookups to have space for them. Having one global
  // variable for these ready signals here makes them easy to keep track of later in the module
  val sram_read_resp_readies = sramsSorted.map(sram => VecInit.fill(sram.nBanks)(true.B))
  def get_sram_read_resp_readies(sramCode: Int) = {
    val swapped = sramCodes.map(_.swap)
    val sram = swapped(sramCode)
    val index = sramsSorted.indexOf(sram)
    sram_read_resp_readies(index)
  }

  // Connect the SRAMs to the outer IO
  sramsSorted.zip(sram_read_resp_readies).foreach { case (sram, read_resp_readies) =>
    val sram_mod = sram2Module(sram)
    val sram_code = sramCodes(sram)

    // Connect read-reqs
    val read_reqs = getReadReqPortsForSramCode(sram_code)
    sram_mod.io.read_reqs.zip(read_reqs).foreach { case (r1, r2) =>
      r1 <> r2
    }

    // Connect write-reqs
    val write_req = getWritePortForSramCode(sram_code)
    sram_mod.io.write <> write_req

    // Connect read-resps
    val io_read_resps = getReadRespPortsForSramCode(sram_code)
    val sram_read_resps = sram_mod.io.read_resps
    io_read_resps.zip(sram_read_resps).zip(read_resp_readies).foreach { case ((io_read_resp, sram_read_resp), read_resp_ready) =>
      io_read_resp.valid := sram_read_resp.valid && !sram_read_resp.bits.to_regfile
      io_read_resp.bits := sram_read_resp.bits

      when (!io_read_resp.bits.to_regfile) {
        read_resp_ready := io_read_resp.ready
      }
      sram_read_resp.ready := read_resp_ready
    }

    // Connect write/read-from-regfile-reqs
    sram_mod.io.write_from_regfile_reqs.flatten.foreach(_.ready := false.B)
    sram_mod.io.write_from_regfile_resps := DontCare
    sram_mod.io.read_from_regfile_reqs.flatten.foreach(_.ready := false.B)
    sram_mod.io.read_from_regfile_resps := DontCare
  }

  spatialArray2Module.foreach { case (spatialArray, spatialArrayMod) =>
    val ioVars = spatialArray.allIOPorts.keys.toSet
    val var2RegFileConns = var2RegFileConnections.filter(t => ioVars.contains(t._1: Variable)).toSeq

    var2RegFileConns.foreach {
      case (input: stellar.Input, rf, connType: Mem2MemConnType, None, popBitsOpt, maxRfOutOpt) if connType != NElemLookupConn =>
        val spatialArrayInPorts = spatialArrayMod.inner.getInPortsForVar(input, spatialArrayMod.io.ins.toSeq)

        val rf_start = rfOutPortsConnected(rf)
        val rfMod = regFile2Module(rf)
        val rfOutPorts = rfMod.io.outs.drop(rf_start)

        assert(spatialArrayInPorts.isEmpty || spatialArrayInPorts.size <= rfOutPorts.size,
          s"rf has ${rfOutPorts.size} out ports left (${rfMod.io.outs.size} total), but spatial array has ${spatialArrayInPorts.size} input ports for $input")

        val debug_should_print = false

        spatialArrayInPorts.zip(rfOutPorts).foreach { case (spArrIn, rfOut) =>
          try {
            spArrIn <> rfOut

            val rf_out_data = WireInit(rfOut.data)
            maxRfOutOpt.foreach { maxRfOut =>
              rf_out_data := (rfOut.data.asUInt % maxRfOut.U).asSInt
              spArrIn.data := rf_out_data
            }

            popBitsOpt.foreach { popBits =>
              rfOut.pop.bits := popBits.U
            }

            spArrIn.found := rfOut.found || rfOut.unavailable
            when (!rfOut.found) {
              spArrIn.data := 0.S
            }

            when ((false && debug_should_print).B && spArrIn.valid) {
              printf(p"Spatial array is trying to input $input from ${rf.nameOpt.getOrElse("unknown regfile")} @$debug_cycle\n")
              printf(p"\tsp array in port id = ${spatialArrayMod.io.ins.indexOf(spArrIn)}\n")
              printf(p"\trf port id = ${rfMod.io.outs.indexOf(rfOut)}\n")
              printf(p"\tspArrIn = ${spArrIn}\n")
              printf(p"\trfOut = ${rfOut}\n")
              printf(p"\n")
            }

            connType match {
              case CoordConn(-1) => spArrIn.data := Mux(rfOut.found && !(input.validConds.nonEmpty.B && !rfOut.valid), rf_out_data, maxVal(spArrIn.data))
              case CoordConn(coord) => spArrIn.data := Mux(rfOut.found, rfOut.expanded_coords(coord), maxVal(spArrIn.data))
              case _ =>
            }
          } catch {
            case e: chisel3.ChiselException =>
              println(s"Error when connecting reg-file to $input (connType=$connType)")
              throw e
          }
        }

        rfOutPortsConnected(rf) += spatialArrayInPorts.size

        val rfFeedsSram = regFile2SRAMConnections.toSeq.exists(conn => conn.src == rf && conn.isWrite) // TODO Should 'last-out' signals come from spatial arrays or SRAMs for scatter-gathers?
        if (!rfFeedsSram) {
          val lasts = spatialArrayMod.inner.getLastPortsForVar(input, spatialArrayMod.io.last_in_or_out)
          rfMod.io.last_out := lasts.map(_.valid).reduce(_ || _)

          if (withAsserts)
            for ((last, i) <- lasts.zipWithIndex) {
              for (last2 <- lasts.drop(i+1)) {
                when (last.valid && last2.valid) {
                  assert(last.bits === last2.bits, "trying to assert 'last' on two separate reg-file buffers at once. we don't support this yet")
                }
              }
            }
        }

        coordLookupsFromSramsOrVars.toSeq.collect {
          case ((Right(`input`), coordLookup_), CoordLookupFromSramOrIo(compressedCoords, expandedCoords)) =>
            val coordLookup = coordLookup2Module(coordLookup_)

            val lookup_start = coordLookupLookupsConnected(coordLookup_)

            val n_connections = spatialArrayInPorts.size.min(rfOutPorts.size)
            assert(coordLookup.io.lookup.size - lookup_start >= n_connections,
              s"you need to add ${n_connections - coordLookup.io.lookup.size + lookup_start} more coord-lookup lookup ports for CoordLookup_${coordLookups.toSeq.indexOf(coordLookup_)}")

            coordLookup.io.lookup.drop(lookup_start).zip(spatialArrayInPorts).zip(rfOutPorts).foreach { case ((lookup, spArrayInPort), rfOutPort) =>
              lookup.valid := spArrayInPort.valid

              val compressed_addr = spArrayInPort.coords
              val expanded_addr = VecInit(compressed_addr.toIndexedSeq) // The "toIndexedSeq" was just added here to resolve an obscure compilation error

              compressedCoords.foreach { case (lookupCompressedCoord, ioCoord, ioCoordIsCompressed) =>
                lookup.compressed(lookupCompressedCoord) := {
                  if (ioCoordIsCompressed) compressed_addr(ioCoord)
                  else expanded_addr(ioCoord)
                }
              }

              expandedCoords.foreach { case (lookupExpandedCoord, ioExpandedCoord) =>
                expanded_addr(ioExpandedCoord) := Mux(lookup.found, lookup.expanded(lookupExpandedCoord), maxVal(expanded_addr(ioExpandedCoord)))
              }

              rfOutPort.coords := expanded_addr

              lookup.op_count := spArrayInPort.op_count
              lookup.pop.valid := spArrayInPort.pop.valid
              lookup.pop.bits := spArrayInPort.pop.bits
              assert(!rfFeedsSram.B || !lookup.pop.valid, "i'm not sure if it's ok to pop from this coord-lookup when the reg-file will later feed and sram")

              when (debug_should_print.B && lookup.valid) {
                printf(p"Spatial array is trying to lookup coords for $input from ${coordLookup_.name.getOrElse("unknown regfile")} @$debug_cycle\n")
                printf(p"\tsp array in port id = ${spatialArrayMod.io.ins.indexOf(spArrayInPort)}\n")
                printf(p"\trf port id = ${rfMod.io.outs.indexOf(rfOutPort)}\n")
                printf(p"\tlookup port id = ${coordLookup.io.lookup.indexOf(lookup)}\n")
                printf(p"\tspArrayInPort = ${spArrayInPort}\n")
                printf(p"\tlookup = ${lookup}\n")
                printf(p"\n")
              }

              when (!lookup.found && !lookup.unavailable) {
                // We use Chisel's last-connect semantics here, so it's important that the spArrayInPort is connected to
                // the regfile's out-port BEFORE this line
                spArrayInPort.found := false.B
              }.elsewhen(lookup.unavailable) {
                // TODO The interface currently makes it a bit confusing as to whether the spatial-array should be monitoring the "found" or "unavailable" ports
                spArrayInPort.found := true.B
                spArrayInPort.unavailable := true.B
              }
            }

            coordLookupLookupsConnected(coordLookup_) += n_connections
        }

      case (input: stellar.Input, rf, NElemLookupConn, None, _, _)  =>
        val spatialArrayInPorts = spatialArrayMod.inner.getInPortsForVar(input, spatialArrayMod.io.ins.toSeq)

        val rf_start = rfNElemsLookupPortsConnected(rf)
        val rfMod = regFile2Module(rf)
        val nElemLookupPorts = rfMod.io.n_elems_in_axis_lookups.drop(rf_start)

        assert(spatialArrayInPorts.isEmpty || spatialArrayInPorts.size <= nElemLookupPorts.size,
          s"rf has ${nElemLookupPorts.size} out ports left (${rfMod.io.n_elems_in_axis_lookups.size} total), but spatial array has ${spatialArrayInPorts.size} input ports for $input")

        spatialArrayInPorts.zip(nElemLookupPorts).foreach { case (spArrIn, nElemLookup) =>
          try {
            spArrIn.data := nElemLookup.nElems.zext
            spArrIn.expanded_coords := spArrIn.coords
            nElemLookup.address := spArrIn.coords
            spArrIn.found := spArrIn.op_count === rfMod.io.fillCount // TODO we need to add an "opCount" field to the nElemLookup port
            spArrIn.unavailable := false.B
          } catch {
            case e: chisel3.ChiselException =>
              println(s"Error when connecting reg-file to $input (connType=NElemLookupConn)")
              throw e
          }
        }

        rfNElemsLookupPortsConnected(rf) += spatialArrayInPorts.size

      case (output: stellar.Output, rf, connType, maxOutPortsOpt, _, _) if connType != NElemLookupConn =>
        val spatialArrayOutPorts = {
          val spArrayOutPorts = spatialArrayMod.inner.getOutPortsForVar(output, spatialArrayMod.io.outs.toSeq)

          maxOutPortsOpt match {
            case Some((maxOutPorts, orderBy)) =>
              val top_n_arbiter = Module(new TopNArbiter(spArrayOutPorts.head.bits, nInPorts=spArrayOutPorts.size, nOutPorts=maxOutPorts, orderBy))
              top_n_arbiter.io.in.zip(spArrayOutPorts).foreach(t => t._1 <> t._2)
              top_n_arbiter.io.out

            case None => spArrayOutPorts
          }
        }

        val rfMod = regFile2Module(rf)
        val rfInPorts = rfMod.io.ins

        assert(spatialArrayOutPorts.isEmpty || spatialArrayOutPorts.size == rfInPorts.size,
          s"Spatial array has ${spatialArrayOutPorts.size} output ports for $output, but ${rf.nameOpt.getOrElse("reg-file")} has ${rfInPorts.size} input ports")

        val lasts = spatialArrayMod.inner.getLastPortsForVar(output, spatialArrayMod.io.last_in_or_out)
        rfMod.io.last_in := lasts.map(_.valid).reduce(_ || _)

        val loadBalancerSnoopersOpt = loadBalancerForRf.get(rf).map(_.map { case (lb, iv) => loadBalancer2Module(lb).getRfInsForInVar(iv) })
        loadBalancerSnoopersOpt.foreach(_.foreach({ case (_, last_in, _) =>
          last_in := rfMod.io.last_in
        }))

        spatialArrayOutPorts.zip(rfInPorts).zipWithIndex.foreach { case ((spArrOut, rfIn), i) =>
          try {
            rfIn <> spArrOut
          } catch {
            case e: chisel3.ChiselException =>
              println(s"Error when connecting reg-file to $output.")
              if (rfIn.bits.coords.size != spArrOut.bits.coords.size)
                println(s"\tThe reg-file has ${rfIn.bits.coords.size} coords, but the sp-array has ${spArrOut.bits.coords.size} coords")
              if (rfIn.bits.element.domainCoords.size != spArrOut.bits.element.domainCoords.size)
                println(s"\tThe reg-file has ${rfIn.bits.element.domainCoords.size} domain-coords, but the sp-array has ${spArrOut.bits.element.domainCoords.size} domain-coords")
              throw e
          }

          connType match {
            case CoordConn(coord) => rfIn.bits.element.data := spArrOut.bits.coords(coord)
            case _ =>
          }

          if (output.lastInAxisConds.exists(x => (x._2: Int) != 0)) {
            rfIn.bits.last_in_axis.tail.foreach(_ := false.B)
            when (spArrOut.bits.last_in_axis(1)) {
              rfIn.bits.coords(1) := spArrOut.bits.coords(1) +& 1.S
            }
          }

          loadBalancerSnoopersOpt.foreach(_.foreach { case (snoopers, _, _) =>
            val snooper = snoopers(i)
            snooper.valid := rfIn.fire
            snooper.bits := rfIn.bits
            when (!snooper.ready) {
              spArrOut.ready := false.B
              rfIn.valid := false.B
              rfMod.io.last_in := false.B
            }
          })
        }

        if (withAsserts)
          for ((last, i) <- lasts.zipWithIndex) {
            for (last2 <- lasts.drop(i+1)) {
              when (last.valid && last2.valid) {
                assert(last.bits === last2.bits, "trying to assert 'last' on two separate reg-file buffers at once. we don't support this yet")
              }
            }
          }

        coordLookupsFromSramsOrVars.toSeq.collect {
          case ((Right(`output`), _), _) =>
            throw new Exception("we haven't gotten around to adding the code to support coord lookups to output variables yet")
        }

      case x => throw new Exception("unsupported connection to intermediate variable")
    }

    spatialArray.variables.filter(variable => variable.isIO && !var2RegFileConns.map(_._1: Variable).toSet.contains(variable)).foreach {
      // We connect the ports for un-connected variables here
      case inVar: stellar.Input =>
        val spatialArrayInPorts = spatialArrayMod.inner.getInPortsForVar(inVar, spatialArrayMod.io.ins.toSeq)
        spatialArrayInPorts.foreach { in =>
          in.data := DontCare
          in.found := DontCare
          in.unavailable := DontCare
          in.expanded_coords := DontCare
        }

      case outVar: stellar.Output =>
        val spatialArrayOutPorts = spatialArrayMod.inner.getOutPortsForVar(outVar, spatialArrayMod.io.outs.toSeq)
        spatialArrayOutPorts.foreach(_.ready := false.B)

      case _: Intermediate => assert(false, "UNREACHABLE")
    }

    try {
      connectVecs(spatialArrayMod.io.rfFillCounters, var2RegFileConns.toSeq.collect {
        case (_: Input, rf, _, _, _, _) => regFile2Module(rf).io.fillCount + !regFile2Module(rf).io.isEmpty
      }, fillIn = Some(DontCare))
    } catch {
      case e: Exception if spatialArray.name.nonEmpty =>
        println(s"Error when connecting rfFillCounters for ${spatialArray.name.getOrElse("unnamed spatial array")}")
        throw e
    }
  }

  // Connect regfiles to their source SRAMs
  sram2RegFileConnections.toSeq.map(_._2).distinct.foreach { rf =>
    val rfMod = regFile2Module(rf)

    val srcSrams = sram2RegFileConnections.collect { case (src, `rf`, reorderCoords) => (reorderCoords, src, sram2Module(src), sramCodes(src)) }.toSeq
    assert(PopCount(srcSrams.map(_._3).map(s => any(s.io.read_resps.map(x => x.valid && x.bits.to_regfile)))) <= 1.U, "i'm not sure if it's OK for two SRAMs to feed into the same regfile simultaneously or not")

    val srcIsMultiBanked = srcSrams.exists(_._2.nBanks > 1)
    rfMod.io.last_in := srcIsMultiBanked.B // If the SRAM is multibanked, then we assume that last_in is true by default, but make it false if any of the banks have still not progressed far enough. For single-banked SRAMs, we assume it is false by default, but set it to true whenever any of the source SRAMs specifies that it's making it's last regfile output

    srcSrams.foreach { case (coordOrder, sram, sramMod, sram_code) =>
      sramMod.io.read_resps.zipWithIndex.map { case (sram_out, bankId) =>
        val span = sram_out.bits.spans.head

        val loadBalancerSnoopersOpt = loadBalancerForRf.get(rf).map(_.map { case (lb, iv) => loadBalancer2Module(lb).getRfInsForInVar(iv) })
        require(sram.nBanks == 1 || loadBalancerSnoopersOpt.isEmpty, "I haven't updated the load-balancer snooping code yet to accomodate multi-banked SRAMs")

        val isDefaultSRAM = sram_code == srcSrams.head._4

        val alsoFillsCl = sram2CoordLookupFills.map(_._1).toSeq.contains(sram)
        if (!alsoFillsCl) {
          sramMod.io.n_elems_in_axis_lookups.zip(rfMod.io.n_elems_in_axis_lookups).foreach { case (sram_n_elems_lookup, rf_n_elems_lookup) =>

            val unordered_lookup_address = sram_n_elems_lookup.address.take(rf_n_elems_lookup.address.size)
            val ordered_lookup_address = coordOrder.zipWithIndex.foldLeft(unordered_lookup_address) { case (acc, (oldPos, newPos)) =>
              acc.updated(newPos, unordered_lookup_address(oldPos))
            }.take(rf_n_elems_lookup.address.size)

            rf_n_elems_lookup.address := ordered_lookup_address.reverse
            sram_n_elems_lookup.nElems := rf_n_elems_lookup.nElems
          }
          require(sram2RegFileConnections.count(x => (x._1: SRAM) == sram) == 1 || sram.maxElemsInRf.isEmpty, "we don't yet support feeding multiple rfs from the same SRAM if the SRAM has a maxElemsInRf limit")
        }

        val rfIns = rfMod.io.ins.drop(bankId * sram_out.bits.data.size).take(sram_out.bits.data.size)

        def whenClause(foo: => Any) = if (isDefaultSRAM) {
          foo
        } else {
          when(sram_out.valid && sram_out.bits.to_regfile) {
            foo
          }
        }

        whenClause {
          rfIns.zipWithIndex.foreach { case (rfIn, i) =>
            val rf_dim = rfIn.bits.coords.size

            rfIn.valid := sram_out.fire && sram_out.bits.to_regfile && i.U < span

            val increment_addr = VecInit.fill(sram_out.bits.expanded_addresses(i).size)(false.B)

            val unordered_addr: Seq[UInt] = try {
              MatrixChiselUtil.addvU(sram_out.bits.expanded_addresses(i), increment_addr)
            } catch {
              case e: Exception =>
                println(s"Error when indexing into expanded_addresses for ${rf.nameOpt}\n\texp-addr-size = ${sram_out.bits.expanded_addresses.size}\n\trf in-port size = ${rfMod.io.ins.size}")
                throw e
            }
            val addr = coordOrder.zipWithIndex.foldLeft(unordered_addr) { case (acc, (oldPos, newPos)) =>
              acc.updated(newPos, unordered_addr(oldPos))
            }.take(rf_dim)

            rfIn.bits.element.data := sram_out.bits.data(i)

            rfIn.bits.coords := addr.map(_.zext).reverse

            if (rf.stickyInPortCoords.nonEmpty) {
              when (!sram_out.valid) {
                rf.stickyInPortCoords.foreach { coordId =>
                  rfIn.bits.coords(coordId) := 0.S
                }
              }
              rfIn.bits.increment_sticky_coord := sram_out.fire && sram_out.bits.last_in_axis.head
            }

            rfIn.bits.element.domainCoords := DontCare
            rfIn.bits.element.domainCoords.headOption.foreach(_ := sram_out.bits.compressed_address.head.zext +& i.S)
            rfIn.bits.element.domainCoords.zip(sram_out.bits.compressed_address).tail.foreach { case (dst, src) => dst := src.zext }

            connectVecs(rfIn.bits.axis_spans, sram_out.bits.axis_spans) // TODO this should follow the coord-ordering

            if (srcIsMultiBanked) {
              rfIn.bits.op_count := sram_out.bits.opCount
            } else {
              rfIn.bits.op_count := rfMod.io.fillCount
            }

            rfIn.bits.last_in_axis := (if (i == 0) {
              val sram_last_in_axis = sram_out.bits.last_in_axis

              val merged_sram_last_in_axis = VecInit.fill(sram_last_in_axis.size)(false.B)
              if (coordOrder.isEmpty) {
                var k = 0
                rf.lastInAxisFlags.reverse.zipWithIndex.foreach { case ((_, coordId), j) =>
                  val n_last_signals_to_merge = rf.nIOCoords - 1 - coordId - k
                  merged_sram_last_in_axis(j) := all(sram_last_in_axis.slice(k, k + n_last_signals_to_merge))
                  k += n_last_signals_to_merge
                }
                merged_sram_last_in_axis.drop(rf.lastInAxisFlags.size).zip(sram_last_in_axis.drop(k)).foreach { case (x,y) => x := y }
              } else {
                merged_sram_last_in_axis := sram_last_in_axis
              }

              merged_sram_last_in_axis.take(rfIn.bits.last_in_axis.size).map(_ && sram_out.fire && sram_out.bits.to_regfile)
            } else {
              // To reduce the size of the generated Verilog, we only hook up the last-in-axis signals of the very first "rfIn", since they should all have the same "last-in-axis" signals anyhow
              Seq.fill(rfIn.bits.last_in_axis.size)(false.B)
            })

            loadBalancerSnoopersOpt.foreach(_.foreach { case (snoopers, _, _) =>
              val snooper = snoopers(i)
              snooper.valid := rfIn.fire
              snooper.bits := rfIn.bits
              when(!snooper.ready) {
                get_sram_read_resp_readies(sram_code)(bankId) := false.B
                rfIn.valid := false.B
                rfMod.io.last_in := false.B
                rfIn.bits.last_in_axis.foreach(_ := false.B)
              }
            })
          }

          loadBalancerSnoopersOpt.foreach(_.foreach { case (_, snooper_last_in, snooper_last_in_axis) =>
            snooper_last_in := rfMod.io.last_in
            snooper_last_in_axis := rfMod.io.ins.head.bits.last_in_axis.head;
            assert(rf.lastInAxisFlags.size <= 1, s"the load balancer doesn't yet support multi-dim last-in-axis flags: ${rf.lastInAxisFlags.size}")
          })

          val all_rf_ins_ready = all(rfIns.zipWithIndex.map { case (rfIn, i) => rfIn.ready || i.U >= span })
          when (sram_out.bits.to_regfile && !all_rf_ins_ready) {
            get_sram_read_resp_readies(sram_code)(bankId) := false.B
          }

          if (!srcIsMultiBanked) {
            when (sram_out.fire && sram_out.bits.to_regfile && sram_out.bits.to_regfile_last) {
              rfMod.io.last_in := true.B
            }
          }

          assert(!sram_out.valid || !sram_out.bits.to_regfile || all(sram_out.bits.spans.tail.map(_ <= 1.U)),
            s"only the innermost dimension can span more than one element, currently\n\tSRAM name = ${sram.name} | bankId = $bankId | rf name = ${rf.nameOpt}")
        }

        if (srcIsMultiBanked) {
          val last_now = sram_out.fire && sram_out.bits.to_regfile && sram_out.bits.to_regfile_last
          when ((sram_out.bits.opCount + last_now) <= rfMod.io.fillCount) {
            rfMod.io.last_in := false.B
          }
        }
      }
    }
  }

  // Connect the unconnected regfiles to the destination SRAMs
  regFile2SRAMConnections.toSeq.map(_.src).distinct.foreach { rf =>
    val rfMod = regFile2Module(rf)

    val dstSrams = regFile2SRAMConnections.collect { case RegFile2SRAMConn(`rf`, dst, sramAxisId, coordOrder, isWrite, connType) => (dst, sramAxisId, coordOrder, isWrite, connType) }.toSeq
    val dstIsMultiBanked = dstSrams.exists(_._1.nBanks > 1)

    assert(PopCount(dstSrams.map { case (dst, axisId, _, _, _) => any(sram2Module(dst).io.write_from_regfile_reqs(axisId).map(_.valid)) }) <= 1.U, "i'm not sure if it's OK for two SRAMs to read from the same regfile simultaneously or not")
    require(!dstIsMultiBanked || dstSrams.size == 1, "For now, the last-out code below only really works if we only have a single SRAM reading from the regfile")

    val writeExists = dstSrams.exists { x => require(x._4.isInstanceOf[Boolean]); x._4 } // We don't set last_out for gathers
    if (writeExists)
      rfMod.io.last_out := dstIsMultiBanked.B // If the SRAM is multibanked, then we assume that last_out is true by default, but make it false if any of the banks have still not progressed far enough. For single-banked SRAMs, we assume it is false by default, but set it to true whenever any of the destination SRAMs specifies that it's making it's last regfile request

    dstSrams.foreach { case (sram_, sramAxisId, coordOrder, isWrite, connType) =>
      val sram = sram2Module(sram_)
      val sram_reqs = if (isWrite) sram.io.write_from_regfile_reqs(sramAxisId) else sram.io.read_from_regfile_reqs(sramAxisId)
      val sram_resps = if (isWrite) sram.io.write_from_regfile_resps(sramAxisId) else sram.io.read_from_regfile_resps(sramAxisId)

      val coordLookups = coordLookupsFromSramsOrVars.keys.collect {
        case (Left((`sram_`, `sramAxisId`)), coordLookup) => coordLookup
      }

      var rfPortsConnectedForThisStage = 0

      sram_reqs.zip(sram_resps).zipWithIndex.foreach { case ((sram_req, sram_resp), bankId) =>
        val rf_outs = rfMod.io.outs.drop(rfOutPortsConnected(rf) + rfPortsConnectedForThisStage).take(sram_resp.data.size)
        rfPortsConnectedForThisStage += sram_resp.data.size

        val isDefaultSRAMPort = sram_ == dstSrams.head._1

        val debug_should_print = false // sram_.name.exists(_.contains("Scattered")) && sramAxisId == 0
        // println(s"sram.name = ${sram_.name} | rf.name = ${rf.name} | sramAxisId = $sramAxisId | out-ports = ${rfOutPortsConnected(rf)} | debug_should_print = $debug_should_print")
        when (debug_should_print.B && sram_req.valid) {
          printf(p"\nWriting from ${rf.nameOpt.getOrElse("unknown rf")} @$debug_cycle\n")
          printf(p"\tsram_req.ready = ${sram_req.ready}\n")
          printf(p"\tsram_req.bits.req.address = ${sram_req.bits.req.address}\n")
          printf(p"\tsram_req.bits.req.spans = ${sram_req.bits.req.spans}\n")
          printf(p"\trf ports starting from index ${rfOutPortsConnected(rf)}\n")
          printf(p"\trf_outs.map(_.found) = ${VecInit(rf_outs.map(_.found))}\n")
          printf(p"\tcoordOrder = $coordOrder\n")
          printf(p"\temptyCount = ${rfMod.io.emptyCount}\n")
        }

        def whenClause(foo: => Any) = if (isDefaultSRAMPort) {
          foo
        } else {
          when (sram_req.valid) { foo }
        }

        whenClause {
          val rf_outs_found = VecInit(rf_outs.map(x => x.found || x.unavailable))
          val all_rf_outs_found = all(rf_outs_found) /* TODO do we even need this anymore since the regfile should be telling us whether or not elements are available? || rfMod.io.emptyCount < rfMod.io.fillCount */
          sram_req.ready := all_rf_outs_found

          when (debug_should_print.B && sram_req.valid) {
            printf(p"\trf_outs_found = $rf_outs_found\n")
            printf(p"\tall_rf_outs_found = $all_rf_outs_found\n")
          }

          rf_outs.zipWithIndex.foreach { case (rfOut, i) =>
            val rf_dim = rfOut.coords.size

            val within_span =  i.U < sram_req.bits.req.spans(sramAxisId); assert(!sram_req.valid || sram_req.bits.req.axis === sramAxisId.U)

            val base_addr = sram_req.bits.req.address.drop(sramAxisId)
            val incremented_addr = (base_addr.head + i.U) +: base_addr.tail

            val compressed_addr = coordOrder.zipWithIndex.foldLeft(incremented_addr) { case (acc, (oldPos, newPos)) =>
              acc.updated(newPos, incremented_addr(oldPos))
            }.take(rf_dim).map(_.asTypeOf(UInt(32.W))) // TODO magic number

            val expanded_addr: Vec[UInt] = VecInit(compressed_addr)
            val extended_addr = expanded_addr ++ Seq.fill(rfOut.coords.size - expanded_addr.size)(0.U)

            rfOut.valid := sram_req.fire && within_span
            when (!within_span) {
              rf_outs_found(i) := true.B
            }

            rfOut.coords := extended_addr.map(_.zext).reverse
            rfOut.coords.take(sram_.zeroedRegfileCoords).foreach(_ := 0.S)

            rfOut.pop.valid := (if (isWrite) {
              val is_innermost_axis_for_rf = sramAxisId == regFile2SRAMConnections.toSeq.collect {
                case RegFile2SRAMConn(`rf`, `sram_`, sramAxisId_, _, true, _) => sramAxisId_ // TODO for now, we're assuming that gathers never pop. But is that the best assumption?
              }.min
              is_innermost_axis_for_rf.B && sram_req.valid
            } else {
              val rfFeedsSpArray = var2RegFileConnections.exists {
                case (_: stellar.Input, `rf`, _, _, _, _) => true
                case _ => false
              }
              (!rfFeedsSpArray).B && sram_req.valid // TODO for now, we're assuming that gathers only pop if the rf doesn't feed an sp-array. But is that the best assumption?
            })
            if (sram_.name.contains("sparch_merged_sram")) {
              rfOut.pop.valid := sram_req.valid
              rfOut.pop.bits := 0.U
            } else {
              rfOut.pop.bits := DontCare; require(rfMod.nSubArrays == 1, s"Error connecting ${sram_.name}")
            }

            rfOut.op_count := (if (sram_.nBanks > 1) sram_req.bits.opCount else rfMod.io.emptyCount)

            when (debug_should_print.B && sram_req.valid) {
              printf(p"\trfOut_$i:\n")
              printf(p"\t\tbase_addr = ${VecInit(base_addr)}\n")
              printf(p"\t\tincremented_addr = ${VecInit(incremented_addr)}\n")
              printf(p"\t\tcompressed_addr = ${VecInit(compressed_addr)}\n")
              printf(p"\t\texpanded_addr = $expanded_addr\n")
              printf(p"\t\textended_addr = ${VecInit(extended_addr)}\n")
              printf(p"\t\tcoords = ${rfOut.coords}\n")
              printf(p"\t\tfound = ${rfOut.found} | unavailable = ${rfOut.unavailable}\n")
            }

            sram_resp.data(i) := (connType match {
              case DataConn => rfOut.data
              case CoordConn(coord) => rfOut.expanded_coords(coord).asTypeOf(rfOut.data)
              case _ => throw new Exception("unsupported connection type")
            })
            sram_resp.metadata.zipWithIndex.foreach { case (md, mdId) =>
              val axisId = sram_req.bits.axisId // TODO it doesn't really make sense to have the sram set an 'axisId' variable here when, as far as i can tell, it should just be equal to sramAxisId
              val coord = sram_req.bits.req.from_regfile_metadata(axisId)(mdId).coord
              md(i) := rfOut.expanded_coords(coord).asTypeOf(md(i))
            }
            sram_resp.found(i) := rfOut.found
            connectVecs(sram_resp.addrs, rfOut.expanded_coords.map(_.asUInt), fillIn = Some(0.U))

            // Connect SRAM's write-from-regfile req to coord-lookup modules
            coordLookups.foreach { coordLookup_ =>
              val connection = coordLookupsFromSramsOrVars((Left((sram_, sramAxisId)), coordLookup_))

              val is_innermost_axis_for_cl = sramAxisId == coordLookupsFromSramsOrVars.keys.collect {
                case (Left((`sram_`, sramAxisId_)), `coordLookup_`) => sramAxisId_
              }.min

              val coordLookup = coordLookup2Module(coordLookup_)
              val lookup = coordLookup.io.lookup(coordLookupLookupsConnected(coordLookup_))
              coordLookupLookupsConnected(coordLookup_) += 1
              lookup.valid := sram_req.fire && within_span
              lookup.last := sram_req.fire && sram_req.bits.last && is_innermost_axis_for_cl.B
              lookup.pop.valid := sram_req.valid && within_span && is_innermost_axis_for_cl.B
              lookup.pop.bits := rfOut.pop.bits
              lookup.op_count := rfOut.op_count

              connection.compressedCoords.foreach { case (coordLookupCompressedCoord, sramCoord, sramCoordIsCompressed) =>
                lookup.compressed(coordLookupCompressedCoord) := {
                  require(sramCoord >= sramAxisId, s"${sram_.name.getOrElse("Unnamed SRAM")} is trying to lookup from ${coordLookup_.name.getOrElse("an unnamed coord-loookup")} but sramCoord is $sramCoord and sramAxisId is $sramAxisId")
                  if (sramCoordIsCompressed) {
                    require(sramCoord - sramAxisId < compressed_addr.size, s"${sram_.name.getOrElse("Unnamed SRAM")} is trying to lookup from ${coordLookup_.name.getOrElse("an unnamed coord-loookup")} but sramCoord is $sramCoord, sramAxisId is $sramAxisId, and compressed_addr has ${compressed_addr.size} elements")
                    compressed_addr(sramCoord - sramAxisId).zext
                  }
                  else expanded_addr(sramCoord - sramAxisId).zext
                }
              }

              when (debug_should_print.B && sram_req.valid) {
                printf(p"\t\t${coordLookup_.name.getOrElse("unknown coordLookup")}.io.lookup[${coordLookupLookupsConnected(coordLookup_) - 1}].compressed = ${lookup.compressed}\n")
              }

              connection.expandedCoords.foreach { case (coordLookupExpandedCoord, sramExpandedCoord) =>
                when (debug_should_print.B && sram_req.valid) {
                  printf(p"\t\tUpdating expanded_addr[${sramExpandedCoord - sramAxisId}] with ${coordLookup_.name.getOrElse("unknown coordLookup")}.io.lookup[${coordLookupLookupsConnected(coordLookup_) - 1}].expanded[${coordLookupExpandedCoord}]\n")
                }
                expanded_addr(sramExpandedCoord - sramAxisId) := lookup.expanded(coordLookupExpandedCoord).asUInt
              }

              when (lookup.unavailable && connection.compressedCoords.forall(_._3).B /* TODO we should be able to enter this clause even when some of the 'compressedCoords' are actually expanded from other coord-lookups */) {
                when (debug_should_print.B && sram_req.valid) {
                  printf(p"\t\t${coordLookup_.name.getOrElse("unknown coordLookup")}'s lookup_${coordLookupLookupsConnected(coordLookup_) - 1} is unavailable\n")
                  printf(p"\t\t\tlookup = $lookup\n")
                }

                rf_outs_found(i) := true.B
                sram_resp.found(i) := false.B
              }.elsewhen(!lookup.found) {
                when (debug_should_print.B && sram_req.valid) {
                  printf(p"\t${coordLookup_.name.getOrElse("unknown coordLookup")}'s lookup_${coordLookupLookupsConnected(coordLookup_) - 1} is not found\n")
                  printf(p"\t\t\tlookup = $lookup\n")
                }

                sram_resp.found(i) := false.B

                when(coordLookup.io.emptyCount === coordLookup.io.fillCount) {
                  sram_req.ready := false.B
                  when (debug_should_print.B && sram_req.valid) {
                    printf(p"\t\t\temptyCount = ${coordLookup.io.emptyCount} | coordLookup fillCount = ${coordLookup.io.fillCount}\n")
                  }
                }
              }

              assert(coordLookup.io.emptyCount <= rfMod.io.emptyCount, "coordLookup is being emptied ahead of the RF")
            }
          }

          connectVecs(sram_resp.axis_spans, rf_outs.head.axis_spans)
          assert(!sram_req.fire || all(rf_outs.tail.map(rf_out => !rf_out.valid || rf_out.unavailable || all((rf_out.axis_spans zip rf_outs.head.axis_spans).map { case (x,y) => x same_as y }))),
            p"axis=${sramAxisId} | bank=${bankId} | write=${isWrite}\n\t${VecInit(rf_outs.map(_.axis_spans))}")

          assert(!sram_req.valid || all(sram_req.bits.req.spans.drop(sramAxisId + 1).map(_ === 1.U)) ||
            sram_req.bits.req.spans(sramAxisId) === 0.U && all(sram_req.bits.req.spans.drop(sramAxisId + 1).map(_ <= 1.U)),
            p"only the innermost dimension can span more than one element, currently | sram=${sram_.name} | axis=$sramAxisId | bank=$bankId\n\tspans = ${sram_req.bits.req.spans}")
        }

        if (isWrite) {
          // If the SRAM is multibanked, then we assume that last_out is true by default, but make it false if any of the banks have still not progressed far enough. For single-banked SRAMs, we assume it is false by default, but set it to true whenever any of the destination SRAMs specifies that it's making it's last regfile request
          if (dstIsMultiBanked) {
            when (sram_req.bits.opCount <= rfMod.io.emptyCount) {
              rfMod.io.last_out := false.B
            }
          } else {
            when (sram_req.fire && sram_req.bits.last) {
              rfMod.io.last_out := true.B
            }
          }
        }
      }
    }

    rfOutPortsConnected(rf) += dstSrams.map {
      case (dst, sramAxisId, _, true, _) => sram2Module(dst).io.write_from_regfile_resps(sramAxisId).flatMap(_.data).size
      case (dst, sramAxisId, _, false, _) => sram2Module(dst).io.read_from_regfile_resps(sramAxisId).flatMap(_.data).size
    }.max
  }

  // Fill coord-lookups from SRAMs
  sram2CoordLookupFills.foreach { case (sram_, coordLookup_, coordOrder) =>
    val sram = sram2Module(sram_)
    val coordLookup = coordLookup2Module(coordLookup_)

    val sram_resp = sram.io.read_resps.head.bits; require(sram_.nBanks == 1, "this code assumes just one bank")
    require(sram_resp.expanded_addresses.size == coordLookup.nInPorts)

    val span = sram_resp.spans.head

    val loadBalancerSnoopersOpt = loadBalancerForCl.get(coordLookup_).map(_.map { case (lb, index) =>
      val lbMod = loadBalancer2Module(lb)
      val (snoopers, snooperLast, snooperLastInAxis) = lbMod.getClInsForIndex(index)
      (snoopers, snooperLast, snooperLastInAxis, index, lbMod.indices, lb.spatialArray)
    })
    val last_in_axis = VecInit(sram_resp.last_in_axis.map(_ && sram.io.read_resps.head.fire && sram_resp.to_regfile)); require(sram_.nBanks == 1, "this code assumes just one bank")

    coordLookup.io.n_elems_in_axis_lookup.address := sram.io.n_elems_in_axis_lookups.head.address.take(coordLookup.io.n_elems_in_axis_lookup.address.size)
    sram.io.n_elems_in_axis_lookups.head.nElems := coordLookup.io.n_elems_in_axis_lookup.nElems
    require(coordOrder.isEmpty || sram_.maxElemsInRf.isEmpty, "we don't yet support max-elems-in-rfs when the coordOrder is non-default")
    require(sram_.maxElemsInRf.isEmpty || sram.io.n_elems_in_axis_lookups.size == 1, "we don't yet support coordLookups with multiple n_elems_in_axis_lookup ports")

    coordLookup.io.insert.zip(sram_resp.expanded_addresses).zipWithIndex.foreach { case ((insert, unordered_expanded_addr), i) =>
      val unordered_compressed_addr = (sram_resp.compressed_address.head +& i.U) +: sram_resp.compressed_address.tail

      val (compressed_addr, expanded_addr) = coordOrder.zipWithIndex.foldLeft((unordered_compressed_addr: Seq[UInt], unordered_expanded_addr: Seq[UInt])) { case ((acc1, acc2), (oldPos, newPos)) =>
        (acc1.updated(newPos, unordered_compressed_addr(oldPos)),
          acc2.updated(newPos, unordered_expanded_addr(oldPos)))
      }
      require(coordOrder.isEmpty || insert.bits.mapping.compressed.size == insert.bits.mapping.expanded.size, "I'm not sure how to interpret the coordOrder if the compressed and expanded addrs are different lengths")

      val within_span = i.U < span
      val sram_read_resp_valid = sram.io.read_resps.head.valid && sram_resp.to_regfile && within_span; require(sram_.nBanks == 1, "this code assumes just one bank")

      insert.valid := sram_read_resp_valid && sram.io.read_resps.head.fire; require(sram_.nBanks == 1, "this code assumes just one bank")
      insert.bits.mapping.compressed := compressed_addr.map(_.zext).take(insert.bits.mapping.compressed.size)
      insert.bits.mapping.expanded := expanded_addr.map(_.zext).take(insert.bits.mapping.expanded.size)
      insert.bits.op_count := coordLookup.io.fillCount
      insert.bits.last := sram.io.read_resps.head.fire && sram_resp.to_regfile && sram_resp.to_regfile_last; require(sram_.nBanks == 1, "this code assumes just one bank") // "Last" signals change state even if the "valid" signal isn't set, so we can only set it when the SRAM read resp is actually firing
      insert.bits.last_in_axis := (if (i == 0) sram_resp.last_in_axis.take(insert.bits.last_in_axis.size).map(_ && sram.io.read_resps.head.fire && sram_resp.to_regfile)
        else Seq.fill(insert.bits.last_in_axis.size)(false.B)) // To reduce the size of the generated Verilog, we only hook up the last-in-axis signals of the very first "insert", since they should all have the same "last-in-axis" signals anyhow
      require(sram_.nBanks == 1, "this code assumes just one bank")

      when (sram_resp.to_regfile && within_span && !insert.ready) {
        get_sram_read_resp_readies(sramCodes(sram_)).head := false.B; require(sram_.nBanks == 1, "this code assumes just one bank")
      }

      loadBalancerSnoopersOpt.foreach(_.foreach { case (snoopers, _, _, index, indices, spatialArray) =>
        val CoordLookupFromSpatialArrayIndex(indicesToInputIntoCoordLookup, Seq((`index`, indexId))) = coordLookupsFromSpatialArrayIndices((coordLookup_, spatialArray))

        val snooper = snoopers(i)

        snooper.valid := sram_read_resp_valid // Note: we use "sram_read_resp_valid" rather than "insert.fire" here to avoid a combinational loop. However, this can cause multiple spurious writes into the load-balancer

        snooper.compressed.foreach(_ := 0.S)
        indicesToInputIntoCoordLookup.zip(insert.bits.mapping.compressed).foreach { case ((ind, false), clCompressed) =>
          snooper.compressed(indices.indexOf(ind)) := clCompressed

          case _ => throw new Exception("UNREACHABLE")
        }

        snooper.expanded.foreach(_ := 0.S)
        snooper.expanded(indices.indexOf(index)) := insert.bits.mapping.expanded(indexId)

        when (!snooper.ready.get) {
          insert.valid := false.B
          when (sram_resp.to_regfile && within_span) {
            get_sram_read_resp_readies(sramCodes(sram_)).head := false.B; require(sram_.nBanks == 1, "this code assumes just one bank")
          }
          insert.bits.last := false.B
          last_in_axis.head := false.B; require(coordLookup_.lastInAxisFlags.size <= 1, s"load balancer currently only support one-dim last axis flags: ${coordLookup_.lastInAxisFlags.size}")
        }

        snooper.op_count := insert.bits.op_count
      })
    }

    loadBalancerSnoopersOpt.foreach(_.foreach { case (_, snooperLast, snooperLastInAxis, _, _, _) =>
      snooperLast := any(coordLookup.io.insert.map(_.bits.last))
      snooperLastInAxis := last_in_axis.head; assert(coordLookup_.lastInAxisFlags.size <= 1, s"load balancer currently only support one-dim last axis flags: ${coordLookup_.lastInAxisFlags.size}")
    })
  }

  // Fill coord-lookups from spatial array outputs
  var2CoordLookupFills.foreach { case Var2CoordLookupFill(outVar, coordLookup_, compressedCoords) =>
    val coordLookup = coordLookup2Module(coordLookup_)

    val spatialArray_ = {
      val gens = spatialArrayGens.filter(spArray => compressedCoords.forall(spArray.indices.contains))
      assert(gens.size == 1, s"There are ${gens.size} possible spatial arrays which you might want to tie to the coord lookup")
      gens.head
    }
    val spatialArray = spatialArray2Module(spatialArray_)

    val spatialArrayOutPorts = spatialArray.inner.getOutPortsForVar(outVar, spatialArray.io.outs.toSeq)

    val spatialArrayOutLast = {
      val lasts = spatialArray.inner.getLastPortsForVar(outVar, spatialArray.io.last_in_or_out)

      if (withAsserts)
        for ((last, i) <- lasts.zipWithIndex) {
          for (last2 <- lasts.drop(i+1)) {
            when (last.valid && last2.valid) {
              assert(last.bits === last2.bits, "trying to assert 'last' on two separate reg-file buffers at once. we don't support this yet")
            }
          }
        }

      lasts.map(_.valid).reduce(_ || _)
    }

    coordLookup.io.insert.zip(spatialArrayOutPorts).foreach { case (insert, outPort) =>
      insert.valid := outPort.valid

      insert.bits.mapping.compressed := compressedCoords.map { ind =>
        outPort.bits.element.domainCoords(spatialArray_.indices.indexOf(ind))
      }

      val expandedCoords = outPort.bits.coords.takeRight(insert.bits.mapping.expanded.size)
      val expandedIsValid = outPort.bits.coords.dropRight(insert.bits.mapping.expanded.size).map(_ === 0.S).fold(true.B)(_ && _)
      val invalidExpanded = insert.bits.mapping.expanded.map(_ => (~0.U(32.W)).asSInt) // TODO i'm just setting really unlikely numbers here, but we should choose these numbers more formally
      insert.bits.mapping.expanded := Mux(expandedIsValid, VecInit(expandedCoords), VecInit(invalidExpanded))

      insert.bits.op_count := outPort.bits.op_count

      insert.bits.last := spatialArrayOutLast
      insert.bits.last_in_axis := outPort.bits.last_in_axis.take(insert.bits.last_in_axis.size)

      when (!insert.ready) {
        outPort.ready := false.B
        assert(!outPort.valid, "spatial array is still trying to output something when it should be stalling")
      }
    }

    require(!loadBalancerForCl.contains(coordLookup_), "we don't yet support snooping coord-lookups which are filled by spatial arrays")
  }

  // Connect compressed2expandedMappings in spatial arrays
  spatialArrays.foreach { spatialArray =>
    spatialArray.io.compressed2ExpandedMappings.foreach { mapping =>
      mapping.expanded := DontCare
      mapping.found.foreach(_ := true.B)
      mapping.unavailable.foreach(_ := false.B)
    }
  }

  coordLookupsFromSpatialArrayIndices.toSeq.foreach {
    case ((coordLookup_, spatialArrayGen), CoordLookupFromSpatialArrayIndex(indicesToInputIntoCoordLookup, indicesToInputIntoSpatialArray)) =>
      val spatialArray = spatialArray2Module(spatialArrayGen)
      val coordLookup = coordLookup2Module(coordLookup_)

      val start = coordLookupLookupsConnected(coordLookup_)

      val spatialArrayMappings = spatialArray.io.compressed2ExpandedMappings.filter(coordLookupIsNeededForC2EMapping(_, indicesToInputIntoSpatialArray.map(_._1).toSet))

      assert(start < coordLookup.io.lookup.size &&
        coordLookup.io.lookup.size - start >= spatialArrayMappings.size,
        s"you need ${spatialArrayMappings.size - coordLookup.io.lookup.size + start} more lookup ports for ${coordLookup.name}\n\tstart=${start} | coordLookup.io.lookup.size=${coordLookup.io.lookup.size} | spatialArrayMappings.size=${spatialArrayMappings.size}")

      coordLookup.io.lookup.drop(start).zip(spatialArrayMappings).foreach { case (lookup, spatialArrayMapping) =>
        lookup.valid := spatialArrayMapping.valid

        lookup.pop.valid := false.B
        lookup.pop.bits := DontCare

        lookup.op_count := spatialArrayMapping.op_count

        spatialArrayMapping.found.foreach { found =>
          when (!lookup.found && !lookup.unavailable) {
            found := false.B
          }
        }

        spatialArrayMapping.unavailable.foreach { unavailable =>
          when (lookup.unavailable) {
            unavailable := true.B
          }
        }

        lookup.compressed.zip(indicesToInputIntoCoordLookup).foreach { case (l, (ind, isExpanded)) =>
          val coords = if (isExpanded) spatialArrayMapping.expanded else spatialArrayMapping.compressed
          l := coords(spatialArrayGen.indices.indexOf(ind))
        }

        indicesToInputIntoSpatialArray.foreach { case (index, i) =>
          val mappingExpanded = spatialArrayMapping.expanded(spatialArrayGen.indices.indexOf(index))
          mappingExpanded := Mux(lookup.found, lookup.expanded(i), maxVal(mappingExpanded))
        }

        if (!coordLookupEmpties.keys.toSeq.contains(coordLookup_)) {
          lookup.last := false.B
        }
      }

      coordLookupLookupsConnected(coordLookup_) += spatialArrayMappings.size
  }

  // Empty coord-lookups based on spatial array variables
  coordLookupEmpties.toSeq.foreach { case (coordLookup_, variable) =>
    val last_ports = {
      val spatialArray_ = spatialArrayGens.find(_.variables.contains(variable)).get
      val spatialArray = spatialArray2Module(spatialArray_)
      spatialArray.inner.getLastPortsForVar(variable, spatialArray.io.last_in_or_out)
    }

    if (withAsserts)
      for ((last, i) <- last_ports.zipWithIndex) {
        for (last2 <- last_ports.drop(i+1)) {
          when (last.valid && last2.valid) {
            assert(last.bits === last2.bits, "trying to assert 'last' on two separate coord-lookup buffers at once. we don't support this yet")
          }
        }
      }

    val last = last_ports.map(_.valid).reduce(_ || _)

    val coordLookup = coordLookup2Module(coordLookup_)
    coordLookup.io.lookup.foreach(_.last := last)
  }

  // Connect coordLookup fill and empty counters to spatial arrays
  spatialArray2Module.toSeq.foreach { case (spatialArray_, spatialArray) =>
    val clFillCounters = {
      coordLookupsFromSramsOrVars.toSeq.collect {
        case ((Right(v), cl), _) if spatialArray_.variables.contains(v) => coordLookup2Module(cl).io.fillCount + !coordLookup2Module(cl).io.isEmpty
      } ++
        coordLookupsFromSpatialArrayIndices.toSeq.collect {
          case ((cl, `spatialArray_`), _) => coordLookup2Module(cl).io.fillCount + !coordLookup2Module(cl).io.isEmpty
        }
    }

    spatialArray.io.clFillCounter.valid := clFillCounters.nonEmpty.B
    if (clFillCounters.isEmpty)
      spatialArray.io.clFillCounter.bits := DontCare
    else
      spatialArray.io.clFillCounter.bits := minOf(clFillCounters:_*)
  }

  // Connect load-balancing mappings to spatial arrays
  loadBalancers.toSeq.foreach { loadBalancer =>
    val loadBalancerMod = loadBalancer2Module(loadBalancer)
    val spatialArrayMod = spatialArray2Module(loadBalancer.spatialArray)

    spatialArrayMod.io.loadBalancingMappings <> loadBalancerMod.io.mappings
    loadBalancerMod.io.last_out := spatialArrayMod.io.ending
  }

  // Connect load-balancers to reg-file updates
  loadBalancers.toSeq.foreach { loadBalancer =>
    val loadBalancerMod = loadBalancer2Module(loadBalancer)

    loadBalancerMod.inVars.foreach { inVar =>
      val rf = var2RegFileConnections.collectFirst { case (`inVar`, rf, _, _, _, _) => regFile2Module(rf) }.get
      rf.io.updates <> loadBalancerMod.getRfUpdatesForInVar(inVar)
    }
  }

  // Connect load-balancers to coord-lookup updates
  loadBalancers.toSeq.foreach { loadBalancer =>
    val loadBalancerMod = loadBalancer2Module(loadBalancer)

    loadBalancerMod.indices.filter(_.isSkipped).foreach { index =>
      val Some((cl, indicesToInputIntoCoordLookup)) = coordLookupsFromSpatialArrayIndices.toSeq.collectFirst { case ((coordLookup, loadBalancer.spatialArray), CoordLookupFromSpatialArrayIndex(indicesToInputIntoCoordLookup, Seq((`index`, _)))) =>
        assert(indicesToInputIntoCoordLookup.map(_._2).forall(!_), "not sure what to do with expanded coords here")
        (coordLookup2Module(coordLookup), indicesToInputIntoCoordLookup.map(_._1))
      }

      cl.io.updates.zip(loadBalancerMod.getClUpdatesForIndex(index)).foreach { case (clUpdate, lbUpdate) =>
        clUpdate.valid := lbUpdate.valid

        clUpdate.bits.from.op_count := lbUpdate.bits.from.op_count
        clUpdate.bits.from.coords := indicesToInputIntoCoordLookup.map { ind =>
          lbUpdate.bits.from.coords(loadBalancerMod.indices.indexOf(ind))
        }

        clUpdate.bits.to.op_count := lbUpdate.bits.to.op_count
        clUpdate.bits.to.coords := indicesToInputIntoCoordLookup.map { ind =>
          lbUpdate.bits.to.coords(loadBalancerMod.indices.indexOf(ind))
        }
      }
    }
  }

  // Connect load-balancers to outer IO
  loadBalancersSorted.zip(io.loadBalancerConfigs).foreach { case (lb, outerIoLbConfig) =>
    lb.io.config <> outerIoLbConfig
  }
}
