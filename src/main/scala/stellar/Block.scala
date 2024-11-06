package stellar

import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._

import stellar.rtl.ChiselConverter
import stellar.Util.SMap

class Block(val upperBound: Int = 2, val boundsListOverride: Option[Seq[Int]] = None) {
  private val assignments = new ListBuffer[Assignment]
  private[stellar] val indices = new ListBuffer[Index]
  private val skips = new ListBuffer[Skip]
  private val maps = new ListBuffer[Map]
  var transform: Option[Transform] = None

  private var indicesFrozen = false
  private var assignmentsFrozen = false
  private var skipsFrozen = false
  private var expandedIndices = false

  var timeStepsOpt: Option[Seq[Int]] = None
  var allIOPortsOpt: Option[SMap[Variable, Int]] = None
  var rangeSpaceItsOpt: Option[IterationSpace] = None

  def registerAssignment(a: Assignment): Unit = {
    assert(!assignmentsFrozen, "You can't add new assignments after unrolling all indices")
    assignments += a
  }

  def registerIndex(id: Index): Unit = {
    assert(!indicesFrozen, s"You must declare this index earlier: $id")
    indices += id
  }

  def registerTransform(tr: Transform): Unit = {
    assert(transform.isEmpty, "Only one transform can be set for a block")
    transform = Some(tr)
  }

  def registerSkip(sk: Skip): Unit = {
    assert(!skipsFrozen, "Can't skip any more indices")
    skips += sk
  }

  def registerMap(m: Map): Unit = {
    maps += m
  }

  def ioVariables: Seq[Variable] = {
    assignments.flatMap { asgn =>
      val indexeds = Passes.extractIndexed(asgn.src) ++ Passes.extractIndexed(asgn.dst)
      indexeds.filter(!_.isIntermediate).map(_.variable)
    }.toSeq
  }

  def interVariables: Seq[Intermediate] = {
    assignments.flatMap { asgn =>
      val indexeds = Passes.extractIndexed(asgn.src) ++ Passes.extractIndexed(asgn.dst)
      indexeds.collect { case Indexed(inter: Intermediate, _) => inter }
    }.toSeq
  }

  def expandIndices(): Unit = {
    if (expandedIndices) {
      return
    }

    indicesFrozen = true
    assignmentsFrozen = true
    skipsFrozen = true
    expandedIndices = true

    if (skips.nonEmpty) {
      val expandedAssignments = Passes.expandCompressedIndices(assignments.toSeq, indices.toSeq)

      assignments.clear()
      assignments ++= expandedAssignments
    }
  }

  def unrollIndices(): Unit = {
    indicesFrozen = true
    assignmentsFrozen = true

    val (unrolledAssignments, unrolledIndices) = Passes.unrolledTiledIndices(assignments, indices.toList)

    assignments.clear()
    assignments ++= unrolledAssignments

    indices.clear()
    indices ++= unrolledIndices
  }

  def dimensions: Int = {
    indicesFrozen = true
    indices.size
  }

  val lowerBound = -1
  def boundsList = boundsListOverride match {
    case Some(bs) => bs
    case None => Seq.fill(indices.size)(upperBound)
  }
  def boundsMap = indices.zip(boundsList).map { case (i, b) => i -> (lowerBound, b) }.toMap

  def spatiallyBoundIterators: Seq[Index] = {
    assert(transform.isDefined, "Transform must be defined to know what the spatially bound iterators are")
    val tr = transform.get
    (indices zip tr.spatiallyBoundIterators).collect { case (id, sp) if sp => id }.toSeq
  }

  def timeBoundIterators: Iterable[Iterable[Index]] = {
    // Returns the time-bound iterators for each time-axis
    assert(transform.isDefined, "Transform must be defined to know what the time-bound iterators are")
    transform.get.timeTr.map { tr =>
      indices.zip(tr).filter(_._2 != 0).map(_._1).toSet
    }
  }

  def totalTimeSteps: Expr = Passes({
    // In this function, we assume that each time-axis iterates from 0 to an upper-bound, for each iteration of its'
    // outer-axes
    assert(transform.isDefined, "Transform must be set before time steps can be calculated")
    val maxTimesPerAxis = transform.get.timeTr.map { timeTr =>
      val multiplied: Seq[Expr] = (indices zip timeTr).map { case (id, t) => Multiply(id.upperBound, Const(t)) }.toSeq
      multiplied.reduce(_ + _)
    }
    maxTimesPerAxis.foldLeft[Expr](Const(1))(_ * _)
  })

  def domainConnectionVectors: Iterable[ConnectionVector] = {
    assignments.flatMap(_.connectionVectors(indices.toSeq, boundsMap))
  }

  def denseConnectionVectors: Iterable[ConnectionVector] = {
    assignments.flatMap(_.connectionVectors(indices.toSeq, boundsMap)).map(_.transform(transform.get.tr))
  }

  def denseSpaceConnectionVectors: Iterable[ConnectionVector] = {
    assignments.flatMap(_.connectionVectors(indices.toSeq, boundsMap)).map(_.transform(transform.get.spaceTr))
  }

  def sparseDomainConnectionVectors: Iterable[(Indexed, Variable, Iterable[Expr])] = {
    // Return format: Seq[(src, dst, Seq[connV])]

    val base = indices.toSeq
    val connected = domainConnectionVectors.map { case ConnectionVector(src, dst, diff, _) =>
      val conned = diff.zip(indices).map {case (d, ind) => Add(ind, Const(d))}
      (src, dst, conned)
    }

    def makeSkipped(skipped: Seq[Expr]): Seq[Expr] = {
      // This is basically a function which takes iterator variables like i', and transforms them into their i form.
      // Assumptions: We use the indices in order of their declaration

      indices.zipWithIndex.map { case (id, i) =>
        if (!id.isSkipped) {
          skipped(i)
        } else {
          val deps = id.dependencies
          val depExprs = indices.zipWithIndex.filter(ind => deps.contains(ind._1)).map(_._2).map(j => skipped(j)).toSeq

          IndexSkipFunc(id, skipped(i), depExprs, id.isSyncSkipped)
        }
      }.toSeq
    }

    val sBase = makeSkipped(base)
    val sConnected = connected.map { case (src, dst, conned) => (src, dst, makeSkipped(conned.toSeq)) }

    sConnected.map { case (src, dst, conned) =>
      val diff = conned.zip(sBase).map(v => Passes(v._1 - v._2))
      (src, dst, diff)
    }
  }

  def brokenDomainConnectionVectors: Iterable[ConnectionVector] = {
    sparseDomainConnectionVectors.zip(domainConnectionVectors).collect {
      case ((_, _, scv), dcv) if !scv.forall(e => e.isInstanceOf[Const] || e.isInstanceOf[IndexSkipFunc]) =>
        dcv
    }
  }

  def brokenOptimisticDomainConnectionVectors: Iterable[(ConnectionVector, Int)] = {
    def exprIsSync(expr: Expr, outermost: Boolean): Boolean = expr match {
      case IndexSkipFunc(_, _, _, isSync) => isSync || outermost
      case _: Const => true
      case a: Add => a.ops.forall(o => exprIsSync(o, outermost=false))
      case _ => false
    }

    val bdcvs = brokenDomainConnectionVectors.toSeq

    sparseDomainConnectionVectors.zip(domainConnectionVectors).collect {
      case ((_, _, scv), dcv) if bdcvs.contains(dcv) &&
        scv.forall(e => exprIsSync(e, outermost=true)) =>

        val optimisticSkipIndices = scv.zipWithIndex.collect { case (e,i) if !e.isInstanceOf[Const] && !e.isInstanceOf[IndexSkipFunc] =>  i }
        assert(optimisticSkipIndices.size == 1, "i haven't really thought about what to do if MULTIPLE indexes need to be optimistic-skipped")

        (dcv, optimisticSkipIndices.head)
    }
  }

  def brokenNonSyncDomainConnectionVectors: Iterable[ConnectionVector] = {
    val bodcvs = brokenOptimisticDomainConnectionVectors.map(_._1)
    brokenDomainConnectionVectors.filter(bcv => !bodcvs.toSeq.contains(bcv)).toSet
  }

  def expandIndicesFromRfInputs(): Unit = {
    val expandedIndexCarriers = Passes.canExpandIndicesFromRfInputs(skips.toSeq, assignments.toSeq)

    expandedIndexCarriers.foreach { case (skip, interVar, indexed, i) =>
      val indexCarrierInterVar = new Intermediate(name = s"${interVar.name}_carrier_${indexed.variable}_coord_${i}")(this)
      val assignmentsWithIndexCarriers = ListBuffer.empty[Assignment]

      assignments.foreach {
        case asgn @ Assignment(Indexed(`interVar`, asgnIndices), `indexed`) =>
          assignmentsWithIndexCarriers += asgn
          assignmentsWithIndexCarriers += Assignment(Indexed(indexCarrierInterVar, asgnIndices), CoordOf(indexed,i))(None)

        case asgn @ Assignment(Indexed(`interVar`, asgnIndices), src) =>
          assignmentsWithIndexCarriers += asgn
          assignmentsWithIndexCarriers += Assignment(Indexed(indexCarrierInterVar, asgnIndices), Passes.replaceVariable(src, Seq(interVar -> indexCarrierInterVar).toMap))(None)

        case asgn @ Assignment(Indexed(outVar: Output, asgnIndices), src) =>
          assignmentsWithIndexCarriers += Assignment(Indexed(outVar, asgnIndices.map(ind => Passes.replaceExprs(ind, Seq((skip.index, Indexed(indexCarrierInterVar, indices.toSeq))).toMap, terminateAtIntermediate=true))), src)(None)

        case asgn => assignmentsWithIndexCarriers += asgn
      }

      assignments.clear()
      assignments ++= assignmentsWithIndexCarriers

      skips.remove(skips.indexOf(skip))
    }
  }

  def unrollMaps(): Unit = {
    def unrollRanges(): Unit = {
      var anyUnrolled = false

      val new_maps = maps.flatMap { map =>
        val unrolled = (map.src zip map.dst).zipWithIndex.collect {
          case ((indScalar, indRange: IndexRange), indLocation) if !indScalar.isInstanceOf[IndexRange] =>
            Passes(Passes.replaceBounds(indRange, boundsMap)) match {
              case IndexRange(Some(Const(start)), Some(Const(end))) =>
                (start until end).map { i =>
                  map.copy(_dst = map.dst.updated(indLocation, Const(i)), _fromSweep = true)
                }
            }

          case ((indRange: IndexRange, indScalar), indLocation) if !indScalar.isInstanceOf[IndexRange] =>
            Passes(Passes.replaceBounds(indRange, boundsMap)) match {
              case IndexRange(Some(Const(start)), Some(Const(end))) =>
                (start until end).map { i =>
                  map.copy(_src = map.src.updated(indLocation, Const(i)), _fromSweep = true)
                }
            }
        }.flatten

        if (unrolled.isEmpty)
          Seq(map)
        else {
          anyUnrolled = true
          unrolled
        }
      }

      if (anyUnrolled) {
        maps.clear()
        maps ++= new_maps

        unrollRanges()
      }
    }

    def runSweeps(): Unit = {
      val new_maps = maps.flatMap { map =>
        map match {
          case Map(src, dst) =>
            (src zip dst).foreach { case (se, de) =>
              if (se.indicesInExpr != de.indicesInExpr) {
                assert(se.indicesInExpr.size == 1 && de.indicesInExpr.isEmpty || se.indicesInExpr.isEmpty && de.indicesInExpr.size == 1)
                val index = (se.indicesInExpr ++ de.indicesInExpr).head
                map sweep(index)
              }
            }
        }

        if (map.sweepCmds.isEmpty) {
          Seq(map)
        } else {
          val sweepCmd = map.sweepCmds.head
          map.sweepCmds.remove(0)
          sweepCmd match {
            case SweepIndex(index, step, maxOpt) =>
              assert(step > 0)

              val (lb, _ub) = boundsMap(index)
              val indexLocation = indices.indexOf(index)
              val ub = maxOpt.map(_ min _ub).getOrElse(_ub)

              (lb+1 until ub by step).map { i =>
                val new_src = map.src.updated(indexLocation, Passes(Passes.replaceExprs(map.src(indexLocation), Seq((index, index.as(i))).toMap)))
                val new_dst = map.dst.updated(indexLocation, Passes(Passes.replaceExprs(map.dst(indexLocation), Seq((index, index.as(i))).toMap)))
                map.copy(_src = new_src, _dst = new_dst, _fromSweep = true)
              }

            case StepIndex(index, step) =>
              assert(step > 0)

              val (lb, ub) = boundsMap(index)
              val indexLocation = indices.indexOf(index)

              (lb+1 until ub by step).map { i =>
                val new_src = map.src.updated(indexLocation, Passes(Passes.replaceExprs(map.src(indexLocation), Seq((index, index + i)).toMap)))
                map.copy(_src = new_src, _fromSweep = true)
              }
          }
        }
      }

      maps.clear()
      maps ++= new_maps

      if (maps.exists(_.sweepCmds.nonEmpty))
        runSweeps()
    }

    unrollRanges()
    runSweeps()
    assert(maps.forall(_.indicesMatch))

    val uniqueMappings = maps.distinctBy(m => m.src ++ m.dst)
    maps.clear()
    maps ++= uniqueMappings
  }

  var shouldExpandIndicesFromRf: Boolean = true

  def elaborate(shouldPrint: Boolean, shouldRender: Boolean, shouldUnroll: Boolean, emitVerilog: Boolean) = {
    unrollMaps()

    if (shouldExpandIndicesFromRf)
      expandIndicesFromRfInputs()

    expandIndices()

    if (shouldUnroll)
      unrollIndices()

    transform.foreach(tr => assert(tr.size == indices.size, "Transform must have enough elements to be 1-to-1"))

    var result = "Indices: " + indices.mkString(",")
    result += "\n\n"

    result += assignments.mkString("\n")
    result += "\n\n"

    if (transform.isDefined) {
      val tr = transform.get

      result += "Transform = " + tr.toString
      result += "\n\n"

      result += "Inverse transform = [\n" + tr.inverse.map("\t" + _.mkString(",")).mkString("\n") + "\n]"
      result += "\n\n"

      result += "Total time steps: " + totalTimeSteps.toString
      result += "\n\n"

      result += "Spatially bound iterators: " + spatiallyBoundIterators.mkString(",")
      result += "\n\n"
    } else {
      result += "Transform has not been set yet"
    }
    result += "\n\n"

    result += s"Dense connection vectors: ${denseConnectionVectors.map(_.toString).mkString(", ")}"
    result += "\n\n"

    if (skips.nonEmpty) {
      result += skips.mkString("\n")
      result += "\n\n"
    }

    indices.foreach { id =>
      if (id.isSkipped) {
        val deps = id.dependencies + id
        val depsStr = deps.map(d => s"$d${if (d.isSkipped) "'" else ""}").mkString(",")
        result += s"$id is skipped and depends on {$depsStr}\n"
      }
    }
    result += "\n"

    if (skips.nonEmpty) {
      result += s"Sparse connection vectors:\n"
      sparseDomainConnectionVectors.foreach { spv =>
        result += s"\t$spv\n"
      }
      result += "\n"
    }

    val bcv = brokenDomainConnectionVectors
    if (bcv.nonEmpty) {
      result += s"Broken connection vectors: $bcv"
      result += "\n\n"
    }

    for (m <- maps) {
      result += s"$m\n"

      val domainVectors = m.domainVectors(indices.toSeq)
      result += s"  Domain vectors:\n${domainVectors.map("\t(" + _.mkString(", ") + ")").mkString("\n")}"
      result += "\n"

      val rangeVectors = m.rangeVectors(indices.toSeq, transform.get)
      result += s"  Range vectors:\n${rangeVectors.map("\t(" + _.mkString(", ") + ")").mkString("\n")}"
      result += "\n"
    }

    val domainIts = {
      var result = IterationSpace.fromBoundsAndAssignmentsAndSparsityAndMappings(
        bounds = boundsList,
        indices = indices.toSeq,
        assignments = assignments,
        brokenNonSyncConnectionVectors = brokenNonSyncDomainConnectionVectors.toSet,
        // brokenOptimisticConnVectors = brokenOptimisticDomainConnectionVectors,
        brokenOptimisticConnVectors = Set(),
        mappings = maps,
      )

      val passThruVars = Passes.passThruVariables(assignments)
      result = IterationSpace.pruneBasedOnSkips(result, indices, skips, passThruVars)
      result = result.filterPoints(!_.coords.contains(lowerBound), passThruVars)
      result = result.withBrokenOptimisticConnVectors(brokenOptimisticDomainConnectionVectors.toSet)

      result
    }

    result += s"\nPoints within domain iteration space if all indices go from 0 to $upperBound: ${domainIts.size}\n"

    var spatialArrayGen: Option[(Boolean, Boolean, Boolean, Boolean, Boolean, Int, Boolean, Boolean) => stellar.rtl.ChiselSpatialArray] = None

    if (transform.nonEmpty) {
      val rangeIts = domainIts.transform(transform.get.tr)
      assert(!rangeIts.hasFutureConnections, "Spatial array has future connections, which the laws of physics do not yet permit")

      val rangeSpaceIts = rangeIts.withoutTime(transform.get.nTimeAxes)
      rangeSpaceItsOpt = Some(rangeSpaceIts)

      val ioPorts = IterationSpace.ioPorts(rangeSpaceIts)
      allIOPortsOpt = Some({
        ioPorts.toSeq.flatMap(_._2.toSeq).groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
      })

      val pointTimeSpans = {
        var result = IterationSpace.timeSpanForPoints(rangeIts, transform.get.nTimeAxes)
        assert(!spatiallyBoundIterators.exists(_.isUnbounded), "we don't yet handle unbounding spatially-bound iterators")

        val unboundIndices = indices.filter(_.isUnbounded)
        timeBoundIterators.zipWithIndex.foreach { case (timeBoundIts, timeAxis) =>
          val infiniteTime = unboundIndices.toSet.intersect(timeBoundIts.toSet).nonEmpty
          if (infiniteTime) {
            result = result.view.mapValues { case (start, end) =>
              (start, end.updated(timeAxis, ((BigInt(1) << 31)-1).toInt)) // TODO magic number
            }.toMap
          }
        }

        result
      }

      timeStepsOpt = Some(rangeSpaceIts.ioConns.map(_.time).maxBy(_.reverse))

      result += s"Number of PEs if all indices go from 0 to $upperBound: ${rangeSpaceIts.size}\n"

      result += s"Number of intra-array connections: ${rangeSpaceIts.p2pConns.size}\n"
      result += s"Number of IO ports: ${ioPorts.values.flatMap(_.values).sum}\n"

      if (shouldRender) {
        assert(rangeSpaceIts.dim <= 3, "too many dimensions for us to render (for now)")
        Render(rangeSpaceIts, ioPorts)
      }

      // Generate Verilog
      val canEndEarly = interVariables.exists(_.signalsEndingInnermostTimeAxis)
      val maxTimePerAxis = {
        // TODO When calculating 'maxTimePerAxis', we assume a 'cuboid' type of time-iteration-space, rather than a
        //  parallelogram type of iteration-space. For example, we assume that our iterations go like (0,0) -> (0,1) ->
        //  (1,0) -> (1,1), rather than like (0,0) -> (0,1) -> (1,1) -> (1,2).
        assert(transform.get.nTimeAxes <= 1 || transform.get.timeTr.forall(_.count(_ != 0) <= 1), "for now, we require a 'cuboid' type of time-iteration-space, and i'm not sure if this transform gives us a cuboid-style time-space or not")
        pointTimeSpans.values.map(_._2).toSeq.transpose.map(_.max)
      }
      val genPEs = ChiselConverter.allPEs(rangeSpaceIts, indices.toSeq, boundsList, pointTimeSpans, maxTimePerAxis, transform.get, canEndEarly)
      spatialArrayGen = Some((ignoreBaseC2E: Boolean, alwaysStart: Boolean, stallIfAnyInputsAreNotFound: Boolean, onlyCheckIfOutputsAreReady: Boolean, stallForOutputs: Boolean, dataWidthBits: Int, withAsserts: Boolean, hasSubArrays: Boolean) => ChiselConverter.spatialArrayModule(genPEs=genPEs, its=rangeSpaceIts, transform=transform.get, maxTimePerAxis=maxTimePerAxis, alwaysStart=alwaysStart, stallIfAnyInputsAreFound=stallIfAnyInputsAreNotFound, onlyCheckIfOutputsAreReady=onlyCheckIfOutputsAreReady, stallForOutputs=stallForOutputs, ignoreBaseC2E=ignoreBaseC2E, dataWidthBits=dataWidthBits, withAsserts=withAsserts, hasSubArrays=hasSubArrays))
      if (emitVerilog) {
        ChiselConverter.emitSpatialArrayVerilog(genPEs, rangeSpaceIts, transform.get, maxTimePerAxis)
      }
    }

    if (shouldPrint)
      println(result)

    (result, spatialArrayGen)
  }

  def genSpatialArray = elaborate(
    shouldPrint = true,
    shouldRender = false,
    shouldUnroll = true,
    emitVerilog = false,
  )._2.get

  override def toString: String = elaborate(
    shouldPrint = false,
    shouldRender = false,
    shouldUnroll = true,
    emitVerilog = false,
  )._1
}
