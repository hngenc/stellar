package stellar

import scala.collection.mutable.{Set => MutSet}
import scala.math.Ordering.Implicits._

import MatrixUtil.subv
import Util._

case class Point(coords: Seq[Int]) {
  def dim: Int = coords.size

  def transform(tr: Seq[Seq[Int]]) = Point(MatrixUtil.matvec(tr, coords).toSeq)
  def +(diff: Iterable[Int]) = Point(MatrixUtil.addv(coords, diff.toSeq))

  def time(nTimeAxes: Int): Seq[Int] = coords.takeRight(nTimeAxes)
  def withoutTime(nTimeAxes: Int) = Point(coords.dropRight(nTimeAxes))

  override def toString: String = "(" + coords.mkString(",") + ")"
}

case class PointAssignment(point: Point, originalPoint: Point, assignment: Assignment, priority: Int, mapping: Option[PointsMapping]) {
  def transform(tr: Seq[Seq[Int]]) = copy(point.transform(tr))
  def withoutTime(nTimeAxes: Int) = copy(point.withoutTime(nTimeAxes))
}

case class PointsMapping(mapping: SMap[Point, Point]) {
  def bias = {
    val (from, to) = mapping.toSeq.head
    MatrixUtil.subv(from.coords, to.coords)
  }

  assert(mapping.toSeq.forall { case (from, to) => MatrixUtil.subv(from.coords, to.coords) == bias })

  val froms = mapping.keys.toSet
  val tos = mapping.values.toSet

  def toSeq = mapping.toSeq
}

sealed abstract class Conn {
  def transform(tr: Seq[Seq[Int]]): Conn
  def withoutTime(nTimeAxes: Int): Conn
}

case class Point2PointConn(dst: Point, src: Point, dstVar: Intermediate, srcIndexed: Indexed, delay: Seq[Int]) extends Conn {
  assert(dst.dim == src.dim, "your intra-array connections need to be between points in the same iteration space")

  val srcVar = srcIndexed.variable match {
    case v: Intermediate => v
    case _ => throw new Exception("point2point connections must have intermediate values")
  }

  override def transform(tr: Seq[Seq[Int]]) = Point2PointConn(
    dst = dst.transform(tr),
    src = src.transform(tr),
    dstVar = dstVar,
    srcIndexed = srcIndexed,
    delay = delay,
  )

  override def withoutTime(nTimeAxes: Int) = {
    assert(delay.isEmpty, "you can't call 'withoutTime' twice")

    Point2PointConn(
      dst = dst.withoutTime(nTimeAxes: Int),
      src = src.withoutTime(nTimeAxes: Int),
      dstVar = dstVar,
      srcIndexed = srcIndexed,
      delay = subv(dst.time(nTimeAxes), src.time(nTimeAxes)),
    )
  }

  val diff: Iterable[Int] = subv(dst.coords, src.coords)

  override def toString: String = s"[$dstVar$dst <- $srcVar$src]"
}

case class IOConn(point: Point, ioIndex: Seq[Expr], ioIndexed: Indexed, interVar: Intermediate, time: Seq[Int], mapping: Option[PointsMapping]) extends Conn {
  val ioVar = ioIndexed.variable

  assert(!ioVar.isInstanceOf[Intermediate], "This class is only for Input and Output types")

  val isOutput: Boolean = ioVar.isInstanceOf[Output]

  override def transform(tr: Seq[Seq[Int]]) = IOConn(
    point = point.transform(tr),
    ioIndex = ioIndex,
    ioIndexed = ioIndexed,
    interVar = interVar,
    time = time,
    mapping = mapping,
  )

  override def withoutTime(nTimeAxes: Int): Conn = {
    assert(time.isEmpty, "you can't call 'withoutTime' twice")

    IOConn(
      point = point.withoutTime(nTimeAxes),
      ioIndex = ioIndex,
      ioIndexed = ioIndexed,
      interVar = interVar,
      time = point.time(nTimeAxes),
      mapping = mapping,
    )
  }

  override def toString: String = {
    val timeStr = s"$time" + (mapping match {
      case Some(_) => "*"
      case None => ""
    })
    if (isOutput) s"[$ioVar[${ioIndex.mkString(",")}] <- $interVar$point at $timeStr]"
    else s"[$interVar$point <- $ioVar[${ioIndex.mkString(",")}] at $timeStr]"
  }
}

case class OptimisticSkipConn(conns: Set[Conn], bypass: Option[OptimisticSkipConn], skippedIndex: Int) extends Conn {
  override def transform(tr: Seq[Seq[Int]]): OptimisticSkipConn = OptimisticSkipConn(
    conns = conns.map(_.transform(tr)),
    bypass = bypass.map(_.transform(tr)),
    skippedIndex = skippedIndex,
  )

  override def withoutTime(nTimeAxes: Int): OptimisticSkipConn = OptimisticSkipConn(
    conns = conns.map(_.withoutTime(nTimeAxes)),
    bypass = bypass.map(_.withoutTime(nTimeAxes)),
    skippedIndex = skippedIndex,
  )

  def filterP2pConn(cond: Point2PointConn => Boolean): OptimisticSkipConn = {
    val newConns: Set[Conn] = conns.collect {
      case c: Point2PointConn if cond(c) => c
      case c: IOConn => c
      case c: OptimisticSkipConn if c.filterP2pConn(cond).conns.nonEmpty => c.filterP2pConn(cond)
    }

    val newBypass = bypass.map(_.filterP2pConn(cond)).filter(b => b.conns.nonEmpty)

    OptimisticSkipConn(
      conns = newConns,
      bypass = newBypass,
      skippedIndex = skippedIndex,
    )
  }

  def filterPoint(cond: Point => Boolean): OptimisticSkipConn = {
    val newConns: Set[Conn] = conns.collect {
      case c @ Point2PointConn(dst, src, _, _, _) if cond(dst) && cond(src) => c
      case c @ IOConn(point, _, _, _, _, _) if cond(point) => c
      case c: OptimisticSkipConn if c.filterPoint(cond).conns.nonEmpty => c.filterPoint(cond)
    }

    val newBypass = bypass.map(_.filterPoint(cond)).filter(b => b.conns.nonEmpty)

    OptimisticSkipConn(
      conns = newConns,
      bypass = newBypass,
      skippedIndex = skippedIndex,
    )
  }

  def flatten: Set[Conn] = conns.collect {
    case c: OptimisticSkipConn => c.flatten
    case c => Set(c)
  }.flatten

  val nonEmpty = conns.nonEmpty

  def head = flatten.head

  def isIO = head.isInstanceOf[IOConn]
  def isOutput = flatten.head match {
    case c: IOConn => c.isOutput
    case _ => throw new Exception("is not IO")
  }
  def isInput = !isOutput

  def diff: Iterable[Int] = head match {
    case c: Point2PointConn => c.diff
    case _ => throw new Exception("is not point2point")
  }

  def dstOrInterVar: Intermediate = head match {
    case Point2PointConn(_, _, dstVar, _, _) => dstVar
    case IOConn(_, _, _, interVar, _, _) => interVar
    case _ => throw new Exception("unknown type")
  }

  def srcOrIOVar: Variable = head match {
    case Point2PointConn(_, _, _, Indexed(srcVar, _), _) => srcVar
    case IOConn(_, _, Indexed(ioVar, _), _, _, _) => ioVar
    case _ => throw new Exception("unknown type")
  }

  def srcOrInterVar: Intermediate = head match {
    case Point2PointConn(_, _, _, Indexed(srcVar: Intermediate, _), _) => srcVar
    case IOConn(_, _, _, interVar, _, _) => interVar
    case _ => throw new Exception("unknown type")
  }

  def srcIndexed: Indexed = head match {
    case Point2PointConn(_, _, _, src, _) => src
    case IOConn(_, _, src, _, _, _) => src
    case _ => throw new Exception("unknown type")
  }

  def dstPoints: Set[Point] = flatten.map {
    case c: Point2PointConn => c.dst
    case _ => throw new Exception("is not point2point")
  }.toSet

  def srcPoints: Set[Point] = flatten.map {
    case c: Point2PointConn => c.src
    case _ => throw new Exception("is not point2point")
  }.toSet

  def ioPoints: Set[Point] = flatten.map {
    case c: IOConn => c.point
    case _ => throw new Exception("is not IOConn")
  }.toSet

  def nonSharedDim = {
    nonSharedInds.size
  }

  def nonSharedInds: Set[Int] = head match {
    case c: OptimisticSkipConn => c.nonSharedInds + skippedIndex
    case _ => Set(skippedIndex)
  }

  assert(conns.forall(c => c.getClass == conns.head.getClass), "can't mix types in an OptimisticSkipGroup")
  assert(flatten.forall(c => c.getClass == head.getClass), "can't mix types in the inner conns of an OptimisticSkipGroup")
  assert(flatten.collect { case c: Point2PointConn => c.diff }.toSet.size < 2, "conns can't go in different directions")
}

class IterationSpace(val points: Set[Point], val conns: Set[Conn],
                     val pointAssignments: Set[PointAssignment],
                     val indices: Seq[Index],
                     baseOpt: Option[IterationSpace] = None) {
  // Ideally, this class should not be instantiated itself, but should instead be constructed from the companion object

  val p2pConns = conns.collect { case c: Point2PointConn => c }
  val ioConns = conns.collect { case c: IOConn => c }
  val optimisticSkipConns = conns.collect { case c: OptimisticSkipConn => c }
  val topConns = conns.filter(c => !optimisticSkipConns.exists(s => s.conns.contains(c)))
  val topP2pConns = p2pConns.filter(c => topConns.contains(c))
  val topOptimisticSkipConns = optimisticSkipConns.filter(c => topConns.contains(c))

  val pointsMappings = pointAssignments.collect { case PointAssignment(_, _, _, _, Some(mapping)) => mapping }.toSet
  def sortedPointsMappings = {
    // Any sorting of the points mappings is fine, honestly. It just matters that there exists some stable sorting
    pointsMappings.toSeq.sortBy(_.toString.hashCode)
  }

  def dim: Int = points.headOption.map(_.coords.size).getOrElse(0)

  def size: Int = points.size

  assert(points.forall(_.dim == dim))

  val base = baseOpt.getOrElse(this)

  assert(base.indices == indices)

  private lazy val p2pConnsGroupedForBackTracing = base.p2pConns.groupBy(conn => (conn.dst, conn.dstVar, conn.srcVar)).withDefaultValue(Set.empty) // performance hack
  private lazy val ioConnsGroupedForBackTracing = base.ioConns.filter(!_.isOutput).groupBy(ioc => (ioc.point, ioc.interVar)).withDefaultValue(Set.empty) // performance hack

  private def tracedBackToInputs(conn: Point2PointConn, already_visited: MutSet[Point2PointConn] = MutSet.empty): Set[IOConn] = {
    already_visited.addOne(conn)

    base.p2pConnsGroupedForBackTracing(conn.src, conn.dstVar, conn.srcVar).collect {
      case iac if !already_visited.contains(iac) =>
        tracedBackToInputs(iac, already_visited = already_visited)
    }.flatten ++
      base.ioConnsGroupedForBackTracing((conn.src, conn.srcVar))
  }

  private lazy val p2pConnsGroupedForForwardTracing = base.p2pConns.groupBy(conn => (conn.src, conn.dstVar, conn.srcVar)).withDefaultValue(Set.empty) // performance hack
  private lazy val ioConnsGroupedForForwardTracing = base.ioConns.filter(_.isOutput).groupBy(ioc => (ioc.point, ioc.interVar)).withDefaultValue(Set.empty) // performance hack

  private def tracedForwardToOutputs(conn: Point2PointConn, already_visited: MutSet[Point2PointConn] = MutSet.empty): Set[IOConn] = {
    already_visited.addOne(conn)

    base.p2pConnsGroupedForForwardTracing((conn.dst, conn.dstVar, conn.srcVar)).collect {
      case iac if !already_visited.contains(iac) =>
        tracedForwardToOutputs(iac, already_visited)
    }.flatten ++
      base.ioConnsGroupedForForwardTracing((conn.dst, conn.dstVar))
  }

  private def tracedBackToInputPoints(conn: Point2PointConn, already_visited: MutSet[Point2PointConn] = MutSet.empty): Set[Point] = {
    if (already_visited.contains(conn))
      Set.empty
    else {
      already_visited.addOne(conn)

      val incomingP2pConns = base.p2pConnsGroupedForBackTracing(conn.src, conn.dstVar, conn.srcVar).filter(_ != conn)
      val incomingPoints = incomingP2pConns.flatMap(tracedBackToInputPoints(_, already_visited))

      val ioConnsExist = base.ioConnsGroupedForBackTracing(conn.src, conn.srcVar).nonEmpty

      if (ioConnsExist || incomingP2pConns.isEmpty)
        incomingPoints + conn.src
      else
        incomingPoints
    }
  }

  private def tracedForwardToOutputPoints(conn: Point2PointConn, already_visited: MutSet[Point2PointConn] = MutSet.empty): Set[Point] = {
    if (already_visited.contains(conn))
      Set.empty
    else {
      already_visited.addOne(conn)

      val outgoingP2pConns = base.p2pConnsGroupedForForwardTracing(conn.dst, conn.dstVar, conn.srcVar)
      val outgoingPoints = outgoingP2pConns.flatMap(tracedForwardToOutputPoints(_, already_visited))

      val ioConnsExist = base.ioConnsGroupedForForwardTracing(conn.dst, conn.dstVar).nonEmpty

      if (ioConnsExist || outgoingP2pConns.isEmpty)
        outgoingPoints + conn.dst
      else
        outgoingPoints
    }
  }

  lazy private val pointAssignmentsGroupedForOriginalInputs = pointAssignments.filter(_.mapping.isEmpty).groupBy(pa => (pa.point, pa.assignment.dst.variable)) // performance hack

  private def originalInputs(conn: Point2PointConn): Expr = {
    // This function replaces a PE-to-PE input with a set of IO connections and an arithmetic expression joining them
    // into a single intermediate variable

    val ioConns = tracedBackToInputs(conn)

    val inputPoints = tracedBackToInputPoints(conn)
    require(inputPoints.subsetOf(points), s"$conn has inputPoints that are outside the proper range\n\tinputPoints = $inputPoints")

    // assert(inputPoints.size == 1, "we can't handle more than one input point right now")

    val inputExprs = inputPoints.map((_, conn.srcVar)).toSet.flatMap(key => pointAssignmentsGroupedForOriginalInputs(key)).map {
      case PointAssignment(_, _, Assignment(Indexed(conn.srcVar, _), src), _, None) =>
        val replacements = ioConns.collect { case IOConn(_, ioIndex, ioIndexed, _, _, _) =>
          val newIndices = ioIndex.zip(ioIndexed.indices).map {
            case (_, symbolicIndex) if Passes.containsSkipFunc(symbolicIndex) => symbolicIndex
            case (exactIndex, _) => exactIndex
          }
          (ioIndexed, ioIndexed.copy(indices = newIndices))
        }

        assert(replacements.size <= 1, "not sure how to condense multiple, conflicting ioConns at the same point into one expression")

        Passes.replaceExprs(src, replacements.toMap)

      case _ => throw new Exception("UNREACHABLE")
    }

    assert(inputExprs.size == 1, "we shouldn't have more than one assignment to a variable in a point before we've removed the time part")

    val inputExpr = inputExprs.head

    inputExpr
  }

  lazy private val pointAssignmentsGroupedForFinalOutputs = pointAssignments.
    filter(pa => pa.mapping.isEmpty && pa.assignment.dst.isIOOutput && pa.assignment.src.isInstanceOf[Indexed]).
    groupBy(pa => (pa.point, pa.assignment.src.asInstanceOf[Indexed].variable)).withDefaultValue(Set.empty) // performance hack

  private def finalOutputs(conn: Point2PointConn): Option[Indexed] = {
    // This function replaces a PE-to-PE output with an IO connection
    // Note: Right now, we only support one-to-one output connections, like "C[i,j] = c(i,j,k.upperBound)".
    //       More complicated outputs, like C[i,j] = c(i,j,k.upperBound) + a(i,j,k)" are not supported

    val ioConns = tracedForwardToOutputs(conn)
    val outputPoints = tracedForwardToOutputPoints(conn)

    val outputExprs = outputPoints.map((_, conn.dstVar)).toSet.flatMap(key => pointAssignmentsGroupedForFinalOutputs(key)).map {
      case PointAssignment(_, _, Assignment(dst @ Indexed(_: Output, _), Indexed(conn.dstVar, _)), _, None) =>
        val replacements = ioConns.collect { case IOConn(_, ioIndex, ioIndexed, _, _, _) =>
          val newIndices = ioIndex.zip(ioIndexed.indices).map {
            case (_, symbolicIndex) if Passes.containsSkipFunc(symbolicIndex) => symbolicIndex
            case (exactIndex, _) => exactIndex
          }
          (ioIndexed, ioIndexed.copy(indices = newIndices))
        }

        assert(replacements.size <= 1, "not sure how to condense multiple, conflicting ioConns at the same point into one expression")

        Passes.replaceExprs(dst, replacements.toMap) match {
          case ind: Indexed => ind
          case e => throw new Exception(s"This outputExpr is not of the Indexed type: $e")
        }

      case _ => throw new Exception("UNREACHABLE")
    }
    assert(outputExprs.size <= 1, s"we shouldn't have more than one output from a variable, $conn, $outputExprs")

    val outputExpr = outputExprs.headOption

    outputExpr
  }

  private def filterP2Pconns(cond: Point2PointConn => Boolean, passThruVariables: Set[Intermediate],
                             ignoreCond: (Point2PointConn => Boolean) = (_) => false): (Set[Conn], Set[PointAssignment]) = {
    // This function filters out point-to-point connections, and replaces them with IO connections. It also updates the
    // pointAssignments to accommodate the new IO connections. Then, it returns the conns and pointAssignments
    // that result from that.

    assert(optimisticSkipConns.isEmpty, "We don't supporting filtering on spatial arrays with optSkipConns (for now). You'll need to apply the optSkipConns after you've filtered everything")

    val brokenIntraArrayConns = p2pConns.filter(c => !cond(c) && !ignoreCond(c))
    val remainingP2pConns = p2pConns.filter(c => cond(c))

    val newIOConnsFromBrokenIntraArrayConns = brokenIntraArrayConns.flatMap {
      case iac @ Point2PointConn(dst, src, _, _, _) =>
        (tracedBackToInputs(iac).map(_.copy(point = dst)) ++ {
          if (!passThruVariables.contains(iac.srcVar)) tracedForwardToOutputs(iac).map(_.copy(point = src))
          else Set()
        }).map {
          case IOConn(point, exactIndices, Indexed(variable, symbolicIndices), interVar, time, mapping) =>
            val newIndices = exactIndices.zip(symbolicIndices).map {
              case (_, symbolicIndex) if Passes.containsSkipFunc(symbolicIndex) => symbolicIndex
              case (exactIndex, _) => exactIndex
            }

            IOConn(point, exactIndices, Indexed(variable, newIndices), interVar, time, mapping)
        }
    }

    val newAsgnInputs = {
      val newInputsForEachPoint = brokenIntraArrayConns.groupMap(_.dst)(iac => (iac.srcIndexed, originalInputs(iac))).withDefaultValue(Set.empty)

      pointAssignments.collect { case pointAsgn if newInputsForEachPoint.contains(pointAsgn.point) =>
        (pointAsgn.asInstanceOf[PointAssignment], newInputsForEachPoint(pointAsgn.point))
      }.toMap.withDefaultValue(Set.empty)
    }

    val newAsgnOutputs = {
      val brokenIntraArrayConnsGroupedBySrc = brokenIntraArrayConns.groupBy(_.src).withDefaultValue(Set.empty)

      pointAssignments.map { pointAsgn =>
        val dstVar = pointAsgn.assignment.dst.variable

        val newOutputs = brokenIntraArrayConnsGroupedBySrc(pointAsgn.point).collect {
          case iac @ Point2PointConn(_, pointAsgn.point, `dstVar`, _, _)
            if iac.srcVar == dstVar => finalOutputs(iac)
        }.flatten

        (pointAsgn, newOutputs)
      }.toMap
    }

    val newAssignments = pointAssignments.flatMap { pointAsgn =>
      val newInputs = newAsgnInputs(pointAsgn)
      val newAsgnSrc = Passes.replaceExprs(pointAsgn.assignment.src, newInputs.toMap)

      val newOutputs = newAsgnOutputs(pointAsgn)
      assert(newOutputs.size <= 1, "not sure what to do for now if there are multiple possible outputs")
      val newAsgnDstOption = newOutputs.headOption

      (if (newInputs.isEmpty) Seq(pointAsgn) else {
        val dst = pointAsgn.assignment.dst
        val newDstIndices = dst.indices.map(Passes.replaceExprs(_, newInputs.toMap))
        Seq(pointAsgn.copy(assignment = pointAsgn.assignment.copy(
            dst = dst.copy(indices = newDstIndices),
            src = newAsgnSrc)(None))
        )
      }) ++
        newAsgnDstOption.map(newAsgnDst =>
          pointAsgn.copy(assignment = Assignment(dst = newAsgnDst, src = pointAsgn.assignment.dst)(None))
        )
    }

    val newIOConnsFromNewAsgns = {
      val ioConnsGroupedByPoint = ioConns.groupBy(_.point).withDefaultValue(Set.empty)

      newAsgnInputs.keys.flatMap { pointAsgn =>
        val newInputs = newAsgnInputs(pointAsgn)

        ioConnsGroupedByPoint(pointAsgn.point).map { ioc =>
          val newIndexed = Passes.replaceExprs(ioc.ioIndexed, newInputs.toMap) match {
            case indexed: Indexed => indexed
            case _ => ioc.ioIndexed
          }

          if (newIndexed == ioc.ioIndexed) {
            // There's no point in replacing the ioConn if it's identical to what it was before
            ioc
          } else {
            // Otherwise, we have to re-calculate the io-index based on newIndexed
            val indexMap = indices.zip(ioc.point.coords).toMap
            ioc.copy(
              ioIndex = newIndexed.indices.map(ind => Passes(Passes.replaceIndex(ind, indexMap))),
              ioIndexed = newIndexed
            )
          }
        }
      }
    }

    val newIOConns = {
      // The two variables below are just used for memoization to speed up the "filter" loop below them
      val newAssignmentsGroupedByPoint = newAssignments.groupBy(_.point)
      val newAssignmentSrcsGroupedByPoint = {
        val extractedIndexedsFromSrcs = newAssignments.map(_.assignment.src).map(src => src -> Passes.extractIndexed(src)).toMap
        newAssignmentsGroupedByPoint.view.mapValues { asgns =>
          asgns.map(_.assignment.src).flatMap(extractedIndexedsFromSrcs(_))
        }.toMap
      }

      (ioConns ++ newIOConnsFromBrokenIntraArrayConns ++ newIOConnsFromNewAsgns).filter { ioc =>
        if (ioc.isOutput)
          newAssignmentsGroupedByPoint(ioc.point).map(_.assignment.dst).contains(ioc.ioIndexed)
        else
          newAssignmentSrcsGroupedByPoint(ioc.point).contains(ioc.ioIndexed)
      }
    }

    val newConns: Set[Conn] = remainingP2pConns ++
      newIOConns ++
      optimisticSkipConns.map(_.filterP2pConn(cond)).filter(_.nonEmpty)

    (newConns, newAssignments)
  }

  def filterPoints(cond: Point => Boolean, passThruVariables: Set[Intermediate]): IterationSpace = {
    val (connsAfterBreakingIntraArrayConns, asgnsAfterBreakingIntraArrayConns) =
      filterP2Pconns(c => cond(c.dst) && cond(c.src), passThruVariables,
        ignoreCond = c => !cond(c.dst) && !cond(c.src))

    val new_p2p_and_io_conns = connsAfterBreakingIntraArrayConns.collect {
      case conn @ Point2PointConn(dst, src, _, _, _) if cond(src) && cond(dst) => conn
      case conn @ IOConn(point, _, _, _, _, _) if cond(point) => conn
    }

    val new_optimistic_skip_conns = optimisticSkipConns.map { conn =>
      conn.filterPoint(p => cond(p))
    }.filter(_.nonEmpty)

    val new_conns: Set[Conn] = new_p2p_and_io_conns ++ new_optimistic_skip_conns
    val new_assignments = asgnsAfterBreakingIntraArrayConns.filter(asgn => cond(asgn.point))

    new IterationSpace(
      points = points.filter(cond),
      conns = new_conns,
      pointAssignments = new_assignments,
      indices = indices,
      baseOpt = baseOpt,
    )
  }

  def withoutSelfConnections: IterationSpace = {
    val newConns: Set[Conn] = conns.collect {
      case c @ Point2PointConn(dst, src, _, _, _) if dst != src => c
      case c: IOConn => c
      case c: OptimisticSkipConn if c.filterP2pConn((x: Point2PointConn) => x.dst != x.src).nonEmpty =>
        c.filterP2pConn((x: Point2PointConn) => x.dst != x.src)
    }

    new IterationSpace(
      points = points,
      conns = newConns,
      pointAssignments = pointAssignments,
      indices = indices,
      baseOpt = baseOpt,
    )
  }

  def withoutTime(nTimeAxes: Int): IterationSpace = {
    new IterationSpace(
      points = points.map(_.withoutTime(nTimeAxes)),
      conns = conns.map(_.withoutTime(nTimeAxes)),
      pointAssignments = pointAssignments.map(_.withoutTime(nTimeAxes)),
      indices = indices,
      baseOpt = baseOpt,
    )
  }

  def hasFutureConnections: Boolean = {
    dim > 0 &&
      p2pConns.exists(iac => iac.dst.coords.last < iac.src.coords.last)
  }

  def transform(tr: Seq[Seq[Int]]): IterationSpace = {
    new IterationSpace(
      points = points.map(_.transform(tr)),
      conns = conns.map(_.transform(tr)),
      pointAssignments = pointAssignments.map(_.transform(tr)),
      indices = indices,
      baseOpt = baseOpt match {
        case Some(b) => Some(b.transform(tr))
        case None => None
      },
    )
  }

  def withMapping(pointsMapping: PointsMapping, passThruVariables: Set[Intermediate]): IterationSpace = {
    val (connsAfterBreakingIntraArrayConns, asgnsAfterBreakingIntraArrayConns) =
      filterP2Pconns(c =>
        !pointsMapping.toSeq.exists { case (from, to) =>
          (c.dst == from && !pointsMapping.froms.contains(c.src)) ||
            (c.src == to && !pointsMapping.tos.contains(c.dst))
        },
        passThruVariables)

    // To calculate the new IO conns, we use both the relevant remapped intra-array conns, and the IO conns that we
    // calculate directly from the pointsMapping
    val newIOConns = pointsMapping.toSeq.flatMap { case (from, to) =>
        connsAfterBreakingIntraArrayConns.collect {
          case ioc @ IOConn(`from`, _, _, _, _, None) => ioc.copy(point = to, mapping = Some(pointsMapping))
        }
      }

    val newAssignments = asgnsAfterBreakingIntraArrayConns ++
      pointsMapping.toSeq.flatMap { case (from, to) =>
        asgnsAfterBreakingIntraArrayConns.collect {
          case pointAsgn @ PointAssignment(`from`, _, _, _, None) =>
            pointAsgn.copy(point = to, mapping = Some(pointsMapping))
        }
      }

    val newConns = connsAfterBreakingIntraArrayConns ++ newIOConns

    new IterationSpace(
      points = points,
      conns = newConns,
      pointAssignments = newAssignments,
      indices = indices,
      baseOpt = Some(base),
    )
  }

  def withBrokenConnectionVectors(brokenConnVs: Set[ConnectionVector], passThruVars: Set[Intermediate]): IterationSpace = if (brokenConnVs.nonEmpty) {
    val (newConns, newAssignments) = filterP2Pconns(c =>
      !brokenConnVs.exists(bcv =>
        bcv.dstVar == c.dstVar && bcv.srcIndexed == c.srcIndexed &&
          subv(c.dst.coords, c.src.coords) == bcv.diff
      ),
      passThruVars)

    new IterationSpace(
      points = points,
      conns = newConns,
      pointAssignments = newAssignments,
      indices = indices,
      baseOpt = Some(base),
    )
  } else this

  private def withOptimisticBrokenConnectionVector(brokenSyncConn: ConnectionVector, skippedIndex: Int): IterationSpace = {
    // "skippedIndex" is the exact index in the point coords which is sparse. For example, for the A100 sparsity model,
    // "skippedIndex" would be "3", which corresponds to the "ki" index.
    //
    // Note: I'm not sure yet if this will work when load-balancing mappings are also applied. In particular, I'm not
    //   sure if it'll be OK for IOConns with different "mapping" values to be in the same optimisticSkipGroup

    // Collect the point2point (and enveloping optimisticSkipGroupConns) which will also be grouped together now
    val skippedTopConns: Set[Conn] = topConns.collect {
      case c @ Point2PointConn(_, _, brokenSyncConn.dstVar, Indexed(brokenSyncConn.srcVar, _), _)
        if c.diff == brokenSyncConn.diff => c

      case c: OptimisticSkipConn if c.skippedIndex != skippedIndex &&
        !c.isIO && c.dstOrInterVar == brokenSyncConn.dstVar &&
        c.srcOrIOVar == brokenSyncConn.srcVar && c.diff == brokenSyncConn.diff => c

      case c @ IOConn(_, _, Indexed(_: Input, _), brokenSyncConn.srcVar, _, _) => c
      case c @ IOConn(_, _, Indexed(_: Output, _), brokenSyncConn.dstVar, _, _) => c

      case c: OptimisticSkipConn if c.skippedIndex != skippedIndex &&
        c.isIO && c.isInput && c.dstOrInterVar == brokenSyncConn.srcVar => c
      case c: OptimisticSkipConn if c.skippedIndex != skippedIndex &&
        c.isIO && c.isOutput && c.dstOrInterVar == brokenSyncConn.dstVar => c
    }

    val groupedAcrossBoundaries = skippedTopConns.groupBy {
      case Point2PointConn(dst, src, dstVar, Indexed(srcVar, _), _) =>
        (Set(without(dst.coords, skippedIndex)), Set(without(src.coords, skippedIndex)), dstVar, srcVar)

      case IOConn(point, _, Indexed(ioVar: Input, _), interVar, _, _) =>
        (Set(without(point.coords, skippedIndex)), Seq(), interVar, ioVar)

      case IOConn(point, _, Indexed(ioVar: Output, _), interVar, _, _) =>
        (Seq(), Set(without(point.coords, skippedIndex)), ioVar, interVar)

      case c: OptimisticSkipConn if !c.isIO =>
        (c.dstPoints.map(x => without(x.coords, skippedIndex)).toSet,
          c.srcPoints.map(x => without(x.coords, skippedIndex)).toSet,
          c.dstOrInterVar, c.srcOrIOVar)

      case c: OptimisticSkipConn if c.isIO && c.isInput =>
        (c.ioPoints.map(x => without(x.coords, skippedIndex)).toSet,
          Set(), c.dstOrInterVar, c.srcOrIOVar)

      case c: OptimisticSkipConn if c.isIO && c.isOutput =>
        (Set(), c.ioPoints.map(x => without(x.coords, skippedIndex)).toSet,
          c.srcOrIOVar, c.dstOrInterVar)

      case _ => throw new Exception("unknown type")
    }

    val newConnsWithoutBypasses = groupedAcrossBoundaries.values.map { cs =>
      OptimisticSkipConn(cs, None, skippedIndex)
    }

    val newConns = newConnsWithoutBypasses.map {
      case c @ OptimisticSkipConn(cs, None, `skippedIndex`) =>
        val potentialBypasses = newConnsWithoutBypasses ++ optimisticSkipConns
        val bypass = potentialBypasses.filter { x =>
          val srcs = if (x.isIO) x.ioPoints else x.dstPoints
          val dsts = if (c.isIO) c.ioPoints else c.srcPoints

          (!c.isIO || c.isOutput) && (!x.isIO || x.isInput) &&
            x.dstOrInterVar == c.srcOrInterVar && srcs == dsts
        } match {
          case Seq(b) => Some(b)
          case Seq() => None
          case bs => throw new Exception(s"too many potential bypasses: $bs")
        }

        OptimisticSkipConn(cs, bypass, skippedIndex)

      case OptimisticSkipConn(_, None, _) => throw new Exception("this has the wrong skippedIndex")
      case OptimisticSkipConn(_, Some(_), _) => throw new Exception("this shouldn't have any bypasses yet")
    }

    new IterationSpace(
      points = points,
      conns = conns ++ newConns,
      pointAssignments = pointAssignments,
      indices = indices,
      baseOpt = Some(base)
    )
  }

  def withBrokenOptimisticConnVectors(brokenOptimisticConns: Set[(ConnectionVector, Int)]): IterationSpace = if (brokenOptimisticConns.nonEmpty) {
    // brokenOptimisticConns" format: [(connV, skippedIndex)]

    brokenOptimisticConns.foldLeft(this) { case (acc, (bsc, skippedIndex)) =>
      acc.withOptimisticBrokenConnectionVector(bsc, skippedIndex)
    }
  } else this
}

object IterationSpace {
  def fromPoints(points: Set[Point], intraArrayConns: Set[Point2PointConn], ioConns: Set[IOConn], pointAssignments: Set[PointAssignment], indices: Seq[Index]): IterationSpace = {
    new IterationSpace(points, intraArrayConns ++ ioConns, pointAssignments, indices)
  }

  def fromBoundsAndAssignments(bounds: Seq[Int], indices: Seq[Index], assignments: Iterable[Assignment]): IterationSpace = {
    val lowerBound = -1
    val boundsMap = indices.zip(bounds.map(b => (lowerBound,b))).toMap

    val points = Util.crossJoin(bounds.map(b => lowerBound until b)).map(r => Point(r.toSeq)).toSet

    val pointAssignments = {
      assignments.zipWithIndex.flatMap { case (asgn, i) =>
        val preceding = assignments.take(i)
        val fixedVals = asgn.fixedVals(boundsMap)
        val excludedVals = asgn.excludedVals(preceding, boundsMap)

        points.filter { p =>
          def overlaps(fixed: Iterable[Option[Int]]) = Util.overlaps(fixed, p.coords)
          overlaps(fixedVals) && !excludedVals.exists(overlaps)
        }.map { p =>
          PointAssignment(p, p, asgn, i, None)
        }
      }.toSet
    }

    // First, we add all the intraArrayConnections
    val intraArrayConns = {
      val memoizeRangesCache = scala.collection.mutable.Map.empty[(Seq[Seq[Int]], Seq[Seq[Option[Int]]]), Iterable[Seq[Int]]]

      assignments.flatMap { assignment =>
        val preceding = assignments.takeWhile(_ != assignment)

        val excluded = assignment.excludedVals(preceding, boundsMap)
        val single_excluded = excluded.filter(_.count(_.nonEmpty) == 1)
        val multiple_excluded = excluded.filter(_.count(_.nonEmpty) > 1).map(_.toSeq).toSeq

        val conns = assignment.connectionVectors(indices, boundsMap) // These are simple fixed offsets between connected points

        conns.flatMap { case ConnectionVector(srcIndexed, dstVar, diff, dstFixedVals) =>
          val ps = {
            // As a performance hack, we regenerate the points here instead of using a filtered version of the "points"
            // variable above. Regenerating them allows us to avoid some spurious comparisons that we would have to do
            // if we wanted to filter the full "points" set.
            val dst_ranges = bounds.zip(dstFixedVals).zip(diff).map {
              case ((b, None), d) => ((lowerBound+d) until b).toSeq
              case ((_, Some(fixed)), _) => Seq(fixed)
            }

            val dst_ranges_with_single_exclusions = single_excluded.foldLeft(dst_ranges) { (rs, excs) =>
              rs.zip(excs).map {
                case (r, Some(e)) => without(r, r.indexOf(e)).toSeq
                case (r, None) => r
              }
            }

            val dst_ps = memoizeRangesCache.getOrElseUpdate((dst_ranges_with_single_exclusions, multiple_excluded), {
              Util.crossJoin(dst_ranges_with_single_exclusions).filter { p =>
                !multiple_excluded.exists(Util.overlaps(_, p))
              }.map(_.toSeq)
            })

            val src_ps = dst_ps.map(MatrixUtil.subv(_, diff.toSeq))
            src_ps.filter(_.forall(_ >= lowerBound)).toSet.map(cs => Point(cs))
          }

          ps.map(p =>
            Point2PointConn(
              dst = p + diff,
              src = p,
              dstVar = dstVar,
              srcIndexed = srcIndexed,
              delay = Seq.empty,
            )
          )
        }.toSet
      }.toSet
    }.map(_.asInstanceOf[Point2PointConn] /* For some reason, this "asInstanceOf" is required to make the compiler happy */)

    // Next, we get the IO connections
    val ioConns = assignments.zipWithIndex.flatMap { case (asgn, i) =>
      val fixedVals = asgn.fixedVals(boundsMap)
      val excludedVals = asgn.excludedVals(assignments.take(i), boundsMap)
      val ps = points.filter(p => Util.overlaps(fixedVals, p.coords))
          .filter(p => Util.noOverlap(excludedVals, p.coords))

      asgn.ioConns.collect {
        // Loop through all the IO assignments we have, while filtering out the ones that do not pass through the array...
        case (symDst, symSrc) if symDst.variable.isInstanceOf[Intermediate] || symSrc.variable.isInstanceOf[Intermediate] =>
          // Now loop through all of the points for which the assignment is valid, and map each one to specific,
          // non-symbolic IO connections
          ps.map { p =>
            def passes(indexed: Indexed): Indexed = {
              val indexMap = indices.zip(p.coords).toMap
              Passes(Passes.replaceBounds(Passes.replaceIndex(indexed, indexMap), boundsMap)).asInstanceOf[Indexed]
            }

            val dstExpr = passes(symDst)
            val srcExpr = passes(symSrc)

            // We use "collect" instead of "map" here because we can safely ignore all non-Const indices, as these will
            // only exist for the ioVars whose indices don't need to be mapped to integer constants anyways
            val dstCoords = dstExpr.indices.collect { case Const(c) => c }
            val srcCoords = srcExpr.indices.collect { case Const(c) => c }

            (dstExpr.variable, srcExpr.variable) match {
              case (dstVar: Intermediate, _) =>
                IOConn(
                  point = Point(dstCoords),
                  ioIndex = srcExpr.indices,
                  ioIndexed = symSrc,
                  interVar = dstVar,
                  time = Seq.empty,
                  None,
                )
              case (_, srcVar: Intermediate) =>
                IOConn(
                  point = Point(srcCoords),
                  ioIndex = dstExpr.indices,
                  ioIndexed = symDst,
                  interVar = srcVar,
                  time = Seq.empty,
                  None,
                )
              case _ => throw new Exception("IO conns must be between unreachable regions of code")
            }
          }
      }
    }.flatten.toSet

    fromPoints(points, intraArrayConns, ioConns, pointAssignments, indices.toSeq)
  }

  def fromBoundsAndAssignmentsAndSparsityAndMappings(bounds: Seq[Int], indices: Seq[Index],
                                                     assignments: Iterable[Assignment],
                                                     brokenNonSyncConnectionVectors: Set[ConnectionVector],
                                                     brokenOptimisticConnVectors: Set[(ConnectionVector, Int)],
                                                     mappings: Iterable[Map]): IterationSpace = {
    val lowerBound = -1 // TODO we should also instantiate the upperBound points (i.e. [lowerBound, upperBound], rather than [lowerBound, upperBound))
    val points = Util.crossJoin(bounds.map(b => lowerBound until b)).map(r => Point(r.toSeq)).toSet
    val nonEdgePoints = points.filter(!_.coords.contains(lowerBound))

    val pointsMapping = mappings.flatMap { case mapping @ Map(src, dst) =>
      val result = nonEdgePoints.flatMap { p =>
        def passes(exprs: Iterable[Expr]) = {
          val indexMap = indices.zip(p.coords).toMap
          val boundsMap = indices.zip(bounds.map(b => (lowerBound,b))).toMap
          exprs.map(e => Passes(Passes.replaceBounds(Passes.replaceIndex(e, indexMap), boundsMap)))
        }

        def toRanges(exprs: Iterable[Expr]) = exprs.map {
          case range: IndexRange => range
          case expr => IndexRange(Some(expr), Some(expr + 1))
        }

        def fillInRanges(exprs: Iterable[(IndexRange, IndexRange)]) = exprs.map { case (e1, e2) =>
          (Passes(e1.fillInRange(e2)), Passes(e2.fillInRange(e1)))
        }

        val srcExpr = passes(src)
        val dstExpr = passes(dst)

        val srcRanges = toRanges(srcExpr)
        val dstRanges = toRanges(dstExpr)

        val (srcFilledRanges, dstFilledRanges) = fillInRanges(srcRanges zip dstRanges).unzip

        val dstCoordRanges = dstFilledRanges.map { case IndexRange(Some(Const(s)), Some(Const(e))) => s until e }
        val srcCoordRanges = srcFilledRanges.map { case IndexRange(Some(Const(s)), Some(Const(e))) => s until e }

        val dstCoords = Util.crossJoin(dstCoordRanges)
        val srcCoords = Util.crossJoin(srcCoordRanges)

        srcCoords.zip(dstCoords).map { case (s, d) => (Point(s.toSeq), Point(d.toSeq)) }
      }

      val all_within_range = result.forall { case (x,y) => nonEdgePoints.contains(x) && nonEdgePoints.contains(y) }

      Option.when(!mapping.fromSweep || all_within_range)(result.toMap)
    }

    // Sometimes, we map points outside of the spatial array into the spatial array. We need to temporarily extend our
    // bounds to deal with those cases
    val extendedBounds = bounds.indices.map { i => (points ++ pointsMapping.flatMap(_.keys) ++ pointsMapping.flatMap(_.values)).map(_.coords(i)).max + 1 }

    val passThruVars = Passes.passThruVariables(assignments)

    val base = fromBoundsAndAssignments(extendedBounds, indices, assignments).withBrokenConnectionVectors(brokenNonSyncConnectionVectors, passThruVars)

    pointsMapping.foldLeft(base)((acc, x) => acc.withMapping(PointsMapping(x), passThruVars))
      // And now, we remove the extended points that we added earlier
      .filterPoints(p => points.contains(p), passThruVars)
      // And now, we add the optimistic-skip buffers
      .withBrokenOptimisticConnVectors(brokenOptimisticConnVectors)
  }

  def ioPorts(withoutTime: IterationSpace): SMap[Point, SMap[Variable, Int]] = {
    // We don't distinguish between IOConns that have the same ioVar and ioIndex, but which map to different interVars.
    // So first, we de-duplicated ioConns that differ only in the interVar
    val dedupedIoConns = {
      val replacedInterVar = withoutTime.ioConns.headOption.map(_.interVar)
      val replacedExprInds = withoutTime.ioConns.headOption.map(_.ioIndexed.indices)
      withoutTime.ioConns
        .map(_.copy(interVar = replacedInterVar.get))
        .map(ioc => ioc.copy(ioIndexed = Indexed(ioc.ioVar, replacedExprInds.get)))
    }

    dedupedIoConns.groupBy(_.point)
      .view.mapValues(_.groupBy(_.ioVar)
        .view.mapValues(_.groupBy(_.time).toSeq.map(_._2.size).max).toMap).toMap
  }

  def ioLastTimes(withTime: IterationSpace): SMap[Variable, Seq[Int]] = {
    // TODO This function ignores the load-balancing scheme, which means that it always considers the worst-case end-time for each variable, when the actual lastTimes could sometimes be earlier
    withTime.ioConns.map(ioc => (ioc.ioVar, ioc.time)).groupBy(_._1).view.mapValues(_.map(_._2).maxBy(_.reverse)).toMap
  }

  def totalTimeSteps(withTime: IterationSpace, nTimeAxes: Int): Seq[Int] = withTime.points.map(_.time(nTimeAxes)).maxBy(_.reverse)

  def timeSpanForPoints(withTime: IterationSpace, nTimeAxes: Int): SMap[Point, (Seq[Int], Seq[Int])] = {
    // Return format: {pointWithoutTime: (inclusiveMinTime, inclusiveMaxTime)}
    withTime.points.map(p => (p.withoutTime(nTimeAxes), p.time(nTimeAxes)))
      .groupBy(_._1).view.mapValues(_.map(_._2))
      .view.mapValues(s => (s.minBy(_.reverse), s.maxBy(_.reverse))).toMap
  }

  def pruneBasedOnSkips(its: IterationSpace, indices: Iterable[Index], skips: Iterable[Skip],
                        passThruVars: Set[Intermediate]): IterationSpace = {
    // Removes points which are known at compile-time to always be skipped (such as for diagonal sparsity).

    skips.foldLeft(its) { case (acc, skip) =>
      acc.filterPoints({ p =>
        val indexMap = indices.zip(p.coords).toMap
        val cond = Passes(Passes.replaceIndex(skip.cond, indexMap))
        cond != True
      }, passThruVars)
    }
  }

  def connectedClusters(its: IterationSpace): Set[Set[Point]] = {
    def helper(clusters: Set[Set[Point]], p2pConn: Point2PointConn) = {
      val srcCluster = clusters.find(_.contains(p2pConn.src)).get
      val dstCluster = clusters.find(_.contains(p2pConn.dst)).get
      clusters.diff(Set(srcCluster, dstCluster)) + (srcCluster ++ dstCluster)
    }

    val disjointClusters = its.points.map(p => Set(p))
    its.topP2pConns.foldLeft(disjointClusters)((acc, x) => helper(acc, x))
  }
}
