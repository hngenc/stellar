package stellar

class Map (val src: Seq[Expr], val dst: Seq[Expr], val fromSweep: Boolean = false)(implicit blockOpt: Option[Block]) {
  assert(src.size == dst.size, "The dimensions of your mapping must match")
  assert((src ++ dst).forall(e => e.indicesInExpr.size <= 1), "Only one index allowed in each expression")

  val indicesMatch: Boolean = {
    (src zip dst).forall { case (se, de) => se.indicesInExpr == de.indicesInExpr }
  }

  blockOpt.foreach(_.registerMap(this))

  protected[stellar] val sweepCmds = scala.collection.mutable.ListBuffer.empty[MapSweepCmd]
  def sweep(index: Index, step: Int = 1, max: Int = -1): this.type = {
    sweepCmds.addOne(SweepIndex(index, step, Option.when(max >= 0)(max)))
    this
  }
  def step(index: Index, by: Int): this.type = {
    sweepCmds.addOne(StepIndex(index, by))
    this
  }
  def copy(_src: Seq[Expr] = src, _dst: Seq[Expr] = dst, _fromSweep: Boolean = fromSweep): Map = {
    val result = new Map(_src, _dst, _fromSweep)(None)
    assert(result.sweepCmds.isEmpty)
    result.sweepCmds ++= sweepCmds
    result
  }

  def domainVectors(indices: Seq[Index]): Seq[Seq[Expr]] = {
    // This function returns the set of vectors which define the parallelepiped that defines the shape of the region of
    // iterator space which will be remapped.
    //
    // Phew. What a mouthful.

    def rangedIndexUpperBounds(rangedIndices: Seq[Expr]): Seq[Option[Expr]] = rangedIndices.map {
      case IndexRange(_, None) => None
      case IndexRange(_, Some(end)) => Some(end)
      case e => Some(e)
    }

    def rangedIndexLowerBounds(rangedIndices: Seq[Expr]): Seq[Expr] = (rangedIndices zip indices).map {
      case (IndexRange(Some(start), _), _) => start
      case (IndexRange(None, _), ind) => ind.lowerBound
      case (e, ind) => e
    }

    def rangedIndexLens(upper: Seq[Option[Expr]], lower: Seq[Expr]): Seq[Option[Expr]] = (upper zip lower).map {
      case (Some(u), l) => Some(Passes(u - l))
      case (None, _) => None
    }

    val srcUpperBounds = rangedIndexUpperBounds(src)
    val dstUpperBounds = rangedIndexUpperBounds(dst)

    val srcLowerBounds = rangedIndexLowerBounds(src)
    val dstLowerBounds = rangedIndexLowerBounds(dst)

    val srcLens = rangedIndexLens(srcUpperBounds, srcLowerBounds)
    val dstLens = rangedIndexLens(dstUpperBounds, dstLowerBounds)

    val lens = (srcLens zip dstLens).map {
      case (None, Some(l)) => l
      case (Some(l), None) => l
      case (Some(l1), Some(l2)) if l1 == l2 => l1
      case (None, None) => throw new Exception("Lens in mapping are underspecified")
      case _ => throw new Exception("Lens in mapping do not match")
    }

    lens.zipWithIndex.collect {
      case (l, i) => Seq.fill(i)(Const(0)) ++ Seq(Passes(l + Const(1))) ++ Seq.fill(indices.size - i - 1)(Const(0))
    }
  }

  def rangeVectors(indices: Seq[Index], transform: Transform): Seq[Seq[Expr]] = {
    val spaceVectors = domainVectors(indices).map(dV => transform.spaceTransformExpr(dV))

    // Some of these spaceVectors may be parallel with each other. We prune them out here
    val result = MatrixExprUtil.nonParallel(spaceVectors)

    assert(result.size == transform.spaceDim)

    result
  }

  override def toString: String = s"Map [${src.mkString(",")}] to [${dst.mkString(",")}]"
}

class MapWithoutDst(src: Expr*)(implicit blockOpt: Option[Block]) {
  def to(dst: Expr*): Map = new Map(src, dst)
}

object Map {
  def apply(src: Expr*)(implicit blockOpt: Option[Block]): MapWithoutDst = new MapWithoutDst(src:_*)

  def unapply(mapping: Map): Option[(Iterable[Expr], Iterable[Expr])] = {
    Some((mapping.src, mapping.dst))
  }
}

sealed abstract class MapSweepCmd
case class SweepIndex(index: Index, step: Int, maxOpt: Option[Int]) extends MapSweepCmd
case class StepIndex(index: Index, step: Int) extends MapSweepCmd
