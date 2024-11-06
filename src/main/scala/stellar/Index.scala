package stellar

class Index(val name: String = "<UNKNOWN>", val nSubtiles: Int = 0, val isUnbounded: Boolean = false)(implicit block: Option[Block]) extends Expr {
  override def toString: String = name

  // Bounds
  val lowerBound = IndexLowerBound(this)
  val upperBound = IndexUpperBound(this)

  // Skipping (sparsity)
  private var skips: Option[Skip] = None
  def addSkip(skip: Skip): Unit = {
    assert(skip.index == this)
    assert(skips.isEmpty, "We can't currently skip the same index more than once in two different skip functions")
    skips = Some(skip)
  }

  def isSkipped: Boolean = skips.nonEmpty
  def isSyncSkipped: Boolean = skips.nonEmpty && skips.get.isOptimistic

  def dependencies: Set[Index] = {
    // Perform a breadth-first search for all dependencies
    // Note: this function can get caught in an infinite loop if there is a cycle in the dependency graph

    var deps = Set(this)
    var nextDeps = Set.empty[Index]
    var visited = Set(this)

    while (deps.nonEmpty) {
      for (dep <- deps) {
        for (s <- dep.skips) {
          nextDeps = nextDeps ++ s.cond.indicesInExpr.filter(d => !visited.contains(d))
          visited = visited ++ s.cond.indicesInExpr.filter(d => !visited.contains(d))
        }
      }

      deps = nextDeps
      nextDeps = Set.empty[Index]
    }

    visited.filter(v => v != this)
  }

  // Tiling
  assert(nSubtiles >= 0, "subtiles must be at least 1")
  val isTiled = nSubtiles > 0
  private val subtile = if (isTiled) Some(new Index(name=s"${name}i", nSubtiles=nSubtiles-1)(None)) else None

  def apply(subTileId: Int): Index = {
    if (subTileId > 0) {
      subtile.getOrElse(
        throw new Exception(s"Not enough subtile levels for $name")
      )(subTileId-1)
    } else {
      this
    }
  }

  def subtiles: Iterable[Index] = (1 to nSubtiles).map(i => apply(i))

  // Misc
  def indicesInExpr = Set(this)

  def dim = 0

  def as(x: Int) = lowerBound + 1 + x

  block.foreach(_.registerIndex(this))
}

case class IndexLowerBound(index: Index) extends Expr {
  override def toString: String = s"MIN($index)"

  def indicesInExpr = Set()

  def dim = 0
}

case class IndexUpperBound(index: Index) extends Expr {
  override def toString: String = s"MAX($index)"

  def indicesInExpr = Set()

  def dim = 0
}

case class IndexSkipFunc(index: Index, indexExpr: Expr, dependencies: Seq[Expr], isSync: Boolean) extends Expr {
  assert(index.dependencies.size == dependencies.size, "This skip function has the wrong number of dependencies")

  override def toString: String = {
    s"${if (isSync) "Sync" else ""}SKIP$index($indexExpr, ${dependencies.mkString(", ")})"
  }

  override def indicesInExpr: Set[Index] = Set(index) ++ indexExpr.indicesInExpr ++ dependencies.flatMap(_.indicesInExpr)

  def dim = 0
}

case class IndexRange(start: Option[Expr], end: Option[Expr]) extends Expr {
  override def toString: String = {
    def optionStr(op: Option[Expr]): String = op match {
      case Some(o) => o.toString
      case None => ""
    }

    s"${optionStr(start)}->${optionStr(end)}"
  }

  def indicesInExpr = {
    (start match {
      case Some(id) => id.indicesInExpr
      case None => Set.empty[Index]
    }) ++ (end match {
      case Some(id) => id.indicesInExpr
      case None => Set.empty[Index]
    })
  }

  def fillInRange(other: IndexRange): IndexRange = (start, end, other) match {
    case (Some(_), Some(_), _) => this

    case (Some(selfStart), None, IndexRange(Some(oStart), Some(oEnd))) =>
      IndexRange(start, Some(selfStart + oEnd - oStart))

    case (None, Some(selfEnd), IndexRange(Some(oStart), Some(oEnd))) =>
      IndexRange(start, Some(selfEnd - oEnd + oStart))

    case (None, None, _) => throw new Exception("We don't allow None->None ranges for now")

    case _ => throw new Exception("too fancy of a range. we can't fill this in yet")
  }

  def dim: Int = 1
}

object -> extends IndexRange(None, None)
