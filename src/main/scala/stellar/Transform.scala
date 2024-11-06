package stellar

class Transform(val nTimeAxes: Int, transform: Int*)(implicit block: Block) {
  require(nTimeAxes >= 0)

  val dim = Math.sqrt(transform.size).toInt
  val spaceDim = dim - nTimeAxes

  val tr: Seq[Seq[Int]] = transform.grouped(dim).toSeq
  val spaceTr = tr.dropRight(nTimeAxes)
  val timeTr = tr.takeRight(nTimeAxes)

  tr.foreach(t => assert(t.size == tr.size, "Space-time transform must be square"))
  block.registerTransform(this)

  def size: Int = tr.size

  def determinant(m: Seq[Seq[Int]] = tr): Int = {
    assert(m.nonEmpty && m.head.nonEmpty)

    if (m.size == 1) {
      m(0)(0)
    } else {
      val firstColumn = m.map(_(0))
      val withoutFirstColumn = m.map(_.tail)

      val partials = firstColumn.zipWithIndex.map { case (c, i) =>
        val sign = if (i % 2 == 0) 1 else -1.toInt
        val cofactor = withoutFirstColumn.take(i) ++ withoutFirstColumn.drop(i+1)

        sign * c * determinant(cofactor)
      }

      partials.sum
    }
  }

  val isInvertible: Boolean = determinant() != 0
  assert(isInvertible, "transformation must be invertible")

  private val cofactor: Seq[Seq[Int]] = {
    if (dim == 1) {
      tr
    } else {
      for (i <- 0 until dim) yield {
        for (j <- 0 until dim) yield {
          val sign = if ((i + j) % 2 == 0) 1 else (-1).toInt
          val dropRow = tr.take(i) ++ tr.drop(i + 1)
          val dropCol = dropRow.map(r => r.take(j) ++ r.drop(j + 1))

          sign * determinant(dropCol)
        }
      }
    }
  }

  val inverse: Seq[Seq[Int]] = {
    val det = determinant()
    val cot = cofactor.transpose
    cot.map(_.map(_ / det))
  }

  def spaceTransform(vertex: Seq[Int]): Seq[Int] = {
    for (row <- spaceTr)
      yield (for ((a,b) <- row zip vertex) yield a*b).sum
  }

  def spaceTransformExpr(vertex: Seq[Expr]): Seq[Expr] = {
    for (row <- spaceTr)
      yield Passes(Add((for ((a,b) <- row zip vertex) yield Const(a)*b):_*))
  }

  def spaceTimeTransform(vertex: Seq[Int]): Seq[Int] = {
    for (row <- tr)
      yield (for ((a,b) <- row zip vertex) yield a*b).sum
  }

  val spatiallyBoundIterators: Seq[Boolean] = {
    val b = spaceTr.map(_.map(_ != 0))

    def orRows(x: Seq[Boolean], y: Seq[Boolean]): Seq[Boolean] = (x zip y).map { case (a,b) => a || b }

    if (b.isEmpty) Seq()
    else b.reduce(orRows)
  }

  override def toString: String = s"[\n${tr.map("\t" + _.mkString(",")).mkString("\n")}\n]"
}

object Transform {
  def apply(transform: Int*)(implicit block: Block): Transform = new Transform(nTimeAxes = 1, transform: _*)
  def withMultipleTimeAxes(nTimeAxes: Int)(transform: Int*)(implicit block: Block): Transform = new Transform(nTimeAxes = nTimeAxes, transform: _*)
  def withoutTime(transform: Int*)(implicit block: Block): Transform = new Transform(nTimeAxes = 0, transform: _*)
}
