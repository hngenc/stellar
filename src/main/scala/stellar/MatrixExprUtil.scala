package stellar

object MatrixExprUtil {
  def determinant(m: Seq[Seq[Expr]]): Expr = {
    assert(m.nonEmpty)
    assert(m.forall(r => r.size == m.size))

    if (m.size == 1) {
      m(0)(0)
    } else {
      val firstColumn = m.map(_(0))
      val withoutFirstColumn = m.map(_.tail)

      val partials = firstColumn.zipWithIndex.map { case (c, i) =>
        val sign = if (i % 2 == 0) Const(1) else Const(-1)
        val cofactor = withoutFirstColumn.take(i) ++ withoutFirstColumn.drop(i+1)

        Multiply(sign, c, determinant(cofactor))
      }

      Passes(Add(partials:_*))
    }
  }

  def matmul(m1: Seq[Seq[Expr]], m2: Seq[Seq[Expr]]): Seq[Seq[Expr]] = {
    for (row <- m1) yield
      for (col <- m2.transpose) yield
        Passes(Add((row zip col).map { case (a, b) => a * b}:_*))
  }

  def areaOfParallelepipedSquared(vectors: Seq[Seq[Expr]]): Expr = {
    determinant(matmul(vectors, vectors.transpose))
  }

  def lengthSquared(vector: Seq[Expr]): Expr = {
    Passes(Add(vector.map(e => e * e):_*))
  }

  def nonParallel(vectors: Seq[Seq[Expr]]): Seq[Seq[Expr]] = {
    def lengthSquaredInt(vector: Seq[Expr]): Option[Int] = {
      val len = Passes(Add(vector.map(e => e * e):_*))

      len match {
        case Const(c) => Some(c)
        case _ => None
      }
    }

    def isParallel(v1: Seq[Expr], v2: Seq[Expr]): Boolean = {
      Passes(areaOfParallelepipedSquared(Seq(v1, v2))) == Const(0)
    }

    val sortedVectors: Seq[Seq[Expr]] = vectors.sortWith { case (x, y) =>
        val xLen = lengthSquaredInt(x)
        val yLen = lengthSquaredInt(y)

      (xLen, yLen) match {
        case (None, _) => true
        case (Some(_), None) => false
        case (Some(l1), Some(l2)) => l1 > l2
      }
    }

    sortedVectors.zipWithIndex.collect {
      case (v, i) if !sortedVectors.take(i).exists(v2 => isParallel(v, v2)) => v
    }
  }
}
