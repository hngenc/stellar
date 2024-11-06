package stellar

object MatrixUtil {
  def determinant[T](m: Seq[Seq[T]])(implicit n: Numeric[T]): T = {
    import n._

    assert(m.nonEmpty, "input matrix to determinant is empty")
    assert(m.forall(r => r.size == m.size))

    if (m.size == 1) {
      m(0)(0)
    } else {
      val firstColumn = m.map(_(0))
      val withoutFirstColumn = m.map(_.tail)

      val partials = firstColumn.zipWithIndex.map { case (c, i) =>
        val sign = if (i % 2 == 0) n.one else -n.one
        val cofactor = withoutFirstColumn.take(i) ++ withoutFirstColumn.drop(i+1)

        sign * c * determinant(cofactor)
      }

      partials.sum
    }
  }

  def dotp[T](v1: Seq[T], v2: Seq[T])(implicit n: Numeric[T]): T = {
    import n._
    (v1 zip v2).map { case (a,b) => a * b }.sum
  }

  def lengthSquared[T: Numeric](v: Seq[T]): T = dotp(v, v)

  def addv(v1: Seq[Int], v2: Seq[Int]): Seq[Int] = {
    assert(v1.size == v2.size, s"you can only add vectors with the same size (v1.size = ${v1.size} | v2.size = ${v2.size})")
    (v1 zip v2).map { case (a,b) => a + b }
  }

  def subv(v1: Seq[Int], v2: Seq[Int]): Seq[Int] = addv(v1, v2.map(-_))

  def addvd(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = (v1 zip v2).map { case (a,b) => a + b }

  def subvd(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = addvd(v1, v2.map(-_))

  def matvec[T](m: Iterable[Iterable[T]], v: Iterable[T])(implicit n: Numeric[T]): Iterable[T] = {
    import n._
    m.map(_.zip(v).map { case (a,b) => a * b }.sum)
  }

  def matmul[T](m1: Seq[Seq[T]], m2: Seq[Seq[T]])(implicit n: Numeric[T]): Seq[Seq[T]] = {
    import n._

    assert(m1.forall(_.size == m2.size), "wrong shape matrix")

    for (row <- m1) yield
      for (col <- m2.transpose) yield
        (row zip col).map { case (a, b) => a * b}.sum
  }

  def matadd(m1: Seq[Seq[Int]], m2: Seq[Seq[Int]])(implicit n: Numeric[Int]): Seq[Seq[Int]] =
    m1.zip(m2).map { case (a,b) => MatrixUtil.addv(a,b) }

  def angleBetweenVectors(v1: Seq[Double], v2: Seq[Double]): Double = {
    Math.acos(dotp(v1, v2) / Math.sqrt(lengthSquared(v1) * lengthSquared(v2)))
  }
}
