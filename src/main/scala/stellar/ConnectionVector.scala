package stellar

case class ConnectionVector(srcIndexed: Indexed, dstVar: Intermediate, diff: Iterable[Int], dstFixedVals: Iterable[Option[Int]]) {
  // "dstFixedVals" are the exact values that the indexed "dst" needs to have for the connection vector to exist.

  val srcVar = srcIndexed.variable

  def transform(tr: Iterable[Iterable[Int]]): ConnectionVector = copy(
    diff = MatrixUtil.matvec(tr, diff)
  )

  override def toString: String = s"{$dstVar <- $srcIndexed | (${diff.mkString(",")})}"
}
