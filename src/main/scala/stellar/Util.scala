package stellar

object Util {
  type SMap[K,V] = scala.collection.Map[K,V]

  def crossJoin[T](list: Iterable[Iterable[T]]): Iterable[Iterable[T]] = {
    // From https://stackoverflow.com/a/42095955
    list.toList match {
      case xs :: Nil => xs map (Iterable(_))
      case x :: xs => for {
        i <- x
        j <- crossJoin(xs)
      } yield Iterable(i) ++ j
      case Nil => throw new Exception("empty crossjoin")
    }
  }

  def without[T](x: Iterable[T], index: Int): Iterable[T] = {
    x.take(index) ++ x.drop(index+1)
  }

  def factorial(n: Int): Int = (1 to n).product

  def lcm(a: Int, b: Int): Int = {
    val smaller = a.min(b)
    var result = smaller
    while (!(result % a == 0 && result % b == 0))
      result += smaller
    result
  }

  def overlaps(fixedVals: Iterable[Option[Int]], coords: Iterable[Int]) =
    fixedVals.zip(coords).forall { case (x, y) => x.getOrElse(y) == y }

  def noOverlap(excludedVals: Iterable[Iterable[Option[Int]]], coords: Iterable[Int]) =
    !excludedVals.exists(evs => overlaps(evs, coords))

  def min(exprs: Expr*): Expr = exprs.reduce { (acc, x) => Select(x < acc, x, acc) }

  def time[T <: Any](gen: => T): (T, Double) = {
    // Returns time in seconds that it takes to perform an operation
    val start = System.nanoTime()
    val result = gen
    val end = System.nanoTime()
    (result, (end - start) / 1e9)
  }

  def gcf(nums: Seq[Int]): Int = {
    (nums.min to 1 by -1).find(n => nums.forall(_ % n == 0)).get
  }
}
