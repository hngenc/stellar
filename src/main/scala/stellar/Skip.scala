package stellar

class Skip(val index: Index, val cond: BoolExpr, val isOptimistic: Boolean)(implicit block: Block) {
  block.registerSkip(this)
  index.addSkip(this)

  override def toString: String = s"${if (isOptimistic) "Optimistic" else ""}Skip $index where $cond"
}

class SkipWithoutCond(index: Index, isSync: Boolean)(implicit block: Block) {
  def where(cond: BoolExpr): Skip = new Skip(index, cond, isSync)
}

object Skip {
  def apply(index: Index)(implicit block: Block) = new SkipWithoutCond(index, isSync = false)
}

object OptimisticSkip {
  def apply(index: Index)(implicit block: Block) = new SkipWithoutCond(index, isSync = true)
}
