package stellar.rtl

import chisel3._

object MatrixChiselUtil {
  // Chisel's const-prop is not as effective as we would like it to be; we try to compensate for that here by doing some
  //   const-prop of our own

  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  def matvecS(m: Iterable[Iterable[SInt]], v: Iterable[SInt]): Iterable[SInt] = {
    m.map(row => sumvS(row.zip(v).map {
      case (a, b) if a.litOption.nonEmpty && b.litOption.nonEmpty => (a.litOption.get * b.litOption.get).S
      case (a, b) if a.litOption.contains(0) || b.litOption.contains(0) => 0.S
      case (a, b) if a.litOption.contains(1) => b
      case (a, b) if b.litOption.contains(1) => a
      case (a, b) if a.litOption.contains(-1) => -b
      case (a, b) if b.litOption.contains(-1) => -a
      case (a, b) => a * b
    }.toSeq))
  }

  def sumvS(v: Seq[SInt]): SInt = {
    val literal = v.collect { case x if x.litOption.nonEmpty => x.litOption.get.toInt }.sum
    val unknowns = v.filter(_.litOption.isEmpty)
    if (unknowns.nonEmpty && literal != 0) unknowns.reduce(_ +& _) + literal.S
    else if (unknowns.nonEmpty) unknowns.reduce(_ +& _)
    else literal.S
  }

  def sumvU(v: Seq[UInt], expandWidths: Boolean = true): UInt = {
    val adder: (UInt, UInt) => UInt = if (expandWidths) {_ +& _} else {_ +% _}
    val literal = v.collect { case x if x.litOption.nonEmpty => x.litOption.get.toInt }.sum
    val unknowns = v.filter(_.litOption.isEmpty)
    if (unknowns.nonEmpty && literal != 0) unknowns.reduce(adder) + literal.U
    else if (unknowns.nonEmpty) unknowns.reduce(adder)
    else literal.U
  }

  def addvS(v1: Seq[SInt], v2: Seq[SInt]): Seq[SInt] = {
    require(v1.size == v2.size, "you can only add vectors with the same size")
    (v1 zip v2).map { case (a, b) => sumvS(Seq(a, b)) }
  }

  def addvU(v1: Seq[UInt], v2: Seq[UInt]): Seq[UInt] = {
    require(v1.size == v2.size, "you can only add vectors with the same size")
    (v1 zip v2).map { case (a, b) => sumvU(Seq(a, b)) }
  }
}
