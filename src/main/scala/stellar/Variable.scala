package stellar

import scala.collection.mutable.{ArrayBuffer, ArrayStack}

sealed abstract class Variable(val name: String = "<UNKNOWN>") {
  def apply(idx: Expr*): Indexed = Indexed(variable = this, indices = idx.toSeq)
  def apply(idx: Vector[Expr]): Indexed = apply(idx:_*)

  def isIO = this match {
    case _: Intermediate => false
    case _ => true
  }

  override def toString: String = name
}

class Input(val isCoordOfOpt: Option[(Input, Int /* coordId */, Boolean /* force */)] = None, val useCompressedCoords: Boolean = false, val unBoundedAccessesPermitted: Boolean = true, override val name: String = "<UNKNOWN>") extends Variable(name = name) {
  val popConds: ArrayBuffer[(Intermediate, Option[Intermediate], Option[Intermediate])] = ArrayBuffer.empty // [(popCond, Some(dstInterVar), Some(coord0MustEqual))]
  def setPopCond(c: Intermediate, dstInterVar: Option[Intermediate] = None, coord0MustEqual: Option[Intermediate] = None): Unit = {
    popConds += ((c, dstInterVar, coord0MustEqual))
  }

  val validConds: ArrayBuffer[(Intermediate, Option[Intermediate])] = ArrayBuffer.empty // [(validCond, Some(whenCoord0Equals)]
  def setValidCond(cond: Intermediate, whenCoord0Equals: Option[Intermediate] = None): Unit = {
    validConds += ((cond, whenCoord0Equals))
  }

  var lastInAxis: Option[Intermediate] = None
  def setLastInAxis(c: Intermediate): Unit = {
    lastInAxis = Some(c)
  }
}

class Output(override val name: String = "<UNKNOWN>") extends Variable(name = name) {
  var validCond: Option[Intermediate] = None
  def setValidCond(c: Intermediate): Unit = {
    validCond = Some(c)
  }

  var lastInAxisConds: ArrayBuffer[(Intermediate, Int)] = ArrayBuffer.empty // [(lastInAxisCond, axisId)]
  def setLastInAxis(c: Intermediate, axisId: Int = 0): Unit = {
    // TODO It might be fine for 'alsoWhenEnding' to always be true
    lastInAxisConds += ((c, axisId))
  }

  var axisSpanVariables: ArrayBuffer[(Intermediate, Int)] = ArrayBuffer.empty // [(span, axisId)]
  def setAxisSpan(span: Intermediate, axisId: Int = 0): Unit = {
    assert(!axisSpanVariables.map(_._2: Int).contains(axisId))
    axisSpanVariables += ((span, axisId))
  }
}

class Intermediate(val signalsEndingInnermostTimeAxis: Boolean = false, val signalsLocalEnding: Boolean = false, val signalsEnding: Boolean = false, override val name: String = "<UNKNOWN>")(implicit block: Block) extends Variable(name = name)
