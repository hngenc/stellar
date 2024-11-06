package stellar

import Util.SMap

abstract class Expr {
  def unary_! = Not(this)
  def asBool: BoolExpr = (this % 2) === 1

  def +(e: Expr) = Add(this, e)
  def -(e: Expr) = Add(this, Multiply(Const(-1), e))
  def *(e: Expr) = Multiply(this, e)
  def /(e: Expr) = Divide(this, e)
  def %(e: Expr) = Modulo(this, e)
  def ===(e: Expr) = Equality(left=this, right=e)
  def =/=(e: Expr) = Not(Equality(left=this, right=e))
  def &&(e: Expr) = And(left=this.asBool, right=e.asBool)
  def ||(e: Expr) = Or(left=this.asBool, right=e.asBool)
  def <(e: Expr) = LessThan(left=this, right=e)
  def >(e: Expr) = GreaterThan(left=this, right=e)
  def <=(e: Expr) = LessThanOrEq(left=this, right=e)
  def >=(e: Expr) = GreaterThanOrEq(left = this, right=e)
  def ->(e: Expr) = IndexRange(start=Some(this), end=Some(e))
  def -> = IndexRange(start=Some(this), end=None)

  def +(c: Int) = Add(this, Const(c))
  def -(c: Int) = Add(this, Const(-c))
  def *(c: Int) = Multiply(this, Const(c))
  def /(c: Int) = Divide(this, Const(c))
  def %(c: Int) = Modulo(this, Const(c))
  def ===(c: Int) = Equality(left=this, right=Const(c))
  def =/=(c: Int) = Not(Equality(left=this, right=Const(c)))
  def &&(c: Boolean): BoolExpr = this.asBool && c
  def <(c: Int) = LessThan(left=this, right=Const(c))
  def >(c: Int) = GreaterThan(left=this, right=Const(c))
  def <=(c: Int) = LessThanOrEq(left=this, right=Const(c))
  def >=(c: Int) = GreaterThanOrEq(left=this, right=Const(c))

  def dim: Int

  def indicesInExpr: Set[Index]
}

case class Const(const: Int) extends Expr {
  override def toString: String = const.toString
  def dim = 0
  def indicesInExpr = Set.empty[Index]
}

case class Add(ops: Expr*) extends Expr {
  assert(ops.size >= 2, "there must be at least two ops being added")

  override def +(e: Expr): Add = Add(ops :+ e:_*)

  override def toString: String = s"(${ops.mkString(" + ")})"

  def dim = ops.head.dim
  assert(ops.forall(_.dim == dim), s"Addition operands have different dimensions: ${ops.map(_.dim)} $toString")

  def indicesInExpr = ops.map(_.indicesInExpr).reduce(_ ++ _)
}

case class Multiply(ops: Expr*) extends Expr {
  assert(ops.size >= 2, "there must be at least two ops being multiplied")

  override def *(e: Expr): Multiply = Multiply(ops :+ e:_*)

  override def toString: String = ops match {
    case Seq(Const(-1), e) => s"-$e"
    case Seq(e, Const(-1)) => s"-$e"
    case _ => s"(${ops.mkString(" * ")})"
  }

  def dim = ops.head.dim
  assert(ops.forall(_.dim == dim), "multiplication operands have different dimensions")

  def indicesInExpr = ops.map(_.indicesInExpr).reduce(_ ++ _)
}

case class Divide(numer: Expr, denom: Expr) extends Expr {
  override def toString: String = s"($numer / $denom)"

  def dim = numer.dim
  assert(denom.dim == dim, "modulo operands have different dimensions")

  def indicesInExpr = Seq(numer,denom).map(_.indicesInExpr).reduce(_ ++ _)
}

case class Modulo(numer: Expr, denom: Expr) extends Expr {
  override def toString: String = s"($numer % $denom)"

  def dim = numer.dim
  assert(denom.dim == dim, "modulo operands have different dimensions")

  def indicesInExpr = Seq(numer,denom).map(_.indicesInExpr).reduce(_ ++ _)
}

case class Select(cond: BoolExpr, iftrue: Expr, iffalse: Expr) extends Expr {
  override def toString: String = s"($cond ? $iftrue : $iffalse)"

  def dim = {
    // assert(iftrue.dim == iffalse.dim) // TODO we should add this assertion back; I just removed it because it made the SpArch merger take forever to elaborate
    iftrue.dim
  }

  def indicesInExpr: Set[Index] = cond.indicesInExpr ++ iftrue.indicesInExpr ++ iffalse.indicesInExpr
}

object Select {
  def apply(cond: Boolean, iftrue: Expr, iffalse: Expr): Expr = if (cond) iftrue else iffalse
  def apply(cond: Expr, iftrue: Expr, iffalse: Expr): Expr = Select(cond.asBool, iftrue, iffalse)
  def apply(cond: Expr, iftrue: Int, iffalse: Expr): Expr = Select(cond, Const(iftrue), iffalse)
  def apply(cond: Expr, iftrue: Expr, iffalse: Int): Expr = Select(cond, iftrue, Const(iffalse))
  def apply(cond: Expr, iftrue: Int, iffalse: Int): Expr = Select(cond, Const(iftrue), Const(iffalse))
}

abstract class BoolExpr extends Expr {
  def ||(e: BoolExpr) = Or(this, e)
  def &&(e: BoolExpr) = And(this, e)

  override def &&(b: Boolean): BoolExpr = if (b) this else False
}

case class Not(expr: Expr) extends BoolExpr {
  override def toString: String = s"!($expr)"
  def dim = 0
  def indicesInExpr: Set[Index] = expr.indicesInExpr
}

case class Equality(left: Expr, right: Expr) extends BoolExpr {
  override def toString: String = s"($left == $right)"
  def dim = 0
  def indicesInExpr = left.indicesInExpr ++ right.indicesInExpr
}

case class LessThan(left: Expr, right: Expr) extends BoolExpr {
  override def toString: String = s"($left < $right)"
  def dim = 0
  def indicesInExpr = left.indicesInExpr ++ right.indicesInExpr
}

case class LessThanOrEq(left: Expr, right: Expr) extends BoolExpr {
  override def toString: String = s"($left <= $right)"
  def dim = 0
  def indicesInExpr = left.indicesInExpr ++ right.indicesInExpr
}

case class GreaterThan(left: Expr, right: Expr) extends BoolExpr {
  override def toString: String = s"($left > $right)"
  def dim = 0
  def indicesInExpr = left.indicesInExpr ++ right.indicesInExpr
}

case class GreaterThanOrEq(left: Expr, right: Expr) extends BoolExpr {
  override def toString: String = s"($left >= $right)"
  def dim = 0
  def indicesInExpr = left.indicesInExpr ++ right.indicesInExpr
}

case class Or(left: BoolExpr, right: BoolExpr) extends BoolExpr {
  override def toString: String = s"($left || $right)"
  def dim = 0
  def indicesInExpr: Set[Index] = left.indicesInExpr ++ right.indicesInExpr
}

case class And(left: BoolExpr, right: BoolExpr) extends BoolExpr {
  override def toString: String = s"($left && $right)"
  def dim = 0
  def indicesInExpr: Set[Index] = left.indicesInExpr ++ right.indicesInExpr
}

object True extends BoolExpr {
  override def toString: String = s"true"
  def dim = 0
  def indicesInExpr: Set[Index] = Set()
}

object False extends BoolExpr {
  override def toString: String = s"false"
  def dim = 0
  def indicesInExpr: Set[Index] = Set()
}

object ConstBool {
  def apply(b: Boolean): BoolExpr = if (b) True else False
}

case class Indexed(variable: Variable, indices: Seq[Expr]) extends Expr {
  def :=(expr: Expr)(implicit blockOpt: Option[Block]): Assignment = Assignment(dst=this, src=Passes(expr))
  def :=(c: Int)(implicit blockOpt: Option[Block]): Assignment = this := Const(c)

  def found = Found(this)
  def unavailable = Unavailable(this)
  def axisSpan(axisId: Int = 0) = AxisSpan(this, axisId)
  def coord(i: Int) = CoordOf(this, i)

  override def toString: String = s"${variable.name}[${indices.map(_.toString).mkString(",")}]"

  // assert(indices.size == variable.dimensions, s"Incorrect number of indices used for ${variable.name}")

  def dim: Int = indices.count(_.isInstanceOf[IndexRange])

  def indicesInExpr = indices.flatMap(_.indicesInExpr).toSet

  def fixedVals(boundsMap: SMap[Index, (Int, Int)]): Iterable[Option[Int]] = indices.map {
    // These are all the iterator values which are harcoded into this set of indices

    case dind if dind.indicesInExpr.isEmpty =>
      val replaced = Passes(Passes.replaceBounds(dind, boundsMap))
      replaced match {
        case Const(n) => Some(n)
        case _ => throw new Exception("Too complicated of an index expression for us to correctly fix")
      }
    case _ => None
  }

  val isIOInput: Boolean = variable.isInstanceOf[Input]
  val isIOOutput: Boolean = variable.isInstanceOf[Output]
  val isIntermediate: Boolean = variable.isInstanceOf[Intermediate]

  def outVariable: Output = variable match {
    case v: Output => v
    case _ => throw new Exception("not output")
  }

  def interVariable: Intermediate = variable match {
    case v: Intermediate => v
    case _ => throw new Exception("not intermediate")
  }
}

case class Found(indexed: Indexed) extends Expr {
  override def toString = s"${indexed}.found"
  override def dim = indexed.dim
  override def indicesInExpr = indexed.indicesInExpr
  require(indexed.isIOInput)
}

case class Unavailable(indexed: Indexed) extends Expr {
  override def toString = s"${indexed}.unavailable"
  override def dim = indexed.dim
  override def indicesInExpr = indexed.indicesInExpr
  require(indexed.isIOInput)
}

case class CoordOf(indexed: Indexed, coordId: Int) extends Expr {
  override def toString = s"${indexed}.coords[$coordId]"
  override def dim = indexed.dim
  override def indicesInExpr = indexed.indicesInExpr
  require(indexed.isIOInput)
}

case class AxisSpan(indexed: Indexed, axisId: Int) extends Expr {
  override def toString: String = s"${indexed}.axis_span[${axisId}]"
  override def dim: Int = indexed.dim
  override def indicesInExpr = indexed.indicesInExpr
}

object OutputPortsStalling extends Expr {
  override def toString: String = s"OutputPortsStalling"
  def dim = 0
  def indicesInExpr = Set.empty
}

object MaxVal extends Expr  {
  override def toString: String = s"MaxVal"
  def dim = 0
  override def indicesInExpr: Set[Index] = Set.empty
}

case class Custom(ops: Seq[Expr], toChisel: Seq[chisel3.SInt] => chisel3.SInt, name: String = "Custom") extends Expr {
  override def toString: String = s"$name(${ops.mkString(",")})"
  override def dim = ops.head.dim; assert(ops.forall(_.dim == dim), s"$name operands have different dimensions: ${ops.map(_.dim)} $toString")
  override def indicesInExpr: Set[Index] = ops.flatMap(_.indicesInExpr).toSet
}
