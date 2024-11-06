package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._

object ChiselUtil {
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  def wrappingIncrement(x: SInt, min: SInt, max: Int): (SInt, Bool) = {
    // min and max are both inclusive

    val inc_x = x +& 1.S
    val wraparound = inc_x > max.S
    val next_x = Mux(wraparound, min, inc_x)

    (next_x, wraparound)
  }

  def wrappingIncrement(xs: Vec[SInt], maxValuePerAxis: Seq[Int], minVector: Seq[Int], maxVector: Seq[Int], forceInnermostWraparound: Bool = false.B): (Vec[SInt], Bool) = {
    // min and max are both inclusive
    // TODO should this be unified with scalarVecAdd?

    val dim = xs.size
    require(xs.size == dim && maxValuePerAxis.size == dim && minVector.size == dim && maxVector.size == dim)

    val next = Wire(getChiselType(xs))
    val wraparound = Wire(Vec(dim, Bool()))
    val shifted_inc = true.B +: wraparound

    for (((((x, max), n), w), sh_inc) <- xs.zip(maxValuePerAxis).zip(next).zip(wraparound).zip(shifted_inc)) {
      val inc = x +& sh_inc.zext
      w := inc > max.S
      n := Mux(w, 0.S, inc)
    }

    when (forceInnermostWraparound) {
      next.headOption.foreach(_ := 0.S)
      wraparound.headOption.foreach(_ := true.B)
    }

    val completed = compareVecsSigned(maxVector.map(_.S), next, {_ < _}) ||
      wraparound.lastOption.getOrElse(true.B)
    val result = Mux(completed, {
      // Instead of just writing 'VecInit(minVector.map(_.S))', we do this weird rigamorole just to satisfy Chisel's
      // width-checkers
      val minVector_ = Wire(getChiselType(xs))
      minVector_ := minVector.map(_.S)
      minVector_
    }, next)

    (result, completed)
  }

  def floorSub(x: UInt, y: UInt): UInt = Mux(x < y, 0.U, x - y)

  def floored(x: SInt): SInt = Mux(x >= 0.S, x, 0.S)

  def minOf(xs: UInt*): UInt = if (xs.size == 1) xs.head else xs.reduce((acc, x) => Mux(acc < x, acc, x))
  def minOf(xs: SInt*): SInt = if (xs.size == 1) xs.head else xs.reduce((acc, x) => Mux(acc < x, acc, x))
  def minOf(xs: OpCount*): OpCount = if (xs.size == 1) xs.head else xs.reduce((acc, x) => Mux(acc < x, acc, x))

  def maxOf(xs: UInt*): UInt = if (xs.size == 1) xs.head else xs.reduce((acc, x) => Mux(acc > x, acc, x))

  def align(x: UInt, alignTo: Int, up: Boolean = false): UInt = {
    require(isPow2(alignTo), s"$alignTo is not a power of 2")
    val upInc = if (up) 1.U else 0.U
    val width = if (up) x.getWidth + 1 else x.getWidth
    (((x / alignTo.U) +& upInc) * alignTo.U).asTypeOf(UInt(width.W))
  }

  def alignS(x: SInt, alignTo: Int, up: Boolean = false): SInt = {
    require(isPow2(alignTo))
    val upInc = if (up) 1.S else 0.S
    ((x >> log2Ceil(alignTo)).asSInt +& upInc) * alignTo.S
  }

  def any(xs: Iterable[Bool]): Bool = {
    // If we weren't doing such aggressive const-prop ourselves, this whole function would just be the one line below:
    //    xs.foldLeft(false.B)(_ || _)
    if (xs.isEmpty) {
      false.B
    } else {
      val literal = xs.collect { case x if x.litOption.nonEmpty => x.litOption.get != 0 }.exists(x => x)
      val unknowns = xs.filter(_.litOption.isEmpty)
      if (literal) true.B
      else if (unknowns.nonEmpty) unknowns.reduce(_ || _)
      else false.B
    }
  }

  def all(xs: Iterable[Bool]): Bool = {
    // If we weren't doing such aggressive const-prop ourselves, this whole function would just be the one line below:
    //    xs.foldLeft(true.B)(_ && _)
    if (xs.isEmpty) {
      true.B
    } else {
      val literal = xs.collect { case x if x.litOption.nonEmpty => x.litOption.get != 0 }.forall(x => x)
      val unknowns = xs.filter(_.litOption.isEmpty)
      if (!literal) false.B
      else if (unknowns.nonEmpty) unknowns.reduce(_ && _)
      else true.B
    }
  }

  def compareVecs(xs: Seq[UInt], ys: Seq[UInt], f: (UInt, UInt) => Bool, orEquals: Boolean = false): Bool = {
    val equals_bv = xs.zip(ys).map { case (x, y) => x === y }
    val op_bv = (f(xs.head, ys.head) || (orEquals.B && equals_bv.head)) +: xs.tail.zip(ys.tail).map { case (x, y) => f(x,y) }

    op_bv.zipWithIndex.map { case (op, id) =>
      op && all(equals_bv.drop(id+1))
    }.reduce(_ || _)
  }

  def compareVecsSigned(xs: Seq[SInt], ys: Seq[SInt], f: (SInt, SInt) => Bool, orEquals: Boolean = false): Bool = {
    // TODO unify this with "compareVecs"
    val equals_bv = xs.zip(ys).map { case (x, y) => x === y }
    val op_bv = (f(xs.head, ys.head) || (orEquals.B && equals_bv.head)) +: xs.tail.zip(ys.tail).map { case (x, y) => f(x,y) }

    op_bv.zipWithIndex.map { case (op, id) =>
      op && all(equals_bv.drop(id+1))
    }.reduce(_ || _)
  }

  def vecEquals(xs: Seq[SInt], ys: Seq[SInt]): Bool = {
    // TODO should we unify this with compareVecs?
    all(xs.zip(ys).map {
      case (x, y) if x.litOption.nonEmpty && y.litOption.nonEmpty => (x.litOption.get == y.litOption.get).B
      case (x, y) => x === y
    })
  }

  def vecEquals(xys: Seq[(SInt, SInt)]): Bool = {
    val xs = xys.map(_._1)
    val ys = xys.map(_._2)
    vecEquals(xs, ys)
  }

  def vecEqualsU(xs: Seq[UInt], ys: Seq[UInt]): Bool = {
    // TODO should we unify this with compareVecs?
    all(xs.zip(ys).map {
      case (x, y) if x.litOption.nonEmpty && y.litOption.nonEmpty => (x.litOption.get == y.litOption.get).B
      case (x, y) => x === y
    })
  }

  def scalarVecAdd(s: UInt, v: Seq[UInt], maxes: Seq[UInt]): Seq[UInt] = {
    // maxes are exclusive
    assert(all(maxes.map(_ > 0.U)))

    val carries = VecInit(Seq.fill(v.size+1)(0.U))
    carries.head := s

    v.zip(maxes).zip(carries).zip(carries.tail).map { case (((x, m), carry), next_carry) =>
      val sum = x + carry
      next_carry := Mux(sum >= m, 1.U, 0.U)
      val result = sum - (next_carry * m)

      assert(result === sum % m && next_carry === sum / m)

      result
    }
  }

  def getChiselType[T <: Data](t: T): T = t.cloneType

  def maxVal(s: SInt): SInt = {
    require(s.getWidth >= 1)
    ((BigInt(1) << (s.getWidth-1)) - 1).S(s.getWidth.W)
  }

  def maxVal(u: UInt): UInt = {
    ((BigInt(1) << u.getWidth) - 1).U(u.getWidth.W)
  }

  def connectVecs[T <: Data](dst: Seq[T], src: Seq[T], fillIn: Option[Data] = None, biconnect: Boolean = false): Unit = {
    dst.zip(src).foreach { case (d, s) =>
      if (biconnect) d <> s
      else d := s
    }
    fillIn.foreach { fill =>
      dst.drop(src.size).foreach { d =>
        d := fill
      }
    }
  }

  def connectVecOfVecs[T <: Data](dst: Vec[Vec[T]], src: Seq[Seq[T]]): Unit = {
    dst.zip(src).foreach { case (d, s) =>
      d.zip(s).foreach { case (_d, _s) =>
        _d := _s
      }
    }
  }

  def connectVecOfVecsOfVecs[T <: Data](dst: Vec[Vec[Vec[T]]], src: Seq[Seq[Seq[T]]]): Unit = {
    (dst zip src).foreach { case (dst1, src1) => (dst1 zip src1).foreach { case (dst2, src2) => (dst2 zip src2).foreach { case (d,s) => d := s }}}
  }

  def dropFromVec[T <: Data](v: Vec[T], n: UInt, start: UInt = 0.U, fillOpt: Option[T] = None): Vec[T] = {
    val result = WireInit(v)
    result.zipWithIndex.foreach { case (r, i) =>
      when (i.U >= start) {
        when (i.U +& n >= v.size.U) {
          r := fillOpt.getOrElse(0.U.asTypeOf(v.head))
        }.otherwise {
          r := v(i.U +& n)
        }
      }
    }
    result
  }

  def rotateVec[T <: Data](vec: Vec[T], n: UInt): Vec[T] = {
    val bitwidth = vec.head.getWidth
    val lo = (vec.asUInt >> (n * bitwidth.U)).asUInt
    val hi = (vec.asUInt << ((vec.size.U - n) * bitwidth.U)).asUInt
    (lo | hi).asTypeOf(vec)
  }

  def reduceWidth(u: UInt, w: Int): UInt = noPrefix {
    u.widthOption match {
      case Some(w2) if w2 > w => u(w-1,0)
      case None => u.asTypeOf(UInt(w.W))
      case _ => u
    }
  }

  def vecContains(vec: Seq[UInt], u: UInt): Bool = if (u.isLit && vec.flatMap(_.litOption).contains(u.litValue)) true.B
    else if (u.isLit && vec.forall(_.isLit)) false.B
    else any(vec.map(_ === u))
  def vecIndexOf(vec: Seq[UInt], u: UInt): UInt = if (vec.isEmpty) 0.U else MuxCase((vec.size - 1).U, vec.init.zipWithIndex.map { case (elem, i) => (elem === u) -> i.U })

  def priorityEncoderWithPosition(in: Seq[Bool], pos: Int): UInt =
    priorityEncoderWithPosition(VecInit(in).asUInt, pos)

  def priorityEncoderWithPosition(in: UInt, pos: Int): UInt = {
    if (pos == 0) {
      PriorityEncoder(in)
    } else {
      val lower = priorityEncoderWithPosition(in, pos-1) +& 1.U
      PriorityEncoder(in >> lower) +& lower
    }
  }

  def popCountWhileTrue(xs: Seq[Bool], start: UInt = 0.U): UInt = {
    val end = WireInit(xs.size.U)
    xs.zipWithIndex.reverse.foreach { case (x, i) =>
      when (i.U >= start && !x) {
        end := i.U
      }
    }

    end - start
  }

  def selectFrom[T <: Data](v: Seq[T], u: UInt): T = if (v.size == 1) v.head else (u.litOption match {
    case Some(c) => v(c.toInt)
    case _ => MuxCase(v.last, v.init.zipWithIndex.map { case (e, i) =>
      (i.U === u) -> e
    })
  })

  def selectIds[T <: Data](v: Vec[T], s: Seq[UInt]): Seq[T] = {
    s.map(id => Mux(id >= v.size.U, 0.U.asTypeOf(v.head), v(id)))
  }

  def vecOr(vs: Seq[Bool]*): Vec[Bool] = {
    val nBits = vs.head.size
    require(vs.forall(_.size == nBits))
    VecInit.tabulate(nBits) { i =>
      any(vs.map(_(i)))
    }
  }

  def vecAnd(vs: Seq[Bool]*): Vec[Bool] = {
    val nBits = vs.head.size
    require(vs.forall(_.size == nBits))
    VecInit.tabulate(nBits) { i =>
      all(vs.map(_(i)))
    }
  }

  class UDValid[T <: Data](gen: T) extends Bundle {
    // An undirectioned version of the Valid class
    val valid = Bool()
    val bits = gen

    def same_as(other: UDValid[T]): Bool = {
      !valid && !other.valid || valid && other.valid && bits === other.bits
    }
  }
  object UDValid{
    def apply[T <: Data](gen: T) = new UDValid(gen)
    def from[T <: Data](directioned: ValidIO[T]) = {
      val result = Wire(new UDValid[T](getChiselType(directioned.bits)))
      result.valid := directioned.valid
      result.bits := directioned.bits
      Mux(true.B, result, result) // The "Mux" here is just meant to make sure we can't set anything to the result of this externally
    }
  }
}
