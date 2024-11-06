package stellar.rtl

import scala.collection.mutable.ArrayBuffer
import chisel3._
import chisel3.util.{Decoupled, Valid}
import stellar.rtl.ChiselUtil.{UDValid, maxVal, reduceWidth}

class SpatialArrayInPort(val nIOCoords: Int, dataWidthBits: Int) extends Bundle {
  val data = chisel3.Input(SInt(dataWidthBits.W))
  val expanded_coords = chisel3.Input(Vec(nIOCoords, SInt(dataWidthBits.W)))

  val coords = chisel3.Output(Vec(nIOCoords, SInt(dataWidthBits.W)))
  val op_count = chisel3.Output(OpCount())

  val found = chisel3.Input(Bool())
  val unavailable = chisel3.Input(Bool())

  val pop = Valid(UInt(32.W)) // When we "pop" elements from regfiles, we can also specify which sub-array they are being popped from // TODO magic number
  val last_in_axis = chisel3.Output(Vec(nIOCoords, Bool()))

  val axis_spans = chisel3.Input(Vec(nIOCoords, UDValid(UInt(32.W))))

  val valid = chisel3.Output(Bool())

  // Const-prop assistants
  var hardCodedCoords: ArrayBuffer[Option[Int]] = ArrayBuffer.fill(nIOCoords)(None)
}

class SpatialArrayOutPort(val nIOCoords: Int, val nDomainCoords: Int, dataWidthBits: Int) extends Bundle {
  val element = new DataElement(nDomainCoords, dataWidthBits = dataWidthBits)

  val coords = Vec(nIOCoords, SInt(dataWidthBits.W))
  val op_count = OpCount()

  val last_in_axis = Vec(nIOCoords, Bool())
  val increment_sticky_coord = Bool()

  val axis_spans = Vec(nIOCoords, UDValid(UInt(32.W)))

  val valid_copy = Bool() // This is meant to simply be a copy of the "valid" signal coming from the PE. We use this to help avoid combinational-loops in a few places

  // Const-prop assistants
  var hardCodedCoords: ArrayBuffer[Option[Int]] = ArrayBuffer.fill(nIOCoords)(None)
  var hardCodedDomainCoords: ArrayBuffer[Option[Int]] = ArrayBuffer.fill(nDomainCoords)(None)
}

class RegfileUpdatePort(val nCoords: Int) extends Bundle {
  val from = new Bundle {
    val coords = Vec(nCoords, SInt(32.W)) // TODO magic number
    val op_count = OpCount()
  }

  val to = new Bundle {
    val coords = Vec(nCoords, SInt(32.W)) // TODO magic number
    val op_count = OpCount()
  }
}

class Compressed2ExpandedMapping(nCoords: Int, snooping: Boolean = false, var relevantIndices: Set[stellar.Index] = Set.empty) extends Bundle {
  def outDir[T <: Data](source: T) = if (snooping) Input(source) else Output(source)

  val compressed = outDir(Vec(nCoords, SInt(32.W))) // TODO magic number
  val expanded = Input(Vec(nCoords, SInt(32.W))) // TODO magic number
  val op_count = outDir(OpCount())

  val valid = outDir(Bool())

  val ready = if (snooping) Some(Output(Bool())) else None

  val found = if (snooping) None else Some(Input(Bool()))
  val unavailable = if (snooping) None else Some(Input(Bool()))

  def fire = valid && ready.getOrElse(true.B)
}

class LoadBalancingMappings(nConfigs: Int) extends Bundle {
  val configs = Flipped(Vec(nConfigs, Decoupled(Bool())))
  val opCount = Output(OpCount()) // TODO magic number
}

class OpCount extends Bundle {
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  import stellar.rtl.OpCount._

  val bits = UInt(bitwidth.W)

  def ===(other: OpCount): Bool = reduceWidth(this.bits, bitwidth) === reduceWidth(other.bits, bitwidth)
  def =/=(other: OpCount): Bool = reduceWidth(this.bits, bitwidth) =/= reduceWidth(other.bits, bitwidth)
  def >(other: OpCount): Bool = if (singleBit) this =/= other else if (isCircular) {
    val op1 = reduceWidth(bits, bitwidth)
    val op2 = reduceWidth(other.bits, bitwidth)
    Mux(op1 > op2, op1 - op2 < maxOpCountOffset.U, (op1 | opCountCarry.U) - op2 < maxOpCountOffset.U)
  } else (bits > other.bits)
  def >=(other: OpCount): Bool = if (isCircular) this === other || this > other else (bits >= other.bits)
  def <(other: OpCount): Bool = other > this
  def <=(other: OpCount): Bool = other >= this

  def +(u: UInt): OpCount = OpCount(bits + u)
  def -(u: UInt): OpCount = OpCount(bits - u)
  def -(other: OpCount): UInt = if (!singleBit && isCircular) {
    // In this function, we assume that you already know that 'this' is larger than (i.e. "ahead of") 'other'
    val op1 = reduceWidth(bits, bitwidth)
    val op2 = reduceWidth(other.bits, bitwidth)
    reduceWidth(Mux(op1 >= op2, op1 - op2, (op1 | opCountCarry.U) - op2), chisel3.util.log2Ceil(maxOpCountOffset+1))
  } else (bits - other.bits)
}
object OpCount {
  import chisel3.experimental.BundleLiterals._

  val singleBit: Boolean = false
  val isCircular: Boolean = false // This should be true for accelerators being physically-designed, but the nice thing about non-circular OpCounts is that they take up way fewer lines of Verilog, which can help with elaboration/simulation times
  if (singleBit) require(isCircular)

  val bitwidth: Int = if (singleBit) 1 else if (isCircular) 8 else 32
  val maxOpCountOffset: Int = 64

  val opCountCarry: BigInt = BigInt(1) << bitwidth

  def apply() = new OpCount
  def apply(u: UInt): OpCount = if (u.isLit) (new OpCount).Lit(_.bits -> u) else (u.asTypeOf(new OpCount))

  def assertInSync(x: OpCount, y: OpCount): Unit =
    if (singleBit) {
      // Nothing to check in this case
    } else if (isCircular)
      assert(x.bits - y.bits < maxOpCountOffset.U || y.bits - x.bits < maxOpCountOffset.U)
    else
      Seq(x,y).foreach(z => assert(z.bits < maxVal(UInt(bitwidth.W))))
}
