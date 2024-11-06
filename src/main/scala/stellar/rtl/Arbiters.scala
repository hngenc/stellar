package stellar.rtl

import chisel3._
import chisel3.util._

import ChiselUtil._

class Dearbiter[T <: Data](gen: T, n: Int, hasSelect: Boolean = true) extends Module {
  // TODO is this class even used anywhere anymore? If not, remove it
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(getChiselType(gen)))
    val out = Vec(n, Decoupled(getChiselType(gen)))
    val select = if (hasSelect) Some(Input(UInt(log2Up(n).W))) else None
  })

  val chosen = if (hasSelect) io.select.get else PriorityEncoder(io.out.map(_.ready))
  val chosenOut = chosen % n.U

  io.in.ready := false.B
  io.out.zipWithIndex.foreach { case (out, outId) =>
    out.valid := io.in.valid && chosenOut === outId.U
    out.bits := io.in.bits

    when (chosenOut === outId.U) {
      io.in.ready := out.ready
    }
  }
}

class RotatingArbiter[T <: Data](gen: T, n: Int, trackBranches: Boolean = false, strictBranches: Boolean = true) extends Module {
  // TODO is this class even used anywhere anymore? If not, remove it
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, Decoupled(getChiselType(gen))))
    val out = Decoupled(getChiselType(gen))

    val startSpan = if (trackBranches) Some(Input(UInt(log2Up(n+1).W))) else None
    val isLast = if (trackBranches) Some(Input(Vec(n, Bool()))) else None

    val chosen = Output(UInt(log2Up(n).W))
  })

  val firstPriority = RegInit(0.U(log2Up(n).W))
  val noneChosen = WireInit(false.B)

  if (trackBranches) {
    val branchIsRunning = RegInit(VecInit(Seq.fill(n)(false.B)))

    val startSpan = Mux(firstPriority === 0.U, io.startSpan.get, 0.U)
    val starting = (0 until n).map(_.U < startSpan && !any(branchIsRunning))

    branchIsRunning.zip(starting).zip(io.isLast.get).zipWithIndex.foreach { case (((running, start), end), branchId) =>
      when (io.out.fire && io.chosen === branchId.U && end) {
        running := false.B
      }.elsewhen(start && io.out.ready) {
        running := true.B
      }
    }

    when (io.out.fire) {
      firstPriority := Mux(io.chosen +& 1.U >= n.U, 0.U, io.chosen +& 1.U)
    }.elsewhen (!branchIsRunning(firstPriority) || !strictBranches.B) {
      firstPriority := Mux(firstPriority +& 1.U >= n.U, 0.U, firstPriority +& 1.U)
    }

    if (strictBranches) {
      noneChosen := !starting.head && !branchIsRunning(firstPriority)
      io.chosen := Mux(starting.head, 0.U, firstPriority)
    } else {
      noneChosen := !starting.head && !any(branchIsRunning)
      io.chosen := Mux(starting.head, 0.U, Mux(branchIsRunning(firstPriority), firstPriority,
        MuxCase(n.U, branchIsRunning.zipWithIndex.map { case (bir, bid) => bir -> bid.U }
      )))
    }
  } else {
    firstPriority := Mux(firstPriority +& 1.U >= n.U, 0.U, firstPriority +& 1.U)
    io.chosen := Mux(io.in(firstPriority).valid, firstPriority, PriorityEncoder(io.in.map(_.valid)))
  }

  io.in.foreach(_.ready := false.B)
  io.out.valid := false.B
  io.out.bits := DontCare

  /* We would rather just have the line below, instead of the more complicted uncommented when-clause below, but that makes FIRRTL passes take an extremely long time, even though the two are equivalent
  when (!noneChosen) {
    io.out <> io.in(io.chosen)
  }
  */
  val chosenIn = io.chosen % n.U
  when (!noneChosen) {
    io.in.zipWithIndex.foreach { case (in, inId) =>
      when (chosenIn === inId.U) {
        io.out.valid := in.valid
        io.out.bits := in.bits
      }

      in.ready := chosenIn === inId.U && io.out.ready
    }
  }
}

class TopNArbiter(gen: => SpatialArrayOutPort, nInPorts: Int, nOutPorts: Int, orderByCoord: Option[Int], withAsserts: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(nInPorts, Decoupled(getChiselType(gen))))
    val out = Vec(nOutPorts, Decoupled(getChiselType(gen)))
  })

  io.in.foreach(_.ready := false.B)
  io.out.foreach(_.valid := false.B)
  io.out.foreach(_.bits := DontCare)

  val in_port_connected_to_out_port_id = VecInit(Seq.fill(io.in.size)(nOutPorts.U)) // This is just used for assertions

  orderByCoord match {
    case Some(orderByCoordId) =>
      io.in.zipWithIndex.foreach { case (in, inPortId) =>
        val outPortId = in.bits.coords(orderByCoordId).asUInt % nOutPorts.U
        when (in.bits.valid_copy) {
          io.out(outPortId) <> in
          if (withAsserts) in_port_connected_to_out_port_id(inPortId) := outPortId
        }
      }

    case None =>
      io.in.zipWithIndex.foldLeft(0.U(log2Up(nInPorts+1).W)) { case (outPortId, (in, inPortId)) =>
        when (outPortId < nOutPorts.U) {
          io.out(outPortId) <> in
          if (withAsserts) in_port_connected_to_out_port_id(inPortId) := outPortId
        }
        outPortId + in.bits.valid_copy
      }
  }

  if (withAsserts) {
    in_port_connected_to_out_port_id.zipWithIndex.foreach { case (connectedTo, inPortId) =>
      val next = in_port_connected_to_out_port_id.drop(inPortId+1)
      assert(!io.in(inPortId).valid || all(next.map(x => x === nOutPorts.U || x =/= connectedTo)), "multiple in-ports connected to same out-port")
    }

    assert(PopCount(io.in.map(_.valid)) <= nOutPorts.U, "can't connect all ports because there's too many of them firing at once")
  }
}
