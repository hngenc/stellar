package stellar.rtl

import chisel3._
import chisel3.util._

import ChiselUtil._

class NoMuxQueue[T <: Data](t: T, n: Int) extends Module {
  // We use a lot of nested Vecs in this project. Chisel gets really angry when you try to Mux these Vecs, which
  // unfortunately makes it impossible to use normal Queues for them. This module is just your bog-standard queue,
  // except that it uses for-loops and when-statements in place of Muxes
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(t))
    val deq = Decoupled(t)
  })

  val entries = Reg(Vec(n, t))

  val wptr = RegInit(0.U(log2Up(n).W))
  val rptr = RegInit(0.U(log2Up(n).W))
  val is_empty = RegInit(true.B)
  val is_full = wptr === rptr && !is_empty

  io.enq.ready := !is_full
  io.deq.valid := !is_empty

  io.deq.bits := entries.head
  entries.zipWithIndex.foreach { case (entry, i) =>
    when (rptr === i.U) {
      io.deq.bits := entry
    }
  }

  when (io.enq.fire) {
    entries.zipWithIndex.foreach { case (entry, i) =>
      when (wptr === i.U) {
        entry := io.enq.bits
      }
    }

    wptr := Mux(wptr + 1.U >= n.U, 0.U, wptr + 1.U)

    when (!io.deq.fire) {
      is_empty := false.B
    }
  }

  when (io.deq.fire) {
    val next_rptr = Mux(rptr + 1.U >= n.U, 0.U, rptr + 1.U)
    rptr := next_rptr

    when (!io.enq.fire && next_rptr === wptr) {
      is_empty := true.B
    }
  }
}

object NoMuxQueue {
  def apply[T <: Data](enq: DecoupledIO[T], n: Int): DecoupledIO[T] = {
    val mod = Module(new NoMuxQueue(getChiselType(enq.bits), n))
    mod.io.enq <> enq
    mod.io.deq
  }
}
