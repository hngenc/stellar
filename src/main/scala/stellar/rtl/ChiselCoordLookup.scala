package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._
import ChiselUtil._
import stellar.RfEntryExitOption

class ChiselCoordLookup[T <: Data](nElems: Int, val nInPorts: Int, nOutPorts: Int, nUpdatePorts: Int, coordT: T,
                                   nCoords: Int, lastInAxisFlags: Seq[(Int, Int)] = Seq.empty, nSubArrays: Int = 1,

                                   entryOption: RfEntryExitOption = RfEntryExitOption.Incrementing(), exitOption: RfEntryExitOption = RfEntryExitOption.Anywhere,
                                   subPipelineOpt: Option[(Int, Int, Boolean, Boolean)] = None, lockstep: Boolean = false,

                                   unavailableIfSmaller: Boolean = false, popIfEqual: Boolean = true,
                                   popIfSmallerThanHead: Seq[Int] = Seq.empty,

                                   coordsToIgnoreForOutputs: Set[Int] = Set.empty,

                                   withAsserts: Boolean = true) extends Module {
  /*
    Note: once this module receives a certain number of lookup requests from an SRAM (which is typically done to enable
    regfile to SRAM data transfers), it assumes that the Lookup unit is finished and new tuples can be inserted)
   */
  def coords_t = Vec(nCoords, coordT)
  class MappingT extends Bundle {
    val compressed = coords_t
    val expanded = coords_t
  }

  val io = IO(new Bundle {
    val insert = Vec(nInPorts, Flipped(Decoupled(new Bundle {
      val mapping = new MappingT
      val op_count = OpCount()
      val last = Bool()
      val last_in_axis = Input(Vec(nCoords, Bool()))
    })))

    val lookup = Vec(nOutPorts, new Bundle {
      val valid = Input(Bool())

      val compressed = Input(coords_t)
      val expanded = Output(coords_t)

      val found = Output(Bool())
      val unavailable = Output(Bool())

      val last = Input(Bool())
      val pop = Flipped(Valid(UInt(32.W))) // When we "pop" elements from regfiles, we can also specify which sub-array they are being popped from // TODO magic number

      val op_count = Input(OpCount())
    })

    val updates = Vec(nUpdatePorts, Flipped(Valid(new RegfileUpdatePort(nCoords))))

    val fillCount = Output(OpCount())
    val emptyCount = Output(OpCount())

    val isEmpty = Output(Bool())

    val n_elems_in_axis_lookup = new NElemsInAxisLookup(nCoords)
  })

  // This module is really just a somewhat hacky wrapper around the RegFile module
  val rf = Module(new ChiselRegFile(nElems = nElems, nInPorts = nInPorts, nOutPorts = nOutPorts, nUpdatePorts = nUpdatePorts,
    nIOCoords = nCoords, nDomainCoords = nCoords,
    lastInAxisFlags = lastInAxisFlags, // TODO the order of coords is reversed for regfiles compared to coord-lookups, which can make it tricky for the user to set the last-in-axis-flag-coord-ids
    nSubArrays = nSubArrays, entryOption = entryOption, exitOption = exitOption, subPipelineOpt = subPipelineOpt,
    lockstepIns = lockstep, lockstepOuts = if (lockstep) (1, nInPorts) else (0, 0), unavailableIfSmallerOpt = if (unavailableIfSmaller) Some((0, true, Set.empty)) else None,
    popIfEqual = popIfEqual, popIfSmallerThanHead = popIfSmallerThanHead, lastInAxisFlagsFromOutPorts = false,
    coordsToIgnoreForOutputs = coordsToIgnoreForOutputs, dataWidthBits = coords_t.getWidth, withAsserts = withAsserts))

  io.insert.zip(rf.io.ins).foreach { case (insert, rfIn) =>
    rfIn.valid := insert.valid
    insert.ready := rfIn.ready

    rfIn.bits.element.data := insert.bits.mapping.expanded.asTypeOf(rfIn.bits.element.data)

    rfIn.bits.coords := insert.bits.mapping.compressed
    rfIn.bits.op_count := insert.bits.op_count

    rfIn.bits.last_in_axis := insert.bits.last_in_axis
    rfIn.bits.increment_sticky_coord := false.B

    rfIn.bits.element.domainCoords := DontCare

    rfIn.bits.valid_copy := DontCare

    rfIn.bits.axis_spans := DontCare
  }

  io.lookup.zip(rf.io.outs).foreach { case (lookup, rfOut) =>
    rfOut.valid := lookup.valid

    rfOut.coords := lookup.compressed
    lookup.expanded := rfOut.data.asTypeOf(lookup.expanded)

    rfOut.pop.valid := lookup.pop.valid
    rfOut.pop.bits := lookup.pop.bits
    lookup.found := rfOut.found
    lookup.unavailable := rfOut.unavailable

    rfOut.op_count := lookup.op_count

    rfOut.last_in_axis.foreach(_ := false.B) // TODO we don't yet support last-in-axis flags for lookups
  }

  rf.io.last_in := io.insert.map(in => in.bits.last).reduce(_ || _)
  rf.io.last_out := io.lookup.map(lu => lu.last).reduce(_ || _)

  io.fillCount := rf.io.fillCount
  io.emptyCount := rf.io.emptyCount
  io.isEmpty := rf.io.isEmpty

  rf.io.updates <> io.updates

  rf.io.n_elems_in_axis_lookups.head <> io.n_elems_in_axis_lookup
}
