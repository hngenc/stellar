package stellar

import java.util.Random
import scala.collection.mutable.ArrayBuffer
import chisel3._
import chisel3.util.DecoupledIO
import chiseltest._
import stellar.rtl._

object TestUtil {
  val reset_cycles = 100

  def resetModule(mod: Module): Unit = {
    mod.reset.poke(true.B)
    mod.clock.step(reset_cycles)
    mod.reset.poke(false.B)
  }

  def resetSpatialArray(mod: ChiselSpatialArray): Unit = {
    mod.io.rfFillCounters.foreach(_.bits.poke(0.U))
    resetModule(mod)
    mod.clock.step(reset_cycles) // Give time for "time" counters in PEs to decrement down to 0
  }

  def startSpatialArray(mod: ChiselSpatialArray): Unit = {
    mod.io.rfFillCounters.foreach(_.bits.poke(1.U))
    mod.io.ins.foreach(_.found.poke(true.B))
    mod.io.outs.foreach(_.ready.poke(true.B))
    mod.io.compressed2ExpandedMappings.foreach(_.found.foreach(_.poke(true.B)))
  }

  def resetAccelerator(mod: ChiselAccelerator): Unit = {
    resetModule(mod)
    mod.clock.step(reset_cycles) // Give time for "time" counters in PEs to decrement down to 0
  }

  def randInt(max: Int = 100, bias: Int = -50)(implicit rand: Random = scala.util.Random.self) = {
    rand.nextInt(max) + bias
  }

  def randBool(probTrue: Double = 0.5)(implicit rand: Random = scala.util.Random.self) = {
    rand.nextDouble() < probTrue
  }

  def waitWhileBusy(mod: ChiselAccelerator, max_cycles: Int = -1, print_cycles: Boolean = false): Unit = {
    var cycle = 0
    while (mod.io.busy.peek().litToBoolean) {
      mod.clock.step()

      cycle += 1
      if (print_cycles && cycle % 10 == 0)
        println(s"Cycle: $cycle")
      assert(max_cycles < 0 || cycle < max_cycles, "timeout in waitWhileBusy")
    }
  }

  def waitUntilReady(clock: Clock, ready: Bool, max_cycles: Int = -1, print_cycles: Boolean = false): Unit = {
    var cycle = 0
    while (!ready.peekBoolean()) {
      clock.step()

      cycle += 1
      if (print_cycles && cycle % 10 == 0)
        println(s"Cycle: $cycle")
      assert(max_cycles < 0 || cycle < max_cycles, "timeout in waitUntilReady")
    }
  }

  def maxVal(u: UInt): BigInt = {
    val size = u.getWidth
    (BigInt(1) << size) - 1
  }

  def pokeMaxVal(u: UInt): Unit = {
    u.poke(maxVal(u))
  }

  def read_out_2d_csr_matrix[T <: Data](mod: ChiselAccelerator, sram: SRAM, result_data: Seq[ArrayBuffer[Int]], result_coords: Seq[ArrayBuffer[Int]], result_row_ids: ArrayBuffer[Int], baseHead: Int = 0): Unit = {
    require(LinkedListMetadata.head_ptr_buffer_id == CompressedMetadata.outer_metadata_buffer_id && LinkedListMetadata.coord_buffer_id == CompressedMetadata.inner_metadata_buffer_id)

    val nRows = result_row_ids.size - 1

    val read_req = mod.getReadReqPortForSram(sram)
    val read_resp = mod.getReadRespPortsForSram(sram)

    read_req.bits.axis.poke(0.U)
    read_req.bits.to_regfile.poke(false.B)
    read_req.bits.reset_running_state.poke(false)

    read_req.bits.address.foreach(_.poke(0.U))

    pokeMaxVal(read_req.bits.spans(0))
    read_req.bits.spans.drop(1).foreach(_.poke(1.U))

    read_req.bits.data_strides(0).poke(1.U)
    read_req.bits.data_strides.tail.foreach(_.poke(0.U))

    read_req.bits.metadata_strides.foreach(_.foreach(_.foreach(_.poke(0.U))))
    read_req.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_.poke(0))))
    read_req.bits.metadata_strides(1)(0)(LinkedListMetadata.head_ptr_buffer_id).poke(1.U)

    read_req.bits.iteration_strides.foreach(_.poke(1.U))

    read_resp.ready.poke(true.B)

    var req_row = 0
    var requested_coords = false
    var requested_row_id = false
    while (req_row < nRows || req_row == nRows && !requested_row_id || mod.io.busy.peekBoolean()) {
      /* We first request the row id, then the coords, then the data
       */
      read_req.valid.poke((req_row < nRows || req_row == nRows && !requested_row_id).B)
      read_req.bits.address(1).poke(baseHead + req_row)
      if (requested_row_id)
        read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.coord_buffer_id.U)
      else
        read_req.bits.metadata_buffer_id.poke(LinkedListMetadata.head_ptr_buffer_id.U)
      read_req.bits.is_data.poke(requested_row_id && requested_coords)

      if (read_req.ready.peek().litToBoolean) {
        if (requested_coords) {
          req_row += 1
          requested_row_id = false
          requested_coords = false
        } else if (requested_row_id) {
          requested_coords = true
        } else {
          requested_row_id = true
        }
      }

      if (read_resp.valid.peek().litToBoolean) {
        if (read_resp.bits.metadata_buffer_id.peek().litValue != LinkedListMetadata.head_ptr_buffer_id) {
          val cols = read_resp.bits.spans(0).peek().litValue.toInt
          val rows = read_resp.bits.spans(1).peek().litValue.toInt
          assert(rows == 1)

          val data = read_resp.bits.data

          val row = read_resp.bits.compressed_address(1).peek().litValue.toInt - baseHead
          val col = read_resp.bits.compressed_address(0).peek().litValue.toInt

          if (read_resp.bits.is_data.peekBoolean()) {
            for (c <- 0 until cols)
              result_data(row)(col + c) = data(c).peek().litValue.toInt
          } else {
            for (c <- 0 until cols)
              result_coords(row)(col + c) = data(c).peek().litValue.toInt
          }
        } else {
          val row = read_resp.bits.compressed_address(1).peek().litValue - baseHead
          assert(read_resp.bits.spans.peek().forall(_.litValue == 1))
          result_row_ids(row.toInt) = read_resp.bits.data.head.peek().litValue.toInt
        }
      }

      mod.clock.step()
    }

    read_req.valid.poke(false.B)
    read_resp.ready.poke(false.B)
  }
}
