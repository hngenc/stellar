package stellar

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

import stellar.rtl._
import TestUtil._

class CoordConversionTests extends AnyFreeSpec with ChiselScalatestTester {
  "Coordinate lookup works" in {
    val dim = 2
    val dim_size = 2

    val nElems = Math.pow(dim_size, dim).toInt
    val nOutPorts = 32
    val coordT = SInt(32.W)

    val golden_coords = {
      val coords = Seq.fill(dim)(ArrayBuffer.empty[Int])
      for (row <- 0 until dim) {
        coords(row) += randInt(max=10, bias=0)
        for (_ <- 0 until dim-1) {
          coords(row) += coords(row).last + randInt(max=10, bias=1)
        }
      }
      coords.toSeq
    }

    test(new ChiselCoordLookup(nElems=nElems, nInPorts=dim_size, nOutPorts=nOutPorts, nUpdatePorts=0, coordT=coordT, nCoords=dim)) { c =>
      resetModule(c)

      // Insert mappings
      for (i <- 0 until dim) {
        for (j <- 0 until dim) {
          c.io.insert(j).valid.poke(true.B)

          c.io.insert(j).bits.mapping.compressed(0).poke(j.S)
          c.io.insert(j).bits.mapping.compressed(1).poke(i.S)

          c.io.insert(j).bits.mapping.expanded(0).poke(golden_coords(i)(j).S)
          c.io.insert(j).bits.mapping.expanded(1).poke(i.S)

          c.io.insert(j).bits.last.poke((i == (dim-1)).B)

          c.io.insert(j).ready.expect(true.B)
        }
        c.clock.step()
      }

      c.io.insert.head.ready.expect(false.B)

      // Lookup mappings (from spatial array)
      for (i <- 0 until nOutPorts) {
        val outer = randInt(dim, 0)
        val inner = randInt(dim, 0)

        c.io.lookup(i).valid.poke(true.B)
        c.io.lookup(i).last.poke(false.B)

        c.io.lookup(i).compressed(0).poke(inner.S)
        c.io.lookup(i).compressed(1).poke(outer.S)

        c.io.lookup(i).expanded(0).expect(golden_coords(outer)(inner).S)
        c.io.lookup(i).expanded(1).expect(outer.S)
      }
      c.clock.step()

      // Lookup mappings (from SRAM)
      for (i <- 0 until dim) {
        c.io.lookup.foreach(_.valid.poke(false.B))
        c.io.lookup.foreach(_.last.poke((i == dim-1).B))

        for (j <- 0 until dim) {
          c.io.lookup(j).valid.poke(true.B)

          c.io.lookup(j).compressed(0).poke(j.S)
          c.io.lookup(j).compressed(1).poke(i.S)

          c.io.lookup(j).expanded(0).expect(golden_coords(i)(j).S)
          c.io.lookup(j).expanded(1).expect(i.S)
        }

        c.clock.step()
      }

      c.io.insert.head.ready.expect(true.B)
    }
  }
}
