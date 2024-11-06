package stellar.rtl

import chisel3._
import chisel3.experimental.BundleLiterals._

class Region extends Bundle {
  val is_regfile = Bool()
  val is_sram = Bool()
  val is_dram = Bool()

  val index = UInt(16.W) // TODO Unify this with Dma.sramCodeBits somehow
}

object Region {
  def sramRegion(sramCode: Int): Region =
    (new Region).Lit(_.is_regfile -> false.B, _.is_sram -> true.B, _.is_dram -> false.B, _.index -> sramCode.U)

  val rfRegion: Region =
    (new Region).Lit(_.is_regfile -> true.B, _.is_sram -> false.B, _.is_dram -> false.B, _.index -> 0.U)

  val dramRegion: Region =
    (new Region).Lit(_.is_regfile -> false.B, _.is_sram -> false.B, _.is_dram -> true.B)

  val nothingRegion: Region =
    (new Region).Lit(_.is_regfile -> false.B, _.is_sram -> false.B, _.is_dram -> false.B)
}
