package stellar.rtl

import chisel3._

class DataElement(val nDomainCoords: Int, dataWidthBits: Int) extends Bundle {
  val data = SInt(dataWidthBits.W)
  val domainCoords = Vec(nDomainCoords, SInt(32.W))
}
