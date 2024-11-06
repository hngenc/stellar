package stellar

class SpatialArray(size: Int = 2, boundsOverride: Option[Seq[Int]] = None, hasSubArrays: Boolean = false, ignoreBaseC2e: Boolean = false, alwaysStart: Boolean = false, val outputDelay: Int = 0, stallIfAnyInputsAreNotFound: Boolean = true, onlyCheckIfOutputsAreReady: Boolean = false, stallForOutputs: Boolean = true, val dataWidthBitsOpt: Option[Int] = None, val name: Option[String] = None) {
  // TODO hasSubArrays should ideally always be True

  implicit val block = new Block(upperBound = size, boundsListOverride = boundsOverride)
  implicit val blockOpt = Some(block)

  def elaborate(shouldPrint: Boolean = true, shouldRender: Boolean = true, shouldUnroll: Boolean = true, emitVerilog: Boolean = false) =
    block.elaborate(shouldPrint=shouldPrint, shouldRender=shouldRender, shouldUnroll=shouldUnroll, emitVerilog=emitVerilog)

  def toChiselModule(dataWidthBits: Int, withAsserts: Boolean) = block.genSpatialArray(ignoreBaseC2e, alwaysStart, stallIfAnyInputsAreNotFound, onlyCheckIfOutputsAreReady, stallForOutputs, dataWidthBits, withAsserts, hasSubArrays)

  def allIOPorts = block.allIOPortsOpt match {
    case Some(x) => x
    case None =>
      elaborate(shouldPrint = true, shouldRender = false, shouldUnroll = true, emitVerilog = false)
      block.allIOPortsOpt.get
  }

  def indices = block.indices
  def variables = block.ioVariables
  def transform = block.transform.get

  override def toString: String = block.toString
}
