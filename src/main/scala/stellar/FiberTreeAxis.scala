package stellar

import chisel3.experimental.ChiselEnum
import chisel3.util.log2Ceil

object FiberTreeAxis extends ChiselEnum {
  val Dense, Compressed, LinkedList, Bitvector = Value
}

sealed trait FiberTreeAxisMetadata
case object DenseMetadata extends FiberTreeAxisMetadata
case class CompressedMetadata(nOuter: Int, nInnerOpt: CompressedMetadata.InnerBufferParams = CompressedMetadata.InnerBuffer(None), canReadFromStage: Boolean = true, nonInfiniteSpan: Option[Int] = None /* If this param is set, that means the requested span can only be inf or this variable */, canReadReverse: Boolean = false) extends FiberTreeAxisMetadata
case class LinkedListMetadata(nHeads: Int, nNodes: Int, nodeSize: Int, nCoordsOpt: CompressedMetadata.InnerBufferParams = CompressedMetadata.InnerBuffer(None), resetRunningLenAutomatically: Boolean = false, headPtrIsHeadAddr: Boolean = false, initialLenIs0ForRfWrites: Boolean = false, nextNodeIsNullForRfWrites: Boolean = false, decrementingFreeNodePointer: Boolean = false, useAddrForSpanTraversed: Boolean = false, supportsImbalancedSpanTraversal: Boolean = false) extends FiberTreeAxisMetadata {
  // TODO nNodes/nodeSize should be calculated automatically based on the SRAM capacity
}
case class BitvectorMetadata(nBits: Int, bitsPerRow: Int) extends FiberTreeAxisMetadata

object FiberTreeAxisMetadata {
  val n_metadata_buffers = 4
  val metadata_buffer_id_bits = log2Ceil(n_metadata_buffers)
}

object CompressedMetadata {
  val outer_metadata_buffer_id = 0
  val inner_metadata_buffer_id = 1
  val outer_metadata_ends_buffer_id = 2

  sealed trait InnerBufferParams
  case class InnerBuffer(nCoordsOpt: Option[Int], expandForRf: Boolean = true) extends InnerBufferParams
  case class DontExpandInner(nCoordsOpt: Option[Int] = None) extends InnerBufferParams

  val can_be_written_by_regfile = Seq(inner_metadata_buffer_id)
}

object LinkedListMetadata {
  val head_ptr_buffer_id = 0
  val coord_buffer_id = 1
  val next_ptr_buffer_id = 2
  val last_node_len_buffer_id = 3 // TODO rename this to "row_len_buffer_id" or something

  val can_be_written_by_regfile = Seq(coord_buffer_id)
}

object BitvectorMetadata {
  val bitvector_buffer_id = 0
}
