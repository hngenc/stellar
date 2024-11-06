
package stellar

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.ClockGate
import freechips.rocketchip.tilelink._
import stellar.rtl._
import stellar.rtl.ChiselUtil._
import stellar.Util.{SMap, without}
import CombPathBreaker._

sealed trait StellarConf {
  val dmaDataWidthInBits: Int = 32 // This does not necessarily match the datatype stored in the spatial arrays or scratchpads
  def spArrayDataWidthInBits: Int = dmaDataWidthInBits

  val nXacts: Int = 16
  val multiPortedXactsTable: Boolean = false
  val extraXactsTableClearPortForWrites: Boolean = false
  val multipleTlWriteQs: Boolean = false
  val multipleTlWriteReqGenerators: Boolean = false

  val beatWidthInBytes: Int = 16
  val isCoherent: Boolean = true
  val nMemoryChannels: Int = 8
  val nTlPorts: Int = 1
  val accesses_to_same_address_in_outer_memory_ordered: Boolean = false
  val nDmaPorts: Int = 1
  val dramSize: BigInt = BigInt(1) << 32 // 4 GB

  val decodedCmdQSize: Int = 2

  val saved_configs: SMap[Int, SavedConfig] = Seq.empty.toMap // { configId: config }

  final def base_addr = if (isCoherent) x"0" else x"10_0000_0000"
  final def beatWidthInBits = beatWidthInBytes * 8
}
case class DenseStellarConf(size: Int, nAxes: Int, has_accumulator: Boolean, supports_convs: Boolean) extends StellarConf {
  override val dmaDataWidthInBits: Int = if (supports_convs) 8 else 32
  override val isCoherent = !has_accumulator // If this is an end-to-end dense matmul pipeline, then we assume that we want incoherent accesses to avoid stalling on broadcasts to the L1
}
case class SparseDenseStellarConf(size: Int, isLoadBalanced: Boolean) extends StellarConf {
  val nAxes = 5

  // TODO we want to be able to handle arbitrarily-large load-balanced matmuls in the future
  val (iTiles, jTiles, kTiles) = if (isLoadBalanced) (128, 8, 128) else (128, 128, 128)
}
case class OuterSpaceStellarConf(size: Int, hasMatmul: Boolean, hasMerger: Boolean) extends StellarConf {
  val nAxes = 4
  override val beatWidthInBytes = 64
  override val isCoherent = false
  override val nXacts = 512
  override val multiPortedXactsTable = true
  override val extraXactsTableClearPortForWrites = true
  override val multipleTlWriteQs = true
  override val multipleTlWriteReqGenerators = true
  override val nMemoryChannels = 16
  override val nTlPorts = 16
  override val accesses_to_same_address_in_outer_memory_ordered = true
  override val nDmaPorts = 16
  override val dramSize = BigInt(1) << 33 // 8 GB
  override val decodedCmdQSize = 16

  def mvinCSRConfig(sramCode: Int, baseAxis: Int, infiniteLenBaseAxis: Boolean, baseAxisAddrIsZero: Boolean,
                    onlyRowIds: Boolean, trueConstants: Set[Int] = Set.empty) = {
    var spans = Seq.tabulate(baseAxis)(axisId => Seq(true,false).map(b => (axisId,b) -> 0.U)).flatten
    var spansThatWontReset = Set(true,false).map(b => (baseAxis+1,b))
    if (infiniteLenBaseAxis)
      spans ++= Seq(true,false).map(b => (baseAxis,b) -> BigInt("2147483647").U)
    else
      spansThatWontReset ++= Set(true,false).map(b => (baseAxis,b))

    var metadataAddrsThatWontReset = Set((baseAxis, CompressedMetadata.outer_metadata_buffer_id, true))
    if (!onlyRowIds)
      metadataAddrsThatWontReset ++= Set((baseAxis, CompressedMetadata.inner_metadata_buffer_id, true))

    SavedConfig(
      srcAndDst = Some((Region.dramRegion, Region.sramRegion(sramCode))),
      axes = Seq(true,false).map(b => (baseAxis, b) -> FiberTreeAxis.Compressed).toMap,
      spans = spans.toMap,
      metadataStrides = Seq(true,false).flatMap(b => Seq((baseAxis, baseAxis, CompressedMetadata.inner_metadata_buffer_id, b) -> 1.U, (baseAxis+1, baseAxis, CompressedMetadata.outer_metadata_buffer_id, b) -> 1.U)).toMap,
      constants = (Seq(ISA.ConstantShouldTrailCoarse -> 1) ++ trueConstants.map(_ -> 1)).toMap,
      spansThatWontReset = spansThatWontReset,
      addrsThatWontReset = if (baseAxisAddrIsZero) Set.empty else Set((baseAxis,true)),
      dataAddrsThatWontReset = if (onlyRowIds) Set.empty else Set(true),
      metadataAddrsThatWontReset = metadataAddrsThatWontReset,
      issues = true,
    )
  }

  def matmulABSavedConfig(sramCode: Int) = SavedConfig(
    srcAndDst = Some(Region.sramRegion(sramCode), Region.rfRegion),
    spans = Seq(true,false).map(b => (0, b) -> size.U).toMap,
    metadataStrides = Seq(true,false).map(b => (1,0,CompressedMetadata.outer_metadata_buffer_id,b) -> 1.U).toMap,
    constants = Seq(ISA.ConstantShouldTrailCoarse -> 1).toMap,
    waits = Some(WaitOptions.literal(3, true, write_from_dram = Set(sramCode))),
    spansThatWontReset = Set(true,false).flatMap(b => Seq((1,b),(2,b))),
    issues = true
  )

  override val saved_configs: SMap[Int, SavedConfig] = Seq(
    // Mvin A
    0 -> mvinCSRConfig(sramCode = 0, baseAxis = 0, infiniteLenBaseAxis = false, baseAxisAddrIsZero = false, onlyRowIds = false),
    // Mvin B
    1 -> mvinCSRConfig(sramCode = 1, baseAxis = 0, infiniteLenBaseAxis = true, baseAxisAddrIsZero = true, onlyRowIds = false, trueConstants = Set(ISA.ConstantDropFollowingCmdsIfEmpty)),
    // Mvin C
    2 -> mvinCSRConfig(sramCode = 2, baseAxis = 1, infiniteLenBaseAxis = false, baseAxisAddrIsZero = false, onlyRowIds = true),

    // Reset C
    3 -> SavedConfig(
      srcAndDst = Some(Region.nothingRegion -> Region.sramRegion(if (hasMatmul) 2 else 0)),
      constants = Seq(ISA.ConstantResetRunningState -> 1, ISA.ConstantShouldTrailCoarse -> 1).toMap,
      issues = true,
    ),

    // Mvout scattered C data and coords
    4 -> SavedConfig(
      srcAndDst = Some((Region.sramRegion(2), Region.dramRegion)),
      axes = Seq((0, false) -> FiberTreeAxis.LinkedList, (1, false) -> FiberTreeAxis.Compressed).toMap,
      spans = Seq.tabulate(2)(axisId => (axisId, false) -> BigInt("2147483647").U).toMap,
      metadataStrides = (Seq((2,1,CompressedMetadata.outer_metadata_buffer_id, true) -> 1.U,
        (1,1,CompressedMetadata.inner_metadata_buffer_id, true) -> 1.U,
        (0,0,LinkedListMetadata.coord_buffer_id, true) -> 1.U)
      ).toMap,
      metadataStridesByAddr = Seq((1,0,LinkedListMetadata.head_ptr_buffer_id,true) -> 1.U).toMap,
      constants = Seq(ISA.ConstantIsData -> 0, ISA.ConstantMetadataBufferId -> LinkedListMetadata.coord_buffer_id, ISA.ConstantShouldTrail -> 1).toMap,
      spansThatWontReset = Set((2,false)),
      dataAddrsThatWontReset = Set(false),
      issues = true,
    ),

    // Mvout C pointers
    5 -> SavedConfig(srcAndDst = Some((Region.sramRegion(2), Region.dramRegion)),
      axes = Seq(true,false).flatMap(b => Seq((0,b) -> FiberTreeAxis.LinkedList, (1,b) -> FiberTreeAxis.Compressed)).toMap,
      spans = Seq(true,false).flatMap(b => Seq.tabulate(2)(axisId => (axisId, b) -> BigInt("2147483647").U)).toMap,
      metadataStrides = Seq(true,false).flatMap(b => Seq((2,1,CompressedMetadata.outer_metadata_buffer_id, b) -> 1.U, (1,1,CompressedMetadata.inner_metadata_buffer_id,b) -> 1.U)).toMap,
      metadataStridesByAddr = Seq((1,0,LinkedListMetadata.head_ptr_buffer_id,true) -> 1.U).toMap,
      metadataStridesByValue = Seq((1,0,LinkedListMetadata.next_ptr_buffer_id,false) -> 4.U).toMap,
      metadataRegions = Seq((1 -> LinkedListMetadata.coord_buffer_id) -> Region.sramRegion(0)).toMap,
      constants = Seq(ISA.ConstantShouldTrailCoarse -> 1).toMap,
      metadataAddrsThatWontReset = Set((0, LinkedListMetadata.next_ptr_buffer_id, false), (0, LinkedListMetadata.last_node_len_buffer_id, false)),
      metadataStridesThatWontReset = Set(true,false).map(b => (1, 1, CompressedMetadata.outer_metadata_ends_buffer_id, b)),
      issues = true),

    // Matmul A
    6 -> matmulABSavedConfig(0),
    // Matmul B
    7 -> matmulABSavedConfig(1),
    // Matmul C
    8 -> SavedConfig(srcAndDst = Some((Region.rfRegion, Region.sramRegion(2))),
      spans = Seq(true,false).flatMap(b => Seq.tabulate(2)(axisId => (axisId, b) -> BigInt("2147483647").U)).toMap,
      metadataStridesByAddr = Seq(true,false).map(b => (1,0,LinkedListMetadata.head_ptr_buffer_id,b) -> 1.U).toMap,
      waits = Some(WaitOptions.literal(3, true, read_from_sram = Set(2))),
      spansThatWontReset = Set(true,false).map(b => (2,b)),
      issues = true),

    // Mvin Scattered
    9 -> SavedConfig(srcAndDst = Some((Region.dramRegion, Region.sramRegion(0))),
      axes = Seq(true,false).flatMap(b => Seq.tabulate(2)(axisId => (axisId,b) -> FiberTreeAxis.LinkedList)).toMap,
      spans = Seq(true,false).flatMap(b => Seq.tabulate(2)(axisId => (axisId, b) -> BigInt("2147483647").U)).toMap,
      metadataStrides = Seq((2,1,LinkedListMetadata.next_ptr_buffer_id,true) -> 4.U).toMap,
      dataAddrsThatWontReset = Set(true),
      metadataAddrsThatWontReset = Set((0, LinkedListMetadata.next_ptr_buffer_id, true), (1, LinkedListMetadata.next_ptr_buffer_id, true)),
      spansThatWontReset = Set(true,false).map(b => (2,b)),
      issues = true),

    // Mvout Merged
    10 -> SavedConfig(srcAndDst = Some((Region.sramRegion(0), Region.dramRegion)),
      axes = Seq(true,false).map(b => (0,b) -> FiberTreeAxis.LinkedList).toMap,
      spans = Seq(true,false).flatMap(b => Seq((0, b) -> BigInt("2147483647").U, (1, b) -> 1.U)).toMap,
      metadataStrides = Seq(true,false).flatMap(b => Seq((0,0,LinkedListMetadata.coord_buffer_id,b) -> 1.U, (2,0,LinkedListMetadata.head_ptr_buffer_id,b) -> 1.U)).toMap,
      dataAddrsThatWontReset = Set(false),
      metadataAddrsThatWontReset = Set((0, LinkedListMetadata.coord_buffer_id, false), (0, LinkedListMetadata.head_ptr_buffer_id, false)),
      spansThatWontReset = Set(true,false).map(b => (2,b)),
      issues = true),

    // Merge Scattered
    11 -> SavedConfig(srcAndDst = Some((Region.sramRegion(0), Region.rfRegion)),
      spans = Seq(true,false).flatMap(b => Seq.tabulate(2)(axisId => (axisId,b) -> BigInt("2147483647").U)).toMap,
      metadataStrides = Seq(true,false).flatMap(b => Seq((2,1,LinkedListMetadata.head_ptr_buffer_id,b) -> 1.U, (1,1,LinkedListMetadata.coord_buffer_id,b) -> 1.U, (0,0,LinkedListMetadata.coord_buffer_id,b) -> 1.U)).toMap,
      metadataStridesByAddr = Seq(true,false).flatMap(b => Seq((1,0,LinkedListMetadata.head_ptr_buffer_id,b) -> 1.U)).toMap,
      constants = Seq(ISA.ConstantLastAxis -> 1, ISA.ConstantShouldTrail -> 1).toMap,
      recursive = Seq((1 -> size)).toMap,
      spansThatWontReset = Set(true,false).map(b => (2,b)),
      issues = true),

    12 -> SavedConfig(srcAndDst = Some((Region.rfRegion, Region.sramRegion(0))),
      spans = Seq(true,false).flatMap(b => Seq.tabulate(2)(axisId => (axisId,b) -> BigInt("2147483647").U)).toMap,
      metadataStrides = Seq(true,false).flatMap(b => Seq((2,1,LinkedListMetadata.head_ptr_buffer_id,b) -> 1.U, (1,1,LinkedListMetadata.coord_buffer_id,b) -> 1.U, (0,0,LinkedListMetadata.coord_buffer_id,b) -> 1.U)).toMap,
      metadataStridesByAddr = Seq(true,false).flatMap(b => Seq((1,0,LinkedListMetadata.head_ptr_buffer_id,b) -> 1.U)).toMap,
      constants = Seq(ISA.ConstantLastAxis -> 1, ISA.ConstantLastAxisLogSize -> 0).toMap,
      waits = Some(WaitOptions.literal(1, wait_for_dma = true, read_from_sram = Set(0), write_from_dram = Set(0), write_into_sram = Set(0))),
      recursive = Seq((1 -> size)).toMap,
      spansThatWontReset = Set(true,false).map(b => (2,b)),
      issues = true),
  ).toMap
}
case class SCNNConf() extends StellarConf {
  val nAxes = SCNN.nAxes
  override val beatWidthInBytes = 64
}
case class SpArchMergerConf(isCheap: Boolean, throughput: Int, nLevels: Int, check_result: Boolean) extends StellarConf {
  val nAxes = if (isCheap) 3 else 2

  override val dmaDataWidthInBits = if (SpArchMerger.bitwidth(isCheap) <= 32) 32 else 64
  override val spArrayDataWidthInBits = SpArchMerger.bitwidth(isCheap)

  override val isCoherent = check_result
  override val beatWidthInBytes = 64
  override val nXacts = 32
  override val nMemoryChannels = 8
  override val accesses_to_same_address_in_outer_memory_ordered = !check_result // If we're not checking the result, then we shouldn't worry about L2 or DRAM controllers reordering requests

  override val saved_configs: SMap[Int, SavedConfig] = Seq(
    // Move merged results from regfiles into SRAM
    0 -> SavedConfig(
      spans = Seq(true,false).flatMap(b => ((0,b) -> BigInt("2147483647").U) +: Option.when(isCheap)((1,b) -> BigInt("2147483647").U).toSeq).toMap,
      metadataStrides = Seq(true,false).flatMap(b => ((0, 0, LinkedListMetadata.coord_buffer_id, b) -> 1.U) +: Option.when(isCheap)((1, 1, LinkedListMetadata.coord_buffer_id, b) -> 1.U).toSeq).toMap,
      metadataStridesByAddr = Option.when(isCheap)(Seq(true,false).map(b => (1, 0, LinkedListMetadata.head_ptr_buffer_id, b) -> 1.U)).toSeq.flatten.toMap,
    ),

    // Move unmerged tensors from SRAM into regfiles
    1 -> SavedConfig(
      spans = Seq(true,false).flatMap(b => ((0,b) -> BigInt("2147483647").U) +: Option.when(isCheap)((1,b) -> BigInt("2147483647").U).toSeq).toMap,
      metadataStrides = Seq(true,false).flatMap(b => ((0, 0, CompressedMetadata.inner_metadata_buffer_id, b) -> 1.U) +: Option.when(isCheap)((1, 1, CompressedMetadata.inner_metadata_buffer_id, b) -> 1.U).toSeq :+ {
        val outer_axis = if (isCheap) 2 else 1
        (outer_axis, outer_axis-1, CompressedMetadata.outer_metadata_buffer_id, b) -> 1.U
      }).toMap,
      metadataStridesByAddr = Option.when(isCheap)(Seq(true,false).map(b => (1, 0, CompressedMetadata.outer_metadata_buffer_id, b) -> 1.U)).toSeq.flatten.toMap,
    )
  ).toMap
}

class Stellar(val config: StellarConf)(implicit p: Parameters) extends LazyRoCC(opcodes = OpcodeSet.custom3, nPTWPorts = config.nTlPorts) {
  val xLen = p(XLen)

  override lazy val module = new StellarModule(this)

  val node = TLClientNode((0 until config.nTlPorts).map { i =>
    val base = i * p(CacheBlockBytes)
    val mask = ~(((BigInt(1) << log2Ceil(config.nTlPorts))-1) * p(CacheBlockBytes))
    val addressSet = AddressSet(base, mask)

    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = "stellar-dma" + (if (i > 0) s"_$i" else ""), sourceId = IdRange(0, config.nXacts),
      visibility = Seq(addressSet))))
  })
  override val tlNode = TLIdentityNode()
  tlNode :=* TLBuffer(8, 8) :=* node
}

class StellarModule(outer: Stellar) extends LazyRoCCModuleImp(outer) with HasCoreParameters {
  import outer.config
  import config.{dmaDataWidthInBits, beatWidthInBits, base_addr, nXacts}

  val maxReqSizeInBytes = p(CacheBlockBytes)

  val sramAelems = 128 * 1024
  val sramBelems = 128 * 1024
  val sramCelems = 64 * 1024 / 4

  val elemT = SInt(dmaDataWidthInBits.W)

  // Instantiate modules
  val design = config match {
    case DenseStellarConf(size, nAxes, has_accumulator, supports_convs) =>
      val inputT = if (supports_convs) SInt(8.W) else SInt(32.W)
      val outputT = SInt(32.W)
      new DenseMatmulAccelerator(size=size, inputT=inputT, outputT=outputT, hasAccumulator=has_accumulator,
        sramAaxes=nAxes+(if (supports_convs) 4 else 0), sramBaxes=nAxes+(if (supports_convs) 1 else 0), sramCaxes=nAxes,
        sramAelems=sramAelems, sramBelems=sramBelems, sramCelems=sramCelems)

    case conf @ SparseDenseStellarConf(size, isLoadBalanced) =>
      new SpDenseMMAccelerator(conf.iTiles, conf.jTiles, conf.jTiles, isLoadBalanced, size,
        sramAElemsOpt = Some(sramAelems), sramBElemsOpt = Some(sramBelems), sramCElemsOpt = Some(sramCelems))

    case OuterSpaceStellarConf(size, hasMatmul, hasMerger) =>
      val result = new OuterSpace(size = size, hasMatmul = hasMatmul, hasMerger = hasMerger, withAsserts = size <= 2)
      result

    case SCNNConf() =>
      new SCNN(
        maxI = 4, maxF = 4, maxWt = 2, maxHt = 2,
        maxInputChannels = 1024*16, maxOutputChannels = 1024*16,
        nBanks = 32, rowSize = 1,
        nSramImageElems = 1024*1024, nSramFilterElems = 1024*1024, nSramParamsElems = 128, nSramResultElems = 1024*1024, // SCNN is able to avoid spilling to DRAM for workloads like AlexNet, so the SRAM size here shouldn't really matter for performance evaluations, as long as it's large enough to store the full layer
        withAsserts = false)

    case SpArchMergerConf(isCheap, throughput, nLevels, check_result) =>
      new SpArchMerger(isCheap = isCheap, throughput = throughput, nLevels = nLevels, check_result = check_result)
  }

  val nAxes = design.srams.map(_.axes.size).max
  val elemTs = design.srams.map(_.elemT).toSeq
  val biggestElemT = elemTs.maxBy(_.getWidth)

  val accelerator = CombPathBreaker(design.toChiselModule(config.spArrayDataWidthInBits)){ _.io }(
    { x => Seq(
      (x.writes, false,
        Seq("busy" -> { _: Data => true.B }).toMap ++
          Seq.tabulate(x.write_busies.size)(i => s"write_busies_$i" -> {_: Data => true.B})),
      (x.read_reqs, false,
        Seq.tabulate(x.read_busies.size)(i => s"read_busies_$i" -> {y: Data => !y.asInstanceOf[ChiselSRAMReadReq].to_regfile}).toMap)
    )},
    {y => Seq("busy" -> y.busy).toMap ++ y.write_busies.zipWithIndex.map { case (z,i) => s"write_busies_$i" -> z } ++ y.read_busies.zipWithIndex.map { case (z,i) => s"read_busies_$i" -> z }})

  val unified_write_req_t = ChiselSRAMWriteReq.unifiedWriteReq(biggestElemT, accelerator.io.writes.map(_.bits))
  val unified_read_req_t = ChiselSRAMReadReq.unifiedReadReq(accelerator.io.read_reqs.map(_.head.bits))
  val unified_read_resp_t = ChiselSRAMReadResp.unifiedReadResp(biggestElemT, accelerator.io.read_resps.flatten.map(_.bits).toSeq)

  val tlPorts = outer.node.out
  val (tls, edges) = outer.node.out.unzip

  val n_non_dense_axes = design.srams.map(sram => sram.axes.size - sram.axes.reverse.takeWhile(_ == FiberTreeAxis.Dense).size).maxOption.getOrElse(0)
  val max_n_sram_elems = design.srams.map(_.nElems).maxOption.getOrElse(0)
  val sramSpanBits = log2Up(max_n_sram_elems)+1

  var ll_node_size = -1
  var total_sram_nodes = -1
  var total_sram_heads = -1
  design.srams.foreach { sram =>
    sram.metadatas.collect {
      case LinkedListMetadata(nHeads, nNodes, nodeSize, _, _, _, _, _, _, _, _) =>
        if (ll_node_size < 0 || nodeSize < ll_node_size)
          ll_node_size = nodeSize
        if (total_sram_nodes < 0 || nNodes < total_sram_nodes)
          total_sram_nodes = nNodes
        if (total_sram_heads < 0 || nHeads < total_sram_heads)
          total_sram_heads = nHeads
    }
  }
  if (ll_node_size < 0) {
    ll_node_size = 1
    total_sram_nodes = 2
    total_sram_heads = 2
  }

  val dma = CombPathBreaker(new Dma(elemTs=elemTs, beatWidthInBits=beatWidthInBits,
      nAxes=nAxes, spanBits=32 /* TODO fuse this with Dma.spanBits */ /* TODO should this just be sramSpanBits? */,
      maxInFlightReqs=nXacts, logMaxTlReqLen=log2Up(maxReqSizeInBytes), nNonDenseAxes=n_non_dense_axes, nSramBanks=design.srams.map(_.nBanks).toSeq,
      sram_read_req_t=unified_read_req_t, sram_read_resp_t=unified_read_resp_t, sram_write_req_t=unified_write_req_t,
      accesses_to_same_address_in_outer_memory_ordered=config.accesses_to_same_address_in_outer_memory_ordered,
      nTlReqPorts = config.nDmaPorts, nReadRespPorts = config.nDmaPorts,

      /* TODO These optimizations shouldn't affect performance, but may reduce the area or power overhead (mainly
          for DMAs which support sparse matrices). Right now, they need to be set directly by the designer, but we
          should try to optimize this based on other hardcoded parameters instead, like we do for other optimizations.
       */
      coords_only_come_from_srams = config.isInstanceOf[OuterSpaceStellarConf],
      coords_only_used_for_ptr_prefetches = config.isInstanceOf[OuterSpaceStellarConf],
      coords_only_requested_once = config.isInstanceOf[OuterSpaceStellarConf],
      no_strides_in_waiting_for_lower_axes_to_wrap_around_states = config.isInstanceOf[OuterSpaceStellarConf],
      stride_by_value_fixed_to_ptr_packet_size = config.isInstanceOf[OuterSpaceStellarConf],
      axis0_writes_are_all_and_only_infinite = config.isInstanceOf[OuterSpaceStellarConf],
      has_repeated_bias_optimizations = config.isInstanceOf[DenseStellarConf],
      nUselessAxes = config match {
        case _: OuterSpaceStellarConf => nAxes - 3
        case x: DenseStellarConf if x.supports_convs => nAxes - 4
        case _ => 0
      },
    sramSpanBits = log2Up(max_n_sram_elems)+1,
    supportedAxes = Set(FiberTreeAxis.Dense) ++ design.srams.flatMap(_.axes).toSet,

    ll_node_size = ll_node_size, total_sram_nodes = total_sram_nodes, total_sram_heads = total_sram_heads,
    ), name = "DmaWrapper") { _.io }({ x => Seq(/* TODO ptr-chasing mvouts break when pipelining the tl-reqs, because RAW hazards can be missed when different tl-req ports read/write from different ports, but the earlier port's pipeline is backed up, causing the later port's pipeline to reach Top's TL-A ports first (x.tl_reqs, true),*/ (x.read_beats, false), (x.snoop_write_beat, false) /* TODO make sure we don't need to change when in-flight-table entries clear to pipeline these ports: (x.inflight_table_is_clearing, false) */) })

  val nSramBanks = design.srams.map(_.nBanks).maxOption.getOrElse(1)
  val nTlWriteReqGenerators = if (config.multipleTlWriteReqGenerators) nSramBanks else 1

  val inflight_table = Module(new InFlightTable(nAxes=nAxes, nSrams=design.srams.size, idBits=log2Up(nXacts), maxReqLenInBytes=maxReqSizeInBytes,
    nAllocPorts=if (config.multiPortedXactsTable) config.nDmaPorts + nTlWriteReqGenerators else 1,
    nLookupPorts=2*config.nDmaPorts + (if (config.extraXactsTableClearPortForWrites) 1 else 0),
    sramSpanBits=sramSpanBits)(
    allocPortVisibility = if (config.multiPortedXactsTable) Seq.fill(nTlWriteReqGenerators)(0 until nXacts/2) ++ Seq.fill(config.nDmaPorts)(nXacts/2 until nXacts) else Seq(0 until nXacts),
    lookupPortVisibility = ((0 until nXacts) +: Seq.fill(2*config.nDmaPorts-1)(if (config.multiPortedXactsTable) nXacts/2 until nXacts else 0 until nXacts)) ++
      (if (config.extraXactsTableClearPortForWrites) Some(0 until nXacts) else None).toSeq))

  // TODO add support for arbitrating a single TLB
  val tlbs = edges.map { edge =>
    implicit val e = edge
    Module(new TLB(false, log2Up(maxReqSizeInBytes), TLBConfig(nSets=1, nWays=4)))
  }

  val beat_packer = CombPathBreaker(new BeatPacker(elemTs=elemTs, beatWidthInBits=beatWidthInBits,
    maxReqLenInBytes=maxReqSizeInBytes, dmaBeatT=dma.inner.dma_beat_t, sramWriteReqT=unified_write_req_t, nSrams=design.srams.size,
    alignedToBytes=(design.srams.map(_.elemT) :+ elemT).map(_.getWidth/8).min,
    has_repeated_bias_optimizations=dma.inner.has_repeated_bias_optimizations),
    name = "BeatPackerWrapper")
    {_.io}({ x => Seq(
    (x.out, true /* TODO Now that we've pipelined accelerator.io.write_req, I don't know if this is required to meet timing anymore */,
      design.sramCodes.values.map { sramCode => s"busies_$sramCode" -> { y: Data => y.asInstanceOf[x.out.bits.type].sram_code === sramCode.U } }.toMap),
    (x.dma_beat, false,
      design.sramCodes.values.map { sramCode => s"busies_$sramCode" -> { y: Data => y.asInstanceOf[DmaBeat].table_entry.sram_code === sramCode.U } }.toMap))
  }, { x => x.busies.zipWithIndex.map { case (busy,i) => s"busies_$i" -> busy }.toMap })

  val tl_write_req_generators = Seq.tabulate(if (config.multipleTlWriteReqGenerators) nSramBanks else 1)(id => Module(new TLReqGenerator(
    elemT=elemT, nBanks=design.srams.map(_.nBanks).maxOption.getOrElse(1), nAdjacents=id,
    maxReqLenInBytes=maxReqSizeInBytes, maxInFlightReqs=nXacts, dmaTLReq=getChiselType(dma.io.tl_reqs.head.bits),
    sramReadRespT=unified_read_resp_t, inflightEntryT=inflight_table.entry_t)))

  val beat_splitters = Seq.fill(if (config.multipleTlWriteReqGenerators) nSramBanks else 1)(Module(new BeatSplitter(elemT=elemT, beatWidthInBits=beatWidthInBits,
    maxReqLenInBytes=maxReqSizeInBytes, dmaBeatT=dma.inner.dma_beat_t, dmaTLReq=getChiselType(dma.io.tl_reqs.head.bits),
    sramReadRespT=unified_read_resp_t)))

  val decoder = Module(new Decoder(nAxes=nAxes, nSrams = design.srams.size, addrBits=64, spanBits=32, strideBits=32,
    cmd_t=new RoCCCommand, dma_req_t=getChiselType(dma.io.req.bits), sram_write_t=unified_write_req_t,
    sram_read_t=unified_read_req_t, saved_configs=config.saved_configs))

  val cmd = Queue(io.cmd)
  val decoded_cmd = {
    val q = NoMuxQueue(decoder.io.out, config.decodedCmdQSize)

    val drop_cmds_until_dma_inst = Reg(UDValid(new Bundle {
      val write = Bool()
      val sram_code = UInt(Dma.sramCodeBits.W)
    }))

    val done_dropping = q.bits.inst_type === DecoderInst.dma_inst &&
      q.bits.dma.write === drop_cmds_until_dma_inst.bits.write &&
      q.bits.dma.sram_code === drop_cmds_until_dma_inst.bits.sram_code

    when (q.valid && done_dropping)
    {
      drop_cmds_until_dma_inst.valid := false.B
    }

    when (dma.io.drop_cmds_until_dma_inst.fire) {
      assert(!drop_cmds_until_dma_inst.valid)
      drop_cmds_until_dma_inst.valid := true.B
      drop_cmds_until_dma_inst.bits := dma.io.drop_cmds_until_dma_inst.bits
    }

    val dropping = drop_cmds_until_dma_inst.valid && !done_dropping

    val waiting = q.bits.waits.wait_for_dma && dma.io.busy ||
      q.bits.inst_type === DecoderInst.sram_write_inst && q.bits.sram_write.should_trail_reads_coarse_grained && accelerator.io.incoming_reads(q.bits.dst.index) ||
      q.bits.inst_type === DecoderInst.sram_read_inst && q.bits.sram_read.should_trail_writes_coarse_grained && accelerator.io.incoming_writes(q.bits.src.index) ||
      any(q.bits.waits.wait_for_srams.map(_.read_from_sram).zip(accelerator.io.read_busies).map { case (x,y) => x && y }) ||
      any(q.bits.waits.wait_for_srams.map(_.write_into_sram).zip(accelerator.io.write_busies).map { case (x,y) => x && y }) ||
      any(q.bits.waits.wait_for_srams.map(_.write_from_dram).zip(inflight_table.io.read_busies).zip(beat_packer.io.busies).map { case ((x,y),z) => x && (y || z) })

    val port = Wire(getChiselType(q))
    port.valid := q.valid && !waiting && !dropping
    port.bits := q.bits
    q.ready := (port.ready && !waiting) || dropping

    when (reset.asBool) {
      drop_cmds_until_dma_inst.valid := false.B
    }

    port
  }

  val tl_write_qs = Seq.fill(if (config.multipleTlWriteQs) nSramBanks else 1)(Module(new Queue(getChiselType(dma.io.tl_reqs.head.bits), entries=64)))

  /*
  Priority of connections to SRAM write ports (in descending order):
    1. TileLink
    2. DMA
    3. Decoder

  Priority of connections to SRAM read-req ports (in descending order):
    1. DMA's TileLink req port
    2. DMA's direct read-req port
    3. Decoder

  Priority of connections to TileLink/TLB (in descending order):
    1. Writes from SRAMs
      - Writes from SRAMs that pass through earlier TL-write-qs have priority over later TL-write-qs
    2. Reads/writes from DMA
  */

  val write_is_connected_to_tl = VecInit(Seq.fill(accelerator.io.writes.size max 1)(false.B))
  val write_is_connected_to_dma = VecInit(Seq.fill(accelerator.io.writes.size max 1)(false.B))

  val read_req_is_connected_to_dma_tl_req_port = VecInit(Seq.fill(accelerator.io.read_reqs.size max 1)(false.B))
  val read_req_is_connected_to_dma_direct = VecInit(Seq.fill(accelerator.io.read_reqs.size max 1)(false.B))

  val tl_a_is_connected_to_sram_read_resps = VecInit(Seq.fill(config.nTlPorts)(false.B))
  val tl_a_is_connected_to_tl_write_q = VecInit.fill(config.nTlPorts, if (config.multipleTlWriteQs) nSramBanks else 1)(false.B) // The very first TL write q only accesses the TL ports through the beat-splitter

  // Default connections
  accelerator.io.read_reqs.flatten.foreach { r =>
    r.valid := false.B
    r.bits := DontCare
  }
  accelerator.io.writes.foreach { w =>
    w.valid := false.B
    w.bits := DontCare
  }
  accelerator.io.read_resps.flatten.foreach(_.ready := false.B)

  decoded_cmd.ready := false.B

  dma.io.req.valid := false.B
  dma.io.req.bits := decoded_cmd.bits.dma

  dma.io.sram_read_req.ready := false.B
  dma.io.sram_read_resp.valid := false.B
  dma.io.sram_read_resp.bits := DontCare
  dma.io.sram_write_req.ready := false.B

  dma.io.sram_read_req_readies.zip(accelerator.io.read_reqs.map(_.map(_.ready))).foreach { case (x,y) => x.zip(y).foreach { case (a,b) => a := b } }
  dma.io.sram_read_busies := accelerator.io.read_busies
  dma.io.sram_write_busies := accelerator.io.write_busies
  dma.io.sram_write_from_dram_busies := vecOr(inflight_table.io.read_busies, beat_packer.io.busies)

  val tl_d_port_id = {
    val port_id_reg = Reg(UInt(log2Up(config.nTlPorts).W))
    val port_id_reg_valid = RegInit(false.B)

    tls.zipWithIndex.reverse.foreach { case (tl, portId) =>
      when (!port_id_reg_valid && tl.d.valid) {
        port_id_reg := portId.U
        port_id_reg_valid := true.B
      }
    }

    val clear_port_id_reg = ChiselUtil.any(tls.zip(edges).map { case (tl, edge) => tl.d.fire && edge.last(tl.d) })
    when (clear_port_id_reg) {
      port_id_reg_valid := false.B
    }

    Mux(port_id_reg_valid, port_id_reg, PriorityEncoder(tls.map(_.d.valid)))
  }

  val tl_d_valid = VecInit(tls.map(_.d.valid))(tl_d_port_id)
  val tl_d_bits = VecInit(tls.map(_.d.bits))(tl_d_port_id)
  val tl_d_first = VecInit(tls.zip(edges).map { case (tl, edge) => edge.first(tl.d) })(tl_d_port_id)
  val tl_d_last = VecInit(tls.zip(edges).map { case (tl, edge) => edge.last(tl.d) })(tl_d_port_id)
//  dontTouch(tl_d_valid)
//  dontTouch(tl_d_bits)
//  dontTouch(tl_d_first)
//  dontTouch(tl_d_last)

  val tl_d_is_write = tl_d_bits.opcode === TLMessages.AccessAck
  val tl_d_is_for_dma = inflight_table.io.lookup_resps.head.bits.read_into_dma
  val tl_d_read_is_ready = Mux(tl_d_is_for_dma, dma.io.read_beats.head.ready, beat_packer.io.dma_beat.ready)
//  dontTouch(tl_d_is_write)
//  dontTouch(tl_d_is_for_dma)
//  dontTouch(tl_d_read_is_ready)

  val (single_beat_tl_d_port_ids, single_beat_tl_d_port_is_valid) = {
    val potential_tl_d_valids = tls.zip(edges).zipWithIndex.map { case ((tl, edge), j) => tl.d.valid &&
      (/*!tl_d_valid ||*/ j.U =/= tl_d_port_id) && // different from the default tl-d port
      tl.d.bits.opcode === TLMessages.AccessAckData && // is a read response
      edge.first(tl.d) && edge.last(tl.d) // is single-beat
    }

    // These ports are meant to be used by the DMA, especially for scattered, OuterSPACE-like pointer chasing. For the
    //   sake of simplicity and low-overhead, we restrict these ports to only look for single-beat transactions
    (0 until config.nDmaPorts-1).map { i =>
      val portId = priorityEncoderWithPosition(potential_tl_d_valids, i)
      val isValid = PopCount(potential_tl_d_valids) > i.U

      (portId, isValid)
    }.unzip
  }

  val extraTlDWritePortIdOpt: Option[(UInt, Bool)] = if (config.extraXactsTableClearPortForWrites) {
    // This tries to clear out TL-D ports which are just returning a "write" (i.e., they have no data to return to the
    // accelerator)
    val potential_tl_d_valids = tls.zipWithIndex.map { case (tl, j) => tl.d.valid &&
      (/*!tl_d_valid ||*/ j.U =/= tl_d_port_id) && // different from the default tl-d port
      tl.d.bits.opcode === TLMessages.AccessAck // is a write response
    }

    val chosen_tl_d_port_id = PriorityEncoder(potential_tl_d_valids)

    assert(!any(potential_tl_d_valids) || !any(single_beat_tl_d_port_ids.zip(single_beat_tl_d_port_is_valid).map { case (port_id, valid) => valid && port_id === chosen_tl_d_port_id }), "the extra write TL-D port which is being cleared conflicts with the single-beat Tl-D ports that are meant for the DMA to handle above")

    Some((chosen_tl_d_port_id, any(potential_tl_d_valids)))
  } else {
    None
  }

  assert(PopCount(tls.map(_.d.fire)) <= config.nDmaPorts.U, "too many tl-d port firing at a time")

  dma.io.read_beats.head.valid := tl_d_valid && !tl_d_is_write && tl_d_is_for_dma
  dma.io.read_beats.head.bits.data := tl_d_bits.data
  dma.io.read_beats.head.bits.table_entry := inflight_table.io.lookup_resps.head.bits
  dma.io.read_beats.head.bits.first := tl_d_first
  dma.io.read_beats.head.bits.last := tl_d_last
  dma.io.read_beats.head.bits.mask := DontCare // The DMA ignores the mask anyways
  dma.io.read_beats.head.bits.id := tl_d_bits.source

  dma.io.read_beats.tail.zip(single_beat_tl_d_port_ids).zip(single_beat_tl_d_port_is_valid).zip(inflight_table.io.lookup_resps.tail.tail).foreach { case (((read_beat, single_beat_tl_d_port_id), single_beat_tl_d_port_is_valid), inflight_table_lookup_resp) =>
    val singe_beat_tl_d_bits = VecInit(tls.map(_.d.bits))(single_beat_tl_d_port_id)

    val single_beat_tl_d_is_for_dma = inflight_table_lookup_resp.valid && inflight_table_lookup_resp.bits.read_into_dma

    read_beat.valid := single_beat_tl_d_port_is_valid && single_beat_tl_d_is_for_dma
    read_beat.bits.data := singe_beat_tl_d_bits.data
    read_beat.bits.table_entry := inflight_table_lookup_resp.bits
    read_beat.bits.first := true.B
    read_beat.bits.last := true.B
    read_beat.bits.mask := DontCare
    read_beat.bits.id := singe_beat_tl_d_bits.source
  }

  dma.io.snoop_write_beat.valid := beat_splitters.head.io.out.fire
  dma.io.snoop_write_beat.bits := beat_splitters.head.io.out.bits.dma_beat

  dma.io.snoop_write_beat_axis_span.valid := false.B
  dma.io.snoop_write_beat_axis_span.bits := DontCare
  accelerator.io.read_resps.foreach { resps =>
    resps.foreach { resp =>
      when (resp.valid && all(resp.bits.axis_spans.map(_.valid))) {
        val total_span = resp.bits.axis_spans.map(_.bits).reduce(_ * _)
        dma.io.snoop_write_beat_axis_span.valid := true.B
        dma.io.snoop_write_beat_axis_span.bits := total_span
      }
    }
  }

  dma.io.tl_reqs.foreach(_.ready := false.B)

  decoder.io.in <> cmd

  when (decoded_cmd.bits.inst_type === DecoderInst.dma_inst) {
    dma.io.req.valid := decoded_cmd.valid
    decoded_cmd.ready := dma.io.req.ready
  }

  if (config.multiPortedXactsTable) {
    inflight_table.io.alloc_reqs.zip(inflight_table.io.alloc_ids).zip(tl_write_req_generators).foreach { case ((inflight_table_io_alloc_req, inflight_table_io_alloc_id), tl_write_req_generator) =>
      inflight_table_io_alloc_req <> tl_write_req_generator.io.inflight_alloc.req
      tl_write_req_generator.io.inflight_alloc.id := inflight_table_io_alloc_id
    }

    inflight_table.io.alloc_reqs.drop(nTlWriteReqGenerators).zip(dma.io.inflight_alloc_reqs).zip(dma.io.inflight_alloc_ids).zip(inflight_table.io.alloc_ids.drop(nTlWriteReqGenerators)).foreach { case (((table_req, dma_req), dma_resp), table_resp) =>
      table_req <> dma_req
      dma_resp := table_resp
    }
  } else {
    val inflight_table_alloc_arb = Module(new Arbiter(getChiselType(inflight_table.io.alloc_reqs.head.bits), 2))
    inflight_table.io.alloc_reqs.head <> inflight_table_alloc_arb.io.out

    val tl_write_req_generator = tl_write_req_generators.head
    require(nTlWriteReqGenerators == 1, "we need to update this code if we want to support non-multiported inflight tables with multiple tl-req-generators")

    inflight_table_alloc_arb.io.in(0) <> tl_write_req_generator.io.inflight_alloc.req
    tl_write_req_generator.io.inflight_alloc.id := inflight_table.io.alloc_ids.head

    inflight_table_alloc_arb.io.in(1) <> dma.io.inflight_alloc_reqs.head
    dma.io.inflight_alloc_ids.head := inflight_table.io.alloc_ids.head

    require(config.nDmaPorts == 1, "we don't support multi-ported DMAs unless the inflight-table is also multiported")
  }

  dma.io.inflight_table_is_clearing.valid := inflight_table.io.lookup_reqs.head.fire && inflight_table.io.lookup_reqs.head.bits.clear
  dma.io.inflight_table_is_clearing.bits := inflight_table.io.lookup_reqs.head.bits.id

  require(tlbs.size == io.ptw.size, "we don't currently support arbitrating for the PTW") // TODO add support for arbitrating a single PTW
  tlbs.zip(io.ptw).foreach { case (tlb, ptw) =>
    tlb.io.req.valid := false.B
    tlb.io.req.bits := DontCare
    tlb.io.kill := false.B

    tlb.io.sfence.valid := cmd.bits.inst.funct === ISA.Flush.U
    tlb.io.sfence.bits := DontCare
    tlb.io.sfence.bits.rs1 := false.B
    tlb.io.sfence.bits.rs2 := false.B
    tlb.io.sfence.bits.hv := false.B
    tlb.io.sfence.bits.hg := false.B

    ptw <> tlb.io.ptw

    val new_status = cmd.valid && cmd.bits.inst.funct === ISA.Flush.U
    tlb.io.ptw.status := Mux(new_status, cmd.bits.status, RegEnable(cmd.bits.status, new_status)) // TODO do we need the Mux here, or would just the RegEnable be enough?
  }

  dma.io.debug_print := cmd.fire && cmd.bits.inst.funct === ISA.Flush.U

  tl_write_qs.zip(dma.io.tl_reqs).foreach { case (tl_write_q, dma_io_tl_req) =>
    tl_write_q.io.enq.valid := false.B
    tl_write_q.io.enq.bits := dma_io_tl_req.bits
    tl_write_q.io.deq.ready := false.B
  }

  inflight_table.io.lookup_reqs.head.valid := tl_d_valid
  inflight_table.io.lookup_reqs.head.bits.id := tl_d_bits.source
  inflight_table.io.lookup_reqs.head.bits.clear := tl_d_valid && ((tl_d_read_is_ready && tl_d_last && !tl_d_is_for_dma) || tl_d_is_write)

  inflight_table.io.lookup_reqs(1).valid := dma.io.clear_inflight_table_entries.head.valid
  inflight_table.io.lookup_reqs(1).bits.id := dma.io.clear_inflight_table_entries.head.bits
  inflight_table.io.lookup_reqs(1).bits.clear := dma.io.clear_inflight_table_entries.head.fire

  inflight_table.io.lookup_reqs.tail.tail.take(config.nDmaPorts-1).zip(single_beat_tl_d_port_ids zip single_beat_tl_d_port_is_valid).foreach { case (lookup_req, (single_beat_tl_d_port_id, single_beat_tl_d_port_is_valid)) =>
    val single_beat_tl_d_source_id = VecInit(tls.map(_.d.bits.source))(single_beat_tl_d_port_id)

    lookup_req.valid := single_beat_tl_d_port_is_valid
    lookup_req.bits.id := single_beat_tl_d_source_id
    lookup_req.bits.clear := false.B
  }

  inflight_table.io.lookup_reqs.tail.tail.drop(config.nDmaPorts-1).zip(dma.io.clear_inflight_table_entries.tail).foreach { case (lookup_req, clear_port) =>
    lookup_req.valid := clear_port.valid
    lookup_req.bits.id := clear_port.bits
    lookup_req.bits.clear := clear_port.fire
  }

  if (config.extraXactsTableClearPortForWrites) {
    val Some((chosen_port_id, port_id_valid)) = extraTlDWritePortIdOpt

    val clear_port = inflight_table.io.lookup_reqs.last
    clear_port.valid := port_id_valid
    clear_port.bits.id := VecInit(tls.map(_.d.bits.source))(chosen_port_id)
    clear_port.bits.clear := true.B
  }

  beat_packer.io.dma_beat.valid := tl_d_valid && !tl_d_is_write && !tl_d_is_for_dma
  beat_packer.io.dma_beat.bits.data := tl_d_bits.data
  beat_packer.io.dma_beat.bits.table_entry := inflight_table.io.lookup_resps.head.bits
  beat_packer.io.dma_beat.bits.first := tl_d_first
  beat_packer.io.dma_beat.bits.last := tl_d_last
  beat_packer.io.dma_beat.bits.id := tl_d_bits.source
  beat_packer.io.dma_beat.bits.mask := DontCare // The beat-packer ignores the mask anyways

  tls.zipWithIndex.foreach { case (tl, portId) =>
    tl.d.ready := tl_d_port_id === portId.U && (tl_d_read_is_ready || tl_d_is_write)

    single_beat_tl_d_port_ids.zip(single_beat_tl_d_port_is_valid).zip(inflight_table.io.lookup_resps.tail.tail).zip(dma.io.read_beats.tail).foreach { case (((single_beat_tl_d_port_id, single_beat_tl_d_port_is_valid), lookup_resp), read_beat) =>
      when (single_beat_tl_d_port_is_valid && single_beat_tl_d_port_id === portId.U &&
        lookup_resp.valid && lookup_resp.bits.read_into_dma &&
        read_beat.ready)
      {
        tl.d.ready := true.B
      }
    }

    if (config.extraXactsTableClearPortForWrites) {
      val Some((chosen_port_id, port_id_valid)) = extraTlDWritePortIdOpt
      when (port_id_valid && portId.U === chosen_port_id) {
        tl.d.ready := true.B
      }
    }
  }

  assert(!inflight_table.io.lookup_reqs.head.valid || inflight_table.io.lookup_resps.head.valid,
    "currently, we assume that instantaneous lookups are possible from the in-flight-table")

  beat_packer.io.out.ready := false.B

  beat_splitters.foreach(_.io.out.ready := false.B)

  tl_write_req_generators.zip(tl_write_qs).zip(beat_splitters).foreach { case ((tl_write_req_generator, tl_write_q), beat_splitter) =>
    tl_write_req_generator.io.in.valid := false.B
    tl_write_req_generator.io.in.bits := DontCare
    tl_write_req_generator.io.in.bits.tl_req := tl_write_q.io.deq.bits
    tl_write_req_generator.io.adjacent_offset.foreach(_.valid := false.B)
    tl_write_req_generator.io.adjacent_offset.foreach(_.bits := DontCare)
    tl_write_req_generator.io.lookahead_read_resps.foreach(_.valid := false.B)
    tl_write_req_generator.io.lookahead_read_resps.foreach(_.bits := DontCare)
    beat_splitter.io.in <> tl_write_req_generator.io.out
  }

  tls.foreach(_.a.valid := false.B)
  tls.foreach(_.a.bits := DontCare)
  tls.zipWithIndex.foreach { case (tl, portId) =>
    tl.a.bits.address := (maxReqSizeInBytes * portId).U | base_addr.U
  }

  io.resp.valid := false.B
  io.resp.bits := DontCare

  io.busy := accelerator.io.busy || dma.io.busy || inflight_table.io.busy ||
    cmd.valid && !decoder.io.out.valid ||
    any(Seq(decoder.io.out, decoded_cmd).map { _decoded_cmd =>
      _decoded_cmd.valid && _decoded_cmd.bits.inst_type === DecoderInst.dma_inst ||
        _decoded_cmd.valid && _decoded_cmd.bits.inst_type === DecoderInst.sram_write_inst ||
        _decoded_cmd.valid && _decoded_cmd.bits.inst_type === DecoderInst.sram_read_inst && !_decoded_cmd.bits.sram_read.to_regfile
    })

  // Connect decoder to SRAMs
  for (sram_code <- design.sramCodes.values) {
    when (decoded_cmd.bits.inst_type === DecoderInst.sram_read_inst && decoded_cmd.bits.src.index === sram_code.U &&
      !read_req_is_connected_to_dma_tl_req_port(sram_code.U) &&
      !read_req_is_connected_to_dma_direct(sram_code.U))
    {
      val read_req = accelerator.io.read_reqs(accelerator.inner.getReadReqPortIdForSramCode(sram_code)).head
      read_req.valid := decoded_cmd.valid
      ChiselSRAMReadReq.connect(read_req.bits, decoded_cmd.bits.sram_read)
      decoded_cmd.ready := read_req.ready
    }

    when (decoded_cmd.bits.inst_type === DecoderInst.sram_write_inst && decoded_cmd.bits.dst.index === sram_code.U &&
      !write_is_connected_to_tl(sram_code) && !write_is_connected_to_dma(sram_code))
    {
      val write_req = accelerator.io.writes(accelerator.inner.getWritePortIdForSramCode(sram_code))
      ChiselSRAMWriteReq.biconnect(write_req, decoded_cmd.bits.sram_write, srcValid=decoded_cmd.valid, srcReady=decoded_cmd.ready)
    }
  }

  // Connect DMA's TileLink req port to SRAM read-req ports
  dma.io.tl_reqs.zipWithIndex.foreach { case (dma_tl_req, dma_tl_req_id) =>
    val sram_code = dma_tl_req.bits.tableEntry.sram_code
    val axis = dma_tl_req.bits.tableEntry.axis

    def connectReadReq(read_req: ChiselSRAMReadReq): Unit = {
      read_req := 0.U.asTypeOf(read_req)

      connectVecs(read_req.address, dma_tl_req.bits.tableEntry.iterator_values)
      connectVecs(read_req.data_strides, dma_tl_req.bits.tableEntry.sram_data_strides)
      read_req.iteration_strides.foreach(_ := 1.U)
      read_req.should_read_data := dma_tl_req.bits.tableEntry.is_data
      read_req.should_read_metadata := !dma_tl_req.bits.tableEntry.is_data
      read_req.axis := dma_tl_req.bits.tableEntry.axis
      read_req.metadata_buffer_id := dma_tl_req.bits.tableEntry.metadata_buffer_id
      connectVecOfVecsOfVecs(read_req.metadata_strides, dma_tl_req.bits.tableEntry.sram_metadata_strides)
      connectVecOfVecsOfVecs(read_req.metadata_strides_by_addr, dma_tl_req.bits.tableEntry.sram_metadata_strides_by_addr)
      read_req.should_trail_writes := dma_tl_req.bits.tableEntry.sram_should_trail
      read_req.should_trail_writes_coarse_grained := dma_tl_req.bits.tableEntry.sram_should_trail_coarse
      read_req.interleave.should_push := false.B
      read_req.interleave.should_pop := false.B
      read_req.interleave.axis := DontCare
      read_req.to_regfile := false.B
      read_req.to_dma := false.B
      read_req.should_gather.foreach(_ := false.B)
      read_req.is_recursive.foreach(_ := false.B)
      read_req.reset_running_state := false.B

      when (dma_tl_req.bits.flattened_mvout || dma_tl_req.bits.write && dma_tl_req.bits.multiaxis_mvout) {
        connectVecs(read_req.spans, dma_tl_req.bits.tableEntry.lens)
      }.otherwise {
        read_req.spans.foreach(_ := 1.U)
        read_req.spans(axis) := dma_tl_req.bits.tableEntry.len
      }
    }

    val makes_adjacent_reads = WireInit(false.B)
    if (dma_tl_req_id == 0)
      design.sramCodes.toSeq.foreach { case (sram, sramCode) =>
        when (sram_code === sramCode.U && (dma_tl_req.bits.tableEntry.is_data || dma_tl_req.bits.tableEntry.metadata_buffer_id =/= 0.U)) {
          sram.readBankingStrategies.headOption.foreach { case ReadBankingStrategy(cond) =>
            val read_req = Wire(getChiselType(accelerator.io.read_reqs(accelerator.inner.getReadReqPortIdForSramCode(sramCode))(dma_tl_req_id).bits))
            connectReadReq(read_req)
            val stratResult = cond(read_req)
            when (PopCount(stratResult.map(_._1)) > 1.U) {
              val read_req2 = Wire(getChiselType(read_req))
              read_req2 := read_req
              stratResult.head._2(read_req2)
              when (read_req2.adjacent) {
                makes_adjacent_reads := true.B
              }
            }
          }
        }
      }

    val adjacent_read_blocked = {
      val blocked_by_other_dma_tl_req_ports = any(without(dma.io.tl_reqs, dma_tl_req_id).map(_.valid))
      val blocked_by_tl_write_req_qs = !all(without(tl_write_qs, dma_tl_req_id).map(_.io.enq.ready))
      blocked_by_other_dma_tl_req_ports || blocked_by_tl_write_req_qs
    }

    when (dma_tl_req.valid && dma_tl_req.bits.write && !dma_tl_req.bits.write_from_dma && (!makes_adjacent_reads || !adjacent_read_blocked)) {
      for (sc <- design.sramCodes.values) {
        val dma_tl_req_can_connect_to_sram = dma_tl_req_id < design.sramCodes.map(_.swap)(sc).nBanks && dma_tl_req_id < tl_write_qs.size

        if (dma_tl_req_can_connect_to_sram) (when(sc.U === sram_code) {
          val read_req = accelerator.io.read_reqs(accelerator.inner.getReadReqPortIdForSramCode(sc))(dma_tl_req_id)
          read_req.valid := tl_write_qs(dma_tl_req_id).io.enq.ready
          connectReadReq(read_req.bits)

          tl_write_qs(dma_tl_req_id).io.enq.valid := read_req.ready

          dma_tl_req.ready := read_req.ready && tl_write_qs(dma_tl_req_id).io.enq.ready

          read_req_is_connected_to_dma_tl_req_port(sc) := true.B
        }) else {
          assert(sc.U =/= sram_code, "this DMA port is trying to read from an SRAM port that it isn't allowed to read from")
        }
      }
    }

    when (dma_tl_req.valid && makes_adjacent_reads && !adjacent_read_blocked) {
      without(tl_write_qs, dma_tl_req_id).foreach { tl_write_q =>
        tl_write_q.io.enq.valid := dma_tl_req.fire
        tl_write_q.io.enq.bits := dma_tl_req.bits
        assert(tl_write_q.io.enq.fire || !dma_tl_req.fire)
      }
    }
  }

  // Connect DMA's direct SRAM read port to SRAM read-req ports
  {
    val sram_code = dma.io.sram_read_req_sram_code
    when(dma.io.sram_read_req.valid) {
      for (sc <- design.sramCodes.values) {
        when(sc.U === sram_code && !read_req_is_connected_to_dma_tl_req_port(sc)) {
          ChiselSRAMReadReq.biconnect(accelerator.io.read_reqs(accelerator.inner.getReadReqPortIdForSramCode(sc)).head, dma.io.sram_read_req)
          read_req_is_connected_to_dma_direct(sc) := true.B
        }
      }
    }
  }

  // Connect DMA's direct SRAM write port to SRAM write ports
  {
    val sram_code = dma.io.sram_write_req.bits.sram_code
    when (dma.io.sram_write_req.valid) {
      for (sc <- design.sramCodes.values) {
        when (sc.U === sram_code && !write_is_connected_to_tl(sc)) {
          ChiselSRAMWriteReq.biconnect(
            accelerator.io.writes(accelerator.inner.getWritePortIdForSramCode(sc)),
            dma.io.sram_write_req.bits.req, dma.io.sram_write_req.valid, dma.io.sram_write_req.ready)
          write_is_connected_to_dma(sc) := true.B
        }
      }
    }
  }

  // Connect BeatPacker to SRAM write ports
  {
    val sram_code = beat_packer.io.out.bits.sram_code
    when (beat_packer.io.out.valid) {
      for (sc <- design.sramCodes.values) {
        when(sc.U === sram_code) {
          val write_req = accelerator.io.writes(accelerator.inner.getWritePortIdForSramCode(sc))
          val out = beat_packer.io.out
          ChiselSRAMWriteReq.biconnect(write_req, out.bits.write_req, out.valid, out.ready)
          write_is_connected_to_tl(sc) := true.B
        }
      }
    }
  }

  // Connect DMA to TL read/write port
  {
    dma.io.tl_reqs.zipWithIndex.foldLeft(VecInit(Seq.fill(config.nTlPorts)(false.B))) { case (tl_a_is_connected_to_prior_dma_tl_req_ports, (dma_tl_req, dma_tl_req_id)) =>
      val tl_req_bits = dma_tl_req.bits

      val next = WireInit(tl_a_is_connected_to_prior_dma_tl_req_ports)

      when (dma_tl_req.valid && (!tl_req_bits.write || tl_req_bits.write_from_dma)) {
        val tl_a_port_id = (tl_req_bits.addr / maxReqSizeInBytes.U) % config.nTlPorts.U
        next(tl_a_port_id) := true.B

        for ((((tl_a, edge), tlb), portId) <- tls.map(_.a).zip(edges).zip(tlbs).zipWithIndex) {
          when (tl_a_port_id === portId.U &&
            !tl_a_is_connected_to_sram_read_resps(portId) &&
            !tl_a_is_connected_to_prior_dma_tl_req_ports(portId))
          {
            tlb.io.req.valid := true.B
            tlb.io.req.bits.cmd := M_XRD
            tlb.io.req.bits.size := tl_req_bits.logLen
            tlb.io.req.bits.vaddr := tl_req_bits.addr | base_addr.U
            val tlb_hit = !tlb.io.resp.miss

            val (getLegal, get) = edge.Get(
              fromSource = tl_req_bits.id,
              toAddress = tlb.io.resp.paddr | base_addr.U,
              lgSize = tl_req_bits.logLen
            )

            val (putLegal, put) = edge.Put(
              fromSource = tl_req_bits.id,
              toAddress = tlb.io.resp.paddr | base_addr.U,
              lgSize = tl_req_bits.logLen,
              data = tl_req_bits.data,
              mask = tl_req_bits.mask,
            )

            tl_a.valid := tlb_hit
            tl_a.bits := Mux(tl_req_bits.write, put, get)

            dma_tl_req.ready := tl_a.ready && tlb_hit

            assert(!tl_a.valid || tl_req_bits.write || getLegal, s"illegal get request when connecting DMA-port-$dma_tl_req_id to tl-$portId")
            assert(!tl_a.valid || !tl_req_bits.write || putLegal, s"illegal put request when connecting DMA-port-$dma_tl_req_id to tl-$portId")
          }
        }
      }

      next
    }
  }

  // Connect Tl write queue and SRAM read resp ports to tl-write-q-generator and beat splitter
  val total_running_lens = RegInit(VecInit.fill(design.srams.size)(0.U(32.W)))
  total_running_lens.zipWithIndex.foreach { case (total_running_len, sramId) =>
    val sram_read_resps = accelerator.io.read_resps(accelerator.inner.getReadRespPortIdForSramCode(sramId))
    total_running_len := total_running_len + sram_read_resps.map { sram_read_resp =>
      Mux(sram_read_resp.fire && sram_read_resp.bits.adjacent && sram_read_resp.bits.first && sram_read_resp.bits.update_running_len,
        sram_read_resp.bits.axis_spans.map(_.bits).reduce(_ * _), 0.U)
    }.reduce(_ +& _)
  }
  tl_write_req_generators.zip(tl_write_qs).zipWithIndex.foreach { case ((tl_write_req_generator, tl_write_q), tl_write_req_id) =>
    val sram_code = {
      val tl_req = tl_write_q.io.deq.bits
      tl_req.tableEntry.sram_code
    }

    val read_resp_is_valid = WireInit(false.B)
    for (sc <- design.sramCodes.values) {
      val sram_read_resps = accelerator.io.read_resps(accelerator.inner.getReadRespPortIdForSramCode(sc))
      val bank_id = if (tl_write_req_id == 0) tl_write_req_generator.io.bankId % design.srams(sc).nBanks.U else tl_write_req_id.U

      val lookahead_banks = {
        val nBanks = tl_write_req_generator.nBanks
        val ordered_banks = rotateVec(VecInit((0 until nBanks).map(_.U)), bank_id)
        val part_of_same_req = ordered_banks.scanLeft(true.B) { (acc, x) =>
          acc && sram_read_resps(x).valid && !sram_read_resps(x).bits.to_dma && !sram_read_resps(x).bits.last &&
            sram_read_resps(x).bits.last_in_axis.head && !sram_read_resps(x).bits.last_in_axis.tail.headOption.getOrElse(true.B)
        }.init
        ordered_banks.zip(part_of_same_req).map { case (bid, same) => Mux(same, bid, nBanks.U)}.tail
      }

      sram_read_resps.zipWithIndex.foreach { case (sram_read_resp, bankId) =>
        when (sc.U === sram_code && !sram_read_resp.bits.to_dma && ((tl_write_req_id == 0).B || sram_read_resp.bits.independent)) {
          when (bankId.U === bank_id) {
            read_resp_is_valid := sram_read_resp.valid

            ChiselSRAMReadResp.connect(tl_write_req_generator.io.in.bits.read_resp, sram_read_resp.bits)
            val interleaved_data_and_coords = sram_read_resp.bits.is_data && sram_read_resp.bits.is_both_data_and_metadata
            tl_write_req_generator.io.in.bits.read_resp.total_running_len := total_running_lens(sc) << interleaved_data_and_coords
            sram_read_resp.ready := tl_write_req_generator.io.in.ready

            assert(!sram_read_resp.valid || tl_write_q.io.deq.valid)

            when (sram_read_resp.valid && sram_read_resp.bits.adjacent) {
              tl_write_req_generators.drop(bankId+1).foreach { adjacent_tl_write_req_generator =>
                adjacent_tl_write_req_generator.io.adjacent_offset(bankId).valid := sram_read_resp.bits.first && sram_read_resp.fire
                adjacent_tl_write_req_generator.io.adjacent_offset(bankId).bits := (sram_read_resp.bits.axis_spans.map(_.bits).reduce(_ * _) << interleaved_data_and_coords).asUInt +&
                  Mux((tl_write_req_id == 0).B, tl_write_req_generator.io.in.bits.read_resp.total_running_len, 0.U)
                assert(ChiselUtil.all(sram_read_resp.bits.axis_spans.map(_.valid)))

                when (sram_read_resp.bits.first && !adjacent_tl_write_req_generator.io.adjacent_offset(bankId).ready) {
                  read_resp_is_valid := false.B
                  sram_read_resp.ready := false.B
                }
              }
            }
          }.elsewhen ((tl_write_req_id == 0).B && vecContains(lookahead_banks, bankId.U) && !sram_read_resps(bank_id).bits.independent) {
            val index = vecIndexOf(lookahead_banks, bankId.U)
            val lookahead_port = tl_write_req_generator.io.lookahead_read_resps(index)
            ChiselSRAMReadResp.connect(lookahead_port.bits, sram_read_resp.bits)
            lookahead_port.valid := sram_read_resp.valid
            sram_read_resp.ready := lookahead_port.ready

            assert(!sram_read_resp.valid || read_resp_is_valid, "looking ahead when the main sram-read-resp port isn't even valid")
          }
        }
      }
    }

    tl_write_q.io.deq.ready := tl_write_req_generator.io.pop_tl_req
    assert(!tl_write_req_generator.io.pop_tl_req || tl_write_q.io.deq.valid, "popping a tl-req which doesn't exist")

    when (tl_write_q.io.deq.valid && read_resp_is_valid) {
      tl_write_req_generator.io.in.valid := true.B
    }
  }

  // Connect SRAM read resp ports to DMA's read-resp-port
  {
    accelerator.io.read_resps.flatten.foreach { read_resp =>
      when (read_resp.valid && read_resp.bits.to_dma) {
        dma.io.sram_read_resp.valid := read_resp.valid
        ChiselSRAMReadResp.connect(dma.io.sram_read_resp.bits, read_resp.bits)
      }
      when (read_resp.bits.to_dma) {
        read_resp.ready := dma.io.sram_read_resp.ready
      }
    }
    assert(PopCount(accelerator.io.read_resps.flatten.map { r => r.valid && r.bits.to_dma }) <= 1.U, "not sure yet how to handle multiple simultaneous read-resps that are headed towards the DMA")
  }

  // Connect beat splitter to TL write port
  beat_splitters.zipWithIndex.foreach { case (beat_splitter, beat_splitter_id) =>
    val tl_req = beat_splitter.io.out.bits.tl_req

    when (beat_splitter.io.out.valid) {
      val tl_a_port_id = (tl_req.addr / maxReqSizeInBytes.U) % config.nTlPorts.U

      for ((((tl_a, edge), tlb), portId) <- tls.map(_.a).zip(edges).zip(tlbs).zipWithIndex) {
        val tl_a_port_is_free = !any(tl_a_is_connected_to_tl_write_q(portId).take(beat_splitter_id))
        when (tl_a_port_id === portId.U && tl_a_port_is_free) {
          tlb.io.req.valid := true.B
          tlb.io.req.bits.cmd := M_XWR
          tlb.io.req.bits.size := tl_req.logLen
          tlb.io.req.bits.vaddr := tl_req.addr
          val tlb_hit = !tlb.io.resp.miss

          val (putLegal, put) = edge.Put(
            fromSource = tl_req.id,
            toAddress = tlb.io.resp.paddr | base_addr.U,
            lgSize = tl_req.logLen,
            data = beat_splitter.io.out.bits.dma_beat.data,
            mask = beat_splitter.io.out.bits.dma_beat.mask,
          )

          tl_a.valid := tlb_hit
          tl_a.bits := put

          beat_splitter.io.out.ready := tl_a.ready && tlb_hit

          tl_a_is_connected_to_sram_read_resps(portId) := true.B
          tl_a_is_connected_to_tl_write_q(portId)(beat_splitter_id) := true.B

          assert(!tl_a.valid || putLegal, s"illegal put request when connecting beat-splitter-$beat_splitter_id to tl-$portId")
        }
      }
    }
  }

  // Connect SRAM read-resp ports (other than for the first ones) directly to TL ports
  if (!config.multipleTlWriteReqGenerators) tl_write_qs.zipWithIndex.tail.foreach { case (tl_write_q, tl_write_q_id) =>
    val tl_req_valid = tl_write_q.io.deq.valid
    val tl_req = tl_write_q.io.deq.bits
    val sram_code = tl_req.tableEntry.sram_code
    val tl_a_port_id = (tl_req.addr / maxReqSizeInBytes.U) % config.nTlPorts.U

    for (sc <- design.sramCodes.values) {
      when (sram_code === sc.U) {
        accelerator.io.read_resps(accelerator.inner.getReadRespPortIdForSramCode(sc)).lift(tl_write_q_id).foreach { sram_read_resp =>
          for ((((tl_a, edge), tlb), portId) <- tls.map(_.a).zip(edges).zip(tlbs).zipWithIndex) {
            val tl_a_port_is_free = !any(tl_a_is_connected_to_tl_write_q(portId).take(tl_write_q_id))
            when (tl_req_valid && sram_read_resp.valid && sram_read_resp.bits.independent && tl_a_port_id === portId.U && tl_a_port_is_free) {
              tlb.io.req.valid := true.B
              tlb.io.req.bits.cmd := M_XWR
              tlb.io.req.bits.size := tl_req.logLen
              tlb.io.req.bits.vaddr := tl_req.addr
              val tlb_hit = !tlb.io.resp.miss

              val offset = tl_req.tableEntry.offset
              val (putLegal, put) = edge.Put(
                fromSource = tl_req.id,
                toAddress = tlb.io.resp.paddr | base_addr.U,
                lgSize = tl_req.logLen,
                data = (sram_read_resp.bits.data.head.asUInt << (offset  * 8.U)).asUInt,
                mask = (VecInit.fill(sram_read_resp.bits.data.head.getWidth / 8)(1.U).asUInt << offset).asUInt,
              )

              tl_a.valid := tlb_hit
              tl_a.bits := put

              tl_write_q.io.deq.ready := tl_a.ready && tlb_hit
              sram_read_resp.ready := tl_a.ready && tlb_hit

              tl_a_is_connected_to_tl_write_q(portId)(tl_write_q_id) := true.B
              tl_a_is_connected_to_sram_read_resps(tl_a_port_id) := true.B

              assert(!tl_a.valid || putLegal, s"illegal put request when connecting tl-write-q #$tl_write_q_id to tl-$portId")
              assert(!tl_a.valid || sram_read_resp.bits.last && all(sram_read_resp.bits.spans.map(_ === 1.U)), "for now, we only support writes from other tl-write-qs that are one single element")
            }
          }
        }
      }
    }
  }

  /*
  val debug_reads = VecInit(Seq.fill(config.nTlPorts)(false.B))
  val debug_writes = VecInit(Seq.fill(config.nTlPorts)(false.B))
  val debug_writes_negate = VecInit(Seq.fill(config.nTlPorts)(false.B))
  dontTouch(debug_reads); dontTouch(debug_writes); dontTouch(debug_writes_negate)
  tls.zip(debug_reads).zip(debug_writes).zip(debug_writes_negate).foreach { case (((tl, r), w), wn) =>
    val single_targets = Seq()
    val range_targets = Seq(("h807911c0".U, 0.U), ("h80002b40".U, "h3be480".U))

    val addr = tl.a.bits.address(31,0)
    val is_write = tl.a.bits.opcode === 1.U
    val start = Mux(is_write, addr +& Mux(tl.a.bits.mask.orR, PriorityEncoder(tl.a.bits.mask), 0.U), addr); require(maxReqSizeInBytes == beatWidthInBits/8, "this assumes we're only making one-beat write requests")
    val len = Mux(is_write, PopCount(tl.a.bits.mask), 1.U << tl.a.bits.size).asUInt; require(maxReqSizeInBytes == beatWidthInBits/8, "this assumes we're only making one-beat write requests")
    val end = start +& len

    val matches = any(single_targets.map(t => start <= t && end > t)) ||
      any(range_targets.map { case (tstart, tlen) => end >= tstart && start < (tstart +& tlen) })

    when (tl.a.valid && len > 0.U) {
      when (matches) {
        w := tl.a.bits.opcode === 1.U
        r := tl.a.bits.opcode === 4.U
        assert(w || r, "unknown opcode when debugging")
      }.otherwise {
        wn := tl.a.bits.opcode === 1.U
      }
    }
  }
  assert(!any(debug_writes_negate), "out-of-range write")
  */

  // Connect load-balancer configs
  accelerator.io.loadBalancerConfigs.zipWithIndex.foreach { case (lb, lbId) =>
    val lb_config = Reg(getChiselType(lb.bits))

    lb.valid := false.B
    lb.bits := lb_config

    val lb_code = cmd.bits.rs2(63,32)
    when (lb_code === lbId.U) {
      val value = cmd.bits.rs2(31,0)

      when (cmd.bits.inst.funct === ISA.SetLoadBalancingAxisSize.U) {
        cmd.ready := true.B
        decoder.io.in.valid := false.B

        val axisId = cmd.bits.rs1
        lb_config.axis_sizes(axisId) := value
      }.elsewhen (cmd.bits.inst.funct === ISA.SetLoadBalancingRfSize.U) {
        cmd.ready := true.B
        decoder.io.in.valid := false.B

        val axisId0 = cmd.bits.rs1(15,0)
        val axisId1 = cmd.bits.rs1(31,16)
        val axisId2 = cmd.bits.rs1(47,32)

        lb_config.rf_in_strides.zipWithIndex.foreach { case (rf_in_strides, i) =>
          when (axisId0 === i.U) {
            rf_in_strides(axisId1)(axisId2) := value
          }
        }
      }.elsewhen (cmd.bits.inst.funct === ISA.SetLoadBalancingClSize.U) {
        cmd.ready := true.B
        decoder.io.in.valid := false.B

        val axisId0 = cmd.bits.rs1(15,0)
        val axisId1 = cmd.bits.rs1(31,16)
        lb_config.cl_in_strides(axisId0)(axisId1) := value
      }.elsewhen (cmd.bits.inst.funct === ISA.IssueLoadBalancer.U) {
        cmd.ready := true.B
        decoder.io.in.valid := false.B
        lb.valid := true.B
      }
    }
  }

  // Flush TLB
  when (cmd.bits.inst.funct === ISA.Flush.U) {
    cmd.ready := true.B
    decoder.io.in.valid := false.B
  }
}

class StellarRoccConfig(conf: StellarConf) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val stellar = LazyModule(new Stellar(conf))
      stellar
    }
  )

  case SystemBusKey => up(SystemBusKey).copy(beatBytes = conf.beatWidthInBytes)

  case BankedL2Key => up(BankedL2Key).copy(
    coherenceManager = CoherenceManagerWrapper.broadcastManager,
  )

  case ExtMem => up(ExtMem).map(x => x.copy(
    nMemoryChannels = conf.nMemoryChannels,
    incohBase = if (conf.isCoherent) None else Some(conf.base_addr),
    master = x.master.copy(size = conf.dramSize),
  ))
})

class DenseStellarConfig extends StellarRoccConfig(DenseStellarConf(size=2, nAxes=2, has_accumulator=false, supports_convs=false))
class LargeDenseStellarConfig(size: Int) extends StellarRoccConfig(DenseStellarConf(size=size, nAxes=4, has_accumulator=true, supports_convs=true))
class SparseDenseStellarConfig(size: Int = 2, isLoadBalanced: Boolean = false) extends StellarRoccConfig(SparseDenseStellarConf(size=size, isLoadBalanced=isLoadBalanced))
class OuterSpaceStellarConfig(size: Int, hasMatmul: Boolean, hasMerger: Boolean) extends StellarRoccConfig(OuterSpaceStellarConf(size, hasMatmul, hasMerger))
class SCNNStellarConfig extends StellarRoccConfig(SCNNConf())
class SpArchMergerStellarConfig(isCheap: Boolean, throughput: Int, nLevels: Int, check_result: Boolean) extends StellarRoccConfig(SpArchMergerConf(isCheap = isCheap, throughput = throughput, nLevels = nLevels, check_result = check_result))

class DefaultStellarConfig extends DenseStellarConfig
