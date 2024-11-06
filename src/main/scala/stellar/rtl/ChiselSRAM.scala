package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._
import stellar.Util._
import stellar.{BitvectorMetadata, CompressedMetadata, DenseMatmulAccelerator, FiberTreeAxis, FiberTreeAxisMetadata, LinkedListMetadata, ReadBankingStrategy, WriteBankingStrategy}
import ChiselUtil._
import MatrixChiselUtil.sumvU

class ChiselSRAMReadReq(val nAxes: Int, val axisIteratorBits: Int, val spanBits: Int, val nMetadataBuffers: Int) extends Bundle {
  val address = Vec(nAxes, UInt(axisIteratorBits.W))
  val spans = Vec(nAxes, UInt(spanBits.W))

  val iteration_strides = Vec(nAxes, UInt(spanBits.W))

  val data_strides = Vec(nAxes, UInt(spanBits.W))
  val metadata_strides = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(spanBits.W)))) // metadata_strides[X][Y][Z] = the stride by which axis X contributes to axis Y's metadata address Z
  val metadata_strides_by_addr = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(spanBits.W)))) // metadata_strides_by_addr[X][Y][Z] = the stride by which axis X's 'addr' contributes to axis Y's metadata address Z

  val use_running_state_for_metadata = Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, Bool()))

  val axis = UInt(log2Up(nAxes).W)

  val should_read_data = Bool()
  val should_read_metadata = Bool()
  val metadata_buffer_id = UInt(FiberTreeAxisMetadata.metadata_buffer_id_bits.W)

  val to_dma = Bool()
  val to_regfile = Bool()
  val to_regfile_last_axis = UInt(log2Up(nAxes).W)
  val to_regfile_last_axis_log_size = UInt(log2Up(spanBits+1).W)

  val independent = Bool() // This tells us whether the request response is headed to the shared tl_write_q, or to an independent tl_write_q. Only relevant for banked SRAMs
  val adjacent = Bool()
  val update_running_len = Bool()
  val fused = Bool()

  val interleave = new Bundle {
    val should_push = Bool()
    val should_pop = Bool()
    val axis = UInt(log2Up(nAxes).W)
  }
  val should_trail_writes = Bool()
  val should_trail_writes_coarse_grained = Bool()

  val should_gather = Vec(nAxes, Bool())

  val is_recursive = Vec(nAxes, Bool())
  val recursive_dim = Vec(nAxes, UInt(spanBits.W))

  val reset_running_state = Bool()
}

object ChiselSRAMReadReq {
  def unifiedReadReq(read_reqs: Iterable[ChiselSRAMReadReq]): ChiselSRAMReadReq =
    new ChiselSRAMReadReq(
      nAxes=read_reqs.map(_.nAxes).maxOption.getOrElse(1),
      axisIteratorBits=read_reqs.map(_.axisIteratorBits).maxOption.getOrElse(1),
      spanBits=read_reqs.map(_.spanBits).maxOption.getOrElse(1),
      nMetadataBuffers=read_reqs.map(_.nMetadataBuffers).maxOption.getOrElse(1),
    )

  def connect(dst: ChiselSRAMReadReq, src: ChiselSRAMReadReq): Unit = {
    connectVecs(dst.address, src.address)
    connectVecs(dst.spans, src.spans)
    connectVecs(dst.iteration_strides, src.iteration_strides)
    connectVecs(dst.data_strides, src.data_strides)
    connectVecOfVecsOfVecs(dst.metadata_strides, src.metadata_strides)
    connectVecOfVecsOfVecs(dst.metadata_strides_by_addr, src.metadata_strides_by_addr)
    connectVecOfVecs(dst.use_running_state_for_metadata, src.use_running_state_for_metadata)
    dst.should_read_data := src.should_read_data
    dst.should_read_metadata := src.should_read_metadata
    dst.axis := src.axis
    dst.metadata_buffer_id := src.metadata_buffer_id
    dst.to_dma := src.to_dma
    dst.to_regfile := src.to_regfile
    dst.to_regfile_last_axis := src.to_regfile_last_axis
    dst.to_regfile_last_axis_log_size := src.to_regfile_last_axis_log_size
    dst.interleave := src.interleave
    dst.should_trail_writes := src.should_trail_writes
    dst.should_trail_writes_coarse_grained := src.should_trail_writes_coarse_grained
    connectVecs(dst.should_gather, src.should_gather, fillIn = Some(false.B))
    connectVecs(dst.is_recursive, src.is_recursive, fillIn = Some(false.B))
    connectVecs(dst.recursive_dim, src.recursive_dim)
    dst.reset_running_state := src.reset_running_state
  }

  def biconnect(dst: ReadyValidIO[ChiselSRAMReadReq], src: ReadyValidIO[ChiselSRAMReadReq]): Unit = {
    dst.valid := src.valid
    src.ready := dst.ready
    connect(dst.bits, src.bits)
  }
}

class ChiselSRAMReadResp[T <: Data](elemT: T, val elemsPerRead: Int, val nAxes: Int, val axisIteratorBits: Int, val expandedAddrBits: Int) extends Bundle {
  val compressed_address = Vec(nAxes, UInt(axisIteratorBits.W))
  val spans = Vec(nAxes, UInt(log2Up(elemsPerRead+1).W))

  val expanded_addresses = Vec(elemsPerRead, Vec(nAxes, UInt(expandedAddrBits.W)))

  val data = Vec(elemsPerRead, getChiselType(elemT))

  val is_data = Bool()
  val metadata_buffer_id = UInt(log2Up(FiberTreeAxisMetadata.n_metadata_buffers).W)

  val is_both_data_and_metadata = Bool()

  val to_dma = Bool()
  val to_regfile = Bool()
  val to_regfile_last = Bool()

  val independent = Bool()
  val adjacent = Bool()
  val update_running_len = Bool()

  val opCount = OpCount()

  val first = Bool()
  val last = Bool()
  val last_in_axis = Vec(nAxes, Bool())

  val axis_spans = Vec(nAxes, UDValid(UInt(axisIteratorBits.W)))

  // The variable below is mostly used for OuterSPACE mvouts right now
  val total_running_len = UInt(32.W) // TODO magic number

  val elemTBits = elemT.getWidth
}

object ChiselSRAMReadResp {
  def unifiedReadResp[T <: Data](elemT: T, resps: Seq[ChiselSRAMReadResp[T]]): ChiselSRAMReadResp[T] = {
    require(resps.forall(_.elemTBits <= elemT.getWidth))
    new ChiselSRAMReadResp(
      elemT=elemT,
      elemsPerRead=resps.map(_.elemsPerRead).maxOption.getOrElse(1),
      nAxes=resps.map(_.nAxes).maxOption.getOrElse(1),
      axisIteratorBits=resps.map(_.axisIteratorBits).maxOption.getOrElse(1),
      expandedAddrBits=resps.map(_.expandedAddrBits).maxOption.getOrElse(1),
    )
  }

  def connect[T <: Data](dst: ChiselSRAMReadResp[T], src: ChiselSRAMReadResp[T]): Unit = {
    connectVecs(dst.compressed_address, src.compressed_address, fillIn = Some(0.U))
    connectVecs(dst.spans, src.spans, fillIn = Some(1.U))
    connectVecOfVecs(dst.expanded_addresses, src.expanded_addresses)
    connectVecs(dst.data, src.data)
    dst.is_data := src.is_data
    dst.is_both_data_and_metadata := src.is_both_data_and_metadata
    dst.metadata_buffer_id := src.metadata_buffer_id
    dst.to_dma := src.to_dma
    dst.to_regfile := src.to_regfile
    dst.to_regfile_last := src.to_regfile_last
    dst.first := src.first
    dst.last := src.last
    connectVecs(dst.last_in_axis, src.last_in_axis, fillIn = Some(false.B))
    dst.total_running_len := src.total_running_len
    dst.independent := src.independent
    dst.adjacent := src.adjacent
    dst.update_running_len := src.update_running_len
  }
}

class ChiselSRAMWriteReq[T <: Data](elemT: T, val elemsPerWrite: Int, val nAxes: Int, val axisIteratorBits: Int, val spanBits: Int, val nMetadataBuffers: Int) extends Bundle {
  val address = Vec(nAxes, UInt(axisIteratorBits.W))
  val spans = Vec(nAxes, UInt(spanBits.W))
  val iteration_strides = Vec(nAxes, UInt(spanBits.W))
  val data_strides = Vec(nAxes, UInt(spanBits.W))

  val metadata_strides = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(spanBits.W))))
  val metadata_strides_by_addr = Vec(nAxes, Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, UInt(spanBits.W))))
  val use_running_state_for_metadata = Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, Bool()))

  val data = Vec(elemsPerWrite, getChiselType(elemT))

  // These params are used when writing DMA-retrieved meta/data into the SRAM
  val is_data = Bool()
  val axis = UInt(log2Up(nAxes).W)
  val metadata_buffer_id = UInt(log2Up(nMetadataBuffers).W)

  val is_both_data_and_metadata = Bool()

  // These params are used when writing meta/data from reg-files into the SRAM
  val from_regfile = Vec(nAxes, Bool())
  val from_regfile_last_axis = UInt(log2Up(nAxes).W)
  val from_regfile_last_axis_log_size = UInt(log2Up(spanBits+1).W)
  val from_regfile_metadata = Vec(nAxes, Vec(FiberTreeAxisMetadata.n_metadata_buffers, new ChiselSRAMWriteReq.FromRegfileMetadata))

  // These params are useful for synchronizing reads and writes that are happening simultaneously from the same SRAM
  val interleave = new Bundle {
    val should_push = Bool()
    val should_pop = Bool()
    val axis = UInt(log2Up(nAxes).W)
  }
  val should_trail_reads = Bool()
  val should_trail_reads_coarse_grained = Bool()

  val should_scatter = Vec(nAxes, Bool())

  val is_recursive = Vec(nAxes, Bool())
  val recursive_dim = Vec(nAxes, UInt(spanBits.W))

  val reset_running_state = Bool()

  val elemTBits: Int = elemT.getWidth
}

object ChiselSRAMWriteReq {
  def unifiedWriteReq[T <: Data](elemT: T, write_reqs: Iterable[ChiselSRAMWriteReq[T]]): ChiselSRAMWriteReq[T] = {
    require(write_reqs.forall(_.elemTBits <= elemT.getWidth))
    new ChiselSRAMWriteReq(
      elemT = elemT,
      elemsPerWrite = write_reqs.map(_.elemsPerWrite).maxOption.getOrElse(1),
      nAxes = write_reqs.map(_.nAxes).maxOption.getOrElse(1),
      axisIteratorBits = write_reqs.map(_.axisIteratorBits).maxOption.getOrElse(1),
      spanBits = write_reqs.map(_.spanBits).maxOption.getOrElse(1),
      nMetadataBuffers = write_reqs.map(_.nMetadataBuffers).maxOption.getOrElse(1),
    )
  }

  class FromRegfileMetadata extends Bundle {
    val valid = Bool()
    val coord = UInt(16.W) // TODO magic number
  }

  def connect[T <: Data](dst: ChiselSRAMWriteReq[T], src: ChiselSRAMWriteReq[T]): Unit = {
    // Connecting two sram-write-reqs can be a little complicated, due to inconsistent Vec lengths. This convenience
    // function tries to make the process a little simpler

    connectVecs(dst.address, src.address)
    connectVecs(dst.spans, src.spans)
    connectVecs(dst.iteration_strides, src.iteration_strides)
    connectVecs(dst.data_strides, src.data_strides)
    connectVecs(dst.iteration_strides, src.iteration_strides)
    connectVecOfVecsOfVecs(dst.metadata_strides, src.metadata_strides)
    connectVecOfVecs(dst.use_running_state_for_metadata, src.use_running_state_for_metadata)
    connectVecOfVecsOfVecs(dst.metadata_strides_by_addr, src.metadata_strides_by_addr)
    connectVecs(dst.data, src.data, fillIn = Some(DontCare))
    dst.is_data := src.is_data
    dst.axis := src.axis
    dst.metadata_buffer_id := src.metadata_buffer_id
    dst.is_both_data_and_metadata := src.is_both_data_and_metadata
    connectVecs(dst.from_regfile, src.from_regfile)
    dst.from_regfile_last_axis := src.from_regfile_last_axis
    dst.from_regfile_last_axis_log_size := src.from_regfile_last_axis_log_size
    dst.interleave := src.interleave
    dst.should_trail_reads := src.should_trail_reads
    dst.should_trail_reads_coarse_grained := src.should_trail_reads_coarse_grained
    connectVecs(dst.should_scatter, src.should_scatter, fillIn = Some(false.B))
    connectVecs(dst.is_recursive, src.is_recursive, fillIn = Some(false.B))
    connectVecs(dst.recursive_dim, src.recursive_dim)
    dst.reset_running_state := src.reset_running_state
    connectVecOfVecs(dst.from_regfile_metadata, src.from_regfile_metadata)
  }

  def biconnect[T <: Data](dst: DecoupledIO[ChiselSRAMWriteReq[T]], src: ChiselSRAMWriteReq[T], srcValid: Bool, srcReady: Bool): Unit = {
    dst.valid := srcValid
    connect(dst.bits, src)
    srcReady := dst.ready
  }

  def biconnect[T <: Data](dst: DecoupledIO[ChiselSRAMWriteReq[T]], src: DecoupledIO[ChiselSRAMWriteReq[T]]): Unit = {
    biconnect(dst, src.bits, srcValid = src.valid, srcReady = src.ready)
  }
}

class ChiselSRAMWriteFromRegfileReq[T <: Data](sramWriteReqT: ChiselSRAMWriteReq[T], nAxes: Int) extends Bundle {
  val req = getChiselType(sramWriteReqT)
  val offset = UInt(32.W) // TODO magic number
  val axisId = UInt(log2Up(nAxes).W)
  val first = Bool()
  val last = Bool()
  val opCount = OpCount()
}

class ChiselSRAMWriteFromRegfileResp[T <: Data, U <: Data](elemT: T, metadataT: U, elemsPerRead: Int, nMetadataBuffers: Int, nAxes: Int) extends Bundle {
  val data = Vec(elemsPerRead, getChiselType(elemT))
  val metadata = Vec(nMetadataBuffers, Vec(elemsPerRead, metadataT))
  val found = Vec(elemsPerRead, Bool())
  val axis_spans = Vec(nAxes, UDValid(UInt(32.W))) // TODO magic number
  val addrs = Vec(nAxes, UInt(32.W)) // TODO magic number
}

class ChiselSRAMDataTag(nAxes: Int) extends Bundle {
  val axisId = UInt(log2Up(nAxes).W)
}

class ChiselSRAMMetadataTag extends Bundle {
  val write = Bool()
}

class ChiselSRAMPipelineData[T <: Data](elemT: T, elemsPerRead: Int, elemsPerWrite: Int, nAxes: Int, nBanks: Int, axisIteratorBits: Int, val expandedAddrBits: Int, spanBits: Int, metadataAddrBits: Int, val nMetadataBuffers: Int) extends Bundle {
  val read_req = new ChiselSRAMReadReq(nAxes=nAxes, axisIteratorBits=axisIteratorBits, spanBits=spanBits, nMetadataBuffers=nMetadataBuffers)
  val write_req = new ChiselSRAMWriteReq(elemT=elemT, elemsPerWrite=elemsPerWrite, nAxes=nAxes, axisIteratorBits=axisIteratorBits, spanBits=spanBits, nMetadataBuffers=nMetadataBuffers)

  val data_addr = UInt(axisIteratorBits.W)
  val metadata_addrs = Vec(nAxes, Vec(nMetadataBuffers, UInt(metadataAddrBits.W)))

  val read_data = Vec(elemsPerRead, getChiselType(elemT))
  val expanded_addrs = Vec(elemsPerRead, Vec(nAxes, UInt(expandedAddrBits.W)))

  val write = Bool()

  // Variables that help synchronize memory units (including reg-files)
  val first_it = Bool()
  val last_it = Bool()
  val first_it_in_axis = Bool()
  val last_it_in_axis = Bool()
  val first_it_in_each_axis = Vec(nAxes, Bool())
  val last_it_in_each_axis = Vec(nAxes, Bool())

  val first_rf_access = Bool()
  val last_rf_access = Bool()

  val last_branch_in_each_axis = Vec(nAxes, Bool())

  val op_count = OpCount()

  val push_to_interleave_q = Bool()

  val span_traversed = Vec(nAxes, UInt(spanBits.W))
  val axis_spans = Vec(nAxes, UDValid(UInt(spanBits.W)))

  val is_split_across_banks = Bool()

  val total_running_len = UInt(32.W) // TODO magic number
}

class ChiselSRAMPipelineStage[T <: Data](elemT: T, metadataElemT: => UInt, canWrite: Boolean, canRead: Boolean, nElems: Int,
                                         elemsPerRead: Int, elemsPerWrite: Int, elemsPerWriteRf: Int, elemsPerRow: Int,
                                         addrBits: Int, spanBits: Int, nMetadataBuffers: Int,
                                         writeFromRfReqPort: ChiselSRAMWriteFromRegfileReq[T],
                                         metadataPorts: Seq[(SyncMemWriteReq[UInt], SyncMemReadReq[ChiselSRAMMetadataTag], SyncMemReadResp[UInt, ChiselSRAMMetadataTag])],
                                         metadataConf: FiberTreeAxisMetadata,
                                         maxElemsInRf: Option[Int], multipleNElemsLookupPorts: Boolean,
                                         axis: FiberTreeAxis.Type, nAxes: Int, val nBanks: Int, nBranches: Int,
                                         axisId: Int, bankId: Int, outermostBranchedAxisId: Int,
                                         hardCodedValues: ChiselSRAMPipelineData[T] => SMap[Data, Data],
                                         strideDivisibleBy: Int,
                                         no_simultaneous_reads_and_writes: Boolean,
                                         interleave_pop_in_last_stage: Boolean,
                                         can_read_inflect: Boolean, can_write_inflect: Boolean,
                                         sram_name_opt: Option[String] = None,
                                         isDummy: Boolean = false) extends Module {
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo
  val for_verilator = true // Verilator sometimes gives different results than VCS does when simulating the same RTL. This flag helps us deal with Verilator quirks

  val is_innermost = axisId == 0
  val is_outermost = axisId == nAxes - 1
  val log_elems_per_row = log2Up(elemsPerRow); assert(isPow2(elemsPerRow))

  val (metadata_write_ts, metadata_read_req_ts, metadata_read_resp_ts) = metadataPorts.unzip3
  val metadataAddrBits = 32 // TODO get this from constructor

  def pipeline_data_t = new ChiselSRAMPipelineData(elemT=elemT, elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite,
    nAxes=nAxes, nBanks=nBanks, axisIteratorBits=addrBits, expandedAddrBits=metadataAddrBits, spanBits=spanBits,
    metadataAddrBits=metadataAddrBits, nMetadataBuffers=nMetadataBuffers)
  def data_sram_row_t = Vec(elemsPerRow, elemT)
  def data_sram_tag_t = new ChiselSRAMDataTag(nAxes)

  def muxOnWrite[U <: Data](in: ChiselSRAMPipelineData[T], ifWrite: => U, ifRead: => U): U = {
    if (canWrite && canRead) Mux(in.write, ifWrite, ifRead)
    else if (canWrite) ifWrite
    else ifRead
  }

  def isWrite(_in: ChiselSRAMPipelineData[T]): Bool = {
    if (!canWrite) false.B
    else if (canWrite && !canRead) true.B
    else _in.write
  }

  val (spanTraversedShouldBeSet, mightAccessRf, alwaysScatters) = {
    // These variables are just used to help with manual const-prop
    val _in = pipeline_data_t
    val _hardCodedValues = hardCodedValues(_in)
    val _spanTraversedShouldBeSet = !Seq(_in.write_req.should_trail_reads, _in.read_req.should_trail_writes).forall(_should_trail => _hardCodedValues.get(_should_trail).exists(x => (x.litOption: Option[BigInt]).contains(BigInt(0))))
    val _mightAccessRf = !(Option.when(canWrite)(_in.write_req.from_regfile.toSeq).toSeq.flatten.toSeq ++ Option.when(canRead)(_in.read_req.should_gather.toSeq :+ _in.read_req.to_regfile).toSeq.flatten.toSeq).forall(_access_rf => _hardCodedValues.get(_access_rf).exists(x => (x.litOption: Option[BigInt]).contains(0)))
    val _alwaysScatters = Option.when(canWrite)(_in.write_req.should_scatter(axisId)).toSeq.forall(_should_scatter => _hardCodedValues.get(_should_scatter).exists(x => (x.litOption: Option[BigInt]).contains(1)))
    (_spanTraversedShouldBeSet, _mightAccessRf, _alwaysScatters)
  }
  val couldBeBranched = nBranches > 1 && mightAccessRf

  val n_shared_state_across_banks = 3

  val io = IO(new Bundle {
    val metadata_write_reqs = MixedVec(metadata_write_ts.map(p => Decoupled(p)))
    val metadata_read_reqs = MixedVec(metadata_read_req_ts.map(p => Decoupled(p)))
    val metadata_read_resps = MixedVec(metadata_read_resp_ts.map(p => Flipped(Decoupled(p))))

    val data_write_req = Decoupled(new SyncMemWriteReq(data_sram_row_t, addrBits))
    val data_read_req = Decoupled(new SyncMemReadReq(addrBits, data_sram_tag_t, nPaths = 1))
    val data_read_resp = Flipped(Decoupled(new SyncMemReadResp(data_sram_row_t, data_sram_tag_t, nPaths = 1)))

    val in = Flipped(Decoupled(pipeline_data_t))
    val out = Decoupled(pipeline_data_t)

    val write_from_regfile_req = Decoupled(writeFromRfReqPort) // TODO rename this to "access_regfile_req" or something since reads can also use it now (for gathering)
    val not_all_rf_outs_found = Input(Bool())
    val found_rf_outs_num = Input(UInt(32.W)) // TODO magic number
    val rf_out = Input(elemT) // This is used for gathers // TODO right now, we only support returning a single value from the RFs/CoordLookups at a time
    val write_from_regfile_axis_spans = Vec(nAxes, Flipped(Valid(UInt(spanBits.W))))

    val interleave_q_push = Output(Bool())
    val interleave_q_pop = new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())

      def fire = valid && ready
    }

    val span_traversed = Vec(nBranches, Valid(Vec(nAxes, UInt(spanBits.W))))
    val last_it_in_axis = Output(Vec(nBranches, Vec(nAxes, Bool())))
    val next_node_added = Vec(nBranches, Output(Bool())) // This is only used for linked-list trailing
    val axis_spans = Vec(nBranches, Vec(nAxes, Valid(UInt(spanBits.W))))
    val other_stage_span_traversed = Vec(nBranches, Flipped(Valid(Vec(nAxes, UInt(spanBits.W)))))
    val other_stage_last_it_in_axis = Input(Vec(nBranches, Vec(nAxes, Bool())))
    val other_stage_next_node_added = Vec(nBranches, Input(Bool()))
    val other_stage_axis_spans = Vec(nBranches, Vec(nAxes, Flipped(Valid(UInt(spanBits.W)))))
    val other_stage_busy = Input(Bool()) // This is used to help with coarse-grained stalling

    val other_banks_shared_state_increment = Input(Vec(n_shared_state_across_banks, Vec(nBanks, UInt(spanBits.W))))
    val other_banks_shared_state_reset = Input(Vec(n_shared_state_across_banks, Bool()))
    val incrementing_shared_state = Output(Vec(n_shared_state_across_banks, UInt(spanBits.W)))
    val resetting_shared_state = Output(Vec(n_shared_state_across_banks, Bool()))

    val op_count = Output(OpCount())
    val other_banks_op_count = Flipped(Valid(OpCount()))

    val recursive_buffer_fill_count = Valid(UInt(32.W)) // TODO magic number
    val other_banks_recursive_buffer_fill_count = Vec(nBanks, Flipped(Valid(UInt(32.W)))) // TODO magic number

    val lookup_future_stages = Vec(if (multipleNElemsLookupPorts) nBranches else 1, Flipped(new NElemsInAxisLookup(nAxes)))
    val lookup_past_stages = Vec(if (multipleNElemsLookupPorts) nBranches else 1 /* TODO does this length work when branch-sizes are different across pipeline stages? */, new NElemsInAxisLookup(nAxes)) // Past-stages can only lookup from this port using the outermost-branched-stage's stageId

    val busy = Output(Bool()) // This is the "busy" that's meant to be used for fencing
    val full_busy = Output(Bool()) // This just tells you whether or not any stage at all is currently busy
    val op_count_busy = Output(Bool()) // This is used to synchronize op-counts across banks. It tells you whether the local op-count is currently being used, or if it's safe to overwrite
  })
  // dontTouch(io.in.valid)
  // dontTouch(io.in.ready)

  val in = Reg(getChiselType(io.in.bits))
  val in_valid = RegInit(false.B)

  val last_rf_axis = muxOnWrite(in, in.write_req.from_regfile_last_axis, in.read_req.to_regfile_last_axis)
  val infinite_last_rf_axis_size = (1.U << spanBits).asUInt
  val last_rf_axis_size = muxOnWrite(in, 1.U << in.write_req.from_regfile_last_axis_log_size, 1.U << in.read_req.to_regfile_last_axis_log_size).asUInt
  val last_rf_axis_size_is_infinite = last_rf_axis_size >= infinite_last_rf_axis_size

  val should_interleave_push = muxOnWrite(in,
    in.write_req.interleave.should_push && in.write_req.interleave.axis === axisId.U,
    in.read_req.interleave.should_push && in.read_req.interleave.axis === axisId.U)
  val should_interleave_pop = muxOnWrite(in,
    in.write_req.interleave.should_pop && (if (interleave_pop_in_last_stage) is_innermost.B else (in.write_req.interleave.axis === axisId.U)),
    in.read_req.interleave.should_pop && (if (interleave_pop_in_last_stage) is_innermost.B else (in.read_req.interleave.axis === axisId.U)))
  val should_trail = muxOnWrite(in, in.write_req.should_trail_reads, in.read_req.should_trail_writes)

  val should_scatter = isWrite(in) && in.write_req.from_regfile(axisId) && in.write_req.should_scatter(axisId)
  val should_gather = !isWrite(in) && in.read_req.should_gather(axisId)

  val reset_running_state = in_valid && muxOnWrite(in, in.write_req.reset_running_state && in.write_req.axis === axisId.U,
    in.read_req.reset_running_state && in.read_req.axis === axisId.U)

  val opCount = RegInit(OpCount(0.U))
  io.op_count := opCount

  io.recursive_buffer_fill_count.valid := false.B
  io.recursive_buffer_fill_count.bits := DontCare

  def is_branched(_in: ChiselSRAMPipelineData[T]): Bool = {
    if (couldBeBranched) muxOnWrite(_in, any(_in.write_req.from_regfile), _in.read_req.to_regfile || any(_in.read_req.should_gather))
    else false.B
  }

  def branch_id_of(address: Seq[UInt]): UInt = {
    // TODO i'm not sure if this will work when the address has a 'bias'
    if (couldBeBranched) address(outermostBranchedAxisId + 1) else 0.U
  }
  def branch_id_of(_in: ChiselSRAMPipelineData[T], assumedIsBranched: Boolean = false): UInt = {
    Mux(assumedIsBranched.B || is_branched(_in), muxOnWrite(_in, branch_id_of(_in.write_req.address), branch_id_of(_in.read_req.address)), 0.U)
  }

  def is_last_arriving_branch(_in: ChiselSRAMPipelineData[T]): Bool = {
    !is_branched(_in) ||
      all(_in.last_it_in_each_axis.zip(_in.last_branch_in_each_axis).slice(axisId+1,outermostBranchedAxisId+2).flatMap(t=>Seq(t._1,t._2))) ||
      _in.last_rf_access && muxOnWrite(_in, any(_in.write_req.from_regfile), _in.read_req.to_regfile || any(_in.read_req.should_gather))
  }

  def is_first_arriving_branch(_in: ChiselSRAMPipelineData[T]): Bool = {
    val first_branch_in_each_axis = _in.span_traversed.map(_ === 0.U)
    !is_branched(_in) ||
      all(_in.first_it_in_each_axis.zip(first_branch_in_each_axis).slice(axisId+1,outermostBranchedAxisId+2).flatMap(t=>Seq(t._1,t._2))) ||
      _in.first_rf_access && muxOnWrite(_in, any(_in.write_req.from_regfile), _in.read_req.to_regfile || any(_in.read_req.should_gather))
  }

  def branched_span_traversed(traversed: Vec[UInt], isBranched: Bool): Vec[UInt] = {
    // TODO instead of just using the lagging branch to determine the "span_traversed," we should output the
    //  span_traversed for each individual branch. This would allow more fine-grained synchronization between branches
    //  in the read and write pipelines
    val result = WireInit(traversed)
    if (!is_outermost && couldBeBranched) {
      when (isBranched) {
        result(outermostBranchedAxisId+1) := 0.U
      }
    }
    result
  }

  val span = muxOnWrite(in, in.write_req.spans(axisId), in.read_req.spans(axisId))
  val span_iterator = RegInit(0.U(spanBits.W))
  val span_step_size = WireInit(0.U(spanBits.W))
  val stepping = WireInit(io.out.ready)

  val starting = WireInit(stepping && (span_iterator === 0.U))
  val ending = WireInit(stepping && (span_iterator +& span_step_size >= span))
  io.in.ready := (!in_valid || ending) &&
    !(muxOnWrite(io.in.bits, io.in.bits.write_req.should_trail_reads_coarse_grained, io.in.bits.read_req.should_trail_writes_coarse_grained) && io.other_stage_busy)

  def isBusy(valid: Bool, in: ChiselSRAMPipelineData[T], for_full: Boolean = false): Bool = {
    valid && (in.write || (for_full.B || !in.read_req.to_regfile))
  }

  io.busy := isBusy(in_valid, in)
  io.full_busy := isBusy(in_valid, in, for_full = true)
  io.op_count_busy := io.full_busy // TODO this is the default, but we can make it more discerning to increase performance

  io.out.valid := in_valid
  io.out.bits := in
  io.out.bits.span_traversed.take(axisId).foreach(_ := 0.U)
  io.out.bits.total_running_len := 0.U

  io.data_read_req.valid := false.B
  io.data_read_req.bits := DontCare
  io.data_write_req.valid := false.B
  io.data_write_req.bits := DontCare
  io.data_read_resp.ready := false.B
  io.metadata_read_reqs.foreach(_.valid := false.B)
  io.metadata_read_reqs.foreach(_.bits := DontCare)
  io.metadata_write_reqs.foreach(_.valid := false.B)
  io.metadata_write_reqs.foreach(_.bits := DontCare)
  io.metadata_read_resps.foreach(_.ready := false.B)

  io.write_from_regfile_req.valid := false.B
  ChiselSRAMWriteReq.connect(io.write_from_regfile_req.bits.req, in.write_req)
  io.write_from_regfile_req.bits.first := io.out.bits.first_rf_access
  io.write_from_regfile_req.bits.last := io.out.bits.last_rf_access
  io.write_from_regfile_req.bits.axisId := axisId.U
  io.write_from_regfile_req.bits.offset := DontCare
  io.write_from_regfile_req.bits.opCount := Mux(last_rf_axis === axisId.U, opCount, in.op_count)

  io.interleave_q_push := false.B
  io.interleave_q_pop.ready := false.B

  if (!spanTraversedShouldBeSet) {
    io.span_traversed := DontCare
    io.last_it_in_axis := DontCare
  }

  io.next_node_added.foreach(_ := false.B)

  io.axis_spans.foreach { io_axis_spans =>
    connectVecs(io_axis_spans.map(_.valid), in.axis_spans.map(_.valid).map(_ && in_valid))
    connectVecs(io_axis_spans.map(_.bits), in.axis_spans.map(_.bits))
  }

  io.incrementing_shared_state.foreach(_ := 0.U)
  io.resetting_shared_state.foreach(_ := false.B)

  io.lookup_future_stages.foreach(_.address := DontCare)
  io.lookup_past_stages.foreach(_.nElems := 0.U)

  when (stepping) {
    span_iterator := span_iterator + span_step_size
  }

  when (io.in.fire) {
    in := io.in.bits
    in_valid := true.B
    span_iterator := 0.U
  }.elsewhen(ending) {
    in_valid := false.B
  }

  when (!in_valid) {
    in.op_count := io.in.bits.op_count
  }

  private def updatedAddrs(_in: ChiselSRAMPipelineData[T], iterator: UInt, addr: UInt = 0.U) = noPrefix {
    val data_stride = align(muxOnWrite(_in, _in.write_req.data_strides(axisId), _in.read_req.data_strides(axisId)), strideDivisibleBy)
    val data_addr = (_in.data_addr +& (iterator * data_stride)).asTypeOf(_in.data_addr)

    val metadata_strides = muxOnWrite(_in, _in.write_req.metadata_strides(axisId), _in.read_req.metadata_strides(axisId))
    val metadata_strides_by_addr = muxOnWrite(_in, _in.write_req.metadata_strides_by_addr(axisId), _in.read_req.metadata_strides_by_addr(axisId))

    val metadata_addrs = noPrefix(_in.metadata_addrs.zip(metadata_strides).zip(metadata_strides_by_addr).map { case ((base_metadata_addr_per_buffer, metadata_stride_per_buffer), metadata_stride_by_addr_per_buffer) =>
      base_metadata_addr_per_buffer.zip(metadata_stride_per_buffer).zip(metadata_stride_by_addr_per_buffer).map { case ((base_metadata_addr, metadata_stride), metadata_stride_by_addr) =>
        val _metadata_stride_by_addr = metadata_stride_by_addr(0).asUInt // TODO just to make timing-critical-paths shorter, we are currently assuming that strides-by-addr are 0 or 1. However, we should remove this assumption later
        (base_metadata_addr +& (iterator * metadata_stride) +& (addr * _metadata_stride_by_addr)).asTypeOf(UInt(_in.metadata_addrs.head.getWidth.W))
      }
    })

    (data_addr, metadata_addrs)
  }

  private def hardCodeValues(_in: ChiselSRAMPipelineData[T], exclude: Seq[Data] = Seq.empty): Unit = {
    hardCodedValues(_in).collect { case (d, s) if !exclude.contains(d) =>
      d := s
    }

    if (!canRead) {
      _in.write := true.B
    } else if (!canWrite) {
      _in.write := false.B
    }
  }
  private def addrFieldsOfThisAxis(_in: ChiselSRAMPipelineData[T]): Seq[Data] = {
    Seq(_in.read_req.address(axisId), _in.write_req.address(axisId))
  }

  if (isDummy) {
    // NOP
  } else if (axis == FiberTreeAxis.Dense) {
    require(nBranches == 1, "branching not yet supported for dense axes")
    require(maxElemsInRf.isEmpty, "max-elems-in-rf not yet supported for dense axes")
    assert(!(in_valid && isWrite(in) && in.write_req.from_regfile(axisId)) || (elemsPerWriteRf == elemsPerWrite).B, "haven't added support for different elemsPerWriteRf and elemsPerWrite to Compressed axis yet")

    val bias = muxOnWrite(in, in.write_req.address(axisId), in.read_req.address(axisId))
    val iterator = span_iterator +& bias

    val (data_addr, metadata_addrs) = updatedAddrs(in, iterator)

    val stride = muxOnWrite(in, in.write_req.iteration_strides(axisId), in.read_req.iteration_strides(axisId))
    span_step_size := stride

    val rf_inflection_point = (nBanks>1).B && stride === 1.U && last_rf_axis === axisId.U &&
      muxOnWrite(in, can_write_inflect.B && any(in.write_req.from_regfile), can_read_inflect.B && (in.read_req.to_regfile || any(in.read_req.should_gather))) &&
      !(last_rf_axis_size_is_infinite && !any(in.read_req.should_gather))
    when (rf_inflection_point) {
      // This when-clause is optional (the code should still functionally work without it), but it helps speed up
      // banking in some cases
      // assert(!last_rf_axis_size_is_infinite, "i'm not sure if this code works when the rf-axis size can be non-infinite")
      span_step_size := minOf(span - span_iterator, nBanks.U); assert(!in_valid || stride === 1.U)
    }

    val span_of_next_stage = {
      assert(!in_valid || stride === 1.U || stride === span_step_size, "we haven't yet added support for 'span_step_size / stride' here, so we assume either than 'stride === 1.U' or that 'span_step_size === stride'")
      Mux(stride === 1.U, span_step_size, 1.U)
    }

    val next_stage_ready = WireInit(io.out.ready)

    val waiting_to_interleave = noPrefix {
      // Handle interleaving stalling code
      val first_read = all(in.read_req.address.updated(axisId, span_iterator).zipWithIndex.map { case (addr, i) =>
        val ignoreAddr = if (interleave_pop_in_last_stage) i.U < in.read_req.interleave.axis else (i < axisId).B
        any(Seq(addr === 0.U, ignoreAddr))
      })

      val new_pop_needed = if (interleave_pop_in_last_stage) {
        all(muxOnWrite(in, in.write_req.address, in.read_req.address).zipWithIndex.map { case (addr, i) =>
          addr === 0.U || i.U >= muxOnWrite(in, in.write_req.interleave.axis, in.read_req.interleave.axis)
        })
      } else
        true.B

      val must_wait_for_interleave = should_interleave_pop && new_pop_needed && (in.write || !first_read)
      val stalling_for_interleaving = must_wait_for_interleave && !io.interleave_q_pop.valid

      when (stalling_for_interleaving) {
        io.out.valid := false.B
        stepping := false.B
      }

      io.interleave_q_pop.ready := in_valid && must_wait_for_interleave && next_stage_ready

      // Seq(in.write_req.interleave, in.read_req.interleave, io.interleave_q_pop, io.interleave_q_push, first_read, new_pop_needed, must_wait_for_interleave, stalling_for_interleaving).filterNot(_.isLit).foreach(dontTouch(_))

      stalling_for_interleaving
    }

    if (axisId == nAxes-1)
      assert(!in_valid || (in.first_it && in.last_it))

    val out = WireInit(in)

    out.data_addr := data_addr
    connectVecOfVecs(out.metadata_addrs, metadata_addrs)
    out.expanded_addrs.foreach(_(axisId) := iterator)
    out.read_req.address(axisId) := iterator
    out.write_req.address(axisId) := iterator
    out.read_req.spans(axisId) := span_of_next_stage
    out.write_req.spans(axisId) := span_of_next_stage
    out.first_it := in.first_it && span_iterator === 0.U
    out.last_it := in.last_it && ending
    out.first_it_in_axis := span_iterator === 0.U && stepping
    out.last_it_in_axis := ending
    out.first_rf_access := Mux(last_rf_axis === axisId.U, (span_iterator % last_rf_axis_size === 0.U) && stepping || starting, in.first_rf_access && starting)
    out.last_rf_access := Mux(last_rf_axis === axisId.U, (span_iterator +& span_step_size) % last_rf_axis_size === 0.U && stepping || ending, in.last_rf_access && ending); assert(!in_valid || !muxOnWrite(in, any(in.write_req.from_regfile), in.read_req.to_regfile || any(in.read_req.should_gather)) || last_rf_axis =/= axisId.U || last_rf_axis_size_is_infinite || last_rf_axis_size % span_step_size === 0.U, "we might need to adjust the span-step-size if it doesn't divide cleanly into the last-rf-axis size")
    out.last_branch_in_each_axis(axisId) := true.B; require(nBranches == 1)
    out.op_count := Mux(last_rf_axis === axisId.U, opCount, in.op_count)
    out.push_to_interleave_q := {
      val last_write = in.last_it && ending
      (should_interleave_push && (!in.write || !last_write)) || (in.push_to_interleave_q && ending)
    }
    out.span_traversed(axisId) := span_iterator

    io.out.bits := out

    if (spanTraversedShouldBeSet) {
      io.span_traversed.foreach { io_span_traversed =>
        io_span_traversed.valid := false.B
        io_span_traversed.bits := DontCare
      }
      io.span_traversed(branch_id_of(in, true)).valid := in_valid && (!in.write || any(in.write_req.from_regfile))
      io.span_traversed(branch_id_of(in, true)).bits := out.span_traversed

      io.last_it_in_axis.foreach(_ := DontCare)
      io.last_it_in_axis(branch_id_of(in, true)) := out.last_it_in_each_axis
    }

    val op_count_incrementing = WireInit(false.B)
    when (in_valid && last_rf_axis === axisId.U &&
      muxOnWrite(in, any(in.write_req.from_regfile.take(axisId+1)), in.read_req.to_regfile || any(in.read_req.should_gather.take(axisId+1))) &&
      ((span_iterator +& span_step_size) % last_rf_axis_size === 0.U || ending)) {
      // Increment op-count // TODO add similar code for non-dense axes
      opCount := opCount + 1.U
      op_count_incrementing := true.B
    }
    when(io.other_banks_op_count.fire) {
      opCount := io.other_banks_op_count.bits
      assert(io.other_banks_op_count.bits >= opCount)
      // if (assert(!in_valid, "i'm not sure if this works when the pipeline stage is currently in operation")
    }

    assert(is_innermost.B || !in_valid || !should_gather, "we don't yet support gathering from outer dense axes")

    if (is_innermost) {
      val dataAddrBits = log2Up(nElems+1)

      // Note: Reads from the SRAM happen over multiple pipeline stages
      val valid2 = RegInit(false.B)
      val valid3 = RegInit(false.B)
      val fire2 = io.data_read_req.fire || in_valid && !in.write && ending
      val fire3 = Wire(Bool())

      val base_addr = data_addr
      val sram_addr = reduceWidth(reduceWidth(base_addr, dataAddrBits) +&
        Mux(should_gather || should_scatter,
          reduceWidth(reduceWidth(io.rf_out.asUInt, dataAddrBits), dataAddrBits),
          0.U),
      dataAddrBits)
      val sram_row = sram_addr >> log_elems_per_row
      val sram_col = sram_addr & ((1 << log_elems_per_row)-1).U

      val read_col_counter = RegInit(0.U(log2Up(elemsPerRead).W))
      val last_read = WireInit(false.B)

      span_step_size := minOf(
        span - span_iterator,
        elemsPerRow.U - sram_col, // we assume here that the bitvector words per row is the same as the elem ones
        muxOnWrite(in, elemsPerWrite.U, elemsPerRead.U - read_col_counter)
      )

      assert(!in_valid || (stride === 1.U && muxOnWrite(in, in.write_req.data_strides(axisId), in.read_req.data_strides(axisId)) <= 1.U))

      val waiting_to_trail = should_trail && io.other_stage_span_traversed.head.valid && {
        assert(is_innermost, "not sure if this code block still works if this isn't the innermost stage")
        assert(!should_trail || (nBranches == 1).B, "this code assumes that we only have to check the other_stage_span_traversed for a single branch")

        val traversed = WireInit(in.span_traversed)
        traversed(axisId) := span_iterator +& span_step_size

        !compareVecs(traversed, io.other_stage_span_traversed.head.bits, {_ < _}, orEquals = true)
      }
      val waiting = waiting_to_trail || waiting_to_interleave

      next_stage_ready := !valid2 || fire3

      if (canRead) {
        io.data_read_req.valid := in_valid && !in.write && !waiting &&
          (!should_gather || io.write_from_regfile_req.ready && !io.not_all_rf_outs_found)
        io.data_read_req.bits.addr := sram_row
        io.data_read_req.bits.tag.axisId := axisId.U
      }

      if (canWrite) {
        io.data_write_req.valid := in_valid && in.write && (!valid2 || fire3) && !waiting &&
          (!in.write_req.from_regfile(axisId) || io.write_from_regfile_req.ready && !io.not_all_rf_outs_found)
        io.data_write_req.bits.addr := sram_row
        io.data_write_req.bits.data.zipWithIndex.foreach { case (d, i) =>
          when (i.U >= sram_col) {
            d := in.write_req.data((i.U -& sram_col) +& span_iterator)
          }.otherwise {
            d := DontCare
          }
        }
        io.data_write_req.bits.mask.zipWithIndex.foreach { case (m, i) =>
          m := i.U >= sram_col && i.U < sram_col +& span_step_size
        }
      }

      if (canWrite && !canRead && mightAccessRf) {
        io.write_from_regfile_req.valid := in_valid && in.write && in.write_req.from_regfile(axisId) && !waiting
        io.write_from_regfile_req.bits.req.is_data := true.B
        io.write_from_regfile_req.bits.req.spans(axisId) := span_step_size
        io.write_from_regfile_req.bits.req.address(axisId) := span_iterator
        io.write_from_regfile_req.bits.offset := sram_col
      } else if (canRead && !canWrite && mightAccessRf) {
        io.write_from_regfile_req.valid := in_valid && !in.write && should_gather && !waiting &&
          io.data_read_req.ready && next_stage_ready
        io.write_from_regfile_req.bits.req.spans(axisId) := span_step_size; assert(!io.write_from_regfile_req.valid || span_step_size === 1.U, "for now, I'm assuming that we only read-out one value every iteration")
        io.write_from_regfile_req.bits.req.address(axisId) := span_iterator
      } else if (canWrite && canRead) {
        require(false, "We haven't yet added support for dense axes which can both write-from-regfiles and gather")
      }

      stepping := muxOnWrite(in,
        io.data_write_req.ready && (!in.write_req.from_regfile(axisId) || io.write_from_regfile_req.fire),
        io.data_read_req.ready && (!should_gather || io.write_from_regfile_req.ready && next_stage_ready))

      when ((should_gather || should_scatter) && io.write_from_regfile_req.ready && io.not_all_rf_outs_found && next_stage_ready) {
        ending := true.B
      }

      out.expanded_addrs.map(_.head).zipWithIndex.foreach { case (ea, i) =>
        ea := iterator +& i.U -& read_col_counter
      }
      out.read_req.spans(axisId) := span_step_size +& read_col_counter
      out.read_req.address(axisId) := iterator -& read_col_counter

      when (fire2) {
        val new_read_col_counter = Mux(ending || elemsPerRead.U === 1.U, 0.U, (read_col_counter + span_step_size)(read_col_counter.getWidth-1, 0))
        read_col_counter := new_read_col_counter
        last_read := new_read_col_counter === 0.U || ending
        assert(isPow2(elemsPerRead) && (new_read_col_counter.getWidth == log2Ceil(elemsPerRead) || elemsPerRead == 1), s"we require elemsPerRead ($elemsPerRead) to be a power-of-2 so that we can guarantee read_col_counter wraps around to 0 with just a simple addition")
      }

      val sram_col2 = RegEnable(sram_col, fire2)
      val read_col_counter2 = RegEnable(read_col_counter, fire2)
      val last_read2 = RegEnable(last_read, fire2)
      val must_wait_for_read_resp2 = RegEnable(io.data_read_req.fire, fire2)
      val starting2 = RegEnable(span_iterator === 0.U && stepping, fire2)
      val ending2 = RegEnable(ending, fire2)
      val out2 = RegEnable(out, fire2)

      when (fire2) {
        valid2 := true.B
      }.elsewhen(fire3) {
        valid2 := false.B
      }

      when (!valid2) {
        out2.op_count := out.op_count
      }

      val last_read3 = RegEnable(last_read2, fire3)
      val must_wait_for_read_resp3 = RegEnable(must_wait_for_read_resp2, fire3)
      val starting3 = RegEnable(starting2, fire3)
      val ending3 = RegEnable(ending2, fire3)
      val out3 = RegEnable(out2, fire3)
      val out_data3 = Reg(getChiselType(io.out.bits.read_data))

      fire3 := io.data_read_resp.fire || (valid2 && !must_wait_for_read_resp2 && (!valid3 || io.out.ready))
      when (fire3) {
        valid3 := true.B
        out_data3.zipWithIndex.foreach { case (d, i) =>
          when (i.U >= read_col_counter2) {
            d := io.data_read_resp.bits.data(sram_col2 +& i.U -& read_col_counter2)
          }
        }
      }.elsewhen(io.out.fire) {
        valid3 := false.B
      }

      when (!valid3) {
        out3.op_count := out2.op_count
      }

      io.out.valid := valid3 && last_read3
      io.out.bits := out3
      io.out.bits.read_data := Mux(out3.read_req.axis === axisId.U, out_data3, out3.read_data)
      io.out.bits.first_it_in_axis := starting3
      io.out.bits.last_it := out3.last_it && ending3
      io.out.bits.last_it_in_axis := ending3

      when (valid3 && !must_wait_for_read_resp3) {
        assert(!in.write, "the line below is meant for empty reads, not for writes")
        io.out.bits.read_req.spans(axisId) := 0.U
      }

      io.data_read_resp.ready := !valid3 || io.out.ready || !last_read3

      io.interleave_q_push := valid3 && out3.push_to_interleave_q && ending3

      when (in_valid && in.write && in.write_req.from_regfile(axisId)) {
        io.out.bits := out
        io.interleave_q_push := in.push_to_interleave_q && ending
      }

      when (isBusy(in_valid, in) || isBusy(valid2, out2) || isBusy(valid3, out3)) {
        io.busy := true.B
      }
      when (isBusy(in_valid, in, for_full=true) || isBusy(valid2, out2, for_full=true) || isBusy(valid3, out3, for_full=true)) {
        io.full_busy := true.B
      }

      io.op_count_busy := isBusy(in_valid, in, for_full=true) && !ending
      when (op_count_incrementing && !io.op_count_busy) {
        io.op_count := opCount + 1.U
      }

      Seq(out2, out3).foreach { x =>
        hardCodeValues(x, addrFieldsOfThisAxis(x))
        when (reset.asBool) {
          x.op_count := OpCount(0.U)
        }
      }
    }
    hardCodeValues(in)
    when (reset.asBool) {
      in.op_count := OpCount(0.U)
    }
  } else if (axis == FiberTreeAxis.Compressed) {
    /* Note: the "compressed" axis has multiple pipeline stages:
        1) Make two SRAM read/write requests: for outer-dim address and for outer-dim length
        2) Buffer "in" while making read-request of prior stage
        3) Capture read result of outer-dim SRAM metadata, and choose which branch to forward to next stage
        4) Make SRAM read/write requests for inner-dim address and data.
           In this stage, we also output outer-dim metadata, if the user requested it
        5) Buffer "in" while making read-request of prior stage
        6) Capture and align result of data/inner-dim SRAM read request
        7) Output final result
    */
    val in2_valid = RegInit(false.B)
    val in4_valid = RegInit(false.B)
    val in5_valid = RegInit(false.B)
    val in6_valid = RegInit(false.B)

    val in3_fire = Wire(Bool())
    val in4_fire = Wire(Bool())
    val in6_fire = Wire(Bool())

    val in6 = Reg(getChiselType(in))

    val is_branched4 = if (couldBeBranched) Reg(Bool()) else false.B

    val branch_id4 = Reg(UInt(log2Ceil(nBranches).W))
    val branch_id5 = Reg(UInt(log2Ceil(nBranches).W))
    val branch_id6 = Reg(UInt(log2Ceil(nBranches).W))

    val span_step_size5 = Reg(UInt(spanBits.W))

    assert(!in_valid || !should_interleave_push && !should_interleave_pop, "we don't yet support interleaving in compressed dimensions")
    assert(!in_valid || !reset_running_state, "the compressed stage doesn't have any running state currently")
    assert(!in_valid || !should_gather, "we don't yet support gathering from compressed axes")
    assert(!(in_valid && isWrite(in) && in.write_req.from_regfile(axisId)) || (elemsPerWriteRf == elemsPerWrite).B, "haven't added support for different elemsPerWriteRf and elemsPerWrite to Compressed axis yet")

    import CompressedMetadata._

    val CompressedMetadata(nOuter, innerBufferParams, canReadFromStage, nonInfiniteSpan, canReadReverse) = metadataConf
    val dont_expand_inner = innerBufferParams match {
      case DontExpandInner(_) => true
      case _ => false
    }
    val expanded_addr_bits = innerBufferParams match {
      case InnerBuffer(Some(nCoords), _) => log2Up(nCoords+1)
      case DontExpandInner(Some(nCoords)) => log2Up(nCoords+1)
      case _ if is_innermost => log2Up(nElems+1)
      case _ => spanBits
    }
    require(expanded_addr_bits <= spanBits)

    // Pipeline stage 1
    val outer_compressed_addr = in.metadata_addrs(axisId)(CompressedMetadata.outer_metadata_buffer_id)

    val write_row_id = in_valid && in.write &&
      Mux(in.write_req.from_regfile(axisId),
        in.write_req.from_regfile_metadata(axisId)(outer_metadata_buffer_id).valid,
        !in.write_req.is_data && in.write_req.axis === axisId.U &&
          in.write_req.metadata_buffer_id === outer_metadata_buffer_id.U)
    val empty_row_id_write = in.write_req.spans(axisId) < Mux(in.is_split_across_banks, (bankId+1).U, 1.U)

    val wait_to_trail = should_trail && io.other_stage_span_traversed(branch_id_of(in, true)).valid && {
      assert(!in_valid || !should_trail || (nBranches == 1).B || (outermostBranchedAxisId == axisId).B, "i am not sure whether this code works when the outermost-branched-axis is different from the current axis")
      val traversed = WireInit(in.span_traversed)
      traversed(axisId) := 0.U

      !compareVecs(traversed, io.other_stage_span_traversed(branch_id_of(in, true)).bits, {_ < _})
    }

    val rowIdsPerWrite = io.metadata_write_reqs(outer_metadata_buffer_id).bits.data.size
    val rowIdsPerRead = io.metadata_read_resps(outer_metadata_buffer_id).bits.data.size
    val row_id_write_addr = outer_compressed_addr +& in.write_req.address(axisId) +& Mux(in.is_split_across_banks, bankId.U, 0.U)
    val row_id_write_row = row_id_write_addr / rowIdsPerWrite.U
    val row_id_write_col = row_id_write_addr % rowIdsPerWrite.U
    val row_id_write_len = Mux(in.is_split_across_banks, 1.U, minOf(in.write_req.spans(axisId), rowIdsPerWrite.U - row_id_write_col))
    val row_id_write_shift = Mux(in.is_split_across_banks, nBanks.U, row_id_write_len)
    for (buffer_id <- Seq(outer_metadata_buffer_id, outer_metadata_ends_buffer_id)) {
      io.metadata_write_reqs(buffer_id).valid := write_row_id && !empty_row_id_write && !wait_to_trail

      io.metadata_write_reqs(buffer_id).bits.addr := row_id_write_row

      if (nBanks > in.write_req.data.size || rowIdsPerWrite > 1) {
        io.metadata_write_reqs(buffer_id).bits.data.zipWithIndex.foreach { case (d,i) =>
          d := in.write_req.data(i.U - row_id_write_col).asUInt
        }

        assert(!io.metadata_write_reqs(buffer_id).valid || !in.is_split_across_banks || (bankId == 0).B,
          "we can't bank the writes to the outer-metadata buffers when the number of SRAM banks is higher than elemsPerWrite, or when more than one row-id can be written per cycle from a single bank")
      } else if (nBanks <= in.write_req.data.size && rowIdsPerWrite == 1) {
        io.metadata_write_reqs(buffer_id).bits.data := VecInit(Mux(in.is_split_across_banks,
          in.write_req.data(bankId).asUInt, in.write_req.data.head.asUInt)); require(elemT.getWidth == metadataElemT.getWidth)
        assert(!io.metadata_write_reqs(buffer_id).valid || row_id_write_len === 1.U, "we only support writing one row-id at a time in this case")
      } else {
        assert(false, "we don't support writing to the row-id buffer with these banking and row-id-write-bandwidt settings")
      }

      io.metadata_write_reqs(buffer_id).bits.mask.zipWithIndex.foreach { case (m,i) =>
        m := i.U >= row_id_write_col && i.U < (row_id_write_col + row_id_write_len)
      }
    }

    assert(PopCount(Seq(outer_metadata_buffer_id, outer_metadata_ends_buffer_id).map(io.metadata_write_reqs(_).fire)) =/= 1.U, "did not write to both buffers simultaneously")

    assert(!in_valid || !in.write || in.write_req.is_data || in.write_req.spans(axisId) >= 1.U ||
      in.write_req.axis =/= axisId.U || in.write_req.metadata_buffer_id =/= outer_metadata_buffer_id.U,
      "can't currently write 0 outer-dim metadata elems")

    assert(!in_valid || !in.write || !in.write_req.from_regfile(axisId) ||
      !in.write_req.from_regfile_metadata(axisId)(outer_metadata_buffer_id).valid,
      "can't currently write outer-dim metadata from reg-files")

    when (io.metadata_write_reqs(outer_metadata_buffer_id).fire && in.write_req.spans(axisId) > row_id_write_shift) {
      // Handle case where we write multiple row-ids simultaneously
      assert(!io.in.fire, "line below might overwrite in.write_req.spans")
      in.write_req.spans(axisId) := in.write_req.spans(axisId) - row_id_write_len
      in.write_req.data := dropFromVec(in.write_req.data, row_id_write_len)
      outer_compressed_addr := outer_compressed_addr + row_id_write_len
    }

    span_step_size := 0.U

    val in2_ready = Wire(Bool())
    val wait_for_other_banks = (nBanks > 1 && bankId < nBanks - 1).B && !in2_ready // This isn't necessary for correctness (as far as I can tell) but it can help improve performance by preventing some bank-conflicts from degrading performance too much

    val row_id_read_hit = Wire(Bool())
    val row_id_end_read_hit = Wire(Bool())
    val get_outer_expanded_addr = in_valid && !write_row_id && !wait_to_trail && !wait_for_other_banks

    val row_id_read_row = outer_compressed_addr / rowIdsPerRead.U
    val row_id_read_col = outer_compressed_addr % rowIdsPerRead.U
    val requested_row_id = Reg(Bool())
    io.metadata_read_reqs(outer_metadata_buffer_id).valid := get_outer_expanded_addr && !row_id_read_hit && !requested_row_id
    io.metadata_read_reqs(outer_metadata_buffer_id).bits.addr := row_id_read_row
    io.metadata_read_reqs(outer_metadata_buffer_id).bits.tag.write := in.write
    io.metadata_read_reqs(outer_metadata_buffer_id).bits.pathId.foreach(_ := in.write)

    val row_id_end_addr = {
      val end_addr = muxOnWrite(in, in.write_req.metadata_strides, in.read_req.metadata_strides)(axisId)(axisId)(CompressedMetadata.outer_metadata_ends_buffer_id) // TODO we use strides to encode addr information here, which might cause a lot of confusion later
      maxOf(end_addr, outer_compressed_addr +& 1.U)
    }
    val row_id_end_read_row = row_id_end_addr / rowIdsPerRead.U
    val row_id_end_read_col = row_id_end_addr % rowIdsPerRead.U
    val requested_row_end = Reg(Bool())
    io.metadata_read_reqs(outer_metadata_ends_buffer_id).valid := get_outer_expanded_addr && !row_id_end_read_hit && !requested_row_end
    io.metadata_read_reqs(outer_metadata_ends_buffer_id).bits.addr := row_id_end_read_row
    io.metadata_read_reqs(outer_metadata_ends_buffer_id).bits.tag.write := in.write
    io.metadata_read_reqs(outer_metadata_ends_buffer_id).bits.pathId.foreach(_ := in.write)

    when (io.in.fire) {
      requested_row_id := false.B
      requested_row_end := false.B
    }.otherwise {
      when (io.metadata_read_reqs(outer_metadata_buffer_id).fire) {
        requested_row_id := true.B
      }
      when (io.metadata_read_reqs(outer_metadata_ends_buffer_id).fire) {
        requested_row_end := true.B
      }
    }

    ending := (if (no_simultaneous_reads_and_writes) {
      in2_ready && (io.metadata_write_reqs(outer_metadata_buffer_id).valid && in.write_req.spans(axisId) <= row_id_write_shift || get_outer_expanded_addr &&
        (row_id_read_hit || requested_row_id || io.metadata_read_reqs(outer_metadata_buffer_id).valid) &&
        (row_id_end_read_hit || requested_row_end || io.metadata_read_reqs(outer_metadata_ends_buffer_id).valid))
    } else {
      val ready = (rowIdsPerRead == 1).B || in2_ready // TODO i don't think we need to check in2_ready if we're only reading one row-id per cycle
      io.metadata_write_reqs(outer_metadata_buffer_id).fire && in.write_req.spans(axisId) <= row_id_write_shift || ready && get_outer_expanded_addr &&
        (row_id_read_hit || requested_row_id || io.metadata_read_reqs(outer_metadata_buffer_id).fire) &&
        (row_id_end_read_hit || requested_row_end || io.metadata_read_reqs(outer_metadata_ends_buffer_id).fire)
    }) || write_row_id && empty_row_id_write && !wait_to_trail

    // Pipeline stage 2
    in2_ready := !in2_valid || in3_fire
    val in2_fire = {
      def makes_read_req(buffer_id: Int): Bool = if (no_simultaneous_reads_and_writes) io.metadata_read_reqs(buffer_id).valid && in2_ready
        else io.metadata_read_reqs(buffer_id).fire
      in2_ready && get_outer_expanded_addr &&
        (row_id_read_hit || requested_row_id || makes_read_req(outer_metadata_buffer_id)) &&
        (row_id_end_read_hit || requested_row_end || makes_read_req(outer_metadata_ends_buffer_id))
    }

    val in2 = RegEnable(in, in2_fire)
    val row_id_read_row2 = RegEnable(row_id_read_row, in2_fire)
    val row_id_read_col2 = RegEnable(row_id_read_col, in2_fire)
    val row_id_end_read_row2 = RegEnable(row_id_end_read_row, in2_fire)
    val row_id_end_read_col2 = RegEnable(row_id_end_read_col, in2_fire)
    val row_id_read_hit2 = RegEnable(row_id_read_hit, in2_fire)
    val row_id_end_read_hit2 = RegEnable(row_id_end_read_hit, in2_fire)

    row_id_read_hit := (rowIdsPerRead > 1).B && !in.first_it && row_id_read_row === row_id_read_row2
    row_id_end_read_hit := (rowIdsPerRead > 1).B && !in.first_it && row_id_end_read_row === row_id_end_read_row2

    when (in2_fire) {
      in2_valid := true.B
      assert(row_id_read_hit || requested_row_id || io.metadata_read_reqs(outer_metadata_buffer_id).fire)
      assert(row_id_end_read_hit || requested_row_end || io.metadata_read_reqs(outer_metadata_ends_buffer_id).fire)
    }.elsewhen(in3_fire) {
      in2_valid := false.B
    }

    when (!in2_valid) {
      in2.op_count := in.op_count
    }

    // Pipeline stage 3
    class BranchData extends Bundle {
      val in = getChiselType(in2)
      val outer_expanded_addr = getChiselType(io.metadata_read_resps(outer_metadata_buffer_id).bits.data.head)
      val outer_expanded_end = getChiselType(io.metadata_read_resps(outer_metadata_ends_buffer_id).bits.data.head)
      val span = UInt(expanded_addr_bits.W)
      val span_iterator = UInt(spanBits.W)
      val read_col_counter = UInt(log2Ceil(elemsPerRead).W)
    }
    val nActualBranches = if (couldBeBranched) nBranches else 1
    val branches3 = Reg(Vec(nActualBranches, new BranchData))
    val branch_valids3 = RegInit(VecInit.fill(nActualBranches)(false.B))

    val cached_outer_expanded_addrs = RegEnable(io.metadata_read_resps(outer_metadata_buffer_id).bits.data, io.metadata_read_resps(outer_metadata_buffer_id).fire)
    val cached_outer_expanded_ends = RegEnable(io.metadata_read_resps(outer_metadata_ends_buffer_id).bits.data, io.metadata_read_resps(outer_metadata_ends_buffer_id).fire)
    val current_outer_expanded_addrs = Mux(row_id_read_hit2, cached_outer_expanded_addrs, io.metadata_read_resps(outer_metadata_buffer_id).bits.data)
    val current_outer_expanded_ends = Mux(row_id_end_read_hit2, cached_outer_expanded_ends, io.metadata_read_resps(outer_metadata_ends_buffer_id).bits.data)

    val in3_ready = Wire(Bool())
    in3_fire := in2_valid && in3_ready && Mux(row_id_read_hit2,
      row_id_end_read_hit2 || io.metadata_read_resps(outer_metadata_ends_buffer_id).fire,
      io.metadata_read_resps(outer_metadata_buffer_id).fire
    )
    when (in3_fire) {
      // We assume here that both required buffer-ids fire at the same time
      assert(row_id_read_hit2 || io.metadata_read_resps(outer_metadata_buffer_id).fire)
      assert(row_id_end_read_hit2 || io.metadata_read_resps(outer_metadata_ends_buffer_id).fire)
    }

    val is_branched3 = if (couldBeBranched) RegEnable(is_branched(in2), in3_fire) else false.B
    val last_branch3 = Reg(UInt(log2Up(nBranches).W))
    val last_branch_received3 = RegEnable(is_last_arriving_branch(in2), in3_fire)
    val last_rf_access3 = RegEnable(in2.last_rf_access, in3_fire)
    val sent_first_branch3 = Reg(Bool())
    val previous_branch_sent3 = RegInit(0.U(log2Up(nBranches).W))

    val overflowingOpt = Option.when(multipleNElemsLookupPorts && couldBeBranched)(VecInit.fill(nBranches)(false.B).suggestName("ov"))

    val axisIsBelowOuterBranches = is_innermost && outermostBranchedAxisId == axisId+1 && nBanks > 1 && nBranches > 1 && maxElemsInRf.nonEmpty

    val branch_to_send3 = if (!couldBeBranched) 0.U else (noPrefix {
      val result = WireInit(previous_branch_sent3)

      val to_be_valids = WireInit(branch_valids3).suggestName("tbv")
      when (in2_valid && is_branched3 && !last_branch_received3) {
        to_be_valids(branch_id_of(in2)) := true.B
      }

      if (multipleNElemsLookupPorts && maxElemsInRf.nonEmpty) {
        val notAllOverflowing = (!all(overflowingOpt.get)).suggestName("nao") // This line is just used to help reduce FIRRTL LOC

        to_be_valids.zip(io.lookup_future_stages).zip(branches3).zipWithIndex.foreach { case (((to_be_valid, lookup), branch), branchId) =>
          val overflows = overflowingOpt.get(branchId)

          if (canRead) noPrefix {
            val stage4_on_this_branch = in4_valid && is_branched4 && branch_id4 === branchId.U
            when (!stage4_on_this_branch) {
              lookup.address := branch.in.read_req.address.map(_.zext)
              lookup.address(axisId) := branch.span_iterator.zext // TODO should we add a "+ bias" here?

              def getSpan(span: UInt): UInt = if (is_innermost) span else maxOf(span, 1.U)
              val elems_in_stage5 = Mux(in5_valid && branch_id5 === branchId.U, getSpan(span_step_size5), 0.U).suggestName("e5")
              val elems_in_stage6 = Mux(in6_valid && branch_id6 === branchId.U, getSpan(in6.read_req.spans(axisId)), 0.U).suggestName("e6")
              val total_elems_in_rf = lookup.nElems +& elems_in_stage5 +& elems_in_stage6

              val elems_to_add = if (is_innermost) elemsPerRead.U else 1.U

              overflows := !isWrite(branch.in) && total_elems_in_rf +& elems_to_add > maxElemsInRf.get.U
            }
          }

          when(overflows && notAllOverflowing) {
            to_be_valid := false.B
          }
        }
      }

      val higher = (to_be_valids.asUInt >> (previous_branch_sent3+&1.U)).asUInt =/= 0.U
      val lower = (to_be_valids.asUInt & ((1.U << previous_branch_sent3).asUInt - 1.U)) =/= 0.U

      when (higher) {
        to_be_valids.zipWithIndex.reverse.foreach { case (v, i) =>
          when (v && i.U > previous_branch_sent3) {
            result := i.U
          }
        }
      }.elsewhen (lower) {
        to_be_valids.zipWithIndex.reverse.foreach { case (v, i) =>
          when (v && i.U < previous_branch_sent3) {
            result := i.U
          }
        }
      }

      if (!axisIsBelowOuterBranches)
        when (!last_branch_received3) {
          result := previous_branch_sent3 + 1.U
        }

      when (!is_branched3 || !sent_first_branch3) {
        result := 0.U
      }

      result
    })

    assert(is_branched3 || !any(branch_valids3.tail), "multiple branches are active even when they shouldn't be")

    val stage3_writing_branch = VecInit.fill(nActualBranches)(false.B)

    when (in3_fire) {
      val branch_id = branch_id_of(in2) % nBranches.U

      val outer_expanded_addr = Mux(in2.metadata_addrs(axisId)(CompressedMetadata.outer_metadata_buffer_id) === 0.U, 0.U, current_outer_expanded_addrs(row_id_read_col2))

      val increment_expanded_end = {
        val end_addr = muxOnWrite(in, in.write_req.metadata_strides, in.read_req.metadata_strides)(axisId)(axisId)(CompressedMetadata.outer_metadata_ends_buffer_id)
        end_addr(end_addr.getWidth-1)
      }
      val outer_expanded_end = current_outer_expanded_ends(row_id_end_read_col2) +& increment_expanded_end

      // Originally, we calculated the span in Stage 4, but that made it difficult to meet timing at 1.5 GHz
      def getSpan(checkAsserts: Bool, _in: ChiselSRAMPipelineData[T], outer_expanded_addr: UInt, outer_expanded_end: UInt): UInt = noPrefix {
        val data_and_coord_addr = _in.metadata_addrs(axisId)(CompressedMetadata.inner_metadata_buffer_id)
        if (is_innermost)
          assert(!checkAsserts || _in.data_addr === data_and_coord_addr)

        val data_and_coord_stride = muxOnWrite(_in, _in.write_req.metadata_strides, _in.read_req.metadata_strides)(axisId)(axisId)(CompressedMetadata.inner_metadata_buffer_id) // TODO add an assertion that this matches data-stride in innermost axes
        val data_and_coord_stride_log2 = noPrefix(PriorityEncoder(data_and_coord_stride)).suggestName("dsl"); assert(!checkAsserts || PopCount(data_and_coord_stride) === 1.U, "currently, we only support pow-2 data-and-coord-strides")

        val _outer_expanded_addr = reduceWidth(outer_expanded_addr, expanded_addr_bits)
        val _outer_expanded_end = reduceWidth(outer_expanded_end, expanded_addr_bits)

        val bias = muxOnWrite(_in, _in.write_req.address(axisId), _in.read_req.address(axisId))
        val base_inner_addr = reduceWidth(_outer_expanded_addr +& bias +& data_and_coord_addr, expanded_addr_bits)

        val req_span = muxOnWrite(_in, _in.write_req.spans(axisId), _in.read_req.spans(axisId))(expanded_addr_bits-1,0).asUInt

        val span = reduceWidth({
          val undivided_span = Mux(_in.write && (_in.write_req.axis === axisId.U || _in.write_req.from_regfile(axisId)), req_span,
            nonInfiniteSpan match {
              case Some(nonInfSpan) =>
                val isInf = req_span(req_span.getWidth-1)
                assert(!checkAsserts || !isInf || req_span >= floorSub(_outer_expanded_end, base_inner_addr))
                Mux(isInf, floorSub(_outer_expanded_end, base_inner_addr),
                  minOf(nonInfSpan.U, floorSub(_outer_expanded_end, base_inner_addr)))
              case None => minOf(req_span, floorSub(_outer_expanded_end, base_inner_addr))
            })

          (undivided_span >> data_and_coord_stride_log2).asUInt +& (undivided_span & ((1.U << data_and_coord_stride_log2).asUInt - 1.U)).orR
        }, expanded_addr_bits)

        val resets_lower_axis = _in.write && _in.write_req.reset_running_state && _in.write_req.axis < axisId.U
        Mux(span === 0.U && resets_lower_axis, 1.U, span)
      }
      val full_span = getSpan(true.B, in2, outer_expanded_addr, outer_expanded_end)

      branch_valids3(branch_id) := true.B
      branches3.zipWithIndex.foreach { case (branch, bid) =>
        when (bid.U === branch_id) {
          branch.in := in2
          branch.outer_expanded_addr := outer_expanded_addr
          branch.outer_expanded_end := outer_expanded_end
          branch.span_iterator := 0.U
          branch.read_col_counter := 0.U

          branch.span := full_span

          when (in2.is_split_across_banks) {
            val split_span = full_span / nBanks.U +& (full_span % nBanks.U =/= 0.U)
            val start = split_span * bankId.U
            val this_span = floorSub(minOf(full_span, split_span * (bankId+1).U), start)

            branch.span := this_span
            branch.outer_expanded_addr := outer_expanded_addr +& start
          }
        }
      }

      last_branch3 := branch_id

      if (couldBeBranched)
        when (is_first_arriving_branch(in2) && is_last_arriving_branch(in2)) {
          // There's no need to branch if there's only one branch anyways
          is_branched3 := false.B
        }

      stage3_writing_branch(branch_id) := true.B
    }.elsewhen(in4_fire && !is_branched3) {
      branch_valids3.foreach(_ := false.B)
    }

    branch_valids3.zip(branches3).foreach { case (branch_valid, branch) =>
      when (!branch_valid) {
        branch.in.op_count := in2.op_count
      }
    }

    when (in3_fire && is_first_arriving_branch(in2)) {
      sent_first_branch3 := false.B
    }.elsewhen(in4_fire && is_branched3) {
      sent_first_branch3 := true.B
    }

    val stage4_clearing_branch_valid = VecInit.fill(nActualBranches)(false.B)
    in3_ready := !any(branch_valids3) ||
      !branch_valids3(branch_id_of(in2)) && !last_branch_received3 ||
      !is_branched3 && in4_fire ||
      last_branch_received3 && all(stage4_clearing_branch_valid) ||
      (axisIsBelowOuterBranches.B && stage4_clearing_branch_valid(branch_id_of(in2)))

    for ((buffer_id, hit) <- Seq((outer_metadata_buffer_id, row_id_read_hit2), (outer_metadata_ends_buffer_id, row_id_end_read_hit2))) {
      // val both_are_available = ((rowIdsPerRead == 1).B || in2_valid) // I'm assuming that since row-ids are only cached when multiple row-ids are stored on a single line, we only need to account for the possibility that in2_valid can be false without both buffer-ids available when multiple row-ids are on the same line. I've added assertions above to check for that anyways
      val both_are_available = in2_valid
      io.metadata_read_resps(buffer_id).ready := in3_ready && !hit &&
        both_are_available
    }

    if (couldBeBranched)
      assert({
        val headBranchId = PriorityEncoder(branch_valids3)
        val head_address = Wire(Vec(nAxes - outermostBranchedAxisId - 2, getChiselType(branches3.head.in.write_req.address.head)))
        head_address := DontCare
        for (branchId <- 0 until nActualBranches) // This ugly for-loop is just used to avoid instantiating a Mux of nested-vectors which takes forever for Chisel to elaborate
          when (branchId.U === headBranchId) {
            head_address := muxOnWrite(branches3(branchId).in, branches3(branchId).in.write_req.address, branches3(branchId).in.read_req.address).drop(outermostBranchedAxisId+2)
          }

        all(branches3.zip(branch_valids3).map { case (branch, valid) =>
          val address = muxOnWrite(branch.in, branch.in.write_req.address, branch.in.read_req.address).drop(outermostBranchedAxisId+2)
          !valid || vecEqualsU(head_address, address)
        })
      }, "addresses in branches don't match each other")

    // Pipeline stage 4
    val in4 = Reg(getChiselType(in))
    val outer_expanded_addr4 = Reg(UInt(expanded_addr_bits.W))
    val outer_expanded_end4 = Reg(UInt(expanded_addr_bits.W))
    if (couldBeBranched) when (in4_fire) { is_branched4 := is_branched3 } // val is_branched4 = RegEnable(is_branched3, in4_fire)

    val must_output_outer_dim_metadata4 = !in4.write && !in4.read_req.should_read_data && in4.read_req.axis === axisId.U &&
      in4.read_req.metadata_buffer_id === outer_metadata_buffer_id.U

    val span4 = Reg(UInt(expanded_addr_bits.W))
    val recursive_original_span4 = Reg(getChiselType(span4))

    val bias4 = muxOnWrite(in4, in4.write_req.address(axisId), {
      val from_reverse = !isWrite(in4) && !in4.read_req.to_regfile && canReadReverse.B
      Mux(from_reverse, (outer_expanded_end4-outer_expanded_addr4)-1.U, in4.read_req.address(axisId))
    })
    val data_and_coord_addr4 = in4.metadata_addrs(axisId)(CompressedMetadata.inner_metadata_buffer_id)
    val data_and_coord_stride4 = muxOnWrite(in4, in4.write_req.metadata_strides, in4.read_req.metadata_strides)(axisId)(axisId)(CompressedMetadata.inner_metadata_buffer_id)
    if (is_innermost) {
      assert(!in4_valid || in4.data_addr === data_and_coord_addr4)
      assert(!in4_valid || muxOnWrite(in4, in4.write_req.data_strides, in4.read_req.data_strides)(axisId) === data_and_coord_stride4)
    }

    val only_check_span_when_calculating_span_step_size4 = (!canWrite || (dont_expand_inner && !is_innermost)) && !canReadFromStage

    val is_recursive4 = if (only_check_span_when_calculating_span_step_size4) muxOnWrite(in4, in4.write_req.is_recursive(axisId), in4.read_req.is_recursive(axisId)) else {
      assert(!in4_valid || !muxOnWrite(in4, in4.write_req.is_recursive(axisId), in4.read_req.is_recursive(axisId)), "the recursive iteration code causes combinational loops when we're not only checking spans")
      false.B
    }
    val recursive_dim4 = muxOnWrite(in4, in4.write_req.recursive_dim(axisId), in4.read_req.recursive_dim(axisId))
    val recursive_span_iterator4 = Reg(UInt(spanBits.W))
    val recursive_add_to_span_iterator4 = Reg(UInt(spanBits.W))
    val recursive_span_traversed4 = Reg(UInt(spanBits.W))
    val recursive_buffer_fill_count4 = Reg(UInt(32.W))
    val recursive_buffer_fill_count_target4 = Reg(UInt(32.W))
    assert(!in4_valid || !is_recursive4 || !is_branched4, "recursive spans are not currently supported with branching")
    val is_last_recursive4 = Wire(Bool())

    io.recursive_buffer_fill_count.valid := in4_valid && isWrite(in4) && is_recursive4
    io.recursive_buffer_fill_count.bits := recursive_buffer_fill_count4

    val span_iterator4 = Reg(UInt(expanded_addr_bits.W))
    val iterator4 = span_iterator4 +& bias4
    val inner_addr4 = {
      val recursive_iterator4 = Mux(isWrite(in4) && is_last_recursive4, recursive_original_span4+1.U, recursive_span_iterator4) +& bias4
      val it4 = Mux(is_recursive4, recursive_iterator4, iterator4)
      reduceWidth(outer_expanded_addr4 +& it4 * data_and_coord_stride4 +& data_and_coord_addr4, expanded_addr_bits)
    }

    val sram_row4 = inner_addr4 >> log_elems_per_row
    val sram_col4 = inner_addr4 & ((1 << log_elems_per_row)-1).U

    val read_col_counter4 = Reg(UInt(log2Ceil(elemsPerRead).W))
    val last_read4 = WireInit(false.B)

    val too_many_elems_in_rf4 = Wire(Bool())
    when (!multipleNElemsLookupPorts.B || in4_valid) {
      io.lookup_future_stages(branch_id4).address := in4.read_req.address.map(_.zext)
      io.lookup_future_stages(branch_id4).address(axisId) := span_iterator4.zext // TODO should we add a "+ bias" here?
    }
    when (in4_valid && is_branched4) {
      overflowingOpt.foreach(_(branch_id4) := too_many_elems_in_rf4)
    }

    val axisId4 = muxOnWrite(in4,
      Mux(in4.write_req.is_data, 0.U, in4.write_req.axis),
      Mux(in4.read_req.should_read_data, 0.U, in4.read_req.axis))

    val span_step_size4 = {
      val bitwidth = log2Up((Seq(elemsPerRow) ++ (if (canRead) Seq(elemsPerRead) else Seq()) ++ (if (canWrite) Seq(elemsPerWrite) else Seq())).min + 1)

      val potential_step_sizes = Seq(span4 - span_iterator4) ++ (
        if (only_check_span_when_calculating_span_step_size4) Seq.empty
        else Seq(elemsPerRow.U - sram_col4, muxOnWrite(in4, elemsPerWrite.U, elemsPerRead.U - read_col_counter4)))

      val size_if_axisId_matches = reduceWidth(Mux(too_many_elems_in_rf4, 0.U, minOf(potential_step_sizes:_*)), bitwidth)

      val single_increment = Mux(is_recursive4 && isWrite(in4), recursive_dim4, 1.U)

      Mux(axisId4 === axisId.U,
        if (dont_expand_inner && !is_innermost) maxVal(span4) else size_if_axisId_matches,
        if (only_check_span_when_calculating_span_step_size4) Mux(span4 === span_iterator4, 0.U, single_increment) else minOf(size_if_axisId_matches, single_increment))
    }
    assert(!in4_valid || data_and_coord_stride4 === 1.U || span_step_size4 <= 1.U, "we don't yet support jumping over inner-coord elements with larger step sizes")

    val not_all_rf_outs_found = in4_valid && in.write && in.write_req.from_regfile(axisId) && io.not_all_rf_outs_found
    assert(is_innermost.B || !in4.write || !in4.write_req.from_regfile(axisId) || !not_all_rf_outs_found, "If this is not the innermost stage, then when 'not_all_rf_outs_found' is true, 'span_step_size4' should be adjusted")

    val should_trail4 = muxOnWrite(in4, in4.write_req.should_trail_reads, in4.read_req.should_trail_writes)
    val waiting_to_trail4 = should_trail4 && io.other_stage_span_traversed(branch_id_of(in4, true)).valid && {
      val traversed = WireInit(branched_span_traversed(in4.span_traversed, is_branched4)); assert(!in4_valid || !should_trail4 || (nBranches == 1).B || (outermostBranchedAxisId == axisId).B, "i am not sure whether this code works when the outermost-branched-axis is different from the current axis")
      traversed(axisId) := Mux(isWrite(in4) && is_recursive4, recursive_span_traversed4, span_iterator4) +& span_step_size4
      !compareVecs(traversed, io.other_stage_span_traversed(branch_id_of(in4, true)).bits, {_ < _}, orEquals = true)
    }

    val waiting_for_recursive_buffer_to_fill4 = isWrite(in4) && is_recursive4 && any(io.other_banks_recursive_buffer_fill_count.map(x => x.valid && x.bits < recursive_buffer_fill_count4))

    val waiting_for_flush4 = ((in4.write && in4.write_req.from_regfile(axisId)) || must_output_outer_dim_metadata4) &&
      (in5_valid || in6_valid)

    val waiting4 = waiting_for_flush4 || waiting_to_trail4 || waiting_for_recursive_buffer_to_fill4

    val read_req_needs_data4 = in4.read_req.should_read_data && is_innermost.B
    val read_req_needs_inner_dim_metadata4 = !dont_expand_inner.B && (in4.read_req.to_regfile || (
      !in4.read_req.should_read_data && in4.read_req.axis === axisId.U && in4.read_req.metadata_buffer_id === inner_metadata_buffer_id.U))
    val skip_data_and_coord_accesses4 = (axisId4 =/= axisId.U && !muxOnWrite(in4, in4.write_req.from_regfile(axisId), in4.read_req.to_regfile)) ||
      dont_expand_inner.B && !(!in4.write && read_req_needs_data4)

    val in5_ready = !in5_valid || in6_fire
    val mem_read_reqs_fired4 = if (no_simultaneous_reads_and_writes) {
      (io.metadata_read_reqs(inner_metadata_buffer_id).valid || io.data_read_req.valid) && in5_ready
    } else {
      io.metadata_read_reqs(inner_metadata_buffer_id).fire || io.data_read_req.fire
    }
    val stepping4 = io.metadata_write_reqs(inner_metadata_buffer_id).fire || io.data_write_req.fire ||
      mem_read_reqs_fired4 ||
      io.write_from_regfile_req.fire ||
      (skip_data_and_coord_accesses4 || span_step_size4 === 0.U) && !must_output_outer_dim_metadata4 && in5_ready

    val ending4 = Mux(must_output_outer_dim_metadata4, !in6_valid && !in5_valid && io.out.fire,
      stepping4 && (span_iterator4 +& span_step_size4 >= span4))
    val starting4 = Mux(must_output_outer_dim_metadata4, !in6_valid && !in5_valid && io.out.fire,
      stepping4 && (span_iterator4 === 0.U))

    val switching_branches4 = in4_valid && is_branched4 && (stepping4 || ending4) && (in4.write || last_read4 || span_step_size4 === 0.U && read_col_counter4 === 0.U)
    val switching_to_same_branch4 = in4_valid && (branch_to_send3 % nBranches.U) === branch_id4 && sent_first_branch3
    assert(!(switching_branches4 && !in4.write && span_step_size4 === 0.U && read_col_counter4 =/= 0.U),
      "Sometimes, we want to switch branches because the current branch can't progress because there are too many elements from that branch in the reg-file. When that's the cause, \"span_step_size\" will be 0, but we want to make sure we didn't leave partial, unaligned read-outs in 'in4'")

    val next_span_iterator4 = Mux(stepping4, span_iterator4 + span_step_size4, span_iterator4)
    when (stepping4) {
      span_iterator4 := next_span_iterator4

      when (is_recursive4) {
        val new_span4 = WireInit(span4)

        recursive_add_to_span_iterator4 := recursive_add_to_span_iterator4 + 1.U
        when (recursive_add_to_span_iterator4 === recursive_dim4-1.U) {
          recursive_add_to_span_iterator4 := 0.U
          when (!is_last_recursive4) {
            span4 := span4 + 1.U
            new_span4 := span4 + 1.U
          }
        }

        recursive_span_iterator4 := //Mux(isWrite(in4) && is_last_recursive4, recursive_original_span4+1.U,
          Mux(recursive_span_iterator4 === recursive_original_span4, 0.U, recursive_span_iterator4 + 1.U)//)
        recursive_span_traversed4 := recursive_span_traversed4 + 1.U

        val increment_buffer_fill_count = span_iterator4 === recursive_buffer_fill_count_target4-1.U
        when (increment_buffer_fill_count) {
          recursive_buffer_fill_count4 := recursive_buffer_fill_count4 + 1.U
          recursive_buffer_fill_count_target4 := new_span4
        }
      }
    }

    is_last_recursive4 := (if (is_recursive4.litToBooleanOption.contains(false)) false.B else (span4 - span_iterator4 <= span_step_size4))

    val last_axis4 = muxOnWrite(in4, in4.write_req.from_regfile_last_axis, in4.read_req.to_regfile_last_axis)
    val last_rf_axis_size4 = muxOnWrite(in4, 1.U << in4.write_req.from_regfile_last_axis_log_size, 1.U << in4.read_req.to_regfile_last_axis_log_size).asUInt
    val is_last_branch4 = if (!couldBeBranched) true.B else noPrefix(!is_branched4 || last_branch_received3 && all(branches3.zip(branch_valids3).zipWithIndex.map { case ((branch, valid), branchId) =>
      !valid || branchId.U === branch_id4 || branch.span_iterator / last_rf_axis_size4 > span_iterator4 / last_rf_axis_size4
    }))
    /*val is_first_branch4 = !is_branched4 || all(branches3.zip(branch_valids3).zipWithIndex.map { case ((branch, valid), branchId) =>
      !valid || branchId.U === branch_id4 || branch.span_iterator / last_rf_axis_size4 > span_iterator4 / last_rf_axis_size4
    })*/
    val last_step_in_rf_axis4 = {
      assert(!in4_valid || !muxOnWrite(in4, any(in4.write_req.from_regfile), in4.read_req.to_regfile || any(in4.read_req.should_gather)) || last_axis4 =/= axisId.U || last_rf_axis_size4 >= infinite_last_rf_axis_size || last_rf_axis_size4 % span_step_size4 === 0.U || last_rf_axis_size4 === 1.U, p"we might need to adjust the span-step-size if it doesn't divide cleanly into the last-rf-axis size | last_rf_axis_size4=${last_rf_axis_size4} | span_step_size4=${span_step_size4}")
      val is_divisible = Mux(last_rf_axis_size4 === 1.U, span_step_size4 >= 1.U,
        (span_iterator4 +& span_step_size4) % last_rf_axis_size4 === 0.U)
      (is_divisible && stepping4 || ending4) && is_last_branch4
    }
    val last4 = Mux(last_axis4 === axisId.U, last_step_in_rf_axis4, in4.last_rf_access && ending4)
    val first4 = Mux(last_axis4 === axisId.U,
      {
        val is_first_branch = !is_branched4 || branch_id4 === 0.U
        ((span_iterator4 % last_rf_axis_size4 === 0.U) && stepping4 || starting4) && is_first_branch
      },
      in4.first_rf_access && starting4)

    when (in4_valid && last_axis4 === axisId.U &&
      muxOnWrite(in4, any(in4.write_req.from_regfile.take(axisId+1)), in4.read_req.to_regfile || any(in4.read_req.should_gather.take(axisId+1))) &&
      last_step_in_rf_axis4)
    {
      opCount := opCount + 1.U // Increment op-count
    }
    when(io.other_banks_op_count.fire) {
      opCount := io.other_banks_op_count.bits
      assert(io.other_banks_op_count.bits >= opCount)
      assert(!in4_valid, "i'm not sure if this works when the pipeline stage is currently in operation")
    }

    if (canWrite) {
      io.metadata_write_reqs(inner_metadata_buffer_id).valid := in4_valid && in4.write && !dont_expand_inner.B && !waiting4 &&
        Mux(in4.write_req.from_regfile(axisId),
          in4.write_req.from_regfile_metadata(axisId)(inner_metadata_buffer_id).valid && io.write_from_regfile_req.fire,
          !in4.write_req.is_data && in4.write_req.axis === axisId.U && in4.write_req.metadata_buffer_id === inner_metadata_buffer_id.U)
      io.metadata_write_reqs(inner_metadata_buffer_id).bits.addr := sram_row4
      io.metadata_write_reqs(inner_metadata_buffer_id).bits.data.zipWithIndex.foreach { case (d, i) =>
        // Verilator seems to have bugs when dealing with negative indexing into Vectors in this part of the code.
        // (VCS doesn't seem to have an issue). The code below has been written to try to avoid any chance of negative
        // indices or underflows when calculating indices.
        val ind: UInt = if (in4.write_req.data.size == 1) 0.U else if (for_verilator) {
          // This is just computing "(i.U +& span_iterator4) - sram_col4"
          val x = Wire(UInt(log2Ceil(in4.write_req.data.size * 2).W)).suggestName(s"verilator_coord_write_req_x_$i"); dontTouch(x)
          x := i.U +& span_iterator4
          val y = Wire(UInt(log2Ceil(in4.write_req.data.size).W)).suggestName(s"verilator_coord_write_req_y_$i"); dontTouch(y)
          y := x - sram_col4
          y
        } else {
          // This is the only statement that's actually needed, the "if" and and "else-if" clauses above could be
          // removed without affecting the generated hardware
          (i.U +& span_iterator4) - sram_col4
        }
        d := in4.write_req.data(ind).asTypeOf(d)
      }
      io.metadata_write_reqs(inner_metadata_buffer_id).bits.mask.zipWithIndex.foreach { case (m, i) =>
        m := i.U >= sram_col4 && i.U < sram_col4 +& span_step_size4
      }
    }

    io.metadata_read_reqs(inner_metadata_buffer_id).valid := in4_valid && !in4.write && span_step_size4 > 0.U &&
      read_req_needs_inner_dim_metadata4 && !must_output_outer_dim_metadata4 &&
      (!is_innermost.B || !in4.read_req.to_regfile || io.data_read_req.ready) &&
      (!in5_valid || in6_fire) // We add this last line because in branched-SRAMs, when overlapping coord and data reads, we would sometimes overwrite the in5 stage before it could be passed on to the in6 stage
    io.metadata_read_reqs(inner_metadata_buffer_id).bits.addr := sram_row4
    io.metadata_read_reqs(inner_metadata_buffer_id).bits.tag.write := in4.write

    val in5_fire = mem_read_reqs_fired4 ||
      in4_valid && stepping4 && !(in4.write && axisId4 === axisId.U) && (skip_data_and_coord_accesses4 || span_step_size4 === 0.U)
    if (canRead)
      assert(in4.write || !in5_fire || span_step_size4 === 0.U || ((!read_req_needs_data4 || io.data_read_req.fire) && (!read_req_needs_inner_dim_metadata4 || io.metadata_read_reqs(inner_metadata_buffer_id).fire)),
        "didn't read from data or inner-metadata buffer when we needed to")
    assert(!in5_fire || in5_ready, "fired in5 when it wasn't even ready")

    if (is_innermost && canWrite) {
      io.data_write_req.valid := in4_valid && in4.write && !waiting4 &&
        Mux(in.write_req.from_regfile(axisId), io.write_from_regfile_req.fire, in4.write_req.is_data)
      io.data_write_req.bits.addr := sram_row4
      io.data_write_req.bits.data.zipWithIndex.foreach { case (d, i) =>
        // Verilator seems to have bugs when dealing with negative indexing into Vectors in this part of the code.
        // (VCS doesn't seem to have an issue). The code below has been written to try to avoid any chance of negative
        // indices or underflows when calculating indices.
        val ind: UInt = if (in4.write_req.data.size == 1) 0.U else if (for_verilator) {
          // This is just computing "(i.U +& span_iterator4) - sram_col4"
          val x = Wire(UInt(log2Ceil(in4.write_req.data.size * 2).W)).suggestName(s"verilator_data_write_req_x_$i"); dontTouch(x)
          x := i.U +& span_iterator4
          val y = Wire(UInt(log2Ceil(in4.write_req.data.size).W)).suggestName(s"verilator_data_write_req_y_$i"); dontTouch(y)
          y := x - sram_col4
          y
        } else {
          // This is the only statement that's actually needed, the "if" and and "else-if" clauses above could be
          // removed without affecting the generated hardware
          (i.U +& span_iterator4) - sram_col4
        }
        d := in4.write_req.data(ind)
      }
      io.data_write_req.bits.mask.zipWithIndex.foreach { case (m, i) =>
        m := i.U >= sram_col4 && i.U < sram_col4 +& span_step_size4
      }
    }

    if (is_innermost && canRead) {
      io.data_read_req.valid := in4_valid && !in4.write && span_step_size4 > 0.U &&
        read_req_needs_data4 && !must_output_outer_dim_metadata4 &&
        (!in5_valid || in6_fire) // We add this last line because in branched-SRAMs, when overlapping coord and data reads, we would sometimes overwrite the in6 stage before it could be passed on to the in6 stage
      io.data_read_req.bits.addr := sram_row4
      io.data_read_req.bits.tag.axisId := axisId.U
    }

    assert(dont_expand_inner.B || !io.data_read_req.fire || !read_req_needs_inner_dim_metadata4 || io.metadata_read_reqs(inner_metadata_buffer_id).fire, "didn't read from the inner-metadata buffer simultaneously with the data-buffer")

    if (canWrite && mightAccessRf) {
      io.write_from_regfile_req.valid := in4_valid && in4.write && in4.write_req.from_regfile(axisId) && !waiting4 && (if (is_innermost) true.B else io.out.ready)
      io.write_from_regfile_req.bits.req := in4.write_req
      io.write_from_regfile_req.bits.req.spans(axisId) := span_step_size4
      io.write_from_regfile_req.bits.req.address := {
        val default_addr = WireInit(in4.write_req.address)
        default_addr(axisId) := span_iterator4
        val result = WireInit(default_addr)
        result(last_axis4) := default_addr(last_axis4) % last_rf_axis_size4
        result
      }
      io.write_from_regfile_req.bits.offset := sram_col4
      io.write_from_regfile_req.bits.opCount := Mux(last_axis4 === axisId.U, opCount, in4.op_count)
    }

    in4_fire := (!in4_valid || ending4 || switching_branches4) && branch_valids3(branch_to_send3) && !switching_to_same_branch4

    val (data_addr4, metadata_addrs4) = updatedAddrs(in4, iterator4, inner_addr4)

    val next_read_col_counter4 = WireInit(read_col_counter4)
    when (in5_fire) {
      if (elemsPerRead > 1) {
        next_read_col_counter4 := Mux(ending4 || axisId4 =/= axisId.U, 0.U, (read_col_counter4 + span_step_size4)(read_col_counter4.getWidth-1, 0))
        assert((isPow2(elemsPerRead) && read_col_counter4.getWidth == log2Ceil(elemsPerRead)).B && next_read_col_counter4 < elemsPerRead.U, "we require next_read_col_counter's width to be a power-of-2 so that we can guarantee read_col_counter wraps around to 0 with just a simple addition")
      }
      read_col_counter4 := next_read_col_counter4
      last_read4 := next_read_col_counter4 === 0.U || ending4
    }

    if (couldBeBranched) when (switching_branches4) {
      val branch_id = branch_id4 % nBranches.U

      previous_branch_sent3 := branch_to_send3 // This assignment might be overwritten by "in4_fire", but that's perfectly fine; both should be setting it to "branch_to_send3" anyways

      when (!stage3_writing_branch(branch_id)) {
        branches3.zipWithIndex.foreach { case (branch, bid) =>
          when(branch_id === bid.U) {
            branch.in := in4
            branch.span_iterator := next_span_iterator4
            branch.read_col_counter := next_read_col_counter4
          }
        }

        branch_valids3(branch_id) := !ending4
      }
      stage4_clearing_branch_valid(branch_id) := ending4

      val switching_to_last_remaining_branch = ending4 && last_branch_received3 && PopCount(branch_valids3) === 2.U && !switching_to_same_branch4
      when (switching_to_last_remaining_branch) {
        is_branched4 := false.B

        when (!stage3_writing_branch.head) {
          branch_valids3.head := false.B
        }
        branch_valids3.tail.foreach(_ := false.B)

        stage4_clearing_branch_valid.foreach(_ := true.B)
      }
    }

    when (in4_fire) {
      in4_valid := true.B

      val branch_id = branch_to_send3 % nBranches.U
      branch_id4 := branch_id
      previous_branch_sent3 := branch_id

      branches3.zipWithIndex.foreach { case (branch, bid) =>
        when (branch_id === bid.U && !switching_to_same_branch4) {
          in4 := branch.in
          if (mightAccessRf) {
            in4.last_rf_access := noPrefix(last_rf_access3 && last_branch_received3 &&
              all(branch_valids3.zipWithIndex.patch(bid, Nil, 1).map { case (v, i) => !v || branch_id4 === i.U && ending4 && is_branched4 }))
          }

          span4 := branch.span
          span_iterator4 := branch.span_iterator
          outer_expanded_addr4 := branch.outer_expanded_addr
          outer_expanded_end4 := branch.outer_expanded_end
          read_col_counter4 := branch.read_col_counter

          val is_recursive3 = muxOnWrite(branch.in, branch.in.write_req.is_recursive(axisId), branch.in.read_req.is_recursive(axisId))
          when (is_recursive3) {
            assert(!is_branched3, "not sure if this works when branching")

            val span3 = floorSub(branch.span, 2.U)
            span4 := span3
            recursive_original_span4 := span3

            val recursive_dim3 = muxOnWrite(branch.in, branch.in.write_req.recursive_dim(axisId), branch.in.read_req.recursive_dim(axisId))
            val is_last_recursive3 = span3 <= recursive_dim3
            recursive_span_iterator4 := muxOnWrite(branch.in, span3 +& is_last_recursive3, 0.U)
            recursive_add_to_span_iterator4 := 0.U
            recursive_span_traversed4 := span3
            recursive_buffer_fill_count4 := 0.U
            recursive_buffer_fill_count_target4 := span3
          }
        }
      }
    }.elsewhen (ending4 || switching_branches4 && !switching_to_same_branch4) {
      in4_valid := false.B
    }

    when (!in4_valid && !any(branch_valids3.tail)) {
      in4.op_count := branches3.head.in.op_count
    }

    if (!canReadFromStage) {
      read_col_counter4 := 0.U
      next_read_col_counter4 := 0.U
    }

    assert(!(in4_valid && in4.write && in4.write_req.axis === axisId.U && !in4.write_req.is_data && in4.write_req.metadata_buffer_id =/= CompressedMetadata.inner_metadata_buffer_id.U), "we can't write outer-metadata in stage-4")

    // Pipeline stage 5
    val in5 = RegEnable(in4, in5_fire)

    val starting5 = RegEnable(span_iterator4 === 0.U && stepping4, in5_fire)
    val ending5 = RegEnable(ending4, in5_fire)
    val is_last_branch5 = RegEnable(is_last_branch4, in5_fire)
    val last_read5 = RegEnable(last_read4, in5_fire)
    val read_col_counter5 = RegEnable(read_col_counter4, in5_fire)
    val sram_col5 = RegEnable(sram_col4, in5_fire)
    val axisId5 = RegEnable(axisId4, in5_fire)

    val read_req_needs_data5 = RegEnable(read_req_needs_data4, in5_fire)
    val read_req_needs_inner_dim_metadata5 = RegEnable(read_req_needs_inner_dim_metadata4, in5_fire)

    val skip_data_and_coord_accesses5 = RegEnable(skip_data_and_coord_accesses4, in5_fire)

    in6_fire := Mux(skip_data_and_coord_accesses5 || span_step_size5 === 0.U, !in6_valid || io.out.ready,
      (!read_req_needs_data5 || io.data_read_resp.fire) &&
        (!read_req_needs_inner_dim_metadata5 || io.metadata_read_resps(inner_metadata_buffer_id).fire))

    assert(!in5_fire || !in5_valid || in6_fire, "overwriting registers in pipeline stage 4 before they can be captured by stage 5")

    when (in5_fire) {
      val in5_will_be_valid = !too_many_elems_in_rf4 || ending4; assert(!too_many_elems_in_rf4 || ending4 || !mem_read_reqs_fired4)

      val step_size4 = Mux(is_recursive4, minOf(span_step_size4, 1.U), span_step_size4); assert(!is_recursive4 || !is_innermost.B, "this line assumes that we're not in the innermost branch, and the step size is therefore normally just 1 (without recursion)")

      in5_valid := in5_will_be_valid
      in5.data_addr := data_addr4
      connectVecOfVecs(in5.metadata_addrs, metadata_addrs4)
      in5.read_req.spans(axisId) := step_size4 +& read_col_counter4
      in5.read_req.address(axisId) := bias4 + span_iterator4 - read_col_counter4
      in5.span_traversed(axisId) := Mux(isWrite(in4) && is_recursive4, recursive_span_traversed4, span_iterator4)
      in5.last_rf_access := last4
      in5.first_rf_access := first4
      in5.axis_spans(axisId).valid := in5_will_be_valid
      in5.axis_spans(axisId).bits := span4
      span_step_size5 := step_size4
      branch_id5 := branch_id4
    }.elsewhen(in6_fire) {
      in5_valid := false.B
    }

    when (in5_fire || !in5_valid) {
      in5.op_count := Mux(last_axis4 === axisId.U, opCount, in4.op_count)
    }

    // Pipeline stage 6
    val in6_data = Reg(getChiselType(io.out.bits.read_data))
    val in6_metadata = Reg(getChiselType(io.out.bits.read_data))

    val last_read6 = RegEnable(last_read5, in6_fire)
    val starting6 = RegEnable(starting5, in6_fire)
    val ending6 = RegEnable(ending5, in6_fire)
    val is_last_branch6 = RegEnable(is_last_branch5, in6_fire)
    val axisId6 = RegEnable(axisId5, in6_fire)

    io.data_read_resp.ready := (!in6_valid || io.out.ready || !last_read6) &&
      (!read_req_needs_inner_dim_metadata5 || io.metadata_read_resps(inner_metadata_buffer_id).valid)
    assert(read_req_needs_data5 && span_step_size5 > 0.U || !io.data_read_resp.fire, "captured data output when it wasn't even supposed to have been requested in the first place")

    io.metadata_read_resps(inner_metadata_buffer_id).ready := (!in6_valid || io.out.ready || !last_read6) &&
      (!read_req_needs_data5 || io.data_read_resp.valid)
    assert(read_req_needs_inner_dim_metadata5 && span_step_size5 > 0.U || !io.metadata_read_resps(inner_metadata_buffer_id).fire, "captured inner-dim-metadata output when it wasn't even supposed to have been requested in the first place")

    when (in6_fire) {
      in6_valid := in5_valid

      in6 := in5

      for ((reg, read_port) <- Seq((in6_data, io.data_read_resp.bits.data), (in6_metadata, io.metadata_read_resps(inner_metadata_buffer_id).bits.data))) {
        reg.zipWithIndex.foreach { case (d,i) =>
          val rport = read_port.asTypeOf(Vec(read_port.size, getChiselType(reg.head)))

          when (axisId5 === axisId.U) {
            when (i.U >= read_col_counter5) {
              d := rport(sram_col5 +& i.U -& read_col_counter5)
            }
          }.otherwise {
            d := rport(sram_col5)

            if (!is_innermost) {
              assert(!io.metadata_read_resps(inner_metadata_buffer_id).valid || read_col_counter5 === 0.U)
              assert(!io.metadata_read_resps(inner_metadata_buffer_id).valid || all(in5.read_req.spans.drop(axisId+1).map(_ === 1.U)),
                "if the outer-dimensions don't have a span of 1, then it might be incorrect to simply set all the metadata addrs simultaneously as we do in this when-clause")
            }
          }
        }
      }

      branch_id6 := branch_id5
    }.elsewhen(io.out.fire) {
      in6_valid := false.B
    }

    when (!in6_valid) {
      in6.op_count := in5.op_count
    }

    io.out.valid := in6_valid && last_read6
    io.out.bits := in6
    io.out.bits.read_data := Mux(in6.read_req.should_read_data, in6_data, in6_metadata)
    io.out.bits.expanded_addrs.zip(in6_metadata).zipWithIndex.foreach { case ((ea, md), i) =>
      val compressedAddr = io.out.bits.read_req.address(axisId) +& Mux(axisId6 === axisId.U, i.U, 0.U)
      ea(axisId) := (innerBufferParams match {
        case CompressedMetadata.DontExpandInner(_) => compressedAddr
        case CompressedMetadata.InnerBuffer(_, false) => Mux(!in6.write && in6.read_req.to_regfile, compressedAddr, md.asUInt)
        case CompressedMetadata.InnerBuffer(_, true) => md.asUInt
      })
    }
    io.out.bits.write_req.address(axisId) := io.out.bits.read_req.address(axisId)
    io.out.bits.write_req.spans(axisId) := io.out.bits.read_req.spans(axisId)
    when (io.out.bits.read_req.spans(axisId) === 0.U) {
      io.out.bits.read_req.spans.take(axisId).foreach(_ := 0.U)
      io.out.bits.write_req.spans.take(axisId).foreach(_ := 0.U)
    }
    io.out.bits.first_it := in6.first_it && io.out.bits.first_it_in_axis
    io.out.bits.first_it_in_axis := starting6 || RegNext(io.out.bits.first_it_in_axis && !io.out.fire) // TODO should we copy the stuff in "RegNext" to the other types of Axes as well?
    io.out.bits.last_it := in6.last_it && ending6 && is_last_branch6
    io.out.bits.last_it_in_axis := ending6
    io.out.bits.last_branch_in_each_axis(axisId) := is_last_branch6
    io.interleave_q_push := in6_valid && in6.push_to_interleave_q && ending6

    when (!in6_valid && !in5_valid && in4_valid && must_output_outer_dim_metadata4) {
      // Output outer-dim metadata from pipeline stage 4 here. This happens in pipeline stage 4; we're only writing it
      // here because of Chisel's last-connect semantics

      io.out.valid := true.B
      io.out.bits := in4
      io.out.bits.read_data.head := outer_expanded_addr4.asTypeOf(elemT); assert(elemT.getWidth == metadataElemT.getWidth)
      io.out.bits.first_rf_access := first4
      io.out.bits.last_rf_access := last4
      io.out.bits.first_it := in4.first_it
      io.out.bits.first_it_in_axis := true.B
      io.out.bits.last_it := in4.last_it && io.out.bits.last_it_in_axis
      io.out.bits.last_it_in_axis := true.B; assert(!io.out.ready || ending4, "we should adjust this line: it's not actually last-it-in-axis if it's not actually ending here")
      io.out.bits.last_branch_in_each_axis(axisId) := true.B

      assert(!in4.push_to_interleave_q, "we don't yet support interleaving outer-dim metadata reads")
    }.elsewhen(!in6_valid && !in5_valid && in4_valid && in4.write && in4.write_req.from_regfile(axisId)) {
      // Input data from reg-files here. This happens in pipeline stage 4; we're only writing it here because of
      // Chisel's last-connect semantics
      if (!is_innermost) {
        io.out.valid := true.B
        io.out.bits := in4
        io.out.bits.span_traversed(axisId) := span_iterator4
        io.out.bits.first_it := in4.first_it && io.out.bits.first_it_in_axis
        io.out.bits.first_it_in_axis := span_iterator4 === 0.U
        io.out.bits.last_it := in4.last_it && ending4 && is_last_branch4
        io.out.bits.last_it_in_axis := ending4
        io.out.bits.last_branch_in_each_axis(axisId) := is_last_branch4
        io.out.bits.op_count := Mux(last_axis4 === axisId.U, opCount, in4.op_count)
      }
      io.out.bits.first_rf_access := first4
      io.out.bits.last_rf_access := last4
      io.interleave_q_push := in4.push_to_interleave_q && ending4
    }

    if (spanTraversedShouldBeSet) {
      io.span_traversed.foreach { io_span_traversed =>
        io_span_traversed.valid := false.B
        io_span_traversed.bits := DontCare
      }

      io.last_it_in_axis.foreach(_ := DontCare)

      io.span_traversed(branch_id_of(in, true)).valid := in_valid
      io.span_traversed(branch_id_of(in, true)).bits := in.span_traversed
      io.span_traversed(branch_id_of(in, true)).bits(axisId) := 0.U

      io.last_it_in_axis(branch_id_of(in, true)) := in.last_it_in_each_axis

      when (in2_valid) {
        io.span_traversed(branch_id_of(in2, true)).valid := true.B
        io.span_traversed(branch_id_of(in2, true)).bits := in2.span_traversed
        io.span_traversed(branch_id_of(in2, true)).bits(axisId) := 0.U

        io.last_it_in_axis(branch_id_of(in2, true)) := in2.last_it_in_each_axis
      }

      when (branch_valids3.head) {
        io.span_traversed(branch_id_of(branches3.head.in, true)).valid := true.B
        io.span_traversed(branch_id_of(branches3.head.in, true)).bits := branches3.head.in.span_traversed
        io.span_traversed(branch_id_of(branches3.head.in, true)).bits(axisId) := branches3.head.span_iterator

        io.last_it_in_axis(branch_id_of(branches3.head.in, true)) := branches3.head.in.last_it_in_each_axis
      }

      io.span_traversed.zip(branches3).zip(branch_valids3).zip(io.last_it_in_axis).tail.foreach { case (((io_span_traversed, branch), branch_valid), last_in_axis) =>
        when (branch_valid) {
          io_span_traversed.valid := true.B
          io_span_traversed.bits := branch.in.span_traversed
          io_span_traversed.bits(axisId) := branch.span_iterator

          last_in_axis := branch.in.last_it_in_each_axis
        }
      }

      when (in4_valid) {
        io.span_traversed(branch_id_of(in4, true)).valid := true.B
        io.span_traversed(branch_id_of(in4, true)).bits := in4.span_traversed
        io.span_traversed(branch_id_of(in4, true)).bits(axisId) := span_iterator4

        io.last_it_in_axis(branch_id_of(in4, true)) := in4.last_it_in_each_axis
        io.last_it_in_axis(branch_id_of(in4, true))(axisId) := must_output_outer_dim_metadata4 || span_iterator4 +& span_step_size4 >= span4
      }

      when (in5_valid && axisId5 < axisId.U && (!in5.write || any(in5.write_req.from_regfile))) {
        io.span_traversed(branch_id_of(in5, true)).valid := true.B
        io.span_traversed(branch_id_of(in5, true)).bits := in5.span_traversed

        io.last_it_in_axis(branch_id_of(in5, true)) := in5.last_it_in_each_axis
        io.last_it_in_axis(branch_id_of(in5, true))(axisId) := ending5
      }

      when (in6_valid && axisId6 < axisId.U && (!in6.write || any(in6.write_req.from_regfile))) {
        io.span_traversed(branch_id_of(in6, true)).valid := true.B
        io.span_traversed(branch_id_of(in6, true)).bits := in6.span_traversed

        io.last_it_in_axis(branch_id_of(in6, true)) := in6.last_it_in_each_axis
        io.last_it_in_axis(branch_id_of(in6, true))(axisId) := ending6
      }
    }

    io.axis_spans.foreach(_.foreach(_.valid := false.B))
    io.axis_spans.foreach(_.foreach(_.valid := DontCare))

    io.axis_spans.zip(branches3).zip(branch_valids3).tail.foreach { case ((io_axis_spans, branch), branch_valid) =>
      connectVecs(io_axis_spans.map(_.valid), branch.in.axis_spans.map(_.valid && branch_valid))
      connectVecs(io_axis_spans.map(_.bits), branch.in.axis_spans.map(_.bits))
      io_axis_spans(axisId).valid := branch_valid
      io_axis_spans(axisId).bits := branch.span
    }

    when (branch_valids3.head) {
      val in3 = branches3.head.in
      connectVecs(io.axis_spans(branch_id_of(in3, true)).map(_.valid), in3.axis_spans.map(_.valid))
      connectVecs(io.axis_spans(branch_id_of(in3, true)).map(_.bits), in3.axis_spans.map(_.bits))
      io.axis_spans(branch_id_of(in3, true))(axisId).valid := true.B
      io.axis_spans(branch_id_of(in3, true))(axisId).bits := branches3.head.span
    }

    when (in4_valid) {
      connectVecs(io.axis_spans(branch_id4).map(_.valid), in4.axis_spans.map(_.valid))
      connectVecs(io.axis_spans(branch_id4).map(_.bits), in4.axis_spans.map(_.bits))
      io.axis_spans(branch_id4)(axisId).valid := true.B
      io.axis_spans(branch_id4)(axisId).bits := span4
    }

    // The 'too_many_elems_in_rf4' value is actually calculated in stage-4, but we have to put it here to avoid forward
    //   references.
    too_many_elems_in_rf4 := (if (!canRead) false.B else (!isWrite(in4) && is_branched4 && (maxElemsInRf match {
      case Some(maxElems) =>
        def getSpan(span: UInt): UInt = if (is_innermost) span else maxOf(span, 1.U)
        val elems_in_stage5 = Mux(in5_valid && branch_id5 === branch_id4, getSpan(span_step_size5), 0.U)
        val elems_in_stage6 = Mux(in6_valid && branch_id6 === branch_id4, getSpan(in6.read_req.spans(axisId)), 0.U)
        val total_elems_in_rf = io.lookup_future_stages(branch_id4).nElems +& elems_in_stage5 +& elems_in_stage6
        val elems_to_add = if (is_innermost) elemsPerRead.U - read_col_counter4 else 1.U
        total_elems_in_rf +& elems_to_add > maxElems.U

      case None => false.B
    })))

    if (mightAccessRf) {
      io.lookup_past_stages.zipWithIndex.foreach { case (lookup, lookupPortId) => noPrefix {
        val lookup_branch_id = branch_id_of(lookup.address.map(_.asUInt))

        val stages_to_look_at = Seq((in_valid, in), (in2_valid, in2)).map { case (v, _in) => (v, _in, branch_id_of(_in)) } ++ {
          if (multipleNElemsLookupPorts) {
            // To reduce the number of lines of FIRRTL generated, we assume here that if "multipleNElemsLookupPorts = true", then each lookupPortId is just checking for a single branch. // TODO add assertions to verify that this is correct
            // Remove this "if (multipleNElemsLookupPorts)" clause completely and just replacing it with the "else" clause below should NOT break correctness; this is only an LOC optimization
            Seq((branch_valids3(lookupPortId), branches3(lookupPortId).in, if (lookupPortId == 0) branch_id_of(branches3(lookupPortId).in) else lookupPortId.U))
          } else
            branch_valids3.zip(branches3.map(_.in)).zipWithIndex.map { case ((v, _in), branchId) => (v, _in, if (branchId == 0) branch_id_of(branches3(lookupPortId).in) else branchId.U) }
        }

        lookup.nElems := PopCount(stages_to_look_at.map { case (valid, _in, branch_id) =>
          valid && branch_id === lookup_branch_id && muxOnWrite(_in, _in.write_req.spans(axisId), _in.read_req.spans(axisId)) =/= 0.U
        })
      }}
    }

    io.busy := isBusy(in_valid, in) || isBusy(in2_valid, in2) ||
      any(branch_valids3.zip(branches3).map { case (valid3, branch3) => isBusy(valid3, branch3.in) }) ||
      isBusy(in4_valid, in4) || isBusy(in5_valid, in5) || isBusy(in6_valid, in6)

    io.full_busy := isBusy(in_valid, in, for_full=true) || isBusy(in2_valid, in2, for_full=true) ||
      any(branch_valids3.zip(branches3).map { case (valid3, branch3) => isBusy(valid3, branch3.in, for_full=true) }) ||
      isBusy(in4_valid, in4, for_full=true) || isBusy(in5_valid, in5, for_full=true) || isBusy(in6_valid, in6, for_full=true)

    (Seq(in, in2, in4, in5, in6) ++ branches3.map(_.in)).foreach { x =>
      hardCodeValues(x, Seq(in5, in6).flatMap(addrFieldsOfThisAxis))
      when (reset.asBool) {
        x.op_count := OpCount(0.U)
      }
    }
  } else if (axis == FiberTreeAxis.LinkedList) {
    /* Note: The linked-list" axis has multiple pipeline stages:
        1) Make an SRAM read to find the head-ptr if it already exists. Otherwise, make an SRAM write to instantiate
           a new head-ptr.
        2) Buffer "in" while making read-request of prior stage
        3) Select which branch to forward to next step
        4) Stage 3 has multiple sequential steps (kind of like a little FSM)

           i)  Increment through node while making SRAM reads/writes for data. If you're at the end of the node, make an
               SRAM read for the next ptr. If we are at the end of the axis, then there's no need to read the next-ptr.

           ii) If the next-ptr is null, then you'll need to allocate a new node and increment the free-ptr (if writing).
               Either way, go back to step 3i with your new next-ptr (unless we've already finished all the SRAM
               accesses required in this axis).

        5) Buffer "in" while making data read-requests of prior stage
        6) Capture and align result of data SRAM read requests
        7) Output final results
     */
    import LinkedListMetadata._

    assert(!in_valid || !should_interleave_push && !should_interleave_pop, "we don't yet support interleaving in linked-list dimensions")
    assert(!in_valid || !should_gather, "we don't yet support gathering from linked-list axes")
    assert(!in_valid || !muxOnWrite(in, any(in.write_req.from_regfile), in.read_req.to_regfile || any(in.read_req.should_gather)) || last_rf_axis =/= axisId.U || last_rf_axis_size_is_infinite, "we don't yet support custom rf-axis sizes in linked-list dimensions")
    require(maxElemsInRf.isEmpty || !multipleNElemsLookupPorts, s"multiple n-elem lookup ports not yet supported for LL axes | axisId=$axisId bankId=$bankId")

    val LinkedListMetadata(n_heads, n_nodes, node_size, coordParams, resetRunningLenAutomatically, headPtrIsHeadAddr, initialLenIs0ForRfWrites, nextNodeIsNullForRfWrites, decrementingFreeNodePointer, useAddrForSpanTraversed, supportsImbalancedSpanTraversal) = metadataConf
    val expanded_addr_bits = coordParams match {
      case CompressedMetadata.InnerBuffer(Some(nCoords), _) => log2Up(nCoords+1)
      case CompressedMetadata.DontExpandInner(Some(nCoords)) => log2Up(nCoords+1)
      case _ if is_innermost => log2Up(nElems+1)
      case _ => spanBits
    }
    require(expanded_addr_bits <= spanBits)

    val Seq(next_free_node_shared_state_id, total_running_len_shared_state_id, max_head_shared_state_id) = (0 until n_shared_state_across_banks).toSeq

    val max_head_initialized = RegInit(0.U(log2Up(n_heads).W))

    // TODO we should add assertions to make sure that we don't run out of free nodes
    val next_free_node_reset_value = if (decrementingFreeNodePointer) n_nodes-1 else 0
    val next_free_node_reg = RegInit(next_free_node_reset_value.U(log2Up(n_nodes).W)) // TODO this should be shared between read and write stages
    def updated_next_free_node(others: Seq[UInt]): UInt = if (decrementingFreeNodePointer && headPtrIsHeadAddr) {
        assert(all(others.map(_ <= 1.U)))
        others.scanLeft(next_free_node_reg)((acc,x) => Mux(x(0), Mux(acc === max_head_initialized, next_free_node_reset_value.U, acc - 1.U), acc)).last
      } else if (decrementingFreeNodePointer) {
        next_free_node_reg - sumvU(others)
      } else {
        next_free_node_reg + sumvU(others)
      }
    val next_free_node_wire = updated_next_free_node(io.other_banks_shared_state_increment(next_free_node_shared_state_id).take(bankId))

    val null_ptr = (~(0.U.asTypeOf(metadataElemT))).asUInt
    def is_null_ptr(x: UInt) = {
      val node_addr_bits = log2Up(n_nodes)
      x.widthOption match {
        case Some(w) if w > node_addr_bits => x(w-1)
        case _ => x.andR
      }
    }

    // Arbitrators
    // Note: Generally, later stages get precedence over earlier stages
    val stage4_accessing_free_pointer = Wire(Bool())
    val stage4_accessing_next_ptr_buffer_addr = Wire(Bool())
    val stage4_accessing_last_len_buffer_addr = Wire(Bool())
    val stage4_writing_to_next_ptr_buffer = Wire(Bool())
    val stage4_writing_to_last_len_buffer = Wire(Bool())

    // Stage 1
    val should_use_running_state_for_head_ptr = muxOnWrite(in, in.write_req.use_running_state_for_metadata(axisId)(LinkedListMetadata.head_ptr_buffer_id),
      in.read_req.use_running_state_for_metadata(axisId)(LinkedListMetadata.head_ptr_buffer_id)) && !in.first_it
    val running_head_ptr = RegInit(0.U(log2Up(n_heads).W))
    val head_ptr_addr = Mux(should_use_running_state_for_head_ptr, running_head_ptr, in.metadata_addrs(axisId)(LinkedListMetadata.head_ptr_buffer_id) % n_heads.U)
    val allocate_new_head = isWrite(in) && !in.write_req.is_data && in.write_req.axis === axisId.U && in.write_req.metadata_buffer_id === head_ptr_buffer_id.U

    val output_running_len = !isWrite(in) && !in.read_req.should_read_data && in.read_req.axis === axisId.U && in.read_req.metadata_buffer_id === head_ptr_buffer_id.U && !reset_running_state
    assert(!in_valid || !in.write || !output_running_len || in.read_req.spans.drop(axisId+1).map(_ === 1.U).foldLeft(true.B)(_ && _), "we don't yet support reading out multiple running-lens at once")

    val wait_for_flush = Wire(Bool())

    val get_node_len_from_other_axis_span = WireInit(false.B)
    val wait_to_trail = should_trail && io.other_stage_span_traversed(branch_id_of(in, true)).valid && {
      assert(!(in_valid && should_trail) || is_innermost.B, "not sure if this code block still works if this isn't the innermost stage")

      val traversed = WireInit(in.span_traversed)
      val use_addr = useAddrForSpanTraversed.B && !isWrite(in) && in.read_req.adjacent
      when (use_addr) {
        traversed := in.read_req.address
      }
      traversed(axisId) := 0.U
      when (!isWrite(in)) {
        // We need special behavior here to accommodate the case where the write-stage may updates the node-len after we
        // read it. To deal with that possibility, we stall here till the write-stage finishes this node.
        /* TODO Ideally, we would be able to continue down the pipeline here and just snoop on the write-stages to
             see whether or not they update the len-buffer */

        val axis_span_discovered = vecEqualsU(io.other_stage_span_traversed(branch_id_of(in, true)).bits.drop(axisId+1), traversed.drop(axisId+1)) &&
          io.other_stage_axis_spans(branch_id_of(in, true))(axisId).valid
        get_node_len_from_other_axis_span := axis_span_discovered

        when (!axis_span_discovered) {
          traversed(axisId) := maxVal(traversed(axisId))
        }
      }
      when (supportsImbalancedSpanTraversal.B && !isWrite(in) && !in.read_req.to_regfile) {
        traversed(axisId+1) := maxVal(traversed(axisId+1))
        traversed(axisId) := maxVal(traversed(axisId))
      }

      !compareVecs(traversed, io.other_stage_span_traversed(branch_id_of(in, true)).bits, {_ < _})
    }
    // dontTouch(wait_to_trail)

    val waiting = wait_for_flush || wait_to_trail

    val outputs_running_len_from_buffer = resetRunningLenAutomatically

    val can_read_len_from_later_stage = Wire(Bool())

    val ready2 = Wire(Bool())
    val must_read_head = in_valid && !waiting && !allocate_new_head && !output_running_len && !reset_running_state && !headPtrIsHeadAddr.B
    val must_read_len = in_valid && !waiting && !allocate_new_head && (outputs_running_len_from_buffer.B || !output_running_len) && !reset_running_state && !(initialLenIs0ForRfWrites.B && isWrite(in) && in.write_req.from_regfile(axisId)) &&
      !can_read_len_from_later_stage
    when (must_read_head) { assert(must_read_len, "we assume that must-read-len is like a superset of must-read-head. you might need to adjust 'headPtrIsHeadAddr' and 'initialLenIs0ForRfWrites' to guarantee this") }

    io.metadata_read_reqs(head_ptr_buffer_id).valid := must_read_head && ready2
    io.metadata_read_reqs(head_ptr_buffer_id).bits.addr := head_ptr_addr
    io.metadata_read_reqs(head_ptr_buffer_id).bits.tag.write := in.write
    io.metadata_read_reqs(head_ptr_buffer_id).bits.pathId.foreach(_ := in.write)

    io.metadata_read_reqs(last_node_len_buffer_id).valid := must_read_len && ready2 && (!must_read_head || io.metadata_read_reqs(head_ptr_buffer_id).ready)
    io.metadata_read_reqs(last_node_len_buffer_id).bits.addr := head_ptr_addr
    io.metadata_read_reqs(last_node_len_buffer_id).bits.tag.write := in.write
    io.metadata_read_reqs(last_node_len_buffer_id).bits.pathId.foreach(_ := in.write)

    if (canWrite) {
      val can_allocate_new_head = !wait_for_flush && !stage4_accessing_free_pointer && !stage4_accessing_next_ptr_buffer_addr && !stage4_accessing_last_len_buffer_addr

      io.metadata_write_reqs(head_ptr_buffer_id).valid := in_valid && in.write && allocate_new_head && can_allocate_new_head &&
        /* io.metadata_write_reqs(next_ptr_buffer_id).ready && */ io.metadata_write_reqs(last_node_len_buffer_id).ready
      io.metadata_write_reqs(head_ptr_buffer_id).bits.addr := head_ptr_addr
      io.metadata_write_reqs(head_ptr_buffer_id).bits.data.head := (if (headPtrIsHeadAddr) head_ptr_addr else next_free_node_wire)
      io.metadata_write_reqs(head_ptr_buffer_id).bits.mask.foreach(_ := true.B)

      /*
      io.metadata_write_reqs(next_ptr_buffer_id).valid := in_valid && in.write && allocate_new_head && can_allocate_new_head &&
        io.metadata_write_reqs(head_ptr_buffer_id).ready && io.metadata_write_reqs(last_node_len_buffer_id).ready
      io.metadata_write_reqs(next_ptr_buffer_id).bits.addr := (if (headPtrIsHeadAddr) head_ptr_addr else next_free_node_wire)
      io.metadata_write_reqs(next_ptr_buffer_id).bits.data.head := null_ptr
      io.metadata_write_reqs(next_ptr_buffer_id).bits.mask.foreach(_ := true.B)
       */
      assert(!io.metadata_write_reqs(next_ptr_buffer_id).fire || stage4_writing_to_next_ptr_buffer, "we no longer initialize the next-ptr buffer in stage 1")

      io.metadata_write_reqs(last_node_len_buffer_id).valid := in_valid && in.write && allocate_new_head && can_allocate_new_head &&
        io.metadata_write_reqs(head_ptr_buffer_id).ready // && io.metadata_write_reqs(next_ptr_buffer_id).ready
      io.metadata_write_reqs(last_node_len_buffer_id).bits.addr := head_ptr_addr
      io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head := 0.U
      io.metadata_write_reqs(last_node_len_buffer_id).bits.mask.foreach(_ := true.B)
    }

    when (io.metadata_write_reqs(head_ptr_buffer_id).fire) {
      io.incrementing_shared_state(next_free_node_shared_state_id) := 1.U
      io.incrementing_shared_state(max_head_initialized) := 1.U
      running_head_ptr := head_ptr_addr + 1.U
    }

    val mem_reqs_fired = if (no_simultaneous_reads_and_writes) {
      any(Seq((head_ptr_buffer_id, false.B), (last_node_len_buffer_id, stage4_writing_to_last_len_buffer)).flatMap { case (buffer_id, stage4_writing) =>
        assert(!(io.metadata_read_reqs(buffer_id).valid && ready2) || io.metadata_read_reqs(buffer_id).ready)
        assert(!(io.metadata_write_reqs(buffer_id).valid && ready2) || io.metadata_write_reqs(buffer_id).ready)
        Seq(io.metadata_read_reqs(buffer_id).valid, !stage4_writing && io.metadata_write_reqs(buffer_id).valid)
      }) && ready2
    } else {
      any(Seq((head_ptr_buffer_id, false.B), (last_node_len_buffer_id, stage4_writing_to_last_len_buffer)).flatMap { case (buffer_id, stage4_writing) =>
        Seq(io.metadata_read_reqs(buffer_id).fire, !stage4_writing && io.metadata_write_reqs(buffer_id).fire)
      })
    }
    ending := mem_reqs_fired ||
      in_valid && output_running_len && !outputs_running_len_from_buffer.B && ready2 ||
      in_valid && !allocate_new_head && !must_read_head && !must_read_len && !waiting && ready2 ||
      reset_running_state && !wait_for_flush

    when (io.metadata_write_reqs(head_ptr_buffer_id).fire) {
      val all_written = all(Seq(head_ptr_buffer_id, /*next_ptr_buffer_id,*/ last_node_len_buffer_id).map(io.metadata_write_reqs).map(_.fire))
      assert(all_written, "not all SRAMs were written to which need to be written to when allocating a new head ptr")
    }
    when (io.metadata_read_reqs(head_ptr_buffer_id).fire) {
      assert(!must_read_len || io.metadata_read_reqs(last_node_len_buffer_id).fire, "did not read last-node-len-buffer at same time as head-buffer")
    }
    when (io.metadata_read_reqs(last_node_len_buffer_id).fire) {
      assert(!must_read_head || io.metadata_read_reqs(head_ptr_buffer_id).fire, "did not read head-buffer at same time as last-node-len-buffer")
    }

    // Stage 2
    val fire2 = ending && !allocate_new_head && !reset_running_state
    val fire3 = Wire(Bool())
    val valid2 = RegInit(false.B)
    ready2 := !valid2 || fire3

    val in2 = RegEnable(in, fire2)
    val free_node2 = RegEnable(next_free_node_wire, fire2)
    val head_ptr_addr2 = RegEnable(head_ptr_addr, fire2)
    val wait_for_read_resp2 = RegEnable(io.metadata_read_reqs(last_node_len_buffer_id).valid, fire2)
    val output_running_len2 = RegEnable(output_running_len, fire2)
    val must_read_head2 = RegEnable(must_read_head, fire2)
    val must_read_len2 = RegEnable(must_read_len, fire2)
    val must_read_axis_span2 = RegEnable(get_node_len_from_other_axis_span, fire2) // TODO when this is true, we should avoid reading the last-node-len buffer. It's not incorrect to read from it, but it will waste energy and possibly hurt performance due to bank conflicts
    val axis_span_from_other_stage2 = RegEnable(io.other_stage_axis_spans(branch_id_of(in, true))(axisId).bits, fire2)

    val is_regfile = Wire(Bool())
    def stage4_writing_last_node_len_buffer_for(_head_ptr_addr: UInt, _in: ChiselSRAMPipelineData[T]): Bool = {
      stage4_writing_to_last_len_buffer && !is_regfile && io.metadata_write_reqs(last_node_len_buffer_id).bits.addr === _head_ptr_addr &&
        isWrite(_in) && !_in.write_req.from_regfile(axisId)
    }
    val valid4_wire = Wire(Bool())
    val head_ptr_addr4_wire = Wire(getChiselType(head_ptr_addr2))
    val last_node_len4_wire = Wire(UInt((expanded_addr_bits+1).min(spanBits).W))
    val branch_valids3_wire = Wire(Vec(nBranches, Bool()))
    val stage3_is_regfile_wire = Wire(Vec(nBranches, Bool()))
    val branch_head_ptrs_wire = Wire(Vec(nBranches, getChiselType(head_ptr_addr2)))
    val branch_last_node_len_wire = Wire(Vec(nBranches, getChiselType(io.metadata_read_resps(last_node_len_buffer_id).bits.data.head)))

    if (canWrite) {
      when (fire2 && !in.write_req.from_regfile(axisId)) {
        when (valid2 && isWrite(in2) && !in2.write_req.from_regfile(axisId) && head_ptr_addr === head_ptr_addr2 && io.metadata_read_resps(last_node_len_buffer_id).valid) {
          must_read_len2 := true.B
          must_read_axis_span2 := true.B
          axis_span_from_other_stage2 := io.metadata_read_resps(last_node_len_buffer_id).bits.data.head
        }

        (0 until nBranches).foreach { branch_id =>
          when (branch_valids3_wire(branch_id) && !stage3_is_regfile_wire(branch_id) && head_ptr_addr === branch_head_ptrs_wire(branch_id)) {
            must_read_len2 := true.B
            must_read_axis_span2 := true.B
            axis_span_from_other_stage2 := branch_last_node_len_wire(branch_id)
          }
        }

        when (valid4_wire && !is_regfile && head_ptr_addr === head_ptr_addr4_wire && isWrite(in) && !in.write_req.from_regfile(axisId)) {
          must_read_len2 := true.B
          must_read_axis_span2 := true.B
          axis_span_from_other_stage2 := last_node_len4_wire
        }

        when (stage4_writing_last_node_len_buffer_for(head_ptr_addr, in)) {
          must_read_len2 := true.B
          must_read_axis_span2 := true.B
          axis_span_from_other_stage2 := io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head
        }
      }.elsewhen (valid2 && !fire3 && !in2.write_req.from_regfile(axisId)) {
        when (stage4_writing_last_node_len_buffer_for(head_ptr_addr, in2)) {
          must_read_len2 := true.B
          must_read_axis_span2 := true.B
          axis_span_from_other_stage2 := io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head
        }
      }
    }

    when (fire2) {
      valid2 := true.B
    }.elsewhen(fire3) {
      valid2 := false.B
    }

    when (!valid2) {
      in2.op_count := in.op_count
    }

    // Stage 3
    class CleanupProgress extends Bundle {
      val ended_early = Bool()
      val wrote_last_node_len = Bool()
      // val wrote_next_null_ptr = Bool()

      def cleaned_up: Bool = wrote_last_node_len // && wrote_next_null_ptr
    }

    class BranchData extends Bundle {
      val in = getChiselType(in2)

      val output_running_len = Bool()

      val head_ptr_addr = getChiselType(head_ptr_addr2)
      val last_node_len = getChiselType(io.metadata_read_resps(last_node_len_buffer_id).bits.data.head)

      val node_id = UInt(log2Up(n_nodes).W)
      val next_node_id = UInt(log2Up(n_nodes).W)
      val next_node_id_valid = Bool()
      val requested_next_node = Bool()
      val reached_null_ptr = Bool()
      val added_new_nodes = Bool()
      val added_new_node_after_this_node = Bool()
      val wrote_last_node_len = Bool()

      val cleanup_after_early_rf_ending = new CleanupProgress

      val node_iterator = UInt(log2Up(n_nodes).W)
      val span_iterator = UInt(spanBits.W)
      val read_col_counter = UInt(log2Ceil(elemsPerRead).W)
    }

    val branches3 = Reg(Vec(nBranches, new BranchData))
    val branch_valids3 = RegInit(VecInit(Seq.fill(nBranches)(false.B)))
    val branch_to_send3 = RegInit(0.U(log2Up(nBranches).W))

    branch_valids3_wire := branch_valids3
    stage3_is_regfile_wire.zip(branches3).foreach { case (x, branch) =>
      x := isWrite(branch.in) && branch.in.write_req.from_regfile(axisId)
    }
    branch_head_ptrs_wire.zip(branches3).foreach { case (x, branch) =>
      x := branch.head_ptr_addr
    }
    branch_last_node_len_wire.zip(branches3).foreach { case (x, branch) =>
      x := branch.last_node_len
    }

    val fire4 = Wire(Bool())
    fire3 := io.metadata_read_resps(last_node_len_buffer_id).ready && (io.metadata_read_resps(last_node_len_buffer_id).valid ||
      valid2 && !wait_for_read_resp2)
    assert(!fire3 || !wait_for_read_resp2 || (outputs_running_len_from_buffer.B && output_running_len2) ||
      (!must_read_head2 || io.metadata_read_resps(head_ptr_buffer_id).fire) && (!must_read_len2 || io.metadata_read_resps(last_node_len_buffer_id).fire),
      "did not grab both the head-ptr and the last-node-len simultaneously")

    val is_branched3 = if (couldBeBranched) RegEnable(is_branched(in2), fire3) else false.B
    val last_branch3 = Reg(UInt(log2Up(nBranches).W))
    val last_branch_received3 = RegEnable(is_last_arriving_branch(in2), fire3)
    val last_rf_access3 = RegEnable(in2.last_rf_access, fire3)
    val sent_first_branch3 = Reg(Bool())

    val incremented_branch3 = {
      val result = WireInit(branch_to_send3)

      val higher = (branch_valids3.asUInt >> (branch_to_send3+&1.U)).asUInt =/= 0.U
      val lower = (branch_valids3.asUInt & ((1.U << branch_to_send3).asUInt - 1.U)) =/= 0.U

      when (higher) {
        branch_valids3.zipWithIndex.reverse.foreach { case (v, i) =>
          when (v && i.U > branch_to_send3) {
            result := i.U
          }
        }
      }.elsewhen (lower) {
        branch_valids3.zipWithIndex.reverse.foreach { case (v, i) =>
          when (v && i.U < branch_to_send3) {
            result := i.U
          }
        }
      }

      when (!last_branch_received3) {
        result := branch_to_send3 + 1.U
      }

      when (!is_branched3) {
        result := 0.U
      }

      result
    }

    assert(is_branched3 || !any(branch_valids3.tail), "multiple branches are active even when they shouldn't be")

    assert(all(branch_valids3.zip(branches3).map { case (v, b) => !v || b.node_id < n_nodes.U }))

    val stage3_writing_first_branch = WireInit(false.B)

    when (fire3) {
      val branch_id = branch_id_of(in2) % nBranches.U

      branch_valids3(branch_id) := true.B
      branches3.zipWithIndex.foreach { case (branch, bid) =>
        when (bid.U === branch_id) {
          branch.in := in2
          branch.in.last_rf_access := last_rf_access3 && last_branch_received3 && !any(branch_valids3.patch(bid, Nil, 1))

          branch.node_iterator := 0.U
          branch.span_iterator := 0.U
          branch.read_col_counter := 0.U

          branch.output_running_len := output_running_len2

          branch.head_ptr_addr := head_ptr_addr2
          branch.last_node_len := Mux(must_read_len2, Mux(must_read_axis_span2, axis_span_from_other_stage2, io.metadata_read_resps(last_node_len_buffer_id).bits.data.head), 0.U)

          branch.node_id := Mux(must_read_head2, io.metadata_read_resps(head_ptr_buffer_id).bits.data.head, if (headPtrIsHeadAddr) head_ptr_addr2 else free_node2)
          branch.next_node_id_valid := false.B
          branch.requested_next_node := false.B
          branch.reached_null_ptr := false.B
          branch.added_new_nodes := false.B
          branch.added_new_node_after_this_node := false.B
          branch.wrote_last_node_len := false.B

          branch.cleanup_after_early_rf_ending := 0.U.asTypeOf(new CleanupProgress)

          if (canWrite) {
            when (!in2.write_req.from_regfile(axisId) && stage4_writing_last_node_len_buffer_for(head_ptr_addr2, in2)) {
              branch.last_node_len := io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head
            }
          }
        }
      }

      last_branch3 := branch_id

      if (couldBeBranched)
        when (is_first_arriving_branch(in2) && is_last_arriving_branch(in2)) {
          // There's no need to branch if there's only one branch anyways
          is_branched3 := false.B
        }

      stage3_writing_first_branch := branch_id === 0.U
    }.elsewhen(fire4 && !is_branched3) {
      branch_valids3.foreach(_ := false.B)
    }

    if (canWrite) {
      branches3.zip(branch_valids3).zipWithIndex.foreach { case ((branch, branch_valid), bid) =>
        when(!branch.in.write_req.from_regfile(axisId) && branch_valid && !(fire3 && branch_id_of(in2) % nBranches.U === bid.U) && !(fire4 && branch_to_send3 % nBranches.U === bid.U) && stage4_writing_last_node_len_buffer_for(branch.head_ptr_addr, branch.in)) {
          branch.last_node_len := io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head
        }
      }
    }

    branch_valids3.zip(branches3).foreach { case (branch_valid, branch) =>
      when (!branch_valid) {
        branch.in.op_count := in2.op_count
      }
    }

    val stage4_should_not_overwrite_branch_to_send3 = WireInit(false.B)
    when (fire3 && is_first_arriving_branch(in2)) {
      sent_first_branch3 := false.B
      stage4_should_not_overwrite_branch_to_send3 := true.B
      branch_to_send3 := 0.U
    }.elsewhen(fire4 && is_branched3) {
      sent_first_branch3 := true.B
    }

    val stage4_clearing_all_branch_valids = WireInit(false.B)
    for ((buffer_id, must_read) <- Seq((head_ptr_buffer_id, must_read_head2), (last_node_len_buffer_id, true.B))) {
      io.metadata_read_resps(buffer_id).ready := must_read && (
        !any(branch_valids3) ||
        !branch_valids3(branch_id_of(in2)) && !last_branch_received3 ||
        !is_branched3 && fire4 ||
        stage4_clearing_all_branch_valids)
    }

    io.next_node_added.zip(branches3).zip(branch_valids3).foreach { case ((io_next_node_added, branch), branch_valid) =>
      val last_node = branch.node_iterator === branch.last_node_len / node_size.U - (branch.last_node_len % node_size.U === 0.U)
      io_next_node_added := branch_valid && branch.added_new_node_after_this_node || last_node
    }

    // Stage 4
    val valid4 = RegInit(false.B)
    valid4_wire := valid4

    val in4 = Reg(getChiselType(in))
    val branch_id4 = Reg(UInt(log2Ceil(nBranches).W))
    val is_branched4 = if (couldBeBranched) RegEnable(is_branched3, fire4) else false.B

    is_regfile := isWrite(in4) && in4.write_req.from_regfile(axisId)

    val output_running_len4 = Reg(Bool())

    val head_ptr_addr4 = Reg(getChiselType(head_ptr_addr2))
    head_ptr_addr4_wire := head_ptr_addr4
    val last_node_len4 = Reg(UInt((expanded_addr_bits+1).min(spanBits).W))
    last_node_len4_wire := last_node_len4

    val node_id4 = Reg(UInt(log2Up(n_nodes).W))
    val next_node_id4 = Reg(UInt(log2Up(n_nodes).W))
    val next_node_id_valid4 = Reg(Bool())
    val requested_next_node4 = Reg(Bool())
    val reached_null_ptr4 = Reg(Bool())
    val added_new_nodes4 = Reg(Bool())
    val added_new_node_after_this_node4 = Reg(Bool())
    val wrote_last_node_len4 = Reg(Bool())

    val cleanup_after_early_rf_ending4 = Reg(new CleanupProgress)

    val node_addr4 = node_id4 * node_size.U
    val node_iterator4 = Reg(UInt(log2Up(n_nodes).W))

    val bias4 = muxOnWrite(in4, in4.write_req.address(axisId), in4.read_req.address(axisId))

    val data_and_coord_addr4 = in4.metadata_addrs(axisId)(LinkedListMetadata.coord_buffer_id)(expanded_addr_bits-1,0).asUInt
    if (is_innermost)
      assert(!valid4 || in4.data_addr === data_and_coord_addr4)

    val span_iterator4 = Reg(UInt(expanded_addr_bits.W))
    val iterator4 = (span_iterator4 +& bias4 +& data_and_coord_addr4)(expanded_addr_bits-1,0).asUInt

    val next_node_resp4 = Mux(nextNodeIsNullForRfWrites.B && in.write && in.write_req.from_regfile(axisId), null_ptr,
      io.metadata_read_resps(next_ptr_buffer_id).bits.data.head)
    val next_node_id_wire4 = Mux(next_node_id_valid4, next_node_id4, Mux(stage4_writing_to_next_ptr_buffer,
      io.metadata_write_reqs(next_ptr_buffer_id).bits.data.head, next_node_resp4))
    val next_node_id_available4 = next_node_id_valid4 || io.metadata_read_resps(next_ptr_buffer_id).valid
    val next_node_is_null4 = reached_null_ptr4 ||
      !next_node_id_valid4 && io.metadata_read_resps(next_ptr_buffer_id).valid && is_null_ptr(next_node_resp4) ||
      node_iterator4 >= (bias4 +& data_and_coord_addr4) / node_size.U && align(bias4 +& data_and_coord_addr4 +& span_iterator4, node_size, up=true) >= last_node_len4

    val resets_lower_axis4 = in4.write && in4.write_req.reset_running_state && in4.write_req.axis < axisId.U

    val req_span4 = muxOnWrite(in4, in4.write_req.spans(axisId), in4.read_req.spans(axisId))(expanded_addr_bits-1,0).asUInt
    val span4 = Mux(in4.write && (in4.write_req.axis === axisId.U || in4.write_req.from_regfile(axisId) || is_innermost.B), req_span4,
      Mux(resets_lower_axis4 && req_span4 === 0.U, 1.U, minOf(req_span4,
        floorSub(reduceWidth(last_node_len4, expanded_addr_bits), reduceWidth(bias4 +& data_and_coord_addr4, expanded_addr_bits).asUInt))))
    // dontTouch(span4)

    assert(!valid4 || node_iterator4 * node_size.U <= span_iterator4 +& bias4 +& data_and_coord_addr4, "node iterator seems to have run ahead of span iterator")

    val final_iterator4 = (span4 +& bias4 +& data_and_coord_addr4)(expanded_addr_bits-1,0).asUInt
    val final_node_iterator4 = Mux(!in4.write && next_node_is_null4,
      node_iterator4, final_iterator4 / node_size.U - (final_iterator4 =/= 0.U && final_iterator4 % node_size.U === 0.U))
    val last_node4 = node_iterator4 === final_node_iterator4

    val inner_addr4 = node_addr4 +& (iterator4 % node_size.U)
    val sram_row4 = inner_addr4 >> log_elems_per_row
    val sram_col4 = inner_addr4 & ((1 << log_elems_per_row)-1).U

    val read_col_counter4 = if (elemsPerRead == 1) 0.U else RegInit(0.U(log2Ceil(elemsPerRead).W))
    val last_read4 = WireInit(false.B)

    val node_iterator_is_behind4 = (iterator4 / node_size.U) > node_iterator4 && (in4.write || !next_node_is_null4)

    val axisId4 = muxOnWrite(in4,
      Mux(in4.write_req.is_data, 0.U, in4.write_req.axis),
      Mux(in4.read_req.should_read_data, 0.U, in4.read_req.axis))

    val too_many_elems_in_rf4 = Wire(Bool())
    when (valid4) {
      io.lookup_future_stages(branch_id4).address := in4.read_req.address.map(_.zext)
      io.lookup_future_stages(branch_id4).address(axisId) := span_iterator4.zext // TODO should we add a "+ bias" here?
    }

    val span_step_size4 = {
      val _elemsPerRead = Mux(!isWrite(in4) && in4.read_req.should_read_data && in4.read_req.should_read_metadata, (elemsPerRead/2).U, elemsPerRead.U)
      val size_if_axisId_matches = Mux(too_many_elems_in_rf4, 0.U, minOf(
        span4 - span_iterator4, // till end of span
        elemsPerRow.U - sram_col4, // till end of SRAM row
        muxOnWrite(in4, Mux(in4.write_req.from_regfile(axisId), elemsPerWriteRf.U, elemsPerWrite.U), _elemsPerRead - read_col_counter4), // till end of read/write packet
        node_size.U - (iterator4 % node_size.U) // till end of node
      ))

      Mux(axisId4 === axisId.U, size_if_axisId_matches, minOf(size_if_axisId_matches, 1.U))
    }

    val last_step_in_span4 = (span_iterator4 +& span_step_size4 >= span4) || span_iterator4 === span4
    val last_step_in_node4 = node_iterator_is_behind4 || span_step_size4 > 0.U && (iterator4 +& span_step_size4) % node_size.U ===  0.U

    val empty_write4 = in4.write && in4.write_req.spans(axisId) === 0.U

    // TODO We shouldn't need to wait till 'next_node_id_valid4' is true if 'next_node_id_available4' during writes
    val waiting_for_next_node4 = !empty_write4 && !last_node4 && Mux(in4.write && (in4.write_req.axis === axisId.U || in4.write_req.from_regfile(axisId)),
      !next_node_id_valid4 && !(next_node_id_available4 && !next_node_is_null4) && !(stage4_writing_to_next_ptr_buffer && io.metadata_write_reqs(next_ptr_buffer_id).fire) &&
        !(in4.write_req.from_regfile(axisId) && (!last_step_in_node4 || io.not_all_rf_outs_found || cleanup_after_early_rf_ending4.ended_early)),
      !next_node_id_available4 && !next_node_is_null4 && !output_running_len4 && last_step_in_node4)

    val waiting_for_next_node_rf_version4 = {
      // This is just a modified version of "waiting_for_next_node4" that we use to avoid combinational loops when performing writes from regfiles
      !empty_write4 && !last_node4 &&
        !next_node_id_valid4 && !(next_node_id_available4 && !next_node_is_null4) && !io.metadata_write_reqs(next_ptr_buffer_id).ready &&
        last_step_in_node4 && !io.not_all_rf_outs_found && !cleanup_after_early_rf_ending4.ended_early
    }

    val should_trail4 = muxOnWrite(in4, in4.write_req.should_trail_reads, in4.read_req.should_trail_writes)
    val waiting_to_trail4 = should_trail4 && io.other_stage_span_traversed(branch_id_of(in4, true)).valid && {
      assert(!(valid4 && should_trail4) || is_innermost.B, "not sure if this code block still works if this isn't the innermost stage")
      assert(!valid4 || !should_trail4 || (nBranches == 1).B || (outermostBranchedAxisId == axisId).B, "i am not sure whether this code works when the outermost-branched-axis is different from the current axis")
      val traversed = WireInit(in4.span_traversed)
      val use_addr = useAddrForSpanTraversed.B && !isWrite(in4) && in4.read_req.adjacent
      when (use_addr) {
        traversed := in4.read_req.address
      }
      traversed(axisId) := span_iterator4 +& span_step_size4
      !compareVecs(traversed, io.other_stage_span_traversed(branch_id_of(in4, true)).bits, {_ < _}, orEquals = true)
    }

    io.next_node_added(branch_id4) := valid4 && (added_new_node_after_this_node4 || last_node4)

    val not_all_rf_outs_found = valid4 && isWrite(in4) && in4.write_req.from_regfile(axisId) && io.not_all_rf_outs_found &&
      !cleanup_after_early_rf_ending4.ended_early
    val not_all_rf_outs_captured = not_all_rf_outs_found && io.write_from_regfile_req.fire

    val next_step_cleanup_after_early_rf_ending4 = WireInit(cleanup_after_early_rf_ending4)
    when (not_all_rf_outs_captured) {
      next_step_cleanup_after_early_rf_ending4.ended_early := true.B
      // next_step_cleanup_after_early_rf_ending4.wrote_next_null_ptr := !next_node_is_null4
      next_step_cleanup_after_early_rf_ending4.wrote_last_node_len := !next_node_is_null4

      cleanup_after_early_rf_ending4.ended_early := next_step_cleanup_after_early_rf_ending4.ended_early
      // cleanup_after_early_rf_ending4.wrote_next_null_ptr := next_step_cleanup_after_early_rf_ending4.wrote_next_null_ptr
      cleanup_after_early_rf_ending4.wrote_last_node_len := next_step_cleanup_after_early_rf_ending4.wrote_last_node_len
    }

    val can_cleanup_now4 = /* io.metadata_write_reqs(next_ptr_buffer_id).ready && */ io.metadata_write_reqs(last_node_len_buffer_id).ready || (
      cleanup_after_early_rf_ending4.ended_early &&
        // (cleanup_after_early_rf_ending4.wrote_next_null_ptr || io.metadata_write_reqs(next_ptr_buffer_id).ready) &&
        (cleanup_after_early_rf_ending4.wrote_last_node_len || io.metadata_write_reqs(last_node_len_buffer_id).ready))
    val cleaning_up_now4 = not_all_rf_outs_captured /* && stage4_writing_to_next_ptr_buffer && io.metadata_write_reqs(next_ptr_buffer_id).fire */ && stage4_writing_to_last_len_buffer && io.metadata_write_reqs(last_node_len_buffer_id).fire || (
      cleanup_after_early_rf_ending4.ended_early &&
      // (cleanup_after_early_rf_ending4.wrote_next_null_ptr || stage4_writing_to_next_ptr_buffer && io.metadata_write_reqs(next_ptr_buffer_id).fire) &&
      (cleanup_after_early_rf_ending4.wrote_last_node_len || stage4_writing_to_last_len_buffer && io.metadata_write_reqs(last_node_len_buffer_id).fire))
    val waiting_to_cleanup4 = cleanup_after_early_rf_ending4.ended_early && !(
      // (cleanup_after_early_rf_ending4.wrote_next_null_ptr || stage4_writing_to_next_ptr_buffer && io.metadata_write_reqs(next_ptr_buffer_id).fire) &&
      (cleanup_after_early_rf_ending4.wrote_last_node_len || stage4_writing_to_last_len_buffer && io.metadata_write_reqs(last_node_len_buffer_id).fire))

    val wrote_or_writing_last_node_len4 = wrote_last_node_len4 || stage4_writing_to_last_len_buffer && io.metadata_write_reqs(last_node_len_buffer_id).fire
    val waiting_to_write_last_node_len4 = in4.write && !empty_write4 && (in4.write_req.from_regfile(axisId) || in4.write_req.axis === axisId.U) && next_node_is_null4 && last_node4 && !wrote_or_writing_last_node_len4

    val waiting_to_write_last_node_len_rf_version4 = {
      // This is just a modified version of "waiting_for_next_node4" that we use to avoid combinational loops when performing writes from regfiles
      !empty_write4 && next_node_is_null4 && last_node4 && !(
        wrote_last_node_len4 || io.metadata_write_reqs(last_node_len_buffer_id).ready)
    }

    val waiting_for_flush4 = Wire(Bool())

    val waiting4 = waiting_for_next_node4 || waiting_to_trail4 || waiting_to_write_last_node_len4 || waiting_to_cleanup4 || waiting_for_flush4 || node_iterator_is_behind4
//    dontTouch(waiting4)
//    dontTouch(waiting_for_next_node4)
//    dontTouch(waiting_to_trail4)
//    dontTouch(waiting_to_write_last_node_len4)
//    dontTouch(waiting_to_cleanup4)
//    dontTouch(waiting_for_flush4)
//    dontTouch(node_iterator_is_behind4)

    def get_waiting_rf_version4(without: Bool*): Bool = {
      val conds = Seq(waiting_for_next_node_rf_version4, waiting_to_trail4, waiting_to_write_last_node_len_rf_version4, /* !can_cleanup_now4, */ waiting_for_flush4, node_iterator_is_behind4)
      assert(without.forall(conds.contains))
      any(conds.filterNot(without.contains))
    }
    val waiting_rf_version4 = {
      // This is just a modified version of "waiting_for_next_node4" that we use to avoid combinational loops when performing writes from regfiles
      get_waiting_rf_version4()
    }
//    dontTouch(waiting_rf_version4)
//    dontTouch(waiting_for_next_node_rf_version4)
//    dontTouch(waiting_to_trail4)
//    dontTouch(waiting_to_write_last_node_len_rf_version4)
//    dontTouch(can_cleanup_now4)
//    dontTouch(waiting_for_flush4)
//    dontTouch(node_iterator_is_behind4)

    val skip_data_and_coord_accesses4 = axisId4 =/= axisId.U &&
      !muxOnWrite(in4, in4.write_req.from_regfile(axisId), in4.read_req.to_regfile)
    val skip_axis4 = axisId4 > axisId.U

    val ready5 = Wire(Bool())

    val mem_reqs_fired4 = if (no_simultaneous_reads_and_writes) {
      (io.data_read_req.valid || io.data_write_req.valid || io.metadata_read_reqs(coord_buffer_id).valid || io.metadata_write_reqs(coord_buffer_id).valid) && ready5
    } else {
      io.data_read_req.fire || io.data_write_req.fire || io.metadata_read_reqs(coord_buffer_id).fire || io.metadata_write_reqs(coord_buffer_id).fire
    }
    val span_stepping4 = !waiting4 && (mem_reqs_fired4 ||
      (skip_data_and_coord_accesses4 || span_step_size4 === 0.U) && !skip_axis4 && ready5)
    val node_stepping4 = ((node_iterator_is_behind4 && !waiting_for_next_node4) || (span_stepping4 && last_step_in_node4)) &&
      !not_all_rf_outs_found

    val ending4 = !waiting4 && (!not_all_rf_outs_found || cleaning_up_now4) && (
      (span_stepping4 && (span_iterator4 +& span_step_size4 >= span4)) ||
      ((in4.write && (axisId4 === axisId.U || in4.write_req.from_regfile(axisId)) || ready5) && span_iterator4 >= span4) ||
      output_running_len4 && ready5 ||
      cleanup_after_early_rf_ending4.ended_early || not_all_rf_outs_found && cleaning_up_now4 ||
      skip_axis4)
    val starting4 = span_iterator4 === 0.U && (span_stepping4 || ending4)

    val switching_branches4 = valid4 && is_branched4 &&
      (ending4 || (span_stepping4 && !next_step_cleanup_after_early_rf_ending4.ended_early)) &&
      (in4.write || last_read4 || span_step_size4 === 0.U && read_col_counter4 === 0.U)
    val switching_to_same_branch4 = valid4 && (branch_to_send3 % nBranches.U) === branch_id4 && sent_first_branch3
    assert(!(switching_branches4 && !in4.write && span_step_size4 === 0.U && read_col_counter4 =/= 0.U),
      "Sometimes, we want to switch branches because the current branch can't progress because there are too many elements from that branch in the reg-file. When that's the cause, \"span_step_size\" will be 0, but we want to make sure we didn't leave partial, unaligned read-outs in 'in4'")

    fire4 := (!valid4 || ending4 || switching_branches4) && branch_valids3(branch_to_send3) && !switching_to_same_branch4

    val running_len4 = RegInit(0.U(spanBits.W)) // This is used to convert the LinkedList format to the CSR format when writing out to the DMA
    val total_running_len4 = RegInit(0.U(32.W)) // TODO magic number

    val next_step_span_iterator4 = WireInit(span_iterator4)
    when (span_stepping4) {
      val increment = Mux(not_all_rf_outs_found, io.found_rf_outs_num, span_step_size4)
      next_step_span_iterator4 := span_iterator4 + increment
      span_iterator4 := next_step_span_iterator4

      when (valid4 && !in4.write && !in4.read_req.to_regfile) {
        when (in4.read_req.should_read_data) {
          io.incrementing_shared_state(total_running_len_shared_state_id) := increment << (in4.read_req.should_read_data && in4.read_req.should_read_metadata)
        }.otherwise {
          running_len4 := running_len4 + increment
        }
      }
    }

    total_running_len4 := total_running_len4 + sumvU(io.other_banks_shared_state_increment(total_running_len_shared_state_id))

    val next_step_node_iterator4 = WireInit(node_iterator4)
    val next_step_node_id4 = WireInit(node_id4)
    val next_step_next_node_id_valid4 = WireInit(next_node_id_valid4)
    val next_step_added_new_node_after_this_node4 = WireInit(added_new_node_after_this_node4)
    val next_step_requested_next_node4 = WireInit(requested_next_node4)
    when (node_stepping4) {
      next_step_node_iterator4 := node_iterator4 + 1.U
      next_step_node_id4 := next_node_id_wire4
      next_step_next_node_id_valid4 := false.B
      next_step_added_new_node_after_this_node4 := false.B
      next_step_requested_next_node4 := false.B

      node_iterator4 := next_step_node_iterator4
      node_id4 := next_step_node_id4
      next_node_id_valid4 := next_step_next_node_id_valid4
      added_new_node_after_this_node4 := next_step_added_new_node_after_this_node4
      requested_next_node4 := next_step_requested_next_node4

      assert(!valid4 || ending4 || !is_null_ptr(next_node_id_wire4), "going into the null pointer")
    }
    // dontTouch(node_stepping4)

    val last_axis4 = muxOnWrite(in4, in4.write_req.from_regfile_last_axis, in4.read_req.to_regfile_last_axis)
    val last_rf_axis_size4 = muxOnWrite(in4, 1.U << in4.write_req.from_regfile_last_axis_log_size, 1.U << in4.read_req.to_regfile_last_axis_log_size).asUInt
    val is_last_branch4 = !is_branched4 || last_branch_received3 && all(branches3.zip(branch_valids3).zipWithIndex.map { case ((branch, valid), branchId) =>
      !valid || branchId.U === branch_id4 || branch.span_iterator / last_rf_axis_size4 > span_iterator4 / last_rf_axis_size4
    })
    val last_step_in_rf_axis4 = {
      assert(!valid4 || !muxOnWrite(in4, any(in4.write_req.from_regfile), in4.read_req.to_regfile || any(in4.read_req.should_gather)) || last_axis4 =/= axisId.U || last_rf_axis_size4 >= infinite_last_rf_axis_size || last_rf_axis_size4 % span_step_size4 === 0.U, "we might need to adjust the span-step-size if it doesn't divide cleanly into the last-rf-axis size")
      (((span_iterator4 +& span_step_size4) % last_rf_axis_size4 === 0.U) && span_stepping4 || ending4) && is_last_branch4
    }
    val last4 = (last_axis4 === axisId.U || in4.last_rf_access) &&
      (not_all_rf_outs_found || !cleanup_after_early_rf_ending4.ended_early && ending4)
    val first4 = (last_axis4 === axisId.U || in4.first_rf_access) && starting4

    when (valid4 && last_axis4 === axisId.U &&
      muxOnWrite(in4, any(in4.write_req.from_regfile.take(axisId+1)), in4.read_req.to_regfile || any(in4.read_req.should_gather.take(axisId+1))) &&
      last_step_in_rf_axis4)
    {
      opCount := opCount + 1.U // Increment op-count
    }
    when(io.other_banks_op_count.fire) {
      opCount := io.other_banks_op_count.bits
      assert(io.other_banks_op_count.bits >= opCount)
      assert(!valid4, "i'm not sure if this works when the pipeline stage is currently in operation")
    }

    val next_step_axis_spans = WireInit(in4.axis_spans)
    in4.axis_spans := next_step_axis_spans

    if (canWrite && mightAccessRf) {
      io.write_from_regfile_req.valid := valid4 && in4.write && in4.write_req.from_regfile(axisId) &&
        !cleanup_after_early_rf_ending4.ended_early &&
        !waiting_rf_version4 &&
        (if (is_innermost) io.data_write_req.ready else io.metadata_write_reqs(coord_buffer_id).ready) &&
        (if (is_innermost) true.B else io.out.ready)
      io.write_from_regfile_req.bits.req := in4.write_req
      io.write_from_regfile_req.bits.req.axis := axisId.U
      io.write_from_regfile_req.bits.req.spans(axisId) := span_step_size4
      io.write_from_regfile_req.bits.req.address := {
        val default_addr = WireInit(in4.write_req.address)
        default_addr(axisId) := span_iterator4 // TODO should we add a "+ bias" here?
        val result = WireInit(default_addr)
        result(last_axis4) := default_addr(last_axis4) % last_rf_axis_size4
        result
      }
      io.write_from_regfile_req.bits.offset := sram_col4
      io.write_from_regfile_req.bits.opCount := Mux(last_axis4 === axisId.U, opCount, in4.op_count)

      when (io.write_from_regfile_req.fire) {
        next_step_axis_spans(axisId) := io.write_from_regfile_axis_spans(axisId)
      }

      assert(!io.write_from_regfile_req.fire || ending4 || span_stepping4, "wrote from regfile without iterating")
    }

    if (is_innermost) {
      if (canRead) {
        io.data_read_req.valid := valid4 && !in.write && (in4.read_req.should_read_data || in4.read_req.to_regfile) &&
          !waiting4 && span_step_size4 > 0.U && ready5
        io.data_read_req.bits.addr := sram_row4
        io.data_read_req.bits.tag.axisId := axisId.U
      }

      if (canWrite) {
        io.data_write_req.valid := valid4 && in4.write && !waiting4 && !node_iterator_is_behind4 &&
          !cleanup_after_early_rf_ending4.ended_early &&
          ((!in4.write_req.from_regfile(axisId) && in4.write_req.is_data) || io.write_from_regfile_req.fire)
        io.data_write_req.bits.addr := sram_row4
        io.data_write_req.bits.data.zipWithIndex.foreach { case (d, i) =>
          when(i.U >= sram_col4) {
            val col = (i.U +& span_iterator4 -& sram_col4) << in4.write_req.is_both_data_and_metadata
            d := in4.write_req.data(col)
          }.otherwise {
            d := DontCare
          }
        }
        io.data_write_req.bits.mask.zipWithIndex.foreach { case (m, i) =>
          m := i.U >= sram_col4 && i.U < sram_col4 +& span_step_size4
        }
      }
    }

    io.metadata_read_reqs(coord_buffer_id).valid := {
      val data_read_is_ready = if (no_simultaneous_reads_and_writes) true.B else {
        !(in4.read_req.to_regfile && is_innermost.B || in4.read_req.should_read_data && in4.read_req.axis === axisId.U) ||
          io.data_read_req.ready
      }
      valid4 && !in4.write && !waiting4 && span_step_size4 > 0.U && ready5 && data_read_is_ready &&
        (in4.read_req.to_regfile || (in4.read_req.should_read_metadata && in4.read_req.axis === axisId.U && in4.read_req.metadata_buffer_id === coord_buffer_id.U))
    }
    io.metadata_read_reqs(coord_buffer_id).bits.addr := sram_row4
    io.metadata_read_reqs(coord_buffer_id).bits.tag.write := in4.write

    if (canWrite) {
      io.metadata_write_reqs(coord_buffer_id).valid := valid4 && in4.write && !waiting4 && !node_iterator_is_behind4 &&
        !cleanup_after_early_rf_ending4.ended_early &&
        Mux(in4.write_req.from_regfile(axisId), io.write_from_regfile_req.fire,
          in4.write_req.axis === axisId.U &&
          (!in4.write_req.is_data && in4.write_req.metadata_buffer_id === coord_buffer_id.U ||
            in4.write_req.is_both_data_and_metadata))
      io.metadata_write_reqs(coord_buffer_id).bits.addr := sram_row4
      io.metadata_write_reqs(coord_buffer_id).bits.data.zipWithIndex.foreach { case (d, i) =>
        when (i.U >= sram_col4) {
          val col = ((i.U +& span_iterator4 -& sram_col4) << in4.write_req.is_both_data_and_metadata).asUInt +& in4.write_req.is_both_data_and_metadata
          d := in4.write_req.data(col).asTypeOf(metadataElemT)
        }.otherwise {
          d := DontCare
        }
      }
      io.metadata_write_reqs(coord_buffer_id).bits.mask.zipWithIndex.foreach { case (m, i) =>
        m := i.U >= sram_col4 && i.U < sram_col4 +& span_step_size4
      }
    }

    assert(!io.metadata_write_reqs(coord_buffer_id).valid || io.metadata_write_reqs(coord_buffer_id).bits.addr === sram_row4.asUInt, "overflow in addr when writing to coord buffer in linked-list sram")

    if (is_innermost) {
      assert(!in4.read_req.to_regfile || io.data_read_req.fire +& io.metadata_read_reqs(coord_buffer_id).fire =/= 1.U,
        "did not read from data SRAM and coords SRAM simultaneously when reading out to regfile")
      assert((!io.data_read_req.fire && !io.data_write_req.fire && !io.metadata_read_reqs(coord_buffer_id).fire && !io.metadata_write_reqs(coord_buffer_id).fire) || span_stepping4, "unnecessary or incorrect sram accesses are being made")
    }

    def should_read_next_ptr(valid: Bool, _write: Bool, next_node_id_available: Bool, requested_next_node: Bool, next_node_is_null: Bool, output_running_len: Bool, _should_trail: Bool, _span_iterator: UInt, _traversed: Vec[UInt], branch_id: UInt, last_node: Bool, node_stepping: Bool = false.B, switching_branches: Bool = false.B, switching_to_same_branch: Bool = false.B): Bool = {
      val other_stage_writer_wrote_next_node = _write || !_should_trail || !io.other_stage_span_traversed(branch_id).valid || {
        val traversed = WireInit(_traversed)
        traversed(axisId) := _span_iterator

        val writer_is_on_next_node = vecEqualsU(io.other_stage_span_traversed(branch_id).bits.drop(axisId+1), traversed.drop(axisId+1)) &&
          traversed(axisId) / node_size.U +& node_stepping === io.other_stage_span_traversed(branch_id).bits(axisId) / node_size.U

        !writer_is_on_next_node || io.other_stage_next_node_added(branch_id)
      }

      // Note: The third line of this boolean conditions can be ignored for stage3 reads of the next-ptr buffer
      valid && !last_node && other_stage_writer_wrote_next_node && (
        !next_node_id_available && !requested_next_node && !next_node_is_null && !output_running_len ||
        node_stepping && !next_node_is_null4 && !(switching_branches && !switching_to_same_branch))
    }
    val use_addr_for_span_traversed4 = useAddrForSpanTraversed.B && !isWrite(in4) && in4.read_req.adjacent
    val stage4_reading_next_ptr = should_read_next_ptr(valid4, isWrite(in4), next_node_id_available4, requested_next_node4, next_node_is_null4, output_running_len4, should_trail4, span_iterator4, Mux(use_addr_for_span_traversed4, VecInit(in4.read_req.address.map(_.asTypeOf(in4.span_traversed.head))), in4.span_traversed), branch_id_of(in4, true), last_node4, node_stepping4, switching_branches4, switching_to_same_branch4)
    when (stage4_reading_next_ptr) {
      io.metadata_read_reqs(next_ptr_buffer_id).valid := true.B
      io.metadata_read_reqs(next_ptr_buffer_id).bits.addr := Mux(node_stepping4, next_node_id_wire4, node_id4)
      io.metadata_read_reqs(next_ptr_buffer_id).bits.tag.write := in4.write
    }

    val next_ptr_buffer_req_fired4 = stage4_reading_next_ptr && (if (no_simultaneous_reads_and_writes) {
      assert(!io.metadata_read_reqs(next_ptr_buffer_id).valid || io.metadata_read_reqs(next_ptr_buffer_id).ready)
      io.metadata_read_reqs(next_ptr_buffer_id).valid
    } else {
      io.metadata_read_reqs(next_ptr_buffer_id).fire
    })
    when (next_ptr_buffer_req_fired4) {
      next_step_requested_next_node4 := true.B
      requested_next_node4 := next_step_requested_next_node4
    }

    io.metadata_read_resps(next_ptr_buffer_id).ready := true.B
    val next_step_next_node_id4 = WireInit(next_node_id4)
    val next_step_reached_null_ptr4 = WireInit(reached_null_ptr4)
    when (io.metadata_read_resps(next_ptr_buffer_id).fire) {
      next_step_next_node_id4 := next_node_resp4
      next_node_id4 := next_step_next_node_id4

      when (is_null_ptr(next_node_resp4)) {
        next_step_reached_null_ptr4 := true.B
        reached_null_ptr4 := next_step_reached_null_ptr4
      }.otherwise {
        next_step_next_node_id_valid4 := true.B
        next_node_id_valid4 := next_step_next_node_id_valid4
      }
    }

    stage4_writing_to_next_ptr_buffer := valid4 && isWrite(in4) && !empty_write4 && !last_node4 && !not_all_rf_outs_found &&
      (in4.write_req.from_regfile(axisId) || in4.write_req.axis === axisId.U) &&
      (!in4.write_req.from_regfile(axisId) || io.write_from_regfile_req.ready && last_step_in_node4) &&
      (next_node_is_null4 || !next_node_id_available4 && requested_next_node4) &&
      !added_new_node_after_this_node4 &&
      !cleanup_after_early_rf_ending4.ended_early

    if (canWrite) {
      when (stage4_writing_to_next_ptr_buffer) {
        io.metadata_write_reqs(next_ptr_buffer_id).valid := true.B
        io.metadata_write_reqs(next_ptr_buffer_id).bits.mask.foreach(_ := true.B)

        // val must_cleanup = not_all_rf_outs_found || cleanup_after_early_rf_ending4.ended_early && !cleanup_after_early_rf_ending4.wrote_next_null_ptr
        assert(!not_all_rf_outs_captured && !cleanup_after_early_rf_ending4.ended_early, "we no longer write next-null ptrs after finishing early")

        io.metadata_write_reqs(next_ptr_buffer_id).bits.data.head := Mux(last_node4 /* || must_cleanup */,
          null_ptr, next_free_node_wire)

        assert(stage4_accessing_next_ptr_buffer_addr, "fired a mem write request without setting the address correctly")
      }

      stage4_accessing_next_ptr_buffer_addr := valid4 && in4.write && !empty_write4 && (in4.write_req.from_regfile(axisId) || in4.write_req.axis === axisId.U)
      when (stage4_accessing_next_ptr_buffer_addr) {
        io.metadata_write_reqs(next_ptr_buffer_id).bits.addr := node_id4
      }
    } else {
      stage4_accessing_next_ptr_buffer_addr := false.B
    }

    assert(!(stage4_writing_to_next_ptr_buffer && stage4_reading_next_ptr), "why would be we be reading and writing the next-ptr buffer at the same time?")

    val writing_null_ptr4 = is_null_ptr(io.metadata_write_reqs(next_ptr_buffer_id).bits.data.head)
    stage4_accessing_free_pointer := stage4_writing_to_next_ptr_buffer // TODO we can make this condition more specific, because we often don't need to access the free pointer if we're only writing a null-ptr as the next-node

    val next_step_added_new_nodes4 = WireInit(added_new_nodes4)
    when (stage4_writing_to_next_ptr_buffer && io.metadata_write_reqs(next_ptr_buffer_id).fire) {
      when (valid4 && !writing_null_ptr4) {
        io.incrementing_shared_state(next_free_node_shared_state_id) := true.B

        next_step_added_new_nodes4 := true.B
        next_step_added_new_node_after_this_node4 := !node_stepping4

        added_new_nodes4 := next_step_added_new_nodes4
        added_new_node_after_this_node4 := next_step_added_new_node_after_this_node4
      }

      assert(!cleanup_after_early_rf_ending4.ended_early && !not_all_rf_outs_captured && (!in4.write_req.from_regfile(axisId) || last_step_in_node4), "we don't cleanup the next-ptr writes anymore")

      when (!node_stepping4) {
        next_step_next_node_id4 := io.metadata_write_reqs(next_ptr_buffer_id).bits.data.head
        next_step_next_node_id_valid4 := true.B

        next_node_id4 := next_step_next_node_id4
        next_node_id_valid4 := next_step_next_node_id_valid4
      }
    }

    if (canWrite) {
      stage4_writing_to_last_len_buffer := valid4 && isWrite(in4) && !empty_write4 && (in4.write_req.from_regfile(axisId) || in4.write_req.axis === axisId.U) &&
        next_node_is_null4 && (
          last_node4 && !wrote_last_node_len4 ||
          not_all_rf_outs_found && !get_waiting_rf_version4(waiting_to_write_last_node_len_rf_version4) ||
          cleanup_after_early_rf_ending4.ended_early && !cleanup_after_early_rf_ending4.wrote_last_node_len)
      when (stage4_writing_to_last_len_buffer) {
        val must_cleanup = not_all_rf_outs_found || cleanup_after_early_rf_ending4.ended_early && !cleanup_after_early_rf_ending4.wrote_last_node_len

        val total_len = Mux(must_cleanup, iterator4 +& Mux(cleanup_after_early_rf_ending4.ended_early, 0.U, io.found_rf_outs_num), final_iterator4)

        io.metadata_write_reqs(last_node_len_buffer_id).valid := true.B
        io.metadata_write_reqs(last_node_len_buffer_id).bits.addr := head_ptr_addr4
        io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head := maxOf(total_len, last_node_len4)

        assert(stage4_accessing_last_len_buffer_addr, "fired a mem write request without setting the address correctly")
      }

      stage4_accessing_last_len_buffer_addr := valid4 && in4.write && !empty_write4 && (in4.write_req.from_regfile(axisId) || in4.write_req.axis === axisId.U)
      when (stage4_accessing_next_ptr_buffer_addr) {
        io.metadata_write_reqs(last_node_len_buffer_id).bits.addr := head_ptr_addr4
      }
    } else {
      stage4_writing_to_last_len_buffer := false.B
      stage4_accessing_last_len_buffer_addr := false.B
    }

    val next_step_wrote_last_node_len4 = WireInit(wrote_last_node_len4)
    when (stage4_writing_to_last_len_buffer && io.metadata_write_reqs(last_node_len_buffer_id).fire) {
      next_step_wrote_last_node_len4 := true.B

      wrote_last_node_len4 := next_step_wrote_last_node_len4
      when (not_all_rf_outs_found || cleanup_after_early_rf_ending4.ended_early) {
        next_step_cleanup_after_early_rf_ending4.wrote_last_node_len := true.B
        cleanup_after_early_rf_ending4.wrote_last_node_len := next_step_cleanup_after_early_rf_ending4.wrote_last_node_len
      }
    }

    val (data_addr4, metadata_addrs4) = updatedAddrs(in4, span_iterator4 +& bias4, inner_addr4)

    val next_step_read_col_counter4 = WireInit(read_col_counter4)
    when (switching_branches4) {
      val branch_id = branch_id4 % nBranches.U

      when (!(stage3_writing_first_branch && branch_id === 0.U)) {
        branches3.zipWithIndex.foreach { case (branch, bid) =>
          when (branch_id === bid.U) {
            branch.in := in4
            branch.in.axis_spans := next_step_axis_spans

            branch.node_iterator := next_step_node_iterator4
            branch.span_iterator := next_step_span_iterator4
            branch.read_col_counter := next_step_read_col_counter4

            branch.output_running_len := output_running_len4

            branch.head_ptr_addr := head_ptr_addr4
            branch.last_node_len := last_node_len4

            branch.node_id := next_step_node_id4
            branch.next_node_id := next_step_next_node_id4
            branch.next_node_id_valid := next_step_next_node_id_valid4
            branch.requested_next_node := next_step_requested_next_node4
            branch.reached_null_ptr := next_step_reached_null_ptr4
            branch.added_new_nodes := next_step_added_new_nodes4
            branch.added_new_node_after_this_node := next_step_added_new_node_after_this_node4
            branch.wrote_last_node_len := next_step_wrote_last_node_len4

            branch.cleanup_after_early_rf_ending := next_step_cleanup_after_early_rf_ending4
          }
        }

        branch_valids3(branch_id) := !ending4
      }

      val switching_to_last_remaining_branch = ending4 && last_branch_received3 && PopCount(branch_valids3) === 2.U
      when (switching_to_last_remaining_branch) {
        if (couldBeBranched) is_branched4 := false.B

        when (!stage3_writing_first_branch) {
          branch_valids3.head := false.B
        }
        branch_valids3.tail.foreach(_ := false.B)

        stage4_clearing_all_branch_valids := true.B
      }
    }

    when (fire4) {
      valid4 := true.B

      when (!stage4_should_not_overwrite_branch_to_send3) {
        branch_to_send3 := Mux(is_branched3, incremented_branch3, 0.U)
      }

      val branch_id = branch_to_send3 % nBranches.U
      branch_id4 := branch_id

      branches3.zipWithIndex.foreach { case (branch, bid) =>
        when (branch_id === bid.U && !switching_to_same_branch4) {
          in4 := branch.in
          in4.last_rf_access := last_rf_access3 && last_branch_received3 &&
            all(branch_valids3.zipWithIndex.patch(bid, Nil, 1).map { case (v, i) => !v || branch_id4 === i.U && ending4 && is_branched4 })

          node_iterator4 := branch.node_iterator
          span_iterator4 := branch.span_iterator
          if (elemsPerRead > 1) read_col_counter4 := branch.read_col_counter

          output_running_len4 := branch.output_running_len

          head_ptr_addr4 := branch.head_ptr_addr
          last_node_len4 := branch.last_node_len

          node_id4 := branch.node_id
          next_node_id4 := branch.next_node_id
          next_node_id_valid4 := branch.next_node_id_valid
          requested_next_node4 := branch.requested_next_node
          reached_null_ptr4 := branch.reached_null_ptr
          added_new_nodes4 := branch.added_new_nodes
          added_new_node_after_this_node4 := branch.added_new_node_after_this_node
          wrote_last_node_len4 := branch.wrote_last_node_len

          cleanup_after_early_rf_ending4 := branch.cleanup_after_early_rf_ending

          val node_id3 = WireInit(branch.node_id)
          if (canWrite) {
            val iterator3 = branch.span_iterator +& branch.in.write_req.address(axisId) +& branch.in.metadata_addrs(axisId)(LinkedListMetadata.coord_buffer_id)
            val node_iterator3 = iterator3 / node_size.U

            when (valid4 && isWrite(branch.in) && isWrite(in4) && !branch.in.write_req.from_regfile(axisId) && !in4.write_req.from_regfile(axisId) &&
              branch.head_ptr_addr === head_ptr_addr4 && node_iterator4 <= node_iterator3)
            {
              node_iterator4 := node_iterator4
              node_id4 := node_id4
              node_id3 := node_id4
            }
          }

          val last_node_len = WireInit(branch.last_node_len)
          if (canWrite) {
            when (stage4_writing_last_node_len_buffer_for(branch.head_ptr_addr, branch.in)) {
              last_node_len4 := io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head
              last_node_len := io.metadata_write_reqs(last_node_len_buffer_id).bits.data.head
            }
          }

          // We try to read the next-ptr a little early here, if possible, in order to save a cycle
          when (!stage4_reading_next_ptr) {
            val use_addr_for_span_traversed3 = useAddrForSpanTraversed.B && !isWrite(branch.in) && branch.in.read_req.adjacent
            val last_node = branch.node_iterator >= last_node_len / node_size.U - (last_node_len =/= 0.U && last_node_len % node_size.U === 0.U)
            io.metadata_read_reqs(next_ptr_buffer_id).valid := should_read_next_ptr(true.B, branch.in.write, branch.next_node_id_valid, branch.requested_next_node, branch.reached_null_ptr, branch.output_running_len, muxOnWrite(branch.in, branch.in.write_req.should_trail_reads, branch.in.read_req.should_trail_writes), branch.span_iterator, Mux(use_addr_for_span_traversed3, VecInit(branch.in.read_req.address.map(_.asTypeOf(branch.in.span_traversed.head))), branch.in.span_traversed), branch_id_of(branch.in, true), last_node)
            io.metadata_read_reqs(next_ptr_buffer_id).bits.addr := node_id3
            io.metadata_read_reqs(next_ptr_buffer_id).bits.tag.write := isWrite(branch.in)

            val next_ptr_buffer_req_fired = if (no_simultaneous_reads_and_writes) {
              assert(!io.metadata_read_reqs(next_ptr_buffer_id).valid || io.metadata_read_reqs(next_ptr_buffer_id).ready)
              io.metadata_read_reqs(next_ptr_buffer_id).valid
            } else {
              io.metadata_read_reqs(next_ptr_buffer_id).fire
            }
            when (next_ptr_buffer_req_fired) {
              requested_next_node4 := true.B
            }
          }
        }
      }
    }.elsewhen (any(ending4 +: Option.when(couldBeBranched)(switching_branches4 && !switching_to_same_branch4).toSeq)) {
      valid4 := false.B
    }

    when (!valid4 && !any(branch_valids3.tail)) {
      in4.op_count := branches3.head.in.op_count
    }

    Seq(last_node_len_buffer_id, next_ptr_buffer_id).foreach { buffer_id =>
      val write = io.metadata_write_reqs(buffer_id)
      val read = io.metadata_read_reqs(buffer_id)
      when (write.fire && read.fire) {
        assert(write.bits.addr =/= read.bits.addr, s"reading and writing to same address in buffer $buffer_id simultaneously")
      }
    }

    // Stage 5
    val valid5 = RegInit(false.B)
    val fire6 = Wire(Bool())
    val mem_reqs_fired5 = if (no_simultaneous_reads_and_writes) {
      (io.data_read_req.valid || io.metadata_read_reqs(coord_buffer_id).valid) && ready5
    } else {
      io.data_read_req.fire || io.metadata_read_reqs(coord_buffer_id).fire
    }
    val fire5 = mem_reqs_fired5 || valid4 && (
      !in4.write && (ending4 || (!is_innermost.B && span_stepping4)) ||
      in4.write && !in4.write_req.from_regfile(axisId) && axisId4 < axisId.U && span_stepping4 && ready5)

    ready5 := !valid5 || fire6

    // dontTouch(valid5)

    val in5 = RegEnable(in4, fire5)
    val last_read5 = RegEnable(last_read4, fire5)
    val read_col_counter5 = RegEnable(read_col_counter4, fire5)
    val sram_col5 = RegEnable(sram_col4, fire5)
    val starting5 = RegEnable(starting4, fire5)
    val ending5 = RegEnable(ending4, fire5)
    val is_last_branch5 = RegEnable(is_last_branch4, fire5)
    val empty5 = RegEnable(!span_stepping4, fire5)
    val skip5 = RegEnable(skip_axis4 || skip_data_and_coord_accesses4, fire5)
    val zero_span5 = RegEnable(span_step_size4 === 0.U, fire5)
    val axisId5 = RegEnable(axisId4, fire5)
    val running_len5 = RegEnable(if (outputs_running_len_from_buffer) last_node_len4 else running_len4, fire5)
    val total_running_len5 = RegEnable(total_running_len4, fire5)
    val output_running_len5 = RegEnable(output_running_len4, fire5)
    val branch_id5 = RegEnable(branch_id4, fire5)
    val span_step_size5 = RegEnable(span_step_size4, fire5)

    when (fire5) {
      val in5_will_be_valid = !too_many_elems_in_rf4 || ending4; assert(!too_many_elems_in_rf4 || ending4 || !mem_reqs_fired4)

      valid5 := in5_will_be_valid
      assert(ready5, "overwriting data in stage 4")

      val span_step_size5 = Mux(span_stepping4, span_step_size4, 0.U)
      if (elemsPerRead > 1) {
        val read_col_counter_step_size = span_step_size5 << (!isWrite(in4) && in4.read_req.should_read_data && in4.read_req.should_read_metadata)
        next_step_read_col_counter4 := Mux(ending4 || axisId4 =/= axisId.U, 0.U, (read_col_counter4 + read_col_counter_step_size)(read_col_counter4.getWidth-1, 0))
        read_col_counter4 := next_step_read_col_counter4
        assert(isPow2(elemsPerRead) && read_col_counter4.getWidth == log2Ceil(elemsPerRead), "we require isPow2 to be a power-of-2 so that we can guarantee read_col_counter wraps around to 0 with just a simple addition")
      }
      last_read4 := next_step_read_col_counter4 === 0.U || ending4

      if (resetRunningLenAutomatically)
        when (output_running_len4) {
          running_len4 := 0.U
        }

      in5.data_addr := data_addr4
      connectVecOfVecs(in5.metadata_addrs, metadata_addrs4)

      in5.read_req.spans(axisId) := span_step_size4 +& read_col_counter4
      in5.read_req.address(axisId) := muxOnWrite(in4, in4.write_req.address(axisId), in4.read_req.address(axisId)) +&
        span_iterator4 -& read_col_counter4

      in5.span_traversed(axisId) := span_iterator4

      in5.first_rf_access := first4
      in5.last_rf_access := last4

      when (!in4.write) {
        in5.axis_spans(axisId).valid := valid4
        in5.axis_spans(axisId).bits := span4
      }.elsewhen (next_step_axis_spans(axisId).valid) {
        in5.axis_spans(axisId).valid := valid4
        in5.axis_spans(axisId).bits := next_step_axis_spans(axisId).bits
      }
    }.elsewhen(fire6) {
      valid5 := false.B
    }

    when (fire5 || !valid5) {
      in5.op_count := Mux(last_axis4 === axisId.U, opCount, in4.op_count)
    }

    // Stage 6
    val valid6 = RegInit(false.B)
    val in6 = RegEnable(in5, fire6)
    val last_read6 = RegEnable(last_read5, fire6)
    val read_data6 = Reg(getChiselType(io.out.bits.read_data))
    val read_metadata6 = Reg(getChiselType(io.out.bits.read_data)); assert(metadataElemT.getWidth == read_data6.head.getWidth)
    val starting6 = RegEnable(starting5, fire6)
    val ending6 = RegEnable(ending5, fire6)
    val is_last_branch6 = RegEnable(is_last_branch5, fire6)
    val empty6 = RegEnable(empty5, fire6)
    val skip6 = RegEnable(skip5, fire6)
    val axisId6 = RegEnable(axisId5, fire6)
    val running_len6 = RegEnable(running_len5, fire6)
    val total_running_len6 = RegEnable(total_running_len5, fire6)
    val output_running_len6 = RegEnable(output_running_len5, fire6)
    val branch_id6 = RegEnable(branch_id5, fire6)

    fire6 := io.data_read_resp.fire || io.metadata_read_resps(coord_buffer_id).fire ||
      valid5 && (empty5 || skip5 || zero_span5 || output_running_len5) && (!valid6 || io.out.fire) // TODO should the last clause be '(!valid6 || !io.out.valid || io.out.ready)'?
    when (fire6) {
      valid6 := true.B
      assert(io.out.ready || !io.out.valid || !valid6, "overwriting valid data in LinkedList SRAM's final pipeline stage")

      for ((reg, read_port) <- Seq((read_data6, io.data_read_resp.bits.data), (read_metadata6, io.metadata_read_resps(coord_buffer_id).bits.data))) {
        reg.zipWithIndex.foreach { case (d,i) =>
          val rport = read_port.asTypeOf(Vec(read_port.size, getChiselType(reg.head)))

          when (axisId5 === axisId.U) {
            when (i.U >= read_col_counter5) {
              d := rport(sram_col5 +& i.U -& read_col_counter5)
            }
          }.otherwise {
            d := rport(sram_col5)

            if (!is_innermost) {
              assert(!io.metadata_read_resps(coord_buffer_id).valid || read_col_counter5 === 0.U)
              assert(!io.metadata_read_resps(coord_buffer_id).valid || all(in5.read_req.spans.drop(axisId+1).map(_ === 1.U)),
                "if the outer-dimensions don't have a span of 1, then it might be incorrect to simply set all the metadata addrs simultaneously as we do in this when-clause")
            }
          }
        }
      }
    }.elsewhen(io.out.fire) {
      valid6 := false.B
    }

    val read_data_and_metadata6 = VecInit((read_data6.take(read_data6.size/2) zip read_metadata6.take(read_metadata6.size/2)).flatten {case (a,b) => List(a,b)})

    when (!valid6) {
      in6.op_count := in5.op_count
    }

    io.data_read_resp.ready := !valid6 || !last_read6 || io.out.fire
    io.metadata_read_resps(coord_buffer_id).ready := !valid6 || !last_read6 || io.out.fire

    io.out.valid := valid6 && (last_read6 || skip6)
    io.out.bits := in6
    io.out.bits.read_data := Mux(in6.read_req.axis =/= axisId.U, in6.read_data,
      Mux(in6.read_req.should_read_data && in6.read_req.should_read_metadata, read_data_and_metadata6,
        Mux(in6.read_req.should_read_data || in6.read_req.to_regfile, read_data6, read_metadata6)))
    io.out.bits.expanded_addrs.zip(read_metadata6).zipWithIndex.foreach { case ((ea, md), i) =>
      val compressedAddr = io.out.bits.read_req.address(axisId) +& Mux(axisId6 === axisId.U, i.U, 0.U)
      ea(axisId) := (coordParams match {
        case CompressedMetadata.DontExpandInner(_) => compressedAddr
        case CompressedMetadata.InnerBuffer(_, false) => Mux(!in6.write && in6.read_req.to_regfile, compressedAddr, md.asUInt)
        case CompressedMetadata.InnerBuffer(_, true) => md.asUInt
      })
    }
    io.out.bits.read_req.spans(axisId) := Mux(empty6, 0.U, in6.read_req.spans(axisId)) << (in6.read_req.should_read_data && in6.read_req.should_read_metadata)
    io.out.bits.write_req.spans(axisId) := io.out.bits.read_req.spans(axisId)
    io.out.bits.write_req.address(axisId) := io.out.bits.read_req.address(axisId)
    when (io.out.bits.read_req.spans(axisId) === 0.U) {
      io.out.bits.read_req.spans.take(axisId).foreach(_ := 0.U)
      io.out.bits.write_req.spans.take(axisId).foreach(_ := 0.U)
    }
    io.out.bits.first_it := in6.first_it && starting6
    io.out.bits.first_it_in_axis := starting6
    io.out.bits.last_it := in6.last_it && ending6 && is_last_branch6
    io.out.bits.last_it_in_axis := ending6
    io.out.bits.total_running_len := total_running_len6
    io.interleave_q_push := valid6 && in6.push_to_interleave_q && ending6

    when (valid6 && output_running_len6) {
      // Output the running-len here (for LL -> CSR conversions)
      io.out.valid := true.B
      io.out.bits := in6
      io.out.bits.read_data.head := running_len6.asTypeOf(io.out.bits.read_data.head)
      io.out.bits.read_req.spans.foreach(_ := 1.U)
      io.out.bits.first_rf_access := true.B
      io.out.bits.last_rf_access := true.B
      io.out.bits.first_it_in_axis := true.B
      io.out.bits.last_it := in6.last_it
      io.out.bits.last_it_in_axis := true.B; assert(is_last_branch6, "we should adjust this line if we're not actually on the last branch")
      io.interleave_q_push := in6.push_to_interleave_q && ending6
    }.elsewhen (!valid6 && !valid5 && valid4 && in4.write && in4.write_req.from_regfile(axisId)) {
      // Input data from reg-files here. This happens in pipeline stage 3; we're only writing it here because of
      // Chisel's last-connect semantics
      if (!is_innermost) {
        io.out.valid := io.write_from_regfile_req.fire; if (!is_innermost) assert(!io.out.valid || io.out.ready, "next stage missed this output")
        io.out.bits := in4
        io.out.bits.write_req := io.write_from_regfile_req.bits.req
        io.out.bits.data_addr := data_addr4
        connectVecOfVecs(io.out.bits.metadata_addrs, metadata_addrs4)
        io.out.bits.span_traversed(axisId) := span_iterator4
        io.out.bits.first_it := in4.first_it && io.out.bits.first_it_in_axis
        io.out.bits.first_it_in_axis := span_iterator4 === 0.U
        io.out.bits.last_it := in4.last_it && io.out.bits.last_it_in_axis && is_last_branch4
        io.out.bits.last_it_in_axis := ending4 && !cleanup_after_early_rf_ending4.ended_early || not_all_rf_outs_found
        io.out.bits.op_count := Mux(last_axis4 === axisId.U, opCount, in4.op_count)

        when (not_all_rf_outs_found) {
          io.out.bits.write_req.spans(axisId) := io.found_rf_outs_num
          when (io.found_rf_outs_num === 0.U) {
            io.out.bits.write_req.spans.take(axisId).foreach(_ := 0.U)
          }
        }
      }
      io.out.bits.first_rf_access := first4
      io.out.bits.last_rf_access := last4
      io.interleave_q_push := in4.push_to_interleave_q && ending4
    }
    assert(!(valid4 && in4.write && in4.write_req.from_regfile(axisId)) || (!valid6 && !valid5), "we don't yet stall stage-4 writes from regfiles when the other pipeline stages are busy")

    if (spanTraversedShouldBeSet) {
      io.span_traversed.foreach { io_span_traversed =>
        io_span_traversed.valid := false.B
        io_span_traversed.bits := DontCare
      }

      io.last_it_in_axis.foreach(_ := DontCare)

      io.span_traversed(branch_id_of(in, true)).valid := in_valid
      io.span_traversed(branch_id_of(in, true)).bits := in.span_traversed
      io.span_traversed(branch_id_of(in, true)).bits(axisId) := 0.U

      io.last_it_in_axis(branch_id_of(in, true)) := in.last_it_in_each_axis

      when (valid2) {
        io.span_traversed(branch_id_of(in2, true)).valid := true.B
        io.span_traversed(branch_id_of(in2, true)).bits := in2.span_traversed
        io.span_traversed(branch_id_of(in2, true)).bits(axisId) := 0.U

        io.last_it_in_axis(branch_id_of(in2, true)) := in2.last_it_in_each_axis
      }

      when (branch_valids3.head) {
        io.span_traversed(branch_id_of(branches3.head.in, true)).valid := true.B
        io.span_traversed(branch_id_of(branches3.head.in, true)).bits := branches3.head.in.span_traversed
        io.span_traversed(branch_id_of(branches3.head.in, true)).bits(axisId) := branches3.head.span_iterator

        io.last_it_in_axis(branch_id_of(branches3.head.in, true)) := branches3.head.in.last_it_in_each_axis
      }

      io.span_traversed.zip(branches3).zip(branch_valids3).zip(io.last_it_in_axis).tail.foreach { case (((io_span_traversed, branch), branch_valid), last_in_axis) =>
        when (branch_valid) {
          io_span_traversed.valid := true.B
          io_span_traversed.bits := branch.in.span_traversed
          io_span_traversed.bits(axisId) := branch.span_iterator

          last_in_axis := branch.in.last_it_in_each_axis
        }
      }

      when (valid4) {
        io.span_traversed(branch_id_of(in4, true)).valid := true.B
        io.span_traversed(branch_id_of(in4, true)).bits := in4.span_traversed
        io.span_traversed(branch_id_of(in4, true)).bits(axisId) := span_iterator4

        io.last_it_in_axis(branch_id_of(in4, true)) := in4.last_it_in_each_axis
        io.last_it_in_axis(branch_id_of(in4, true))(axisId) := last_step_in_span4
      }
    }

    io.axis_spans.foreach(_.foreach(_.valid := false.B))
    io.axis_spans.foreach(_.foreach(_.valid := DontCare))

    io.axis_spans.zip(branches3).zip(branch_valids3).tail.foreach { case ((io_axis_spans, branch), branch_valid) =>
      connectVecs(io_axis_spans.map(_.valid), branch.in.axis_spans.map(_.valid && branch_valid))
      connectVecs(io_axis_spans.map(_.bits), branch.in.axis_spans.map(_.bits))
      io_axis_spans(axisId).valid := branch.in.axis_spans(axisId).valid && branch_valid
      io_axis_spans(axisId).bits := branch.in.axis_spans(axisId).bits
    }

    when (branch_valids3.head) {
      val in3 = branches3.head.in
      connectVecs(io.axis_spans(branch_id_of(in3, true)).map(_.valid), in3.axis_spans.map(_.valid))
      connectVecs(io.axis_spans(branch_id_of(in3, true)).map(_.bits), in3.axis_spans.map(_.bits))
      io.axis_spans(branch_id_of(in3, true))(axisId).valid := in3.axis_spans(axisId).valid
      io.axis_spans(branch_id_of(in3, true))(axisId).bits := in3.axis_spans(axisId).bits
    }

    when (valid4) {
      connectVecs(io.axis_spans(branch_id_of(in4)).map(_.valid), in4.axis_spans.map(_.valid))
      connectVecs(io.axis_spans(branch_id_of(in4)).map(_.bits), in4.axis_spans.map(_.bits))
      when (!in4.write) {
        io.axis_spans(branch_id_of(in4))(axisId).valid := true.B
        io.axis_spans(branch_id_of(in4))(axisId).bits := span4
      }.elsewhen (next_step_axis_spans(axisId).valid) {
        io.axis_spans(branch_id_of(in4))(axisId).valid := true.B
        io.axis_spans(branch_id_of(in4))(axisId).bits := next_step_axis_spans(axisId).bits
      }
    }

    // This line below would ideally be in Stage 1 (which is where it really happens), but we had to put it down here
    // to avoid forward definitions
    wait_for_flush := !(canWrite.B && !in.write_req.from_regfile(axisId)) && ((valid2 && in2.write && head_ptr_addr === head_ptr_addr2) ||
      any(branches3.zip(branch_valids3).map { case (branch, branch_valid) => branch_valid && branch.in.write && head_ptr_addr === branch.head_ptr_addr }) ||
      (valid4 && in4.write && head_ptr_addr === head_ptr_addr4)) ||
      (valid6 || valid5 || valid4 || any(branch_valids3) || valid2) && reset_running_state

    can_read_len_from_later_stage := (if (canWrite) {
      !in.write_req.from_regfile(axisId) && (
        valid2 && isWrite(in2) && head_ptr_addr === head_ptr_addr2 ||
        any(branches3.zip(branch_valids3).map { case (branch, branch_valid) => branch_valid && branch.in.write && head_ptr_addr === branch.head_ptr_addr }) ||
        valid4 && isWrite(in4) && head_ptr_addr === head_ptr_addr4)
      } else {
        false.B
      })

    // This line below would ideally be in Stage 3 (which is where it really happens), but we had to put it down here
    // to avoid forward definitions
    waiting_for_flush4 := in4.write && in4.write_req.from_regfile(axisId) && (valid5 || valid6)

    // This line below would ideally be in Stage 1 (which is where it really happens), but we had to put it down here
    // to avoid problems with Chisel's last-connect semantics
    when (in_valid && !waiting && reset_running_state) {
      running_len4 := 0.U
      io.resetting_shared_state(next_free_node_shared_state_id) := true.B
      io.resetting_shared_state(max_head_shared_state_id) := true.B
    }

    // The 'too_many_elems_in_rf4' value is actually calculated in stage-4, but we have to put it here to avoid forward
    //   references.
    too_many_elems_in_rf4 := (if (!canRead) false.B else (!isWrite(in4) && is_branched4 && (maxElemsInRf match {
      case Some(maxElems) =>
        def getSpan(span: UInt): UInt = if (is_innermost) span else maxOf(span, 1.U)
        val elems_in_stage5 = Mux(valid5 && branch_id5 === branch_id4, getSpan(span_step_size5), 0.U)
        val elems_in_stage6 = Mux(valid6 && branch_id6 === branch_id4, getSpan(in6.read_req.spans(axisId)), 0.U)
        val total_elems_in_rf = io.lookup_future_stages(branch_id4).nElems +& elems_in_stage5 +& elems_in_stage6
        val elems_to_add = if (is_innermost) elemsPerRead.U - read_col_counter4 else 1.U
        total_elems_in_rf +& elems_to_add > maxElems.U

      case None => false.B
    })))

    when (io.other_banks_shared_state_reset(next_free_node_shared_state_id)) {
      assert(!any(io.other_banks_shared_state_increment(next_free_node_shared_state_id).map(_.orR)), "not sure what to do if we're incrementing the shared state in the same cycle that we reset it")
      next_free_node_reg := next_free_node_reset_value.U
    }.otherwise {
      next_free_node_reg := updated_next_free_node(io.other_banks_shared_state_increment(next_free_node_shared_state_id))
    }

    when (io.other_banks_shared_state_reset(max_head_shared_state_id)) {
      assert(!any(io.other_banks_shared_state_increment(max_head_shared_state_id).map(_.orR)), "not sure what to do if we're incrementing the shared state in the same cycle that we reset it")
      max_head_initialized := 0.U
    }.otherwise {
      max_head_initialized := max_head_initialized +& sumvU(io.other_banks_shared_state_increment(max_head_shared_state_id))
    }

    io.busy := isBusy(in_valid, in) || isBusy(valid2, in2) ||
      any(branch_valids3.zip(branches3).map { case (valid3, branch3) => isBusy(valid3, branch3.in) }) ||
      isBusy(valid4, in4) || isBusy(valid5, in5) || isBusy(valid6, in6)

    io.full_busy := isBusy(in_valid, in, for_full=true) || isBusy(valid2, in2, for_full=true) ||
      any(branch_valids3.zip(branches3).map { case (valid3, branch3) => isBusy(valid3, branch3.in, for_full=true) }) ||
      isBusy(valid4, in4, for_full=true) || isBusy(valid5, in5, for_full=true) || isBusy(valid6, in6, for_full=true)

    (Seq(in, in2, in4, in5, in6) ++ branches3.map(_.in)).foreach { x =>
      hardCodeValues(x, Seq(in5, in6).flatMap(addrFieldsOfThisAxis))
      when (reset.asBool) {
        x.op_count := OpCount(0.U)
      }
    }
  } else if (axis == FiberTreeAxis.Bitvector) {
    /* Note: the "bitvector" axis has multiple pipeline stages:
        1) Make SRAM read requests to the bitmap buffer
            - All writes also happen in this stage
            - Note: unlike with the compressed or linked-list axes, in the bitvector axis, the read-span is the number
                    of _bits_ to read out, rather than the number of compressed _elements_ to read out. This makes the
                    logic a bit simpler, and in either case, is more likely to be what the programmer wants anyways.
        2) Buffer "in" while making read-request of prior stage
        3) Capture result of bitvector read and use it to calculate how many elements to read out. Make SRAM read reqs
        4) Buffer "in" while making read-request of prior stage
        5) Capture and align result of data read request
        6) Output final result
     */

    import BitvectorMetadata._
    val BitvectorMetadata(nBits: Int, bitsPerRow: Int) = metadataConf
    require(bitsPerRow / metadataElemT.getWidth == elemsPerRow, s"Currently, this code assumes that the number of words per row on the bitvector and data srams are the same. If we relax this assumption later, be sure to remember to update 'span_step_size' below | bitsPerRow = $bitsPerRow (${bitsPerRow/metadataElemT.getWidth}) | elemsPerRow = $elemsPerRow")

    assert(!(in_valid && isWrite(in) && in.write_req.from_regfile(axisId)), "currently, we only support writing bitmap axes from DRAM, not from regfiles")

    val valid2 = RegInit(false.B)
    val valid3 = RegInit(false.B)
    val valid4 = RegInit(false.B)
    val valid5 = RegInit(false.B)
    val valid6 = RegInit(false.B)

    val fire2 = Wire(Bool())
    val fire3 = Wire(Bool())
    val fire4 = Wire(Bool())
    val fire5 = Wire(Bool())

    val bias = muxOnWrite(in, in.write_req.address(axisId), in.read_req.address(axisId))

    val iterator = span_iterator +& bias

    val (data_addr, metadata_addrs) = updatedAddrs(in, iterator)

    val stride = muxOnWrite(in, in.write_req.iteration_strides(axisId), in.read_req.iteration_strides(axisId))
    span_step_size := stride

    val sram_row = data_addr >> log_elems_per_row
    val sram_col = data_addr & ((1 << log_elems_per_row)-1).U

    val read_col_counter = RegInit(0.U(log2Up(elemsPerRead).W))
    val last_read = WireInit(false.B)

    span_step_size := minOf(
      span - span_iterator,
      elemsPerRow.U - sram_col,
      muxOnWrite(in, elemsPerWrite.U, elemsPerRead.U - read_col_counter)
    )

    if (canWrite) {
      io.data_write_req.valid := in_valid && isWrite(in) && in.write_req.is_data && in.write_req.axis === axisId.U
      io.data_write_req.bits.addr := sram_row
      io.data_write_req.bits.data.zipWithIndex.foreach { case (d, i) =>
        when (i.U >= sram_col) {
          d := in.write_req.data((i.U -& sram_col) +& span_iterator)
        }.otherwise {
          d := DontCare
        }
      }
      io.data_write_req.bits.mask.zipWithIndex.foreach { case (m, i) =>
        m := i.U >= sram_col && i.U < sram_col +& span_step_size
      }

      io.metadata_write_reqs(bitvector_buffer_id).valid := in_valid && isWrite(in) && !in.write_req.is_data && in.write_req.axis === axisId.U && in.write_req.metadata_buffer_id === bitvector_buffer_id.U
      io.metadata_write_reqs(bitvector_buffer_id).bits.addr := sram_row
      io.metadata_write_reqs(bitvector_buffer_id).bits.data.zipWithIndex.foreach { case (d, i) =>
        when (i.U >= sram_col) {
          d := in.write_req.data((i.U -& sram_col) +& span_iterator).asTypeOf(d)
        }.otherwise {
          d := DontCare
        }
      }
      io.metadata_write_reqs(bitvector_buffer_id).bits.mask.zipWithIndex.foreach { case (m, i) =>
        m := i.U >= sram_col && i.U < sram_col +& span_step_size
      }
    }

    if (canRead) {
      io.metadata_read_reqs(bitvector_buffer_id).valid := in_valid && !isWrite(in)
      io.metadata_read_reqs(bitvector_buffer_id).bits.addr := sram_row
      io.metadata_read_reqs(bitvector_buffer_id).bits.tag.write := isWrite(in)
    }

    fire2 := io.metadata_read_reqs(bitvector_buffer_id).fire

    when (fire2) {
      val new_read_col_counter = Mux(ending || elemsPerRead.U === 1.U, 0.U, (read_col_counter + span_step_size)(read_col_counter.getWidth-1, 0))
      read_col_counter := new_read_col_counter
      last_read := new_read_col_counter === 0.U || ending
      assert(isPow2(elemsPerRead) && (new_read_col_counter.getWidth == log2Ceil(elemsPerRead) || elemsPerRead == 1), s"we require elemsPerRead ($elemsPerRead) to be a power-of-2 so that we can guarantee read_col_counter wraps around to 0 with just a simple addition")
    }

    val in2 = RegEnable(in, fire2)
    val sram_col2 = RegEnable(sram_col, fire2)
    val read_col_counter2 = RegEnable(read_col_counter, fire2)
    val span_step_size2 = RegEnable(span_step_size, fire2)
    val last_read2 = RegEnable(last_read, fire2)
    val starting2 = RegEnable(span_iterator === 0.U && stepping, fire2)
    val ending2 = RegEnable(ending, fire2)
    val iterator2 = RegEnable(iterator, fire2)

    when (fire2) {
      valid2 := true.B
    }.elsewhen (fire3) {
      valid2 := false.B
    }

    fire3 := io.metadata_read_resps(bitvector_buffer_id).valid

    val in3 = RegEnable(in2, fire3)
    val last_read3 = RegEnable(last_read2, fire3)
    val read_col_counter3 = RegEnable(read_col_counter2, fire3)
    val starting3 = RegEnable(starting2, fire3)
    val ending3 = RegEnable(ending2, fire3)
    val iterator3 = RegEnable(iterator2, fire3)

    val elem_iterator3 = RegInit(0.U(spanBits.W)) // TODO is there a way to get rid of the RegInit and just make this a Reg?
    val bitvector_iterator3 = RegInit(0.U(log2Up(bitsPerRow).W)) // TODO is there a way to get rid of the RegInit and just make this a Reg?

    val bitmap_resp3 = VecInit(dropFromVec(io.metadata_read_resps(bitvector_buffer_id).bits.data, sram_col2).zipWithIndex.map { case (word, i) => Mux(i.U < span_step_size2, word, 0.U) }).asUInt

    val remaining_bits3 = PopCount(bitmap_resp3 >> bitvector_iterator3)

    val elem_sram_row3 = elem_iterator3 >> log_elems_per_row
    val elem_sram_col3 = elem_iterator3 & ((1 << log_elems_per_row)-1).U

    val elem_step_size3 = minOf(
      remaining_bits3,
      elemsPerRow.U - elem_sram_col3,
    )

    if (canRead) {
      io.metadata_read_resps(bitvector_buffer_id).ready := remaining_bits3 - elem_step_size3 === 0.U

      when (io.metadata_read_resps(bitvector_buffer_id).valid) {
        io.data_read_req.valid := true.B
      }

      io.data_read_req.bits.addr := elem_sram_row3
      io.data_read_req.bits.tag.axisId := axisId.U

      when (io.data_read_req.fire) {
        elem_iterator3 := elem_iterator3 + elem_step_size3
        bitvector_iterator3 := bitvector_iterator3 + elem_step_size3

        when (fire3 && ending2) {
          elem_iterator3 := 0.U
          bitvector_iterator3 := 0.U
        }
      }
    }

    when (fire3) {
      valid3 := true.B
    }.elsewhen (fire4) {
      valid3 := false.B
    }

    fire4 := io.data_read_req.fire

    val in4 = RegEnable(in3, fire4)
    val starting4 = RegEnable(starting3, fire4)
    val ending4 = RegEnable(ending3, fire4)
    val read_col_counter4 = RegEnable(read_col_counter3, fire4)
    val elem_sram_col4 = RegEnable(elem_sram_col3, fire4)
    val last_read4 = RegEnable(last_read3, fire4)
    val remaining_bits4 = RegEnable(remaining_bits3, fire4)
    val elem_iterator4 = RegEnable(elem_iterator3, fire4)
    val iterator4 = RegEnable(iterator3, fire4)
    val elem_step_size4 = RegEnable(elem_step_size3, fire4)

    when (fire4) {
      valid4 := true.B
    }.elsewhen (fire5) {
      valid4 := false.B
    }

    val in5 = RegEnable(in4, fire5)
    val starting5 = RegEnable(starting4, fire5)
    val ending5 = RegEnable(ending4, fire5)
    val read_col_counter5 = RegEnable(read_col_counter4, fire5)
    val elem_sram_col5 = RegEnable(elem_sram_col4, fire5)
    val last_read5 = RegEnable(last_read4, fire5)
    val remaining_bits5 = RegEnable(remaining_bits4, fire5)
    val elem_iterator5 = RegEnable(elem_iterator4, fire5)
    val iterator5 = RegEnable(iterator4, fire5)
    val elem_step_size5 = RegEnable(elem_step_size4, fire5)

    fire5 := io.data_read_resp.fire
    io.data_read_resp.ready := !valid5 || io.out.ready || !last_read5

    val in5_data = Reg(getChiselType(io.out.bits.read_data))

    when (fire5) {
      valid5 := true.B
      in5_data.zipWithIndex.foreach { case (d, i) =>
        when (i.U >= read_col_counter4) {
          d := io.data_read_resp.bits.data(elem_sram_col4 +& i.U -& read_col_counter4)
        }
      }
    }.elsewhen(io.out.fire) {
      valid5 := false.B
    }

    io.out.valid := valid5 && last_read5
    io.out.bits := in5
    io.out.bits.read_data := Mux(in5.read_req.axis === axisId.U, in5_data, in5.read_data)
    io.out.bits.expanded_addrs.zipWithIndex.foreach { case (ea, i) =>
      ea(axisId) := iterator5 + i.U
    }
    io.out.bits.read_req.address(axisId) := elem_iterator5
    io.out.bits.first_it_in_axis := starting5
    io.out.bits.last_it := in5.last_it && ending5
    io.out.bits.last_it_in_axis := ending5
    io.out.bits.last_branch_in_each_axis(axisId) := DontCare; assert(nBranches == 1, "branching not yet supported for bitvector srams")

    io.interleave_q_push := valid5 && in5.push_to_interleave_q && ending5

    when (isBusy(in_valid, in) || isBusy(valid2, in2) || isBusy(valid3, in3) || isBusy(valid4, in4) || isBusy(valid5, in5)) {
      io.busy := true.B
    }
    when (isBusy(in_valid, in, for_full=true) || isBusy(valid2, in2, for_full=true) || isBusy(valid3, in3, for_full=true) || isBusy(valid4, in4, for_full=true) || isBusy(valid5, in5, for_full=true)) {
      io.full_busy := true.B
    }

    Seq(in3, in3, in4, in5).foreach { x =>
      hardCodeValues(x, addrFieldsOfThisAxis(x))
      when (reset.asBool) {
        x.op_count := OpCount(0.U)
      }
    }
  } else {
    assert(false)
  }

  io.out.bits.first_it_in_each_axis(axisId) := io.out.bits.first_it_in_axis
  io.out.bits.last_it_in_each_axis(axisId) := io.out.bits.last_it_in_axis

  // Assertions
  val stall_counter = RegInit(0.U(32.W))
  when (!io.out.valid || io.out.ready) {
    stall_counter := 0.U
  }.otherwise {
    stall_counter := stall_counter + 1.U
  }
  assert(stall_counter <= Math.pow(10, (axisId+4).min(6)).toInt.U, s"SRAM pipeline stage is stalling: axisId=$axisId bankId=$bankId canWrite=$canWrite canRead=$canRead")
}

class ChiselSRAM[T <: Data](elemT: T, nElems: Int, elemsPerRead: Int, elemsPerWrite: Int,
                            axes: Seq[FiberTreeAxis.Type], metadatas: Seq[FiberTreeAxisMetadata],
                            branchSizes: Seq[Int] = Seq.empty, readBranchSizes: Option[Seq[Int]] = None, writeBranchSizes: Option[Seq[Int]] = None,
                            val nBanks: Int = 1, readBankingStrategies: Seq[ReadBankingStrategy] = Seq.empty, writeBankingStrategies: Seq[WriteBankingStrategy[T]] = Seq.empty,
                            maxElemsInRf: Option[Int] = None, multipleNElemsLookupPorts: Boolean = false,
                            elemsPerRowMultiplier: Int = 1, bankMultiplier: Int = 1,
                            dont_prevent_simultaneous_accesses: Boolean = false,
                            no_simultaneous_reads_and_writes: Boolean = false,
                            pipeline_stages: Boolean = false,
                            hardCodedValues: ChiselSRAMPipelineData[T] => stellar.Util.SMap[Data, Data] = {_: ChiselSRAMPipelineData[T] => scala.collection.Map.empty[Data,Data]},
                            stridesDivisibleBy: SMap[Int, Int] = Seq.empty.toMap, // {axisId: divisibleBy}
                            elemsPerWriteRf: Int = -1, val independentBanks: Boolean = false,
                            dummyData: Boolean = false, dummyReadStages: Boolean = false,
                            nameOpt: Option[String] = None) extends Module {
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  val addrBits = log2Up(nElems)
  val spanBits = log2Up(nElems+1).max(32) // Spans for "loop-axes" may be larger than n-elems // TODO magic number
  val nAxes = axes.size
  val elemsPerRow = (1 << log2Ceil(lcm(elemsPerWrite, elemsPerRead))) * elemsPerRowMultiplier
  val nRows = nElems / elemsPerRow
  val readBranchSizes_ = readBranchSizes.getOrElse(branchSizes) ++ Seq.fill(nAxes - branchSizes.size)(1)
  val writeBranchSizes_ = writeBranchSizes.getOrElse(branchSizes) ++ Seq.fill(nAxes - branchSizes.size)(1)
  val branchSizes_ = branchSizes ++ Seq.fill(nAxes - branchSizes.size)(1)
  def nBranches = {
    assert(readBranchSizes == writeBranchSizes, "there is not a single unified nBranches")
    branchSizes_.max
  }

  require(nElems % elemsPerRow == 0)
  require(axes.size == metadatas.size)
  require(!no_simultaneous_reads_and_writes || nBanks == 1, "we don't yet support all the hardcoding optimizations that take advantage of no simultaneous reads and writes when banking is employed")
  require(elemsPerWriteRf == -1 || elemsPerWrite % elemsPerWriteRf == 0)

  def sram_row_t = Vec(elemsPerRow, getChiselType(elemT))
  def metadata_elem_t = UInt(elemT.getWidth.W) // TODO in most cases, the metadata does not need this many bits. this should also be unrelated to the data-width
  def data_sram_tag_t = new ChiselSRAMDataTag(nAxes)
  def metadata_sram_tag_t = new ChiselSRAMMetadataTag

  val mem = Module(new SyncMemWrapper(sram_row_t, nRows, data_sram_tag_t, prevent_simultaneous_access = !dont_prevent_simultaneous_accesses && axes.head==FiberTreeAxis.LinkedList, nBanks = nBanks * bankMultiplier, nPorts = nBanks, independentBanks = independentBanks, isDummy = dummyData))
  val metadata_mems = metadatas.zipWithIndex.map {
    case (CompressedMetadata(nOuter, innerParams, _, _, _), axisId) =>
      val nInnerOpt = innerParams match {
        case CompressedMetadata.InnerBuffer(nInnerOpt_, _) =>
          require(axisId == 0 || nInnerOpt_.nonEmpty)
          nInnerOpt_
        case CompressedMetadata.DontExpandInner(nInnerOpt_) => nInnerOpt_
      }
      val nInner = nInnerOpt.getOrElse(nElems)

      val innerIsDummy = innerParams match {
        case CompressedMetadata.DontExpandInner(_) => true
        case _ => false
      }

      val headPtrT = UInt(log2Up(nInner+1).W)
      def outer_metadata_buffer = Module(new SyncMemWrapper(Vec(1, headPtrT), nOuter, metadata_sram_tag_t, nPaths = 2, nBanks = nBanks * bankMultiplier, nPorts = nBanks))

      Seq(outer_metadata_buffer,
        {
          val nInnerRows = nInner / elemsPerRow + (if (nInner % elemsPerRow == 0) 0 else 1)
          Module(new SyncMemWrapper(Vec(elemsPerRow, metadata_elem_t), nInnerRows, metadata_sram_tag_t, nBanks = nBanks * bankMultiplier, nPorts = nBanks, isDummy = innerIsDummy))
        },
        outer_metadata_buffer
      )

    case (LinkedListMetadata(nHeads, nNodes, _, coordParams, _, _, _, _, _, _, _), axisId) =>
      val headPtrT = UInt((log2Up(nNodes+1)+1).min(32).W) // TODO The "+1"s are there to make sure we support null-ptrs, but I'm not actually sure if we need _both_ +1s?

      Seq(
        Module(new SyncMemWrapper(Vec(1, headPtrT), nHeads, metadata_sram_tag_t, prevent_simultaneous_access = !dont_prevent_simultaneous_accesses, nPaths = 2, nBanks = nBanks * bankMultiplier, nPorts = nBanks)),
        {
          val nCoordsOpt = coordParams match {
            case CompressedMetadata.InnerBuffer(nCoordsOpt_, _) => nCoordsOpt_
            case CompressedMetadata.DontExpandInner(nCoordsOpt_) => nCoordsOpt_
          }
          val nCoords = nCoordsOpt.getOrElse(if (axisId == 0) nElems else { nNodes * elemsPerRow }); if (axisId == 0) require(nCoordsOpt.isEmpty)
          val nCoordsRows = nCoords / elemsPerRow + (if (nCoords % elemsPerRow == 0) 0 else 1)

          val isDummy = coordParams match {
            case CompressedMetadata.DontExpandInner(_) => true
            case _ => false
          }

          Module(new SyncMemWrapper(Vec(elemsPerRow, metadata_elem_t), nCoordsRows, metadata_sram_tag_t, prevent_simultaneous_access = !dont_prevent_simultaneous_accesses, nBanks = nBanks * bankMultiplier, nPorts = nBanks, isDummy = isDummy))
        },
        Module(new SyncMemWrapper(Vec(1, metadata_elem_t), nNodes, metadata_sram_tag_t, prevent_simultaneous_access = !dont_prevent_simultaneous_accesses, nBanks = nBanks * bankMultiplier, nPorts = nBanks)),
        Module(new SyncMemWrapper(Vec(1, metadata_elem_t), nHeads, metadata_sram_tag_t, prevent_simultaneous_access = !dont_prevent_simultaneous_accesses, nPaths = 2, nBanks = nBanks * bankMultiplier, nPorts = nBanks)),
      )

    case (BitvectorMetadata(nBits, bitsPerRow), axisId) =>
      require(nBits % metadata_elem_t.getWidth == 0)
      require(bitsPerRow % metadata_elem_t.getWidth == 0)
      val nRows = nBits / metadata_elem_t.getWidth
      val nCols = bitsPerRow / metadata_elem_t.getWidth
      Seq(
        Module(new SyncMemWrapper(Vec(nCols, metadata_elem_t), nRows, metadata_sram_tag_t)),
      )

    case _ => Seq()
  }
  val nMetadataBuffers = metadata_mems.map(_.size).max

  def write_req_t = new ChiselSRAMWriteReq(elemT=elemT, elemsPerWrite=elemsPerWrite, nAxes=nAxes, axisIteratorBits=addrBits, spanBits=spanBits, nMetadataBuffers=nMetadataBuffers)
  def write_from_rf_req_t = new ChiselSRAMWriteFromRegfileReq(write_req_t, nAxes)

  val io = IO(new Bundle {
    val read_reqs = Vec(nBanks, Flipped(Decoupled(new ChiselSRAMReadReq(nAxes=nAxes, axisIteratorBits=addrBits, spanBits=spanBits, nMetadataBuffers=nMetadataBuffers))))
    val read_resps = Vec(nBanks, Decoupled(new ChiselSRAMReadResp(elemT=elemT, elemsPerRead=elemsPerRead,
      nAxes=nAxes, axisIteratorBits=addrBits, expandedAddrBits=metadata_elem_t.getWidth)))

    val write = Flipped(Decoupled(write_req_t))

    val write_from_regfile_reqs = Vec(nAxes, Vec(nBanks, Decoupled(write_from_rf_req_t)))
    val write_from_regfile_resps = Vec(nAxes, Vec(nBanks, Input(new ChiselSRAMWriteFromRegfileResp(elemT, metadata_elem_t, elemsPerWrite, nMetadataBuffers, nAxes))))

    val read_from_regfile_reqs = Vec(if (dummyReadStages) 1 else nAxes, Vec(if (dummyReadStages) 1 else nBanks, Decoupled(write_from_rf_req_t)))
    val read_from_regfile_resps = Vec(nAxes, Vec(nBanks, Input(new ChiselSRAMWriteFromRegfileResp(elemT, metadata_elem_t, elemsPerRead, nMetadataBuffers, nAxes))))

    val n_elems_in_axis_lookups = Vec(nBanks * (if (multipleNElemsLookupPorts) nBranches else 1), Flipped(new NElemsInAxisLookup(nAxes)))

    val busy = Output(Bool())
    val read_busy = Output(Bool())
    val write_busy = Output(Bool())
  })

  // Metadata SRAMs can be written to either from reg-files, or directly from the write stages themselves. To correctly
  // arbitrate between the two sources, we have a few lines of scaffolding below
  val n_metadata_buffers = metadata_mems.map(_.size).max
  val metadata_mem_is_being_written_to_from_regfile = VecInit.fill(nAxes, nBanks)(
    VecInit(Seq.fill(n_metadata_buffers.max(1))(false.B)) // Chisel can't handle zero-size vecs, so we need there to be at least one element here
  ).suggestName("mmibwtfr")

  metadata_mems.foreach(_.foreach(_.io.write_reqs.foreach(_.bits := DontCare)))

  val can_read_inflect = nBanks > 1 && readBankingStrategies.isEmpty
  val can_write_inflect = nBanks > 1 && writeBankingStrategies.isEmpty

  // Read and write pipeline
  def gen_pipeline_stages(read: Boolean, write: Boolean) = {
    // Note: Writes get priority over reads
    val stages = axes.zipWithIndex.map { case (axis, axisId) =>
      val metadata_ports = metadata_mems(axisId).map { m =>
        (getChiselType(m.io.write_reqs.head.bits),
          getChiselType(m.io.read_reqs.head.bits),
          getChiselType(m.io.read_resps.head.bits))
      }

      val branchSizes__ = if (read && write) {
        assert(readBranchSizes_ == writeBranchSizes_)
        branchSizes_
      } else if (read) {
        readBranchSizes_
      } else if (write) {
        writeBranchSizes_
      } else
        throw new Exception("UNREACHABLE")

      val _elemsPerWriteRf = if (elemsPerWriteRf > 0) elemsPerWriteRf else elemsPerWrite

      Seq.tabulate(nBanks)(bankId => CombPathBreaker(new ChiselSRAMPipelineStage(
        elemT=elemT, metadataElemT=metadata_elem_t, canWrite=write, canRead=read, nElems=nElems,
        elemsPerRead=elemsPerRead, elemsPerWrite=elemsPerWrite, elemsPerWriteRf = _elemsPerWriteRf, elemsPerRow=elemsPerRow,
        addrBits=addrBits, spanBits=spanBits, nMetadataBuffers=nMetadataBuffers,
        writeFromRfReqPort=write_from_rf_req_t,
        metadataPorts=metadata_ports, metadataConf=metadatas(axisId),
        maxElemsInRf=if (read && axisId == 0) maxElemsInRf else if (read && maxElemsInRf.nonEmpty && branchSizes__(axisId) > 1) Some(1) else None,
        multipleNElemsLookupPorts = multipleNElemsLookupPorts,
        axis=axis, nAxes=nAxes, nBanks=nBanks, nBranches=branchSizes__(axisId),
        axisId=axisId, bankId=bankId, outermostBranchedAxisId=branchSizes__.takeWhile(_ > 1).size-1,
        hardCodedValues=hardCodedValues, no_simultaneous_reads_and_writes=no_simultaneous_reads_and_writes,
        strideDivisibleBy = stridesDivisibleBy.getOrElse(axisId, 1),
        interleave_pop_in_last_stage = axes.forall(_ == FiberTreeAxis.Dense),
        can_read_inflect = can_read_inflect, can_write_inflect = can_write_inflect,
        sram_name_opt = nameOpt, isDummy = !write && dummyReadStages,
      ){
        override def desiredName = {
          if (read) { nameOpt.map(_ + "_").getOrElse("") + s"ReaderPipe_${axisId}_$bankId" }
          else { nameOpt.map(_ + "_").getOrElse("") + s"WriterPipe_${axisId}_$bankId" }
        }
      }) { _.io }({ x => if (pipeline_stages && axisId < nAxes-1) Seq((x.in, false, if (write) Seq("busy" -> { _: Data => true.B }).toMap[String, Data => Bool] else Map.empty[String, Data => Bool])) else Seq.empty }, busyPorts = {y => Seq("busy" -> y.busy).toMap}, hardCodedValues = { x => hardCodedValues(x.in.bits) }).suggestName(if (read) s"rd_${axisId}_$bankId" else s"wr_${axisId}_$bankId"))
    }

    // Wire stages together
    stages.flatten.foreach { stage =>
      stage.io.in.valid := false.B
      stage.io.in.bits := DontCare
      stage.io.out.ready := false.B
    }
    stages.foreach { banks =>
      banks.foreach(_.io.other_banks_op_count.valid := false.B)
      banks.foreach(_.io.other_banks_op_count.bits := banks.map(_.io.op_count).reduce((acc,x) => Mux(acc > x, acc, x)))
    }
    stages.foreach { banks =>
      banks.zipWithIndex.foreach { case (bank, bankId) =>
        banks.foreach(_.io.other_banks_recursive_buffer_fill_count(bankId) := bank.io.recursive_buffer_fill_count)
      }
    }
    stages.zipWithIndex.zip(stages.tail).foreach { case ((inners, innerAxisId), outers) =>
      val outerAxisId = innerAxisId + 1

      val headOuter = outers.head
      val headOut = headOuter.io.out.bits

      val rf_inflection_point = (nBanks>1).B && {
        val read_inflection_point = (read && can_read_inflect).B && !headOut.write && headOut.read_req.to_regfile && headOut.read_req.to_regfile_last_axis === outerAxisId.U

        val inf_axis = write.B && headOut.write && headOut.write_req.from_regfile_last_axis_log_size.andR
        val write_inflection_point = (write && can_write_inflect).B && headOut.write && any(headOut.write_req.from_regfile) &&
          headOut.write_req.from_regfile_last_axis +& (!inf_axis) === outerAxisId.U

        write_inflection_point || read_inflection_point
      }

      val non_rf_write_inflection_point = (nBanks>1 && write && can_write_inflect).B && headOut.write && !any(headOut.write_req.from_regfile) &&
        headOut.write_req.spans(innerAxisId) > 1.U && all(headOut.write_req.spans.take(innerAxisId).map(_ === 1.U)) &&
        headOut.write_req.data_strides(innerAxisId) === 0.U && {
          val strides = WireInit(headOut.write_req.metadata_strides(innerAxisId))
          if (axes(innerAxisId) == FiberTreeAxis.Compressed) {
            // TODO this is an ungly way to deal with the unintuitive method of flattening CSR tensors
            strides(innerAxisId)(CompressedMetadata.outer_metadata_ends_buffer_id) := 0.U
            strides(innerAxisId)(CompressedMetadata.inner_metadata_buffer_id) := 0.U
          }
          all(strides.flatten.map(_ === 0.U))
        }

      val non_rf_read_inflection_point = (nBanks>1 && read && can_read_inflect).B && !headOut.write && !headOut.read_req.to_regfile && !any(headOut.read_req.should_gather) && !headOut.read_req.adjacent &&
        headOut.read_req.spans(innerAxisId) > 1.U && all(headOut.read_req.spans.take(innerAxisId).map(_ === 1.U)) &&
        (innerAxisId == 0).B

      when (rf_inflection_point) {
        def muxOnWrite[T <: Data](ifWrite: T, ifRead: T): T = {
          if (write && read) Mux(headOut.write, ifWrite, ifRead)
          else if (write) ifWrite
          else ifRead
        }

        val inf_axis = write.B && headOut.write && headOut.write_req.from_regfile_last_axis_log_size.andR
        val address = muxOnWrite(headOut.write_req.address, headOut.read_req.address)(outerAxisId)
        val span = muxOnWrite(headOut.write_req.spans, headOut.read_req.spans)(outerAxisId)

        when (inf_axis) {
          assert(!headOuter.io.out.valid || (address +& span) <= nBanks.U)

          headOuter.io.out.ready := true.B
          inners.zipWithIndex.foreach { case (inner, innerBankId) =>
            when (address <= innerBankId.U && address +& span > innerBankId.U && !inner.io.in.ready) {
              headOuter.io.out.ready := false.B
            }
          }

          inners.zipWithIndex.foreach { case (inner, innerBankId) =>
            inner.io.in.valid := headOuter.io.out.valid && all(inners.patch(innerBankId, Nil, 1).map(_.io.in.ready))

            inner.io.in.bits := headOut

            inner.io.in.bits.read_req.independent := false.B
            inner.io.in.bits.read_req.adjacent := false.B

            inner.io.in.bits.write_req.spans(outerAxisId) := 1.U
            inner.io.in.bits.read_req.spans(outerAxisId) := 1.U

            inner.io.in.bits.write_req.address(outerAxisId) := innerBankId.U
            inner.io.in.bits.read_req.address(outerAxisId) := innerBankId.U

            val data_stride = muxOnWrite(headOut.write_req.data_strides, headOut.read_req.data_strides)(outerAxisId)
            val metadata_strides = muxOnWrite(headOut.write_req.metadata_strides, headOut.read_req.metadata_strides)(outerAxisId)
            val metadata_strides_by_addr = muxOnWrite(headOut.write_req.metadata_strides_by_addr, headOut.read_req.metadata_strides_by_addr)(outerAxisId)

            assert(!headOuter.io.out.valid || span === 1.U || all(metadata_strides_by_addr.take(outerAxisId + 1).flatten.map(_ === 0.U)))

            val offset = innerBankId.U - address
            inner.io.in.bits.data_addr := headOut.data_addr + offset * data_stride
            inner.io.in.bits.metadata_addrs.zipWithIndex.foreach { case (metadata_addrs, metadataAxisId) =>
              metadata_addrs.zipWithIndex.foreach { case (metadata_addr, metadataBufferId) =>
                val stride = metadata_strides(metadataAxisId)(metadataBufferId)
                metadata_addr := headOut.metadata_addrs(metadataAxisId)(metadataBufferId) + offset * stride
              }
            }
          }
        }.otherwise {
          val sendingOutToInnerBank = VecInit.fill(nBanks)(false.B)
          val addressToSendToInnerBank = Wire(Vec(nBanks, UInt(spanBits.W)))
          addressToSendToInnerBank := DontCare
          Seq.tabulate(nBanks) { outId =>
            when (outId.U < span) {
              val inner_bank_id = outId.U +& address
              sendingOutToInnerBank(inner_bank_id) := true.B
              addressToSendToInnerBank(inner_bank_id) := outId.U +& address
            }
          }
          assert(!headOuter.io.out.valid || span <= nBanks.U)

          val must_synchronize_op_counts = muxOnWrite(headOut.write_req.from_regfile_last_axis, headOut.read_req.to_regfile_last_axis) < outerAxisId.U

          when (!(address === 0.U && must_synchronize_op_counts && any(inners.map(_.io.op_count_busy)))) { // This when-statement makes sure we stall until op-counts can synchronize across banks
            inners.zipWithIndex.foreach { case (inner, innerBankId) =>
              inner.io.in.bits := headOut

              inner.io.in.bits.read_req.independent := false.B
              inner.io.in.bits.read_req.adjacent := false.B

              inner.io.in.bits.write_req.spans(outerAxisId) := 1.U
              inner.io.in.bits.read_req.spans(outerAxisId) := 1.U

              inner.io.in.bits.write_req.address(outerAxisId) := addressToSendToInnerBank(innerBankId)
              inner.io.in.bits.read_req.address(outerAxisId) := addressToSendToInnerBank(innerBankId)
              inner.io.in.bits.expanded_addrs.foreach(_(outerAxisId) := addressToSendToInnerBank(innerBankId)); assert(!inner.io.in.valid || span <= 1.U || axes(outerAxisId) === FiberTreeAxis.Dense, "not sure if this line is correct when outer axis is compressed")

              val other_inners_ready = all(inners.zipWithIndex.patch(innerBankId, Nil, 1).map { case (other_inner, otherInnerBankId) =>
                other_inner.io.in.ready || !sendingOutToInnerBank(otherInnerBankId)
              })
              inner.io.in.valid := headOuter.io.out.valid && sendingOutToInnerBank(innerBankId) && other_inners_ready
            }
          }
          headOuter.io.out.ready := all(inners.zip(sendingOutToInnerBank).map { case (inner, sendingOutTo) => inner.io.in.ready || !sendingOutTo })

          when (address === 0.U && must_synchronize_op_counts && !any(inners.map(_.io.op_count_busy))) {
            inners.foreach(_.io.other_banks_op_count.valid := true.B)
          }

          assert(!headOuter.io.out.valid || span <= 1.U ||
            all(muxOnWrite(headOut.write_req.metadata_strides(outerAxisId), headOut.read_req.metadata_strides(outerAxisId)).flatMap(_.take(outerAxisId).map(_ === 0.U))) ||
            all(muxOnWrite(headOut.write_req.metadata_strides_by_addr(outerAxisId), headOut.read_req.metadata_strides_by_addr(outerAxisId)).flatMap(_.take(outerAxisId).map(_ === 0.U))),
            "we need to update the metadata addresses to the inner axes in this case")
        }
      }.elsewhen (non_rf_write_inflection_point) {
        headOuter.io.out.ready := all(inners.map(_.io.in.ready))
        inners.zipWithIndex.foreach { case (inner, innerBankId) =>
          inner.io.in.valid := headOuter.io.out.valid && all(inners.patch(innerBankId, Nil, 1).map(_.io.in.ready))
          inner.io.in.bits := headOut
          inner.io.in.bits.read_req.independent := false.B
          inner.io.in.bits.read_req.adjacent := false.B
          inner.io.in.bits.is_split_across_banks := true.B
        }
      }.elsewhen (non_rf_read_inflection_point) {
        val address = headOut.read_req.address(outerAxisId)
        val is_first = headOut.first_it

        when (!(is_first && any(inners.map(_.io.full_busy)))) { // This when-statement makes sure we stall until shared state (like "total_running_len") can synchronize across banks
          inners.zipWithIndex.foreach { case (inner, innerBankId) =>
            when (address % nBanks.U === innerBankId.U) {
              inner.io.in <> headOuter.io.out
              inner.io.in.bits.read_req.independent := false.B
              inner.io.in.bits.read_req.adjacent := false.B
            }
          }
        }
      }.otherwise {
        inners.zip(outers).foreach { case (inner, outer) =>
          inner.io.in <> outer.io.out
          inner.io.in.bits.is_split_across_banks := false.B
        }
      }
    }

    // Wire stages to external IO
    val innermosts = stages.head
    val outermosts = stages.last

    val write_req_bank = 0.U

    val valid_read_banking_strategies = VecInit.fill(readBankingStrategies.size max 1)(false.B)
    readBankingStrategies.zipWithIndex.foreach { case (ReadBankingStrategy(cond), strategyId) =>
      val valid_banks = cond(io.read_reqs.head.bits).map(_._1)
      valid_read_banking_strategies(strategyId) := any(valid_banks)
    }
    val chosen_read_banking_strategy = PriorityEncoder(valid_read_banking_strategies)
    val head_read_req_broadcasted = any(valid_read_banking_strategies)
    val broadcasted_read_req_banks = VecInit.fill(nBanks)(false.B)
    when (head_read_req_broadcasted) {
      readBankingStrategies.zipWithIndex.foreach { case (ReadBankingStrategy(cond), strategyId) =>
        when (chosen_read_banking_strategy === strategyId.U) {
          connectVecs(broadcasted_read_req_banks, cond(io.read_reqs.head.bits).map(_._1), fillIn = Some(false.B))
        }
      }
    }
    val wait_to_broadcast_read_req = any(io.read_reqs.tail.map(_.valid))

    val valid_write_banking_strategies = VecInit.fill(writeBankingStrategies.size max 1)(false.B)
    writeBankingStrategies.zipWithIndex.foreach { case (WriteBankingStrategy(cond), strategyId) =>
      val valid_banks = cond(io.write.bits).map(_._1)
      valid_write_banking_strategies(strategyId) := any(valid_banks)
    }
    val chosen_write_banking_strategy = PriorityEncoder(valid_write_banking_strategies)
    val head_write_req_broadcasted = any(valid_write_banking_strategies)
    val broadcasted_write_req_banks = VecInit.fill(nBanks)(false.B)
    when (head_write_req_broadcasted) {
      writeBankingStrategies.zipWithIndex.foreach { case (WriteBankingStrategy(cond), strategyId) =>
        when (chosen_write_banking_strategy === strategyId.U) {
          connectVecs(broadcasted_write_req_banks, cond(io.write.bits).map(_._1), fillIn = Some(false.B))
        }
      }
    }

    outermosts.zip(io.read_reqs).zipWithIndex.foreach { case ((outermost, io_read_req), bankId) =>
      val io_read_req_valid = Mux(head_read_req_broadcasted && !wait_to_broadcast_read_req, io.read_reqs.head.valid, io_read_req.valid)
      val io_read_req_bits = Mux(head_read_req_broadcasted && !wait_to_broadcast_read_req, io.read_reqs.head.bits, io_read_req.bits)

      outermost.io.in.valid := read.B && io_read_req_valid ||
        (write.B && io.write.valid && write_req_bank === bankId.U && !head_write_req_broadcasted)
      outermost.io.in.bits := 0.U.asTypeOf(outermost.io.in.bits)
      outermost.io.in.bits.read_req := (if (read) io_read_req_bits else DontCare)
      if (write) ChiselSRAMWriteReq.connect(outermost.io.in.bits.write_req, io.write.bits) else outermost.io.in.bits.write_req := DontCare
      outermost.io.in.bits.write := (if (write && read) io.write.valid else write.B)
      outermost.io.in.bits.first_rf_access := false.B
      outermost.io.in.bits.last_rf_access := false.B
      outermost.io.in.bits.first_it := true.B
      outermost.io.in.bits.last_it := true.B
      outermost.io.in.bits.last_it_in_axis := DontCare
      outermost.io.in.bits.first_it_in_each_axis.foreach(_ := false.B)
      outermost.io.in.bits.last_it_in_each_axis.foreach(_ := false.B)
      outermost.io.in.bits.push_to_interleave_q := false.B
      outermost.io.in.bits.is_split_across_banks := false.B
      outermost.io.in.bits.read_req.independent := true.B
      outermost.io.in.bits.read_req.adjacent := false.B
      outermost.io.in.bits.read_req.update_running_len := false.B
      outermost.io.in.bits.read_req.fused := false.B
      outermost.io.in.bits.axis_spans.zip(axes).foreach { case (axis_span, axisType) => axis_span.valid := axisType === FiberTreeAxis.Dense }
      connectVecs(outermost.io.in.bits.axis_spans.map(_.bits), if (write && read) Mux(outermost.io.in.bits.write, outermost.io.in.bits.write_req.spans, outermost.io.in.bits.read_req.spans)
        else if (write) outermost.io.in.bits.write_req.spans else outermost.io.in.bits.read_req.spans)

      if (read && readBankingStrategies.nonEmpty) {
        readBankingStrategies.zipWithIndex.foreach { case (ReadBankingStrategy(cond), strategyId) =>
          when (head_read_req_broadcasted && !wait_to_broadcast_read_req && chosen_read_banking_strategy === strategyId.U && broadcasted_read_req_banks(bankId)) {
            outermost.io.in.valid := io.read_reqs.head.valid
            cond(io.read_reqs.head.bits).lift(bankId).foreach(_._2(outermost.io.in.bits.read_req))
          }
        }
      } else if (write && writeBankingStrategies.nonEmpty) {
        writeBankingStrategies.zipWithIndex.foreach { case (WriteBankingStrategy(cond), strategyId) =>
          when (head_write_req_broadcasted && chosen_write_banking_strategy === strategyId.U && broadcasted_write_req_banks(bankId)) {
            outermost.io.in.valid := io.write.valid
            cond(io.write.bits).lift(bankId).foreach(_._2(outermost.io.in.bits.write_req))
          }
        }
      }
    }

    if (write) {
      io.write.ready := false.B

      outermosts.zipWithIndex.foreach { case (outermost, bankId) =>
        when (write_req_bank === bankId.U) {
          io.write.ready := outermost.io.in.ready
        }
      }

      when (head_write_req_broadcasted) {
        io.write.ready := true.B
        for (bankId <- 0 until nBanks) {
          when (broadcasted_write_req_banks(bankId) && !outermosts(bankId).io.in.ready) {
            without(outermosts, bankId).foreach(_.io.in.valid := false.B)
            io.write.ready := false.B
          }
        }
      }
    }

    innermosts.foreach(_.io.out.ready := DontCare)
    if (read) {
      io.read_reqs.zip(outermosts).foreach { case (io_read_req, outermost) =>
        io_read_req.ready := outermost.io.in.ready && !outermost.io.in.bits.write
      }

      when (head_read_req_broadcasted) {
        when (wait_to_broadcast_read_req) {
          io.read_reqs.head.ready := false.B
          outermosts.head.io.in.valid := false.B
        }.otherwise {
          outermosts.zipWithIndex.foreach { case (outermost, bankId) =>
            when (!outermost.io.in.ready && broadcasted_read_req_banks(bankId)) {
              io.read_reqs.head.ready := false.B
              without(outermosts, bankId).foreach(_.io.in.valid := false.B)
            }
          }
        }
      }

      val running_fused_counter = RegInit(0.U(32.W))

      innermosts.zip(io.read_resps).zipWithIndex.foreach { case ((innermost, read_resp), bankId) =>
        read_resp.valid := read.B && innermost.io.out.valid && !innermost.io.out.bits.write
        read_resp.bits.data := innermost.io.out.bits.read_data
        read_resp.bits.spans := innermost.io.out.bits.read_req.spans
        read_resp.bits.is_data := innermost.io.out.bits.read_req.should_read_data
        read_resp.bits.is_both_data_and_metadata := innermost.io.out.bits.read_req.should_read_data && innermost.io.out.bits.read_req.should_read_metadata
        read_resp.bits.metadata_buffer_id := innermost.io.out.bits.read_req.metadata_buffer_id
        read_resp.bits.to_dma := innermost.io.out.bits.read_req.to_dma
        read_resp.bits.to_regfile := innermost.io.out.bits.read_req.to_regfile
        read_resp.bits.independent := innermost.io.out.bits.read_req.independent
        read_resp.bits.adjacent := innermost.io.out.bits.read_req.adjacent
        read_resp.bits.update_running_len := innermost.io.out.bits.read_req.update_running_len
        read_resp.bits.first := innermost.io.out.bits.first_it
        read_resp.bits.last := innermost.io.out.bits.last_it
        read_resp.bits.last_in_axis := innermost.io.out.bits.last_it_in_each_axis.map(_ && innermost.io.out.valid)
        read_resp.bits.compressed_address := innermost.io.out.bits.read_req.address
        read_resp.bits.expanded_addresses := innermost.io.out.bits.expanded_addrs
        read_resp.bits.to_regfile_last := innermost.io.out.valid && innermost.io.out.bits.last_rf_access
        read_resp.bits.opCount := innermost.io.out.bits.op_count
        read_resp.bits.total_running_len := innermost.io.out.bits.total_running_len
        read_resp.bits.axis_spans := innermost.io.out.bits.axis_spans
        innermost.io.out.ready := read_resp.ready

        when (innermost.io.out.bits.read_req.fused) {
          read_resp.bits.adjacent := false.B

          val others_valid = all(without(innermosts.map(_.io.out).map(out => out.valid && !out.bits.write && !out.bits.read_req.to_regfile && out.bits.read_req.should_read_metadata && out.bits.read_req.metadata_buffer_id === LinkedListMetadata.head_ptr_buffer_id.U && out.bits.read_req.adjacent), bankId))

          if (bankId == 0) {
            val rowlens = Wire(getChiselType(read_resp.bits.data))
            rowlens := DontCare
            rowlens.zip(innermosts.map(_.io.out.bits.read_data.head)).foreach { case (x, y) => x := y }
            val outer_ids = rowlens.scanLeft(running_fused_counter)(_.asUInt +& _.asUInt).tail.map(_.asTypeOf(elemT))
            read_resp.bits.data.zip(outer_ids).foreach { case (x, y) => x := y }

            when (innermost.io.out.bits.read_req.adjacent) {
              read_resp.bits.spans.head := nBanks.U

              when (!others_valid) {
                read_resp.valid := false.B
              }
            }

            innermost.io.out.ready := read_resp.ready && others_valid

            when (read_resp.fire) {
              running_fused_counter := selectFrom(outer_ids, read_resp.bits.spans.head-1.U).asUInt
            }
          } else {
            read_resp.valid := false.B
            innermost.io.out.ready := io.read_resps.head.ready && others_valid
          }
        }
      }
    }

    // Wire stages to SRAMs
    stages.flatten.foreach(_.io.data_write_req.ready := DontCare)
    stages.flatten.foreach(_.io.data_read_req.ready := DontCare)
    stages.flatten.foreach(_.io.data_read_resp.valid := false.B)
    stages.flatten.foreach(_.io.data_read_resp.bits := DontCare)

    if (write) {
      mem.io.write_reqs.zip(innermosts).foreach { case (write_req, innermost) =>
        write_req <> innermost.io.data_write_req
      }
    }

    if (read) {
      mem.io.read_reqs.zip(mem.io.read_resps).zip(innermosts).foreach { case ((read_req, read_resp), innermost) =>
        read_req <> innermost.io.data_read_req

        read_resp.ready := false.B // TODO is this line actually necessary? I'm not sure why it's even here
        innermost.io.data_read_resp <> read_resp
      }
    }

    // Wire stages to reg-files
    val mightAccessRf = stages.flatten.exists(_.inner.mightAccessRf) && (write || !dummyReadStages)

    if (!mightAccessRf) {
      stages.flatten.foreach(_.io.not_all_rf_outs_found := false.B)
      stages.flatten.foreach(_.io.found_rf_outs_num := DontCare)
      stages.flatten.foreach(_.io.write_from_regfile_axis_spans.foreach(_.valid := false.B))
      stages.flatten.foreach(_.io.write_from_regfile_axis_spans.foreach(_.bits := DontCare))
    }

    if (write) {
      stages.zipWithIndex.zip(io.write_from_regfile_reqs).zip(io.write_from_regfile_resps).foreach { case (((stage_banks, axisId), io_write_from_regfile_reqs), io_write_from_regfile_resps) =>
        stage_banks.zipWithIndex.zip(io_write_from_regfile_reqs).zip(io_write_from_regfile_resps).foreach { case (((stage, bankId), io_write_from_regfile_req), io_write_from_regfile_resp) =>
          stage.io.write_from_regfile_req <> io_write_from_regfile_req

          if (mightAccessRf) {
            when(io_write_from_regfile_req.valid) {
              val max_offset_bitwidth = log2Up(elemsPerRow * (elemT.getWidth max metadata_elem_t.getWidth)) // Chisel gets mad at you if your left-shift amount has too many bits
              val offset = io_write_from_regfile_req.bits.offset
              val aligned_found = noPrefix(io_write_from_regfile_resp.found.asUInt << offset.asTypeOf(UInt(max_offset_bitwidth.W))).asTypeOf(mem.io.write_reqs(bankId).bits.mask)
              val aligned_data = noPrefix(io_write_from_regfile_resp.data.asUInt << (offset * elemT.getWidth.U).asTypeOf(UInt(max_offset_bitwidth.W))).asTypeOf(mem.io.write_reqs(bankId).bits.data)
              val aligned_metadata = noPrefix(io_write_from_regfile_resp.metadata.zipWithIndex.map { case (meta, buffer_id) =>
                val t = if (metadata_mems(axisId).size > buffer_id) metadata_mems(axisId)(buffer_id).io.write_reqs(bankId).bits.data else Vec(1, UInt(1.W))
                (meta.asUInt << (offset * metadata_elem_t.getWidth.U).asTypeOf(UInt(max_offset_bitwidth.W))).asTypeOf(t)
              })

              def mask[T <: Data](w: DecoupledIO[SyncMemWriteReq[T]]) = aligned_found.zip(w.bits.mask).map { case (x, y) => x && y }

              // TODO it's possible that we might make spurious writes to the SRAMs here if the reg-file data isn't ready yet
              if (axisId == 0) {
                mem.io.write_reqs(bankId).bits.data := aligned_data
                mem.io.write_reqs(bankId).bits.mask := mask(stage.io.data_write_req)
                // mem.io.write_reqs(bankId).bits.addr := stage.io.data_write_req.bits.addr // This line causes a (fake) comb-loop on some versions of Firesim, so we leave it commented out here. The "addr" will always be the same whether or not we go into this when-statement anyways
              }

              val from_regfile_metadata = stage.io.write_from_regfile_req.bits.req.from_regfile_metadata(axisId)
              for ((metadata_mem, metadata_buffer_id) <- metadata_mems(axisId).zipWithIndex) {
                when(from_regfile_metadata(metadata_buffer_id).valid) {
                  metadata_mem.io.write_reqs(bankId).bits.data.zip(aligned_metadata(metadata_buffer_id)).foreach { case (m, r) => m := r.asUInt }
                  metadata_mem.io.write_reqs(bankId).bits.mask.zip(mask(stage.io.metadata_write_reqs(metadata_buffer_id))).foreach { case (m, f) => m := f }
                  // metadata_mem.io.write_reqs(bankId).bits.addr := stage.io.metadata_write_reqs(metadata_buffer_id).bits.addr  // This line causes a (fake) comb-loop on some versions of Firesim, so we leave it commented out here. The "addr" will always be the same whether or not we go into this when-statement anyways
                  metadata_mem_is_being_written_to_from_regfile(axisId)(bankId)(metadata_buffer_id) := true.B
                }
              }
            }

            val axis = io_write_from_regfile_req.bits.req.axis
            val span = io_write_from_regfile_req.bits.req.spans(axis)
            stage.io.not_all_rf_outs_found := stage.io.write_from_regfile_req.ready &&
              !all(io_write_from_regfile_resp.found.zipWithIndex.map { case (f, i) => f || i.U >= span })
            stage.io.found_rf_outs_num := PopCount(io_write_from_regfile_resp.found.zipWithIndex.map { case (f, i) =>
              f && i.U < span
            })
            connectVecs(stage.io.write_from_regfile_axis_spans.map(_.valid), io_write_from_regfile_resp.axis_spans.map(_.valid))
            connectVecs(stage.io.write_from_regfile_axis_spans.map(_.bits), io_write_from_regfile_resp.axis_spans.map(_.bits))
            stage.io.rf_out := io_write_from_regfile_resp.addrs(axisId).asTypeOf(stage.io.rf_out)
          }
        }
      }

      val stall_counter = RegInit(0.U(16.W))
      stall_counter := stall_counter + 1.U
      when (!any(stages.flatMap(_.map(_.io.write_from_regfile_req.valid))) || any(stages.flatMap(_.map(_.io.write_from_regfile_req.fire)))) {
        stall_counter := 0.U
      }
      assert(stall_counter < 10000.U, s"stalling when trying to write from regfile")
    }

    if (read) {
      stages.zipWithIndex.zip(io.read_from_regfile_reqs).zip(io.read_from_regfile_resps).foreach { case (((stage_banks, axisId), io_read_from_regfile_reqs), io_read_from_regfile_resps) =>
        stage_banks.zipWithIndex.zip(io_read_from_regfile_reqs).zip(io_read_from_regfile_resps).foreach { case (((stage, bankId), io_read_from_regfile_req), io_read_from_regfile_resp) =>
          stage.io.write_from_regfile_req <> io_read_from_regfile_req

          if (mightAccessRf) {
            val axis = io_read_from_regfile_req.bits.req.axis
            val span = io_read_from_regfile_req.bits.req.spans(axis)
            stage.io.not_all_rf_outs_found := stage.io.write_from_regfile_req.ready &&
              !all(io_read_from_regfile_resp.found.zipWithIndex.map { case (f, i) => f || i.U >= span })
            stage.io.found_rf_outs_num := PopCount(io_read_from_regfile_resp.found.zipWithIndex.map { case (f, i) =>
              f && i.U < span
            })
            stage.io.rf_out := io_read_from_regfile_resp.data.head // TODO we should be able to return multiple values rather than just one value. Or maybe that's rarely necessary for scatter-gathers?
            connectVecs(stage.io.write_from_regfile_axis_spans.map(_.valid), io_read_from_regfile_resp.axis_spans.map(_.valid))
            connectVecs(stage.io.write_from_regfile_axis_spans.map(_.bits), io_read_from_regfile_resp.axis_spans.map(_.bits))
          } else {
            stage.io.rf_out := DontCare
          }
        }
      }

      if (dummyReadStages) {
        // We can skip "stages.flatten.head" because we already connect it in the line above, and there's no need to generate an extra FIRRTL LOC for it
        stages.flatten.tail.foreach(_.io.write_from_regfile_req.ready := false.B)
        stages.flatten.tail.foreach(_.io.rf_out := DontCare)
      }
    } else {
      stages.flatten.foreach(_.io.rf_out := DontCare)
    }

    stages
  }

  val read_stages = gen_pipeline_stages(read=true, write=false)
  val write_stages = gen_pipeline_stages(read=false, write=true)

  // Wire reads and updates to/from metadata SRAMS
  read_stages.zip(write_stages).zip(metadata_mems).zipWithIndex.foreach { case (((read_stage_banks, write_stage_banks), mms), axisId) =>
    Seq.tabulate(nBanks) { bankId =>
      val read_stage = read_stage_banks(bankId)
      val write_stage = write_stage_banks(bankId)

      val metadata_read_arbs = mms.map(mm => Module(new RRArbiter(getChiselType(mm.io.read_reqs.head.bits), 2)))
      // val metadata_write_arbs = mms.map(mm => Module(new Arbiter(getChiselType(mm.io.write_reqs.head.bits), 2)))

      metadata_read_arbs.zipWithIndex.foreach { case (arb, i) =>
        arb.io.in.zip(Seq(read_stage, write_stage)).foreach { case (arb_in, stage) =>
          arb_in <> stage.io.metadata_read_reqs(i)
        }

        mms(i).io.read_reqs(bankId) <> arb.io.out
      }

      /*
      metadata_write_arbs.zipWithIndex.foreach { case (arb, i) =>
        arb.io.in.zip(Seq(read_stage, write_stage)).foreach { case (arb_in, stage) =>
          arb_in <> stage.io.metadata_write_reqs(i)
        }

        // Metadate mems can be written to by either the read/write pipeline stages, or by regfiles. Therefore, we need
        // a bit of arbitration code below that prevents us from writing a much simpler "mms(i).io.write_req <> arb.io.out"
        mms(i).io.write_reqs(bankId).valid := arb.io.out.valid
        arb.io.out.ready := mms(i).io.write_reqs(bankId).ready

        when(!metadata_mem_is_being_written_to_from_regfile(axisId)(bankId)(i)) {
          mms(i).io.write_reqs(bankId).bits := arb.io.out.bits
        }
      }
      */

      mms.zip(write_stage.io.metadata_write_reqs).zipWithIndex.foreach { case ((mm, write_req), i) =>
        // Metadate mems can be written to by either the read/write pipeline stages, or by regfiles. Therefore, we need
        // a bit of arbitration code below that prevents us from writing a much simpler "mms(i).io.write_req <> write_req"
        mm.io.write_reqs(bankId).valid := write_req.valid
        write_req.ready := mm.io.write_reqs(bankId).ready

        when (!metadata_mem_is_being_written_to_from_regfile(axisId)(bankId)(i)) {
          mm.io.write_reqs(bankId).bits := write_req.bits
        }
        mm.io.write_reqs(bankId).bits.addr := write_req.bits.addr // We need to explicitly move this part out of the when-clause to avoid Firesim mistaking this for a (fake) comb-loop
      }
      read_stage.io.metadata_write_reqs.foreach(_.ready := false.B)
      assert(!any(read_stage.io.metadata_write_reqs.map(_.valid)), "read stages can't write to metadata buffers")

      mms.zipWithIndex.foreach { case (mm, i) =>
        mm.io.read_resps(bankId).ready := false.B

        read_stage.io.metadata_read_resps(i).valid := false.B
        read_stage.io.metadata_read_resps(i).bits := mm.io.read_resps(bankId).bits

        write_stage.io.metadata_read_resps(i).valid := false.B
        write_stage.io.metadata_read_resps(i).bits := mm.io.read_resps(bankId).bits

        when(mm.io.read_resps(bankId).bits.tag.write) {
          write_stage.io.metadata_read_resps(i) <> mm.io.read_resps(bankId)
        }.elsewhen(!mm.io.read_resps(bankId).bits.tag.write) {
          read_stage.io.metadata_read_resps(i) <> mm.io.read_resps(bankId)
        }
      }
    }
  }

  // Write stages to the interleaving queues, span_traversed signals, and coarse-grained stalling, so they can synchronize their traversals with each other
  val interleaving_read_q = RegInit(0.U(32.W))
  val interleaving_write_q = RegInit(0.U(32.W))

  // TODO Currently, we only support interleaving and trailing for one-bank SRAMs
  if (nBanks > 1) {
    io.read_reqs.foreach(io_read_req => assert(!(io_read_req.valid && (io_read_req.bits.interleave.should_pop || io_read_req.bits.interleave.should_push)), "Currently, we only support interleaving for one-bank SRAMs"))
    assert(!(io.write.valid && (io.write.bits.interleave.should_pop || io.write.bits.interleave.should_push)), "Currently, we only support interleaving for one-bank SRAMs")
  }

  (read_stages.flatten ++ write_stages.flatten).foreach(_.io.other_stage_span_traversed := DontCare)
  (read_stages.flatten ++ write_stages.flatten).foreach(_.io.interleave_q_pop.valid := false.B)

  for ((pusher, poppers, q) <- Seq((read_stages.head.head, write_stages.map(_.head), interleaving_write_q), (write_stages.head.head, read_stages.map(_.head), interleaving_read_q))) {
    poppers.foreach { popper =>
      val preceding = poppers.takeWhile(_ != popper)
      popper.io.interleave_q_pop.valid := q > PopCount(preceding.map(_.io.interleave_q_pop.ready))
    }

    val increment = pusher.io.interleave_q_push
    val decrement = PopCount(poppers.map(_.io.interleave_q_pop.fire))

    q := (q +& increment) - decrement
  }

  if (pipeline_stages)
    assert(!(io.write.valid && (io.write.bits.should_trail_reads || io.write.bits.should_trail_reads_coarse_grained)) &&
      !any(io.read_reqs.map(io_read_req => io_read_req.valid && (io_read_req.bits.should_trail_writes || io_read_req.bits.should_trail_writes_coarse_grained))),
      "the code below currently assumes that pipeline_stages is false")

  val spanTraversedShouldBeSet = (read_stages ++ write_stages).flatten.exists(_.inner.spanTraversedShouldBeSet)
  if (spanTraversedShouldBeSet) {
    def min_span_traversed[T <: Data](stages: Iterable[Valid[Vec[UInt]]]): Vec[UInt] = {
      val elem_size = stages.flatMap(_.bits).map(_.getWidth).max

      val traversed = stages.map(stage => Mux(stage.valid,
        stage.bits,
        VecInit.fill(stage.bits.size)(~0.U(elem_size.W)),
      )).toSeq

      traversed.reduce((acc, x) => Mux(compareVecs(acc, x, { _ < _ }), acc, x))
    }

    read_stages.zip(write_stages).zipWithIndex.foreach { case ((read_banks, write_banks), axisId) =>
      read_banks.zip(write_banks).zipWithIndex.foreach { case ((read_bank, write_bank), bankId) =>
        Seq((read_bank, write_bank, write_stages), (write_bank, read_bank, read_stages)).foreach { case (dst, src, srcStages) =>
          val earlier_src_stages = srcStages.drop(axisId+1).map(_(bankId))
          dst.io.other_stage_span_traversed.zip(dst.io.other_stage_last_it_in_axis).zipWithIndex.foreach { case ((other_stage_span_traversed, other_stage_last_in_axis), branchId) =>
            val src_branch_id = branchId % src.io.span_traversed.size // The "%" is useful when read and write branch sizes are different
            when (src.io.span_traversed(src_branch_id).valid) {
              other_stage_span_traversed.valid := true.B
              other_stage_span_traversed.bits := src.io.span_traversed(src_branch_id).bits
              other_stage_last_in_axis := src.io.last_it_in_axis(src_branch_id)
            }.otherwise {
              val earlier_valids = earlier_src_stages.map(_.io.span_traversed.map(_.valid)).map(any)
              val earlier_traverseds = earlier_src_stages.map(_.io.span_traversed).map(min_span_traversed)
              val earlier_lasts_in_axis = earlier_src_stages.map(_.io.last_it_in_axis).map(vecAnd)
              other_stage_span_traversed.valid := any(earlier_src_stages.flatMap(_.io.span_traversed).map(_.valid))
              other_stage_span_traversed.bits := MuxCase(earlier_traverseds.lastOption.getOrElse(DontCare), earlier_valids.zip(earlier_traverseds))
              other_stage_last_in_axis := MuxCase(earlier_lasts_in_axis.lastOption.getOrElse(DontCare), earlier_valids.zip(earlier_lasts_in_axis))
            }
          }
        }
      }
    }
  } else {
    (read_stages ++ write_stages).flatten.foreach(_.io.other_stage_span_traversed := DontCare)
    (read_stages ++ write_stages).flatten.foreach(_.io.other_stage_last_it_in_axis := DontCare)
  }

  read_stages.zip(write_stages).foreach { case (read_banks, write_banks) =>
    read_banks.foreach(_.io.other_stage_busy := any(write_banks.map(_.io.full_busy)))
    write_banks.foreach(_.io.other_stage_busy := any(read_banks.map(_.io.full_busy)))
  }

  read_stages.zip(write_stages).foreach { case (read_banks, write_banks) =>
    (read_banks ++ write_banks).foreach(_.io.other_stage_axis_spans.foreach { x =>
      x.foreach(_.valid := false.B)
      x.foreach(_.bits := DontCare)
    })
    (read_banks ++ write_banks).foreach(_.io.other_stage_next_node_added.foreach(_ := false.B))

    Seq((read_banks, write_banks), (write_banks, read_banks)).foreach { case (dstBanks, srcBanks) =>
      dstBanks.zip(srcBanks).foreach { case (dst, src) =>
        connectVecs(dst.io.other_stage_next_node_added, src.io.next_node_added)
        connectVecs(dst.io.other_stage_axis_spans, src.io.axis_spans)
      }
    }
  }

  // Connect the shared-state signals
  (read_stages ++ write_stages).foreach { banks =>
    banks.foreach { bank =>
      bank.io.other_banks_shared_state_increment.zipWithIndex.foreach { case (bankIo, shared_state_id) =>
        bankIo := noPrefix(banks.map(_.io.incrementing_shared_state(shared_state_id)))
      }
      bank.io.other_banks_shared_state_reset.zipWithIndex.foreach { case (bankIo, shared_state_id) =>
        bankIo := noPrefix(any(banks.map(_.io.resetting_shared_state(shared_state_id))))
      }
    }
  }

  write_stages.flatten.foreach(_.io.lookup_future_stages.foreach(_.nElems := 0.U))
  read_stages.flatten.foreach(_.io.lookup_future_stages.foreach(_.nElems := 0.U))
  write_stages.flatten.foreach(_.io.lookup_past_stages.foreach(_.address := DontCare))
  read_stages.flatten.foreach(_.io.lookup_past_stages.foreach(_.address := DontCare))
  read_stages.zip(read_stages.tail).foreach { case (future_stages, past_stages) =>
    future_stages.zip(past_stages).foreach { case (future_stage, past_stage) =>
      connectVecs(future_stage.io.lookup_past_stages, past_stage.io.lookup_future_stages, biconnect = true)
    }
  }
  io.n_elems_in_axis_lookups.zip(read_stages.head.flatMap(_.io.lookup_future_stages)).foreach { case (io_lookup, read_stage_lookup) => io_lookup <> read_stage_lookup }
  require(maxElemsInRf.isEmpty || io.n_elems_in_axis_lookups.size == read_stages.head.flatMap(_.io.lookup_future_stages).size)

  io.busy := (read_stages ++ write_stages).flatten.map(_.io.busy).reduce(_ || _)
  io.read_busy := read_stages.flatten.map(_.io.busy).reduce(_ || _)
  io.write_busy := write_stages.flatten.map(_.io.busy).reduce(_ || _)

  // Hardcoding
  axes.zip(metadata_mem_is_being_written_to_from_regfile).collect {
    case (FiberTreeAxis.Compressed, mms_written_to_from_rf) =>
      val not_written_to_by_rf = (0 until n_metadata_buffers).toSet.diff(CompressedMetadata.can_be_written_by_regfile.toSet)
      not_written_to_by_rf.foreach { i =>
        mms_written_to_from_rf.foreach(_(i) := false.B)
      }

    case (FiberTreeAxis.LinkedList, mms_written_to_from_rf) =>
      val not_written_to_by_rf = (0 until n_metadata_buffers).toSet.diff(LinkedListMetadata.can_be_written_by_regfile.toSet)
      not_written_to_by_rf.foreach { i =>
        mms_written_to_from_rf.foreach(_(i) := false.B)
      }
  }

//  val read_from_rf_stalling = VecInit(io.read_from_regfile_reqs.map(y => VecInit(y.map(x => x.valid && !x.ready)))) // Debugging signal
//  dontTouch(read_from_rf_stalling)

  assert(!io.write.valid || io.write.bits.spans.map(_ > 0.U).reduce(_ && _))
  io.read_reqs.foreach(io_read_req => assert(!io_read_req.valid || io_read_req.bits.spans.map(_ > 0.U).reduce(_ && _)))
}
