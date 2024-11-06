package stellar.rtl

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.RoCCCommand
import stellar.Util.SMap
import stellar.FiberTreeAxis
import ChiselUtil._
import chisel3.experimental.ChiselEnum

object ISA {
  // Funct values
  val Issue = 0

  val SetSrcAndDst = 1
  val SetConstant = 2

  val SetAddress = 3
  val SetSpan = 4
  val SetStride = 5
  val SetAxis = 6
  val SetDataOrMetadataAddress = 7
  val SetMetadataStride = 8
  val SetMetadataStrideByAddr = 9
  val SetMetadataStrideByValue = 10
  val SetFromRegfileMetadataCoord = 11
  val SetMetadataRegion = 12
  val SetWaits = 13
  val SetRecursive = 14

  val IssueLoadBalancer = 28
  val SetLoadBalancingClSize = 29
  val SetLoadBalancingRfSize = 30
  val SetLoadBalancingAxisSize = 31

  val Flush = 32

  val LoadSavedConfig = 64

  // Consts that can be set
  val ConstantIsData = 0
  val ConstantAxis = 1
  val ConstantMetadataBufferId = 2
  val ConstantLastAxis = 3
  val ConstantInterleavePush = 4
  val ConstantInterleavePop = 5
  val ConstantInterleaveAxis = 6
  val ConstantShouldTrail = 7
  val ConstantResetRunningState = 8
  val ConstantShouldGather = 9
  val ConstantLastAxisLogSize = 10
  val ConstantShouldTrailCoarse = 11
  val ConstantDropFollowingCmdsIfEmpty = 12

  // Rs1/Rs2 bundles
  class AxisRs1 extends Bundle {
    val for_dst = Bool()
    val for_src = Bool()
    val UNUSED = UInt((64 - 8 - 2).W)
    val axis_id = UInt(8.W)

    require(getWidth == 64)

    def setConfig[T <: Data](src: Vec[T], dst: Vec[T], rs2: UInt, tOpt: Option[T] = None): Unit = {
      for ((x,y) <- Seq((for_src, src), (for_dst, dst))) {
        when (x) {
          y(axis_id) := tOpt.map(t => rs2(t.getWidth-1,0).asTypeOf(t)).getOrElse(rs2)
        }
      }
    }
  }

  class AxisAndMetadataRs1 extends Bundle {
    val for_dst = Bool()
    val for_src = Bool()
    val UNUSED = UInt((64 - 2*8 - 2).W)
    val metadata_id = UInt(8.W)
    val axis_id = UInt(8.W)

    require(getWidth == 64)

    def setDataConfig[T <: Data](src: Vec[T], dst: Vec[T], rs2: UInt, tOpt: Option[T] = None): Unit = {
      for ((x,y) <- Seq((for_src, src), (for_dst, dst))) {
        when (x && axis_id.andR) {
          y.foreach(_ := tOpt.map(t => rs2.asTypeOf(t)).getOrElse(rs2))
        }
      }
    }

    def setMetadataConfig[T <: Data](src: Vec[Vec[T]], dst: Vec[Vec[T]], rs2: UInt, tOpt: Option[T] = None): Unit = {
      for ((x,y) <- Seq((for_src, src), (for_dst, dst))) {
        when (x && !axis_id.andR) {
          y(axis_id)(metadata_id) := tOpt.map(t => rs2.asTypeOf(t)).getOrElse(rs2)
        }
      }
    }
  }

  class TwoAxesAndMetadataRs1 extends Bundle {
    val for_dst = Bool()
    val for_src = Bool()
    val UNUSED = UInt((64 - 3*8 - 2).W)
    val metadata_id = UInt(8.W)
    val changed_axis_id = UInt(8.W)
    val iterator_axis_id = UInt(8.W)

    require(getWidth == 64)

    def setConfig[T <: Data](src: Vec[Vec[Vec[T]]], dst: Vec[Vec[Vec[T]]], rs2: UInt, tOpt: Option[T] = None): Unit = {
      for ((x,y) <- Seq((for_src, src), (for_dst, dst))) {
        when (x) {
          y(iterator_axis_id)(changed_axis_id)(metadata_id) := tOpt.map(t => rs2.asTypeOf(t)).getOrElse(rs2)
        }
      }
    }
  }

  class FromRegfileMetadataRs2 extends Bundle {
    val UNUSED = UInt(31.W)
    val valid = Bool()
    val coord = UInt(32.W)

    assert(getWidth == 64)
  }
}

case class SavedConfig(
                       srcAndDst: Option[(Region /* src */, Region /* dst */)] = None,
                       axes: SMap[(Int, Boolean), FiberTreeAxis.Type] = Seq.empty.toMap,
                       spans: SMap[(Int, Boolean), UInt] = Seq.empty.toMap,
                       metadataStrides: SMap[(Int, Int, Int, Boolean), UInt] = Seq.empty.toMap,
                       metadataStridesByAddr: SMap[(Int, Int, Int, Boolean), UInt] = Seq.empty.toMap,
                       metadataStridesByValue: SMap[(Int, Int, Int, Boolean), UInt] = Seq.empty.toMap,
                       metadataRegions: SMap[(Int /* axisId */, Int /* metadataBufferId */), Region] = Seq.empty.toMap,
                       constants: SMap[Int /* constId */, Int /* value to set const to */] = Seq.empty.toMap,
                       waits: Option[WaitOptions] = None,
                       recursive: SMap[Int /* axisId */, Int /* recursiveDim */] = Seq.empty.toMap,
                       spansThatWontReset: Set[(Int, Boolean)] = Set.empty,
                       addrsThatWontReset: Set[(Int, Boolean)] = Set.empty,
                       dataAddrsThatWontReset: Set[Boolean] = Set.empty,
                       metadataAddrsThatWontReset: Set[(Int /* axisId */, Int /* metadataBufferId */, Boolean /* isForSrc */)] = Set.empty,
                       metadataStridesThatWontReset: Set[(Int /* outerAxisId */, Int /* innerAxisId */, Int /* metadataBufferId */, Boolean /* isForSrc */)] = Set.empty,
                       issues: Boolean = false,
                      ) {
  // Note: for every config-dictionary, the key is one of these options:
  //  (axisId, isForSrc is true | isForDst if false)
  //  (outerAxisId, innerAxisId, metadataBufferId, isForSrc is true | isForDst if false)

  private def setSavedConfig_OneAxis[T <: Data](srcConfigs: Vec[T], dstConfigs: Vec[T], saved: SMap[(Int, Boolean), T]): Unit = {
    saved.foreach { case ((axisId, isForSrc), value) =>
      (if (isForSrc) srcConfigs else dstConfigs)(axisId) := value
    }
  }

  private def setSavedConfig_TwoAxes[T <: Data](srcConfigs: Vec[Vec[Vec[T]]], dstConfigs: Vec[Vec[Vec[T]]], saved: SMap[(Int, Int, Int, Boolean), T]): Unit = {
    saved.foreach { case ((outerAxisId, innerAxisId, metadataBufferId, isForSrc), value) =>
      (if (isForSrc) srcConfigs else dstConfigs)(outerAxisId)(innerAxisId)(metadataBufferId) := value
    }
  }

  def setVecSavedConfigs(axesVec: Seq[Vec[FiberTreeAxis.Type]], spansVec: Seq[Vec[UInt]],
                         metadataStridesVec: Seq[Vec[Vec[Vec[UInt]]]],
                         metadataStridesByAddrVec: Seq[Vec[Vec[Vec[UInt]]]],
                         metadataStridesByValueVec: Seq[Vec[Vec[Vec[UInt]]]],
                         metadataRegionsVec: Vec[Vec[Region]],
                        ): Unit = {
    require(Seq(axesVec, spansVec, metadataStridesVec, metadataStridesByAddrVec).forall(_.size == 2))

    setSavedConfig_OneAxis(axesVec.head, axesVec.last, axes)
    setSavedConfig_OneAxis(spansVec.head, spansVec.last, spans)
    setSavedConfig_TwoAxes(metadataStridesVec.head, metadataStridesVec.last, metadataStrides)
    setSavedConfig_TwoAxes(metadataStridesByAddrVec.head, metadataStridesByAddrVec.last, metadataStridesByAddr)
    setSavedConfig_TwoAxes(metadataStridesByValueVec.head, metadataStridesByValueVec.last, metadataStridesByValue)

    metadataRegions.foreach { case ((axisId, metadataBufferId), value) =>
      metadataRegionsVec(axisId)(metadataBufferId) := value
    }
  }

  require((spansThatWontReset.isEmpty && addrsThatWontReset.isEmpty && dataAddrsThatWontReset.isEmpty && metadataAddrsThatWontReset.isEmpty && metadataStridesThatWontReset.isEmpty) || srcAndDst.nonEmpty, "spansThatWontReset is meaningless if srcAndDst isn't being set in the same instruction")
}

object DecoderInst extends ChiselEnum {
  val dma_inst, sram_read_inst, sram_write_inst = Value
}

class WaitOptionForSram extends Bundle {
  val write_from_dram = Bool()
  val write_into_sram = Bool()
  val read_from_sram = Bool()
}
class WaitOptions(nSrams: Int) extends Bundle {
  val wait_for_srams = Vec(nSrams, new WaitOptionForSram)
  val wait_for_dma = Bool()
}
object WaitOptions {
  import chisel3.experimental.BundleLiterals._
  import chisel3.experimental.VecLiterals._
  def literal(nSrams: Int, wait_for_dma: Boolean, read_from_sram: Set[Int] = Set.empty, write_from_dram: Set[Int] = Set.empty, write_into_sram: Set[Int] = Set.empty): WaitOptions =
    (new WaitOptions(nSrams)).Lit(_.wait_for_dma -> wait_for_dma.B, _.wait_for_srams -> Vec.Lit(Seq.tabulate(nSrams)(sramCode => (new WaitOptionForSram).Lit(_.read_from_sram -> read_from_sram.contains(sramCode).B, _.write_into_sram -> write_into_sram.contains(sramCode).B, _.write_from_dram -> write_from_dram.contains(sramCode).B)):_*))
}

class Decoder[T <: Data](nAxes: Int, nSrams: Int, addrBits: Int, spanBits: Int, strideBits: Int,
                         cmd_t: RoCCCommand, dma_req_t: DmaConfiguration,
                         sram_write_t: ChiselSRAMWriteReq[T], sram_read_t: ChiselSRAMReadReq,
                         saved_configs: SMap[Int, SavedConfig]) extends Module
{
  import ISA.{AxisRs1, AxisAndMetadataRs1, TwoAxesAndMetadataRs1}

  val n_metadata_buffer_ids = dma_req_t.metadata_strides.head.head.size

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(getChiselType(cmd_t)))

    val out = Decoupled(new Bundle {
      // This Bundle is basically a tagged union
      val src = Output(new Region)
      val dst = Output(new Region)
      def inst_type = Mux(src.is_dram || dst.is_dram, DecoderInst.dma_inst, Mux(src.is_sram, DecoderInst.sram_read_inst, DecoderInst.sram_write_inst))

      val dma = getChiselType(dma_req_t)
      val sram_read = getChiselType(sram_read_t)
      val sram_write = getChiselType(sram_write_t)

      val waits = new WaitOptions(nSrams)
    })
  })

  // Configuration variables
  val src = Reg(new Region)
  val dst = Reg(new Region)

  val waits = Reg(new WaitOptions(nSrams))

  val isData = Reg(Bool())
  val axis = Reg(UInt(log2Up(nAxes).W))
  val metadataBufferId = Reg(UInt(log2Up(n_metadata_buffer_ids).W))
  val lastAxis = Reg(UInt(log2Up(nAxes).W))
  val lastAxisLogSize = Reg(UInt(log2Up(spanBits+1).W))
  val interleavePush = Reg(Bool())
  val interleavePop = Reg(Bool())
  val interleaveAxis = Reg(UInt(log2Up(nAxes).W))
  val shouldTrail = Reg(Bool())
  val shouldTrailCoarse = Reg(Bool())
  val dropFollowingCmdsIfEmpty = Reg(Bool())
  val resetRunningState = Reg(Bool())
  val shouldGather = Reg(Bool())
  val isRecursive = Reg(Vec(nAxes, Bool()))
  val recursiveDim = Reg(Vec(nAxes, UInt((spanBits+1).W)))

  val srcAxes = Reg(Vec(nAxes, FiberTreeAxis()))
  val srcStrides = Reg(Vec(nAxes, UInt(strideBits.W)))
  val srcSpans = Reg(Vec(nAxes, UInt(spanBits.W)))
  val srcAddrs = Reg(Vec(nAxes, UInt(addrBits.W)))
  val srcDataAddrs = Reg(Vec(nAxes, UInt(addrBits.W)))
  val srcMetaDataAddrs = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(addrBits.W)))))
  val srcMetaDataStrides = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(strideBits.W)))))
  val srcMetaDataStridesByAddr = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(strideBits.W)))))
  val srcMetaDataStridesByValue = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(strideBits.W)))))

  val dstAxes = Reg(Vec(nAxes, FiberTreeAxis()))
  val dstStrides = Reg(Vec(nAxes, UInt(strideBits.W)))
  val dstSpans = Reg(Vec(nAxes, UInt(spanBits.W)))
  val dstAddrs = Reg(Vec(nAxes, UInt(addrBits.W)))
  val dstDataAddrs = Reg(Vec(nAxes, UInt(addrBits.W)))
  val dstMetaDataAddrs = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(addrBits.W)))))
  val dstMetaDataStrides = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(strideBits.W)))))
  val dstMetaDataStridesByAddr = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(strideBits.W)))))
  val dstMetaDataStridesByValue = Reg(Vec(nAxes, Vec(nAxes, Vec(n_metadata_buffer_ids, UInt(strideBits.W)))))
  val dstMetaDataRegions = Reg(Vec(nAxes, Vec(n_metadata_buffer_ids, new Region)))
  val dstFromRfMetadata = Reg(Vec(nAxes, Vec(n_metadata_buffer_ids, new ChiselSRAMWriteReq.FromRegfileMetadata)))

  // Logic
  val done = RegInit(false.B)

  io.in.ready := !done

  io.out.valid := done
  io.out.bits.src := src
  io.out.bits.dst := dst
  io.out.bits.waits := waits

  assert(!(done && src.is_sram && dst.is_sram), "can't move data directly from sram to sram")
  assert(!(done && src.is_dram && dst.is_dram), "can't move data directly from dram to dram")

  io.out.bits.dma.axes := dstAxes
  io.out.bits.dma.spans := dstSpans
  io.out.bits.dma.addrs := Mux(src.is_dram, srcAddrs, dstAddrs)
  io.out.bits.dma.data_base_addrs := Mux(src.is_dram, srcDataAddrs, dstDataAddrs)
  io.out.bits.dma.metadata_base_addrs := Mux(src.is_dram, srcMetaDataAddrs, dstMetaDataAddrs)
  io.out.bits.dma.data_strides := Mux(src.is_dram, srcStrides, dstStrides)
  io.out.bits.dma.metadata_strides := Mux(src.is_dram, srcMetaDataStrides, dstMetaDataStrides)
  connectVecOfVecsOfVecs(io.out.bits.dma.metadata_strides_by_addr, Mux(src.is_dram, srcMetaDataStridesByAddr, dstMetaDataStridesByAddr))
  connectVecOfVecsOfVecs(io.out.bits.dma.metadata_strides_by_value, Mux(src.is_dram, srcMetaDataStridesByValue, dstMetaDataStridesByValue))
  io.out.bits.dma.sram_base_addrs := Mux(dst.is_sram, dstAddrs, srcAddrs)
  io.out.bits.dma.sram_data_strides := Mux(dst.is_sram, dstStrides, srcStrides)
  io.out.bits.dma.sram_metadata_strides := Mux(src.is_sram, srcMetaDataStrides, dstMetaDataStrides)
  io.out.bits.dma.sram_metadata_strides_by_addr := Mux(src.is_sram, srcMetaDataStridesByAddr, dstMetaDataStridesByAddr)
  io.out.bits.dma.sram_should_trail := shouldTrail
  io.out.bits.dma.sram_should_trail_coarse := shouldTrailCoarse
  io.out.bits.dma.sram_code := Mux(dst.is_sram, dst.index, src.index)
  io.out.bits.dma.sram_is_data := isData
  io.out.bits.dma.sram_metadata_buffer_id := metadataBufferId
  io.out.bits.dma.write := dst.is_dram
  io.out.bits.dma.drop_following_cmds_if_empty := dropFollowingCmdsIfEmpty
  io.out.bits.dma.flattened_mvout := dst.is_dram && !vecEqualsU(srcSpans, dstSpans)
  io.out.bits.dma.metadata_regions := dstMetaDataRegions
  when(src.is_dram) {
    io.out.bits.dma.metadata_regions.foreach(_.foreach(_.is_dram := true.B))
  }

  when(io.out.valid && io.out.bits.inst_type === DecoderInst.dma_inst) {
    assert(PopCount(Seq(src.is_dram, dst.is_dram)) === 1.U, "DRAM -> DRAM or SRAM -> SRAM transfers not yet supported")
    when(io.out.bits.dma.flattened_mvout) {
      assert(all(srcSpans.tail.map(_ === 1.U)) && all(srcAxes.map(_ === FiberTreeAxis.Dense)), "incorrect flattened-mvout")
    }.otherwise {
      assert(srcAxes.asUInt === dstAxes.asUInt && srcSpans.asUInt === dstSpans.asUInt)
    }
  }

  connectVecs(io.out.bits.sram_read.address, srcAddrs)
  connectVecs(io.out.bits.sram_read.spans, srcSpans)
  connectVecs(io.out.bits.sram_read.data_strides, srcStrides)
  connectVecOfVecsOfVecs(io.out.bits.sram_read.metadata_strides, srcMetaDataStrides)
  connectVecOfVecsOfVecs(io.out.bits.sram_read.metadata_strides_by_addr, srcMetaDataStridesByAddr)
  io.out.bits.sram_read.iteration_strides.foreach(_ := 1.U)
  io.out.bits.sram_read.should_read_data := isData
  io.out.bits.sram_read.should_read_metadata := !isData
  io.out.bits.sram_read.axis := axis
  io.out.bits.sram_read.metadata_buffer_id := metadataBufferId
  io.out.bits.sram_read.to_dma := false.B
  io.out.bits.sram_read.to_regfile := dst.is_regfile
  io.out.bits.sram_read.to_regfile_last_axis := lastAxis
  io.out.bits.sram_read.to_regfile_last_axis_log_size := lastAxisLogSize; require(lastAxisLogSize.getWidth == io.out.bits.sram_read.to_regfile_last_axis_log_size.getWidth)
  io.out.bits.sram_read.interleave.should_push := interleavePush
  io.out.bits.sram_read.interleave.should_pop := interleavePop
  io.out.bits.sram_read.interleave.axis := interleaveAxis
  io.out.bits.sram_read.should_trail_writes := shouldTrail
  io.out.bits.sram_read.should_trail_writes_coarse_grained := shouldTrailCoarse
  io.out.bits.sram_read.should_gather.foreach(_ := false.B)
  io.out.bits.sram_read.should_gather(axis) := shouldGather
  io.out.bits.sram_read.is_recursive := isRecursive
  io.out.bits.sram_read.recursive_dim := recursiveDim
  io.out.bits.sram_read.reset_running_state := resetRunningState
  io.out.bits.sram_read.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
  io.out.bits.sram_read.independent := false.B
  io.out.bits.sram_read.adjacent := false.B
  io.out.bits.sram_read.update_running_len := false.B
  io.out.bits.sram_read.fused := false.B

  connectVecs(io.out.bits.sram_write.address, dstAddrs)
  connectVecs(io.out.bits.sram_write.spans, dstSpans)
  connectVecs(io.out.bits.sram_write.data_strides, dstStrides)
  connectVecOfVecsOfVecs(io.out.bits.sram_write.metadata_strides, dstMetaDataStrides)
  connectVecOfVecsOfVecs(io.out.bits.sram_write.metadata_strides_by_addr, dstMetaDataStridesByAddr)
  io.out.bits.sram_write.iteration_strides.foreach(_ := 1.U)
  io.out.bits.sram_write.data := DontCare
  io.out.bits.sram_write.is_data := isData
  io.out.bits.sram_write.axis := axis
  io.out.bits.sram_write.metadata_buffer_id := metadataBufferId
  io.out.bits.sram_write.is_both_data_and_metadata := false.B
  connectVecs(io.out.bits.sram_write.from_regfile, (src.index | !src.index.orR).asBools.map(_ && src.is_regfile))
  io.out.bits.sram_write.from_regfile_last_axis := lastAxis
  io.out.bits.sram_write.from_regfile_last_axis_log_size := lastAxisLogSize; require(lastAxisLogSize.getWidth == io.out.bits.sram_write.from_regfile_last_axis_log_size.getWidth)
  io.out.bits.sram_write.interleave.should_push := interleavePush
  io.out.bits.sram_write.interleave.should_pop := interleavePop
  io.out.bits.sram_write.interleave.axis := interleaveAxis
  io.out.bits.sram_write.should_trail_reads := shouldTrail
  io.out.bits.sram_write.should_trail_reads_coarse_grained := shouldTrailCoarse
  io.out.bits.sram_write.should_scatter.foreach(_ := false.B)
  io.out.bits.sram_write.is_recursive := isRecursive
  io.out.bits.sram_write.recursive_dim := recursiveDim
  io.out.bits.sram_write.reset_running_state := resetRunningState
  io.out.bits.sram_write.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))
  io.out.bits.sram_write.from_regfile_metadata.zip(dstFromRfMetadata).foreach { case (x, y) =>
    x.zip(y).foreach { case (x2, y2) =>
      x2 := y2
    }
  }

  when (io.out.fire) {
    done := false.B
  }

  when (io.in.fire) {
    val funct = io.in.bits.inst.funct
    val rs1 = io.in.bits.rs1
    val rs2 = io.in.bits.rs2

    def srcAndDstSetter(_src: Region = rs1.asTypeOf(new Region), _dst: Region = rs2.asTypeOf(new Region)): Unit = {
      src := _src
      dst := _dst

      // Reset various params
      srcAddrs.foreach(_ := 0.U)
      dstAddrs.foreach(_ := 0.U)

      srcDataAddrs.foreach(_ := 0.U)
      dstDataAddrs.foreach(_ := 0.U)

      srcMetaDataAddrs.foreach(_.foreach(_.foreach(_ := 0.U)))
      dstMetaDataAddrs.foreach(_.foreach(_.foreach(_ := 0.U)))

      srcStrides.head := 1.U
      dstStrides.head := 1.U
      srcStrides.tail.foreach(_ := 0.U)
      dstStrides.tail.foreach(_ := 0.U)

      srcMetaDataStrides.foreach(_.foreach(_.foreach(_ := 0.U)))
      dstMetaDataStrides.foreach(_.foreach(_.foreach(_ := 0.U)))
      srcMetaDataStridesByAddr.foreach(_.foreach(_.foreach(_ := 0.U)))
      dstMetaDataStridesByAddr.foreach(_.foreach(_.foreach(_ := 0.U)))
      srcMetaDataStridesByValue.foreach(_.foreach(_.foreach(_ := 0.U)))
      dstMetaDataStridesByValue.foreach(_.foreach(_.foreach(_ := 0.U)))

      dstMetaDataRegions.foreach(_.foreach(_.is_dram := true.B)) // This variable is only relevant to Dma writes

      srcSpans.foreach(_ := 1.U)
      dstSpans.foreach(_ := 1.U)

      srcAxes.foreach(_ := FiberTreeAxis.Dense)
      dstAxes.foreach(_ := FiberTreeAxis.Dense)

      axis := 0.U
      isData := true.B
      resetRunningState := false.B

      interleavePush := false.B
      interleavePop := false.B
      shouldTrail := false.B
      shouldTrailCoarse := false.B
      dropFollowingCmdsIfEmpty := false.B

      isRecursive.foreach(_ := false.B)

      shouldGather := false.B

      lastAxisLogSize := maxVal(lastAxisLogSize)

      dstFromRfMetadata.foreach(_.foreach(_.valid := false.B))

      waits := 0.U.asTypeOf(waits)
    }

    def constantSetter(constant_type: UInt = rs1, value: UInt = rs2): Unit = {
      switch (constant_type) {
        is (ISA.ConstantIsData.U) {
          isData := value
        }

        is (ISA.ConstantAxis.U) {
          axis := value
        }

        is (ISA.ConstantMetadataBufferId.U) {
          metadataBufferId := value
        }

        is (ISA.ConstantLastAxis.U) {
          lastAxis := value
        }

        is (ISA.ConstantInterleavePush.U) {
          interleavePush := value
        }

        is (ISA.ConstantInterleavePop.U) {
          interleavePop := value
        }

        is (ISA.ConstantInterleaveAxis.U) {
          interleaveAxis := value
        }

        is (ISA.ConstantShouldTrail.U) {
          shouldTrail := value
        }

        is (ISA.ConstantResetRunningState.U) {
          resetRunningState := value
        }

        is (ISA.ConstantShouldGather.U) {
          shouldGather := value
        }

        is (ISA.ConstantLastAxisLogSize.U) {
          lastAxisLogSize := value
        }

        is (ISA.ConstantShouldTrailCoarse.U) {
          shouldTrailCoarse := value
        }

        is (ISA.ConstantDropFollowingCmdsIfEmpty.U) {
          dropFollowingCmdsIfEmpty := value
        }
      }
    }

    switch (funct) {
      is (ISA.SetSrcAndDst.U) {
        srcAndDstSetter()
      }

      is (ISA.SetConstant.U) {
        constantSetter()
      }

      is (ISA.SetAddress.U) {
        rs1.asTypeOf(new AxisRs1).setConfig(srcAddrs, dstAddrs, rs2)
      }

      is (ISA.SetSpan.U) {
        rs1.asTypeOf(new AxisRs1).setConfig(srcSpans, dstSpans, rs2)
      }

      is (ISA.SetStride.U) {
        rs1.asTypeOf(new AxisRs1).setConfig(srcStrides, dstStrides, rs2)
      }

      is (ISA.SetAxis.U) {
        rs1.asTypeOf(new AxisRs1).setConfig(srcAxes, dstAxes, rs2, Some(FiberTreeAxis()))
      }

      is (ISA.SetDataOrMetadataAddress.U) {
        val _rs1 = rs1.asTypeOf(new AxisAndMetadataRs1)

        _rs1.setDataConfig(srcDataAddrs, dstDataAddrs, rs2)

        (srcMetaDataAddrs zip dstMetaDataAddrs).foreach { case (src, dst) =>
          _rs1.setMetadataConfig(src, dst, rs2)
        }
      }

      is (ISA.SetMetadataStride.U) {
        rs1.asTypeOf(new TwoAxesAndMetadataRs1).setConfig(srcMetaDataStrides, dstMetaDataStrides, rs2)
      }

      is (ISA.SetMetadataStrideByAddr.U) {
        rs1.asTypeOf(new TwoAxesAndMetadataRs1).setConfig(srcMetaDataStridesByAddr, dstMetaDataStridesByAddr, rs2)
      }

      is (ISA.SetMetadataStrideByValue.U) {
        rs1.asTypeOf(new TwoAxesAndMetadataRs1).setConfig(srcMetaDataStridesByValue, dstMetaDataStridesByValue, rs2)
      }

      is (ISA.SetMetadataRegion.U) {
        val srcMetadataRegions = WireInit(dstMetaDataRegions) // This variable will be thrown away; we only instantiate it now so it can be passed into "setMetadataConfig" below
        rs1.asTypeOf(new AxisAndMetadataRs1).setMetadataConfig(srcMetadataRegions, dstMetaDataRegions, rs2, Some(new Region))
      }

      is (ISA.SetFromRegfileMetadataCoord.U) {
        val _rs2 = rs2.asTypeOf(new ISA.FromRegfileMetadataRs2)
        val __rs2 = Wire(new ChiselSRAMWriteReq.FromRegfileMetadata)
        __rs2.valid := _rs2.valid
        __rs2.coord := _rs2.coord

        val _rs1 = rs1.asTypeOf(new AxisAndMetadataRs1)
        assert(_rs1.for_dst && !_rs1.for_src, "from-regfile-metadata can only be set for the dst")

        dstFromRfMetadata(_rs1.axis_id)(_rs1.metadata_id).valid := _rs2.valid
        dstFromRfMetadata(_rs1.axis_id)(_rs1.metadata_id).coord := _rs2.coord
      }

      is (ISA.SetWaits.U) {
        waits := rs1.asTypeOf(waits)
      }

      is (ISA.SetRecursive.U) {
        val axis = rs1
        val recDim = rs2
        isRecursive(axis) := recDim =/= 0.U
        recursiveDim(axis) := recDim
      }

      is (ISA.LoadSavedConfig.U) {
        val config_id = rs1

        saved_configs.foreach { case (configId, saved_config) =>
          when (config_id === configId.U) {
            saved_config.srcAndDst.foreach { case (_src, _dst) =>
              srcAndDstSetter(_src, _dst)
            }

            saved_config.setVecSavedConfigs(axesVec = Seq(srcAxes, dstAxes), spansVec = Seq(srcSpans, dstSpans),
              metadataStridesVec = Seq(srcMetaDataStrides, dstMetaDataStrides),
              metadataStridesByAddrVec = Seq(srcMetaDataStridesByAddr, dstMetaDataStridesByAddr),
              metadataStridesByValueVec = Seq(srcMetaDataStridesByValue, dstMetaDataStridesByValue),
              metadataRegionsVec = dstMetaDataRegions)

            saved_config.constants.toSeq.foreach { case (constant_type, value) =>
              constantSetter(constant_type.U, value.U)
            }

            saved_config.waits.foreach(waits := _.asTypeOf(waits))

            saved_config.recursive.toSeq.foreach { case (axisId, recursive) =>
              isRecursive(axisId) := (recursive != 0).B
              recursiveDim(axisId) := recursive.U
            }

            saved_config.spansThatWontReset.foreach {
              case (axisId, true) => srcSpans(axisId) := srcSpans(axisId)
              case (axisId, false) => dstSpans(axisId) := dstSpans(axisId)
            }
            saved_config.addrsThatWontReset.foreach {
              case (axisId, isForSrc) =>
                val addrs = if (isForSrc) srcAddrs else dstAddrs
                addrs(axisId) := addrs(axisId)
            }
            saved_config.dataAddrsThatWontReset.foreach {
              case true => srcDataAddrs := srcDataAddrs
              case false => dstDataAddrs := dstDataAddrs
            }
            saved_config.metadataAddrsThatWontReset.foreach {
              case (axisId, metadataBufferId, isForSrc) =>
                (if (isForSrc) srcMetaDataAddrs else dstMetaDataAddrs).foreach(x => x(axisId)(metadataBufferId) := x(axisId)(metadataBufferId))
            }
            saved_config.metadataStridesThatWontReset.foreach {
              case (outerAxisId, innerAxisId, metadataBufferId, isForSrc) =>
                val metadataStrides = if (isForSrc) srcMetaDataStrides else dstMetaDataStrides
                metadataStrides(outerAxisId)(innerAxisId)(metadataBufferId) := metadataStrides(outerAxisId)(innerAxisId)(metadataBufferId)
            }

            if (saved_config.issues) {
              done := true.B
            }
          }
        }
      }

      is (ISA.Issue.U) {
        done := true.B
      }
    }
  }
}
