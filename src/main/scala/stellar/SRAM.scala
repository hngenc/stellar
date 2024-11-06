package stellar

import scala.collection.mutable.ArrayBuffer

import chisel3._

import stellar.rtl.{ChiselSRAM, ChiselSRAMPipelineData, ChiselSRAMReadReq, ChiselSRAMWriteReq}
import Util.SMap

class SRAM(val elemT: SInt, val nElems: Int, val elemsPerRead: Int, val elemsPerWrite: Int,
           protected[stellar] val axes: Seq[FiberTreeAxis.Type], protected[stellar] val metadatas: Seq[FiberTreeAxisMetadata],
           branchSizes: Seq[Int] = Seq.empty, readBranchSizes: Option[Seq[Int]] = None, writeBranchSizes: Option[Seq[Int]] = None,
           val nBanks: Int = 1, val readBankingStrategies: Seq[ReadBankingStrategy] = Seq.empty, val writeBankingStrategies: Seq[WriteBankingStrategy[SInt]] = Seq.empty,
           val maxElemsInRf: Option[Int] = None, multipleNElemsLookupPorts: Boolean = false,
           elemsPerRowMultiplier: Int = 1, bankMultiplier: Int = 1, dont_prevent_simultaneous_accesses: Boolean = false,
           no_simultaneous_reads_and_writes: Boolean = false, pipeline_stages: Boolean = false,
           hardCodedValues: ChiselSRAMPipelineData[SInt] => SMap[Data, Data] = {_: ChiselSRAMPipelineData[SInt] => scala.collection.Map.empty},
           stridesDivisibleBy: SMap[Int, Int] = Seq.empty.toMap,
           elemsPerWriteRf: Int = -1, independentBanks: Boolean = false,
           dummyData: Boolean = false, dummyReadStages: Boolean = false,
           val zeroedRegfileCoords: Int = 0,
           val name: Option[String] = None)
          (implicit accelerator: Option[Accelerator] = None) {
  def toChiselModule =
    new ChiselSRAM(
      elemT=elemT,
      nElems=nElems,
      elemsPerRead=elemsPerRead,
      elemsPerWrite=elemsPerWrite,
      axes=axes,
      metadatas=metadatas,
      branchSizes=branchSizes,
      readBranchSizes=readBranchSizes,
      writeBranchSizes=writeBranchSizes,
      nBanks=nBanks,
      readBankingStrategies=readBankingStrategies,
      writeBankingStrategies=writeBankingStrategies,
      maxElemsInRf=maxElemsInRf,
      multipleNElemsLookupPorts=multipleNElemsLookupPorts,
      elemsPerRowMultiplier=elemsPerRowMultiplier,
      bankMultiplier=bankMultiplier,
      dont_prevent_simultaneous_accesses=dont_prevent_simultaneous_accesses,
      no_simultaneous_reads_and_writes=no_simultaneous_reads_and_writes,
      pipeline_stages=pipeline_stages,
      hardCodedValues=hardCodedValues,
      stridesDivisibleBy=stridesDivisibleBy,
      elemsPerWriteRf=elemsPerWriteRf,
      independentBanks=independentBanks,
      dummyData=dummyData,
      dummyReadStages=dummyReadStages,
      nameOpt=name,
    )

  // TODO replace all uses of "postProcess" with uses of "hardCodedValues"
  val postProcess: ArrayBuffer[ChiselSRAM[chisel3.SInt] => Unit] = ArrayBuffer({ x =>
    import chisel3._
    (Seq(x.io.write.bits.metadata_strides, x.io.write.bits.metadata_strides_by_addr) ++ x.io.read_reqs.map(_.bits.metadata_strides) ++ x.io.read_reqs.map(_.bits.metadata_strides_by_addr)).foreach { strides =>
      axes.zipWithIndex.filter(x => (x._1: FiberTreeAxis.Type) == FiberTreeAxis.Dense).foreach { case (_, axisId) =>
        strides.foreach(_(axisId).foreach(_ := 0.U))
      }
      strides.zipWithIndex.foreach { case (stridess, axisId) =>
        stridess.drop(axisId+1).foreach(_.foreach(_ := 0.U))
      }
    }
  })

  accelerator.foreach(_.registerSRAM(this))
}

case class ReadBankingStrategy(condition: (ChiselSRAMReadReq => Seq[(Bool, ChiselSRAMReadReq => Unit)]))
case class WriteBankingStrategy[T <: Data](condition: (ChiselSRAMWriteReq[T] => Seq[(Bool, ChiselSRAMWriteReq[T] => Unit)]))
