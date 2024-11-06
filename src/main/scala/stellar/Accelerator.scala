package stellar

import stellar.rtl.ChiselAccelerator

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map => MutMap}

case class CoordLookupFromSpatialArrayIndex(indicesToInputIntoCoordLookup: Seq[(Index, Boolean)],
                                            indicesToInputIntoSpatialArray: Seq[(Index, Int)]) {
  // indicesToInputIntoCoordLookup are of form [(index, shouldConnectExpandedForm)]
  // indicesToInputIntoSpatialArray are of form [(spatialArrayIndex, expandedCoordLookupIndex)]
}
case class CoordLookupFromSramOrIo(compressedCoords: Seq[(Int, Int, Boolean)], expandedCoords: Seq[(Int, Int)]) {
  // compressedCoords = [(coordLookupCompressedCoord, sramOrIoCoord, sramOrIoCoordIsCompressed)]
  // expandedCoords = [(coordLookupExpandedCoord, sramOrIoExpandedCoord)]
}
case class Var2CoordLookupFill(outVar: Output, coordLookup: CoordLookup, compressedCoords: Seq[Index]) {
  // "compressedCoords" are a sequence of domain-indices which we use to construct the compressed coord address
}
case class RegFile2SRAMConn(src: RegFile, dst: SRAM, sramAxisId: Int, reorderSramCoords: Seq[Int], isWrite: Boolean, mem2MemConnType: Mem2MemConnType)

sealed trait Mem2MemConnType
object DataConn extends Mem2MemConnType
case class CoordConn(coord: Int) extends Mem2MemConnType
object NElemLookupConn extends Mem2MemConnType

class Accelerator(withAsserts: Boolean = true) {
  implicit val thisOpt = Some(this)

  private val spatialArrays = ListBuffer.empty[SpatialArray]
  private val regFiles = ListBuffer.empty[RegFile]
  protected[stellar] val srams = ListBuffer.empty[SRAM]
  private val coordLookups = ListBuffer.empty[CoordLookup]
  private val loadBalancers = ListBuffer.empty[LoadBalancer]

  private val var2RegFileConnections = ListBuffer.empty[(Variable, RegFile, Mem2MemConnType, Option[(Int, Option[Int])] /* max-ports */, Option[Int] /* popBits */, Option[Int] /* maxRfOut */)]
  private val sram2RegFileConnections = ListBuffer.empty[(SRAM, RegFile, Seq[Int])] // (src sram, dst rf, reorderSramCoords)
  private val regFile2SRAMConnections = ListBuffer.empty[RegFile2SRAMConn]

  private val coordLookupsFromSramsOrVars = MutMap.empty[(Either[(SRAM, Int /* sramAxisId */), Variable], CoordLookup), CoordLookupFromSramOrIo]
  private val coordLookupFromSpatialArrayIndices = MutMap.empty[(CoordLookup, SpatialArray), CoordLookupFromSpatialArrayIndex]

  private val sram2CoordLookupFills = ListBuffer.empty[(SRAM, CoordLookup, Seq[Int])]
  private val var2CoordLookupFills = ListBuffer.empty[Var2CoordLookupFill]

  private val coordLookupEmpties = MutMap.empty[CoordLookup, Variable]

  def registerSpatialArray(spatialArray: SpatialArray): Unit = {
    spatialArray.block.shouldExpandIndicesFromRf = coordLookups.isEmpty
    spatialArrays += spatialArray
  }
  def registerRegFile(regFile: RegFile): Unit = { regFiles += regFile }
  def registerSRAM(sram: SRAM): Unit = { srams += sram }
  def registerCoordLookup(coordLookup: CoordLookup): Unit = {
    spatialArrays.foreach(_.block.shouldExpandIndicesFromRf = false)
    coordLookups += coordLookup
  }
  def registerLoadBalancer(loadBalancer: LoadBalancer): Unit = { loadBalancers += loadBalancer }

  def connectVarToRegFile(v: Variable, regFile: RegFile, connType: Mem2MemConnType = DataConn, maxOutPorts: Option[(Int, Option[Int])] = None, popBits: Option[Int] = None, maxRfOut: Option[Int] = None): Unit = {
    require(v.isInstanceOf[stellar.Output] || maxOutPorts.isEmpty, "maxOutPorts can only be set for output variables")
    var2RegFileConnections += ((v, regFile, connType, maxOutPorts, popBits, maxRfOut))
  }

  def connectSRAMtoRegFile(src: SRAM, dst: RegFile, reorderCoords: Seq[Int] = Seq.empty): Unit = {
    sram2RegFileConnections += ((src, dst, reorderCoords))
  }

  def connectRegFileToSRAM(src: RegFile, dst: SRAM, stageId: Int = 0, reorderCoords: Seq[Int] = Seq.empty, isWrite: Boolean = true, connType: Mem2MemConnType = DataConn): Unit = {
    regFile2SRAMConnections += RegFile2SRAMConn(src, dst, stageId, reorderCoords, isWrite, connType)
  }

  def fillCoordLookupFromSram(sram: SRAM, coordLookup: CoordLookup, reorderCoords: Seq[Int] = Seq.empty): Unit = {
    sram2CoordLookupFills += ((sram, coordLookup, reorderCoords))
  }

  def fillCoordLookupFromOutput(outVar: Output, coordLookup: CoordLookup, compressedCoords: Seq[Index]): Unit = {
    var2CoordLookupFills += Var2CoordLookupFill(outVar, coordLookup, compressedCoords)
  }

  def connectSram2CoordLookup(sram: SRAM, coordLookup: CoordLookup,
                              compressedCoords: Seq[(Int, Int, Boolean)], expandedCoords: Seq[(Int, Int)],
                              sramAxisId: Int = 0): Unit = {
    coordLookupsFromSramsOrVars += (((Left(sram, sramAxisId), coordLookup) -> CoordLookupFromSramOrIo(compressedCoords, expandedCoords)))
  }

  def connectIo2CoordLookup(variable: Variable, coordLookup: CoordLookup,
                              compressedCoords: Seq[(Int, Int, Boolean)], expandedCoords: Seq[(Int, Int)]): Unit = {
    assert(variable.isIO, "Only input and output variables can be connected to a coord lookup")
    coordLookupsFromSramsOrVars += (((Right(variable), coordLookup) -> CoordLookupFromSramOrIo(compressedCoords, expandedCoords)))
  }

  def connectIndex2CoordLookup(coordLookup: CoordLookup, indicesToInputIntoCoordLookup: Seq[(Index, Boolean)],
                               indicesToInputIntoSpatialArray: Seq[(Index, Int)]): Unit = {
    val indices = indicesToInputIntoCoordLookup.map(_._1) ++ indicesToInputIntoSpatialArray.map(_._1)
    val spArrays = spatialArrays.filter(sa => indices.toSet.subsetOf(sa.indices.toSet))
    assert(spArrays.size == 1, "Indices must all come from at most one spatial array")
    val spatialArray = spArrays.head

    coordLookupFromSpatialArrayIndices += ((coordLookup, spatialArray) ->
      CoordLookupFromSpatialArrayIndex(indicesToInputIntoCoordLookup, indicesToInputIntoSpatialArray))
  }

  def emptyCoordLookupWithVariable(coordLookup: CoordLookup, variable: Variable): Unit = {
    coordLookupEmpties += (coordLookup -> variable)
  }

  def sramCodes = srams.zipWithIndex.toMap
  def loadBalancerCodes = loadBalancers.zipWithIndex.toMap

  def MakeSpatialArray[T <: SpatialArray](spArray: => T) = {
    val result = spArray
    registerSpatialArray(result)
    result
  }

  def toChiselModule(dataWidthBits: Int) = new ChiselAccelerator(
    spatialArrayGens = spatialArrays,
    regFiles = regFiles, coordLookups=coordLookups,
    loadBalancers = loadBalancers, loadBalancerCodes = loadBalancerCodes,
    srams = srams.toSeq, sramCodes = sramCodes,
    var2RegFileConnections = var2RegFileConnections.toSeq,
    sram2RegFileConnections = sram2RegFileConnections,
    regFile2SRAMConnections = regFile2SRAMConnections,
    coordLookupsFromSramsOrVars = coordLookupsFromSramsOrVars,
    coordLookupsFromSpatialArrayIndices = coordLookupFromSpatialArrayIndices,
    sram2CoordLookupFills = sram2CoordLookupFills,
    var2CoordLookupFills = var2CoordLookupFills,
    coordLookupEmpties = coordLookupEmpties,
    dataWidthBits = dataWidthBits,
    withAsserts = withAsserts
  )
}
