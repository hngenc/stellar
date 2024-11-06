package stellar

import stellar.rtl.ChiselRegFile

sealed abstract class RfEntryExitOption
object RfEntryExitOption {
  object Anywhere extends RfEntryExitOption
  case class Incrementing(skipOccupiedElems: Boolean = false) extends RfEntryExitOption
  case class Edge(shiftIfAnyFreeSpot: Boolean = false) extends RfEntryExitOption
  object PerpendicularEdge extends RfEntryExitOption // This option is only valid for outputs
}

class RegFile(val nElems: Int, val nIOCoords: Int, nDomainCoords: Int,
              val nPipelinesOpt: Option[Int] = None, triangular: Option[Boolean] = None, nSubArrays: Int = 1,
              val lastInAxisFlags: Seq[(Int, Int)] = Seq.empty,
              lastInAxisFlagsFromOutPorts: Boolean = false,
              separateLastInAxisFlagsForEachPipeline: Boolean = false,
              resetLastInAxisFlagsBasedOnTravellingOpCounts: Boolean = false,
              dontCheckExtraLastInAxisFlag: Boolean = false,
              unavailableIfSmallerOpt: Option[(Int, Boolean, Set[Int])] = None,

              val nameOpt: Option[String] = None,

              entryOption: RfEntryExitOption = RfEntryExitOption.Incrementing(), exitOption: RfEntryExitOption = RfEntryExitOption.Anywhere,
              subPipelineOpt: Option[(Int, Int, Boolean, Boolean)] = None, transposingPipelines: Option[Boolean] = None,
              lockstepIns: Boolean = false, lockstepOuts: (Int, Int) = (0, 0),
              groupedInPortStride: Int = 1,

              constantCoordsForInputs: Seq[Seq[Option[Int]]] = Seq.empty,
              coordIsDivisibleByForPipelines: Seq[Seq[Option[(Int, Int)]]] = Seq.empty,
              incrementingCoordsForInputs: Set[(Int, Boolean)] = Set.empty,
              maxOutCoordOpt: Option[Int] = None, coordsToIgnoreForOutputs: Set[Int] = Set.empty,
              val domainCoordsToUseForOutputs: Util.SMap[Int, Int] = scala.collection.Map.empty,
              checkOpCountForOuts: Boolean = true,

              allElemsWillBePoppedBeforeLastOut: Boolean = true, getLastInFromInOpCounts: Boolean = false,
              getTravellingOpCountFromInPorts: Boolean = false, val getTravellingCoordsFromInPorts: Set[Int] = Set.empty,

              stickyOutPortCoords: Option[(Set[Int], Int, Int)] = None, val stickyInPortCoords: Set[Int] = Set.empty,

              nElemsLookupPortsCoordsOpt: Option[Seq[Int]] = None,

              nUpdatePorts: Int = 0,

              dummyData: Boolean = false,

              val automaticallyOptimize: Boolean = false,
             )
             (implicit accelerator: Option[Accelerator] = None) {
  // TODO nUpdatePorts should be calculated automatically by Stellar, rather than needing to be supplied
  //  by the programmer
  // TODO nDomainCoords is actually pretty useless; we should get rid of that argument
  def toChiselModule(nInPorts: Int, nOutPorts: Int, dataWidthBits: Int, nElemsLookupPorts: Int = 0, constantCoordsForOutputs: Seq[Seq[Option[Int]]] = Seq.empty, constantCoordsForNElemsLookups: Seq[Seq[Option[Int]]] = Seq.empty, coordsThatNeedExpanding: Set[Int] = Set.empty,

                     entryOptionOpt: Option[RfEntryExitOption] = None, exitOptionOpt: Option[RfEntryExitOption] = None,
                     lockstepInsOpt: Option[Boolean] = None,
                     constantCoordsForInputsOpt: Option[Seq[Seq[Option[Int]]]] = None,
                     incrementingCoordsForInputsOpt: Option[Set[(Int, Boolean)]] = None,
                     maxOutCoordOptOpt: Option[Option[Int]] = None,
                     coordsToIgnoreForOutputsOpt: Option[Set[Int]] = None,
                     triangularOpt: Option[Option[Boolean]] = None,
                     lockstepOutsOpt: Option[(Int, Int)] = None,
                     lastInAxisFlagsOpt: Option[Seq[(Int, Int)]] = None,
                     subPipelineOptOpt: Option[Option[(Int, Int, Boolean, Boolean)]] = None,
                     getLastInFromInOpCountsOpt: Option[Boolean] = None,
                     getTravellingOpCountFromInPortsOpt: Option[Boolean] = None,
                     nPipelinesOptOpt: Option[Option[Int]] = None,
                     separateLastInAxisFlagsForEachPipelineOpt: Option[Boolean] = None,
                     resetLastInAxisFlagsBasedOnTravellingOpCountsOpt: Option[Boolean] = None,

                     withAsserts: Boolean = true) = new ChiselRegFile(
    nElems=nElems,

    nInPorts=nInPorts,
    nOutPorts=nOutPorts,
    nUpdatePorts=nUpdatePorts,
    nPipelinesOpt = nPipelinesOptOpt.getOrElse(nPipelinesOpt),
    triangular = triangularOpt.getOrElse(triangular),

    nIOCoords=nIOCoords,
    nDomainCoords=nDomainCoords,

    nSubArrays = nSubArrays,

    lastInAxisFlags = lastInAxisFlagsOpt.getOrElse(lastInAxisFlags),
    lastInAxisFlagsFromOutPorts = lastInAxisFlagsFromOutPorts,
    separateLastInAxisFlagsForEachPipeline = separateLastInAxisFlagsForEachPipelineOpt.getOrElse(separateLastInAxisFlagsForEachPipeline),
    resetLastInAxisFlagsBasedOnTravellingOpCounts = resetLastInAxisFlagsBasedOnTravellingOpCountsOpt.getOrElse(resetLastInAxisFlagsBasedOnTravellingOpCounts),
    dontCheckExtraLastInAxisFlag = dontCheckExtraLastInAxisFlag,
    unavailableIfSmallerOpt = unavailableIfSmallerOpt,

    subPipelineOpt = subPipelineOptOpt.getOrElse(subPipelineOpt),
    entryOption = entryOptionOpt.getOrElse(entryOption), exitOption = exitOptionOpt.getOrElse(exitOption),
    transposingPipelinesOpt = transposingPipelines,
    lockstepIns = lockstepInsOpt.getOrElse(lockstepIns), lockstepOuts = lockstepOutsOpt.getOrElse(lockstepOuts),
    groupedInPortStride = groupedInPortStride,

    constantCoordsForInputs = constantCoordsForInputsOpt.getOrElse(constantCoordsForInputs),
    constantCoordsForOutputs = constantCoordsForOutputs,
    constantCoordsForNElemsLookups = constantCoordsForNElemsLookups,
    coordIsDivisibleByForPipelines = coordIsDivisibleByForPipelines,
    incrementingCoordsForInputs = incrementingCoordsForInputsOpt.getOrElse(incrementingCoordsForInputs),
    maxOutCoordOpt = maxOutCoordOptOpt.getOrElse(maxOutCoordOpt),
    coordsToIgnoreForOutputs = coordsToIgnoreForOutputsOpt.getOrElse(coordsToIgnoreForOutputs),
    domainCoordsToUseForOutputs = domainCoordsToUseForOutputs,
    checkOpCountForOuts = checkOpCountForOuts,

    allElemsWillBePoppedBeforeLastOut = allElemsWillBePoppedBeforeLastOut,
    getLastInFromInOpCounts = getLastInFromInOpCountsOpt.getOrElse(getLastInFromInOpCounts),
    getTravellingOpCountFromInPorts = getTravellingOpCountFromInPortsOpt.getOrElse(getTravellingOpCountFromInPorts),
    getTravellingCoordsFromInPorts = getTravellingCoordsFromInPorts,

    stickyOutPortCoords = stickyOutPortCoords,
    stickyInPortCoords = stickyInPortCoords,

    coordsThatNeedExpanding = coordsThatNeedExpanding,

    dataWidthBits = dataWidthBits,

    nElemsLookupPorts = nElemsLookupPorts,
    nElemsLookupPortsCoordsOpt = nElemsLookupPortsCoordsOpt,

    dummyData = dummyData, withAsserts = withAsserts,
  )

  accelerator.foreach(_.registerRegFile(this))
}
