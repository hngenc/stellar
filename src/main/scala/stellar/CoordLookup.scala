package stellar

import chisel3._
import stellar.rtl.ChiselCoordLookup

class CoordLookup(nElems: Int, val nCoords: Int,
                  val lastInAxisFlags: Seq[(Int, Int)] = Seq.empty, nSubArrays: Int = 1,

                  entryOption: RfEntryExitOption = RfEntryExitOption.Incrementing(), exitOption: RfEntryExitOption = RfEntryExitOption.Anywhere,
                  subPipelineOpt: Option[(Int, Int, Boolean, Boolean)] = None, lockstep: Boolean = false,

                  unavailableIfSmaller: Boolean = false,
                  popIfEqual: Boolean = true, popIfSmallerThanHead: Seq[Int] = Seq.empty,

                  coordsToIgnoreForOutputs: Set[Int] = Set.empty,

                  nUpdatePorts: Int = 0,

                  val name: Option[String] = None,
                 )(implicit accelerator: Option[Accelerator] = None) {
  private val coordT = SInt(32.W)

  // TODO nInPorts and nUpdatePorts should be calculated automatically by Stellar, rather than needing to be supplied
  //  by the programmer
  def toChiselModule(nInPorts: Int, nOutPorts: Int, withAsserts: Boolean = true) = new ChiselCoordLookup(
    nElems=nElems,
    nInPorts=nInPorts,
    nOutPorts=nOutPorts,
    nUpdatePorts=nUpdatePorts,
    coordT=coordT,
    nCoords=nCoords,
    lastInAxisFlags=lastInAxisFlags,
    nSubArrays=nSubArrays,
    entryOption = entryOption,
    exitOption = exitOption,
    subPipelineOpt=subPipelineOpt,
    lockstep=lockstep,
    unavailableIfSmaller=unavailableIfSmaller,
    popIfEqual=popIfEqual,
    popIfSmallerThanHead=popIfSmallerThanHead,
    coordsToIgnoreForOutputs=coordsToIgnoreForOutputs,
    withAsserts=withAsserts,
  )

  accelerator.foreach(_.registerCoordLookup(this))
}
