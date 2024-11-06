package stellar

import stellar.Util.SMap
import stellar.rtl._

class LoadBalancer(val spatialArray: SpatialArray, nAxes: Int, nOpCounts: Int,
                   cl_in_innermost_axes: SMap[Index, Index] = Seq.empty.toMap,
                   nonWaitingInVars: Set[stellar.Input] = Set.empty, instantUpdateIndices: Set[Index] = Set.empty,
                   loadbalancingAcrossOpCounts: Boolean = true,
                  )(implicit accelerator: Option[Accelerator] = None) {

  def toChiselModule(rfInPorts: SMap[stellar.Input, (SpatialArrayOutPort, Int)],
                     rfUpdatePorts: SMap[stellar.Input, (RegfileUpdatePort, Int)],
                     nOutputPorts: Int, nClInPortsPerIndex: Int, nClUpdatePortsPerIndex: Int) = new ChiselLoadBalancer(
    nAxes=nAxes, nOpCounts=nOpCounts, nOutputPorts=nOutputPorts,
    rfInPorts=rfInPorts, rfUpdatePorts=rfUpdatePorts,
    nClInPortsPerIndex=nClInPortsPerIndex, nClUpdatePortsPerIndex=nClUpdatePortsPerIndex,
    its=spatialArray.block.rangeSpaceItsOpt.get, transform=spatialArray.block.transform.get,
    cl_in_innermost_axes=cl_in_innermost_axes,
    nonWaitingInVars=nonWaitingInVars, instantUpdateIndices=instantUpdateIndices,
    loadbalancingAcrossOpCounts=loadbalancingAcrossOpCounts)

  accelerator.foreach(_.registerLoadBalancer(this))
}
