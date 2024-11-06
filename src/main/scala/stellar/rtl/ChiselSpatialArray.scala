package stellar.rtl

import scala.math.Ordering.Implicits._

import chisel3._
import chisel3.util._

import stellar._
import stellar.Util.SMap

import ChiselUtil.{all, any, getChiselType, minOf}

class ChiselSpatialArray(genPes: SMap[Seq[Int], (Int, Set[stellar.Input]) => PE], protected[stellar] val its: IterationSpace, val transform: Transform,
                         maxTimePerAxis: Seq[Int], ignoreBaseC2e: Boolean, alwaysStart: Boolean,
                         stallIfAnyInputsAreNotFound: Boolean, onlyCheckIfOutputsAreReady: Boolean,
                         stallForOutputs: Boolean, dataWidthBits: Int, withAsserts: Boolean, val hasSubArrays: Boolean) extends Module {
  val subSpatialArrays = if (hasSubArrays) {
    val clusters = IterationSpace.connectedClusters(its)
    clusters.zipWithIndex.map { case (cluster, clusterId) =>
      val points = cluster.map(_.coords)
      val gen = genPes.filter(t => points contains t._1)
      Module(new SubSpatialArray(gen, its, transform, maxTimePerAxis, ignoreBaseC2e, alwaysStart, stallIfAnyInputsAreNotFound, onlyCheckIfOutputsAreReady, stallForOutputs, dataWidthBits, withAsserts, clusterId))
    }.toSeq.sortBy(_.endingPe.spaceCoords)
  } else
    Seq(Module(new SubSpatialArray(genPes, its, transform, maxTimePerAxis, ignoreBaseC2e, alwaysStart, stallIfAnyInputsAreNotFound, onlyCheckIfOutputsAreReady, stallForOutputs, dataWidthBits, withAsserts, 0)))

  val ioVars = subSpatialArrays.head.ioVars
  require(subSpatialArrays.map(_.ioVars).distinct.size == 1, "sub-spatial-arrays have different ioVars")

  val io = IO(new Bundle {
    val ins = MixedVec(subSpatialArrays.flatMap(_.io.ins).map(getChiselType))
    val outs = MixedVec(subSpatialArrays.flatMap(_.io.outs).map(getChiselType))

    val optSkipBufferIns = chisel3.Input(MixedVec(subSpatialArrays.flatMap(_.io.optSkipBufferIns).map(getChiselType)))
    val optSkipBufferOuts = chisel3.Output(MixedVec(subSpatialArrays.flatMap(_.io.optSkipBufferOuts).map(getChiselType)))

    val loadBalancingMappings = Vec(genPes.size, new LoadBalancingMappings(its.pointsMappings.size))

    val compressed2ExpandedMappings = Vec(subSpatialArrays.map(_.nCompressed2ExpandedMappings).sum, new Compressed2ExpandedMapping(its.indices.size))

    val rfFillCounters = chisel3.Input(Vec(subSpatialArrays.head.ioInVars.size, OpCount())); require(subSpatialArrays.map(_.ioInVars).distinct.size == 1, "sub-spatial-arrays have different rfFillCounters")
    val clFillCounter = Flipped(Valid(OpCount()))

    val last_in_or_out = chisel3.Output(Vec(ioVars.size, Vec(genPes.size, Valid(OpCount()))))

    val ending = chisel3.Output(Bool())
    val busy = chisel3.Output(Bool())
  })

  def getInPortsForVar(v: stellar.Input, ioIns: Seq[SpatialArrayInPort] = io.ins.toSeq): Iterable[SpatialArrayInPort] = {
    ioIns.zip(subSpatialArrays.flatMap { spArray =>
      spArray.io.ins.map(spArray.getInPortsForVar(v).toSeq.contains)
    }).collect { case (p, true) => p }
  }

  def getOutPortsForVar(v: stellar.Output, ioOuts: Seq[DecoupledIO[SpatialArrayOutPort]] = io.outs.toSeq): Iterable[DecoupledIO[SpatialArrayOutPort]] = {
    ioOuts.zip(subSpatialArrays.flatMap { spArray =>
      spArray.io.outs.map(spArray.getOutPortsForVar(v).toSeq.contains)
    }).collect { case (p, true) => p }
  }

  def getLastPortsForVar(v: stellar.Variable, ioLasts: Vec[Vec[Valid[OpCount]]] = io.last_in_or_out): Iterable[Valid[OpCount]] = ioLasts(ioVars.indexOf(v))

  def getOptSkipBufferIns: Iterable[(stellar.Variable, Vec[OptimisticSkipInPort], Compressed2ExpandedMapping)] = {
    val flattenedSpInPorts = subSpatialArrays.flatMap(_.io.optSkipBufferIns)
    val flattenedSpC2Es = subSpatialArrays.flatMap(_.io.compressed2ExpandedMappings)
    subSpatialArrays.flatMap(_.getOptSkipBufferIns).map { case (ioVar, spInPorts, spComp2ExpMappings) =>
      (ioVar,
        io.optSkipBufferIns(flattenedSpInPorts.indexOf(spInPorts)),
        io.compressed2ExpandedMappings(flattenedSpC2Es.indexOf(spComp2ExpMappings)))
    }
  }

  subSpatialArrays.flatMap(_.io.ins).zip(io.ins).foreach { case (spArrayIoPort, ioPort) =>
    ioPort <> spArrayIoPort
    ioPort.last_in_axis.tail.foreach(_ := false.B); if (withAsserts) assert(!any(spArrayIoPort.last_in_axis.tail), "for now, to help the const-prop do it's job, we manually set outer last-in-axis flags to false.B")
    ioPort.hardCodedCoords = spArrayIoPort.hardCodedCoords
  }

  subSpatialArrays.flatMap(_.io.outs).zip(io.outs).foreach { case (spArrayIoPort, ioPort) =>
    ioPort <> spArrayIoPort
    ioPort.bits.hardCodedCoords = spArrayIoPort.bits.hardCodedCoords
    ioPort.bits.hardCodedDomainCoords = spArrayIoPort.bits.hardCodedDomainCoords
  }

  subSpatialArrays.flatMap(_.io.optSkipBufferIns).zip(io.optSkipBufferIns).foreach { case (spArrayIoPort, ioPort) =>
    spArrayIoPort := ioPort
  }

  subSpatialArrays.flatMap(_.io.optSkipBufferOuts).zip(io.optSkipBufferOuts).foreach { case (spArrayIoPort, ioPort) =>
    ioPort := spArrayIoPort
  }

  subSpatialArrays.flatMap(_.io.loadBalancingMappings).zip(io.loadBalancingMappings).foreach { case (spArrayIoPort, ioPort) =>
    ioPort <> spArrayIoPort
  }

  subSpatialArrays.flatMap(_.io.compressed2ExpandedMappings).zip(io.compressed2ExpandedMappings).foreach { case (spArrayIoPort, ioPort) =>
    ioPort <> spArrayIoPort
    ioPort.relevantIndices = spArrayIoPort.relevantIndices
  }

  subSpatialArrays.foreach { spArray =>
    spArray.io.rfFillCounters := io.rfFillCounters
    spArray.io.clFillCounter := io.clFillCounter
  }

  val trailingSubArrayIsEnding = {
    val busies = subSpatialArrays.map(_.io.busy)
    val endings = subSpatialArrays.map(_.io.ending)
    val earliestOpCount = minOf(endings.map(_.bits):_*)

    val noOtherTrailingSubArrays = PopCount(busies.zip(endings).map { case (b,e) => b && !e.valid && e.bits === earliestOpCount }) === 0.U

    endings.map(e => e.valid && e.bits === earliestOpCount && noOtherTrailingSubArrays)
  }

  io.ending := any(trailingSubArrayIsEnding)

  io.last_in_or_out.zipWithIndex.foreach { case (ioLasts, i) =>
    // TODO can the 'last_in_or_out' port can be true before the trailingSubArray is actually ending?
    subSpatialArrays.zip(trailingSubArrayIsEnding).flatMap(t => t._1.io.last_in_or_out(i).map((_,t._2))).zip(ioLasts).foreach { case ((spArrayIoPort, isEnding), ioPort) =>
      ioPort.valid := all(Seq(spArrayIoPort.valid) ++ Option.when(subSpatialArrays.size > 1)(isEnding))
      ioPort.bits := spArrayIoPort.bits
    }
  }

  io.busy := any(subSpatialArrays.map(_.io.busy))
}
