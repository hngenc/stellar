package stellar.rtl

import scala.math.Ordering.Implicits._
import chisel3._
import chisel3.util._
import stellar._
import stellar.Util.SMap
import ChiselUtil.{all, any, compareVecsSigned, minOf, vecEquals}

class SubSpatialArray(genPes: SMap[Seq[Int], (Int, Set[stellar.Input]) => PE], its: IterationSpace, transform: Transform,
                      maxTimePerAxis: Seq[Int], ignoreBaseC2e: Boolean, alwaysStart: Boolean,
                      stallIfAnyInputsAreNotFound: Boolean, onlyCheckIfOutputsAreReady: Boolean,
                      stallForOutputs: Boolean, dataWidthBits: Int, withAsserts: Boolean, subSpatialArrayId: Int) extends Module
{
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  val nDomainCoords = its.indices.size

  val inVarsToAlwaysPop = {
    val inVars = its.ioConns.map(_.ioVar).collect { case inVar: stellar.Input => inVar }
    inVars.filter { inVar =>
      val ioConns = its.ioConns.filter(_.ioVar == inVar)

      val popAlreadySpecified = inVar.popConds.nonEmpty
      val allCoordsKnownAtElaborationTime = ioConns.forall(_.ioIndex.forall(_.isInstanceOf[Const]))
      val allIoConnsUnique = ioConns.forall { ioc: IOConn =>
        ioConns.filter(_.time != ioc.time).forall(_.ioIndex != ioc.ioIndex)
      }

      // println(s"DEBUG $inVar | allCoordsKnownAtElaborationTime = $allCoordsKnownAtElaborationTime | allIoConnsUnique = $allIoConnsUnique | popAlreadySpecified = $popAlreadySpecified\n\tioConns = $ioConns")
      allCoordsKnownAtElaborationTime && allIoConnsUnique && !popAlreadySpecified
    }
  }
  val inVarsToNeverPop = {
    val inVars = its.ioConns.map(_.ioVar).collect { case inVar: stellar.Input => inVar }
    inVars.filter { inVar =>
      !inVarsToAlwaysPop.contains(inVar) && inVar.popConds.isEmpty
    }
  }
  // println(s"DEBUG inVarsToAlwaysPop = $inVarsToAlwaysPop | inVarsToNeverPop = $inVarsToNeverPop")

  // Instantiate PEs
  val pes: SMap[Seq[Int], PE] = genPes.toSeq.map { case (coords, gen) =>
    val pe = Module(gen(dataWidthBits, inVarsToNeverPop)).suggestName(s"PE_${coords.mkString("_")}")
    (coords, pe)
  }.toMap

  val (pesSortedCoords, pesSorted) = pes.toSeq.sortBy(_._1).unzip

  val startingPe = pesSorted.minBy(_.minTimeForPe)
  val endingPe = pesSorted.reverse.maxBy(_.minTimeForPe)

  val inPorts = pesSorted.flatMap(pe => pe.io.ioIns.toSeq.map(in => (pe.spaceCoords,in)))
  val outPorts = pesSorted.flatMap(pe => pe.io.ioOuts.toSeq.map(out => (pe.spaceCoords,out)))

  // Instantiate optimistic skip buffers
  val ioInputOptSkipConns = its.topOptimisticSkipConns.filter(c => c.isIO && c.isInput).toSeq
  val ioInputOptSkipBuffers = ioInputOptSkipConns.map { optimisticSkip =>
    val compressdDim = optimisticSkip.ioPoints.size
    val expandedDim = if (compressdDim == 1) 1 else 4 // TODO
    Module(new OptimisticSkipBuffer(optimisticSkip, indicesDim = its.indices.size,
      compressedDim = compressdDim, expandedDim = expandedDim))
  }

  val ioOutputOptSkipConns = its.topOptimisticSkipConns.filter(c => c.isIO && c.isOutput).toSeq
  val ioOutputOptSkipBuffers = ioOutputOptSkipConns.map { optimisticSkip =>
    val compressdDim = optimisticSkip.ioPoints.size
    val expandedDim = if (compressdDim == 1) 1 else 4 // TODO
    Module(new OptimisticSkipBuffer(optimisticSkip, indicesDim = its.indices.size,
      compressedDim = compressdDim, expandedDim = expandedDim))
  }

  val pe2peOptSkipConns = its.topOptimisticSkipConns.filter(c => !c.isIO).toSeq
  val pe2peOptSkipBuffers = pe2peOptSkipConns.map { optimisticSkip =>
    val compressdDim = optimisticSkip.dstPoints.size
    val expandedDim = if (compressdDim == 1) 1 else 4 // TODO
    Module(new OptimisticSkipBuffer(optimisticSkip, indicesDim = its.indices.size,
      compressedDim = compressdDim, expandedDim = expandedDim))
  }

  private def getOptSkipBuffer(conn: OptimisticSkipConn): OptimisticSkipBuffer = {
    if (ioInputOptSkipConns.contains(conn)) {
      val index = ioInputOptSkipConns.indexOf(conn)
      ioInputOptSkipBuffers(index)
    } else if (pe2peOptSkipConns.contains(conn)) {
      val index = pe2peOptSkipConns.indexOf(conn)
      pe2peOptSkipBuffers(index)
    } else
      throw new Exception("skip buffer is not in either ioInput or pe2pe groups")
  }

  // Instantiate module IOs
  val nCompressed2ExpandedMappings = pesSorted.map { pe =>
    pe.io.ioCompressed2ExpandedMappings.size + (if (ignoreBaseC2e) 0 else 1)
  }.sum

  val ioLastTimes = IterationSpace.ioLastTimes(its)
  val ioVars = ioLastTimes.keys.toSet.toSeq
  val ioInVars = ioVars.collect { case in: stellar.Input => in }
  val ioOutVars = ioVars.collect { case out: stellar.Output => out }

  val io = IO(new Bundle {
    val ins = MixedVec(inPorts.unzip._2.map(inp => new SpatialArrayInPort(inp.nIOCoords, dataWidthBits = dataWidthBits)))
    val outs = MixedVec(outPorts.unzip._2.map(outp => Decoupled(
      new SpatialArrayOutPort(outp.bits.nIOCoords,outp.bits.nDomainCoords, dataWidthBits = dataWidthBits))))

    val optSkipBufferIns = chisel3.Input(MixedVec(ioInputOptSkipBuffers.map(b => Vec(b.ioInsDim, new OptimisticSkipInPort(b.dataDim)))))
    val optSkipBufferOuts = chisel3.Output(MixedVec(ioOutputOptSkipBuffers.map(b => Vec(b.ioOutsDim, new OptimisticSkipInPort(b.dataDim)))))

    val loadBalancingMappings = Vec(pes.size, new LoadBalancingMappings(its.pointsMappings.size))

    val compressed2ExpandedMappings = Vec(nCompressed2ExpandedMappings, new Compressed2ExpandedMapping(nDomainCoords))

    val rfFillCounters = chisel3.Input(Vec(ioInVars.size, OpCount()))
    val clFillCounter = Flipped(Valid(OpCount()))

    val last_in_or_out = chisel3.Output(Vec(ioVars.size, Vec(pes.size, Valid(OpCount()))))

    val ending = Valid(OpCount())
    val busy = chisel3.Output(Bool())
  })

  def getInPortsForVar(v: stellar.Input): Iterable[SpatialArrayInPort] = {
    val pesInPorts = pesSorted.flatMap(_.getIOInputs(v))
    val allPEInPorts = inPorts.map(_._2)

    pesInPorts.map(allPEInPorts.indexOf).filter(_ >= 0).map(io.ins(_))
  }

  def getOutPortsForVar(v: stellar.Output): Iterable[DecoupledIO[SpatialArrayOutPort]] = {
    val pesOutPorts = pesSorted.flatMap(_.getIOOutputs(v))
    val allPEOutPorts = outPorts.map(_._2)

    pesOutPorts.map(allPEOutPorts.indexOf).filter(_ >= 0).map(io.outs(_))
  }

  def getLastPortsForVar(v: stellar.Variable): Iterable[Valid[OpCount]] = io.last_in_or_out(ioVars.indexOf(v))

  // Stall PEs when any of the inPorts finds that an input isn't available yet
  val smallerGlobalSignals = false
  // val stalling_in_port_id = WireInit(io.ins.size.U); dontTouch(stalling_in_port_id) // Debugging signal
  val stalling_for_ins = WireInit({
    val ioInVarsWhichCanCauseStalls = {
      // Find all the inVars for which the programmer has not assumed responsibility of checking for stalls themselves
      val asgnSrcs = its.pointAssignments.map(_.assignment.src)
      ioInVars.filter(inVar => !asgnSrcs.exists(Passes.containsFound(_, inVar)))
    }.toSet
    var inPortsToWatch = pesSorted.flatMap(pe => ioInVarsWhichCanCauseStalls.flatMap(pe.getIOInputs))
    if (smallerGlobalSignals)
      inPortsToWatch = inPortsToWatch.take(pesSorted.head.io.ioIns.size)

    if (stallIfAnyInputsAreNotFound) {
      // stalling_in_port_id := PriorityEncoder(inPortsToWatch.map(in => in.valid && !in.found))
      any(inPortsToWatch.map(in => in.valid && !in.found))
    } else {
      // We stall only if _every_ input that is being requested can't be found. However, if we're trying to end the
      // overall opCount, then we do need to fall-back to the original method of testing for stalls
      Mux(any(pesSorted.map(_.io.isEnding)), any(inPortsToWatch.map(in => in.valid && !in.found)),
        any(inPortsToWatch.map(_.valid)) && all(inPortsToWatch.map(in => !in.valid || !in.found)))
    }})
  val stalling_for_outs = WireInit(if (smallerGlobalSignals) any(pesSorted.lift(15*16).getOrElse(pesSorted.head).io.ioOuts.map(out => (onlyCheckIfOutputsAreReady.B || out.valid) && !out.ready))
    else any(pesSorted.flatMap(_.io.ioOuts.map(out => (onlyCheckIfOutputsAreReady.B || out.valid) && !out.ready))))
  val stalling_for_c2es = WireInit(any(io.compressed2ExpandedMappings.map(m => m.valid && !m.found.get)))
  val stalling_for_lbs = WireInit(any(pesSorted.flatMap(_.io.loadBalancingMappings.configs.map(x => x.ready && !x.valid))))
  dontTouch(stalling_for_ins)
  dontTouch(stalling_for_outs)
  val stall = any(Seq(stalling_for_ins) ++ Option.when(stallForOutputs)(stalling_for_outs).toSeq ++ Seq(stalling_for_c2es, stalling_for_lbs))
  val stall_full = any(Seq(stalling_for_ins, stalling_for_outs, stalling_for_c2es, stalling_for_lbs))
  pesSorted.foreach(_.io.stall := stall)
  pesSorted.foreach(_.io.outs_stalling := stalling_for_outs)

  val stall_cycles = RegInit(0.U(32.W))
  stall_cycles := Mux(io.busy && stall, stall_cycles + 1.U, 0.U)
  if (!alwaysStart) assert(stall_cycles < 1000.U, "spatial array is stalling")

  // Connect Point2PointConns
  val disconnectedPe2PeInPort = scala.collection.mutable.Set.from(pesSorted.flatMap(_.io.pe2peIns))
  its.topP2pConns.filter(c => genPes.keys.toSeq contains c.src.coords).foreach { p2pConn =>
    val dstPE = pes(p2pConn.dst.coords)
    val srcPE = pes(p2pConn.src.coords)

    val dstPort = dstPE.getPe2PeInput(p2pConn.srcIndexed)
    val srcPort = srcPE.getPe2PeOutput(p2pConn.srcVar)

    val delay = p2pConn.delay.zip(0 +: maxTimePerAxis).map { case (d, t) => d * (t + 1) }.sum
    assert(delay >= 0, "time travel not yet supported")

    dstPort := ShiftRegister(srcPort, delay, !stall)

    disconnectedPe2PeInPort.remove(dstPort)
  }
  disconnectedPe2PeInPort.foreach(_ := DontCare)

  // Connect IOConns
  io.ins.zip(inPorts).foreach { case (ioIn, (_, peIn)) =>
    val shouldAlwaysPop = inVarsToAlwaysPop.exists(inVar => pesSorted.exists(pe => pe.getIOInputs(inVar).contains(peIn)))

    peIn.data := ioIn.data
    peIn.expanded_coords := ioIn.expanded_coords
    peIn.found := ioIn.found
    peIn.unavailable := ioIn.unavailable
    peIn.axis_spans := ioIn.axis_spans
    ioIn.valid := peIn.valid
    ioIn.coords := peIn.coords
    ioIn.op_count := peIn.op_count
    ioIn.pop.valid := (if (shouldAlwaysPop) { /*println(s"\tDEBUG always popping a port");*/ peIn.valid } else peIn.pop.valid) && !stall_full
    ioIn.pop.bits := subSpatialArrayId.U
    ioIn.last_in_axis := peIn.last_in_axis.map(_ && !stall_full)

    ioIn.hardCodedCoords = peIn.hardCodedCoords
  }

  io.outs.zip(outPorts).foreach { case (ioOut, (_, peOut)) =>
    ioOut <> peOut
    when (stall_full) {
      ioOut.valid := false.B
    }

    ioOut.bits.hardCodedCoords = peOut.bits.hardCodedCoords
    ioOut.bits.hardCodedDomainCoords = peOut.bits.hardCodedDomainCoords
  }

  // Connect load-balancing mappings
  io.loadBalancingMappings.zip(pesSorted).foreach { case (ioMapping, pe) =>
    ioMapping.opCount := pe.io.loadBalancingMappings.opCount
    ioMapping.configs.zip(pe.io.loadBalancingMappings.configs).foreach { case (ioConfig, peConfig) =>
      peConfig.valid := ioConfig.valid
      peConfig.bits := ioConfig.bits
      ioConfig.ready := peConfig.ready && !stall_full // TODO should this be "stall" or "stall_full"?
    }
  }

  // Connect compressed2expanded mappings
  var baseComp2ExpMappings = scala.collection.mutable.Map.empty[Seq[Int], Compressed2ExpandedMapping]

  var compress2ExpandedMappingId = 0
  for (pe <- pesSorted) {
    def connectPeMapping(peMapping: Compressed2ExpandedMapping): Unit = {
      val ioMapping = io.compressed2ExpandedMappings(compress2ExpandedMappingId)

      ioMapping <> peMapping
      its.indices.zipWithIndex.foreach { case (ind, i) =>
        val expanded = if (ind.isSkipped) ioMapping.expanded(i) else peMapping.compressed(i)
        peMapping.expanded(i) := expanded
      }

      ioMapping.relevantIndices = peMapping.relevantIndices

      compress2ExpandedMappingId += 1
    }

    if (!ignoreBaseC2e) {
      baseComp2ExpMappings += ((pe.spaceCoords, io.compressed2ExpandedMappings(compress2ExpandedMappingId)))
      connectPeMapping(pe.io.baseCompressed2ExpandedMappings)
    } else {
      pe.io.baseCompressed2ExpandedMappings.found.foreach(_ := true.B)
      pe.io.baseCompressed2ExpandedMappings.unavailable.foreach(_ := false.B)
      pe.io.baseCompressed2ExpandedMappings.expanded := DontCare
    }

    pe.io.ioCompressed2ExpandedMappings.foreach(connectPeMapping)
  }

  assert(compress2ExpandedMappingId == io.compressed2ExpandedMappings.size)

  // Connect optimistic skip buffers
  def getOptSkipBufferIns: Iterable[(stellar.Variable, Vec[OptimisticSkipInPort], Compressed2ExpandedMapping)] = {
    require(!ignoreBaseC2e)
    ioInputOptSkipConns.zip(io.optSkipBufferIns).map { case (conn, ioIn) =>
      // TODO this should also return a "valid" signal so we can avoid useless reads from the outer memory
      val srcPeCoords = conn.ioPoints.head.coords
      (conn.srcOrIOVar, ioIn, baseComp2ExpMappings(srcPeCoords))
    }
  }

  ioInputOptSkipConns.zip(ioInputOptSkipBuffers).zip(io.optSkipBufferIns).foreach { case ((conn, buffer), ioIns) =>
    // Connect the inputs to the ioInput skip buffers
    buffer.io.ins := ioIns
    buffer.io.outs.foreach(_.coords := DontCare)
  }

  (pe2peOptSkipConns zip pe2peOptSkipBuffers).foreach { case (conn, buffer) =>
    // Connect the inputs to the pe2pe skip buffers
    val srcVar = conn.srcOrIOVar.asInstanceOf[Intermediate]
    val srcPes = conn.srcPoints.map(p => pes(p.coords))

    val delay = conn.head match {
      case Point2PointConn(_, _, _, _, d) => d
      case _ => throw new Exception("must be p2p conn")
    }

    conn.bypass match {
      case Some(bypassConn) =>
        val bypass = getOptSkipBuffer(bypassConn)
        val data = OptimisticSkipBuffer.bypassedData(conn, bypass, srcPes.map(_.getPe2PeOutput(conn.srcOrInterVar)))
        buffer.io.ins := ShiftRegister(data, delay.head, !stall)
        require(delay.tail.forall(_ == 0))

      case None =>
        buffer.io.ins.foreach { bufferIn =>
          bufferIn.data := 0.S
          bufferIn.coords := DontCare
        }

        srcPes.zip(buffer.io.ins).foreach { case (srcPE, bufferIn) =>
          val srcPEOut = srcPE.getPe2PeOutput(srcVar)
          bufferIn.data := ShiftRegister(srcPEOut.data, delay.head, !stall)
          bufferIn.coords := ShiftRegister(OptimisticSkipBuffer.notSharedIndices(srcPEOut.domainCoords, conn), delay.head, !stall)
          require(delay.tail.forall(_ == 0))
        }
    }

    // Connect the inputs to the PEs which connect to optimistic skip buffers
    val dstPEs = conn.dstPoints.map(p => pes(p.coords))
    assert(dstPEs.size == buffer.io.outs.size)

    dstPEs.zip(buffer.io.outs).foreach { case (dstPE, bufferOut) =>
      val peIn = dstPE.getPe2PeInput(conn.srcIndexed)
      peIn.data := bufferOut.data

      val srcPE = srcPes.head
      peIn.domainCoords := srcPE.getPe2PeOutput(srcVar).domainCoords
      bufferOut.coords := OptimisticSkipBuffer.notSharedIndices(dstPE.io.baseCompressed2ExpandedMappings.expanded, conn)
    }
  }

  ioOutputOptSkipConns.zip(ioOutputOptSkipBuffers).zip(io.optSkipBufferOuts).foreach { case ((conn, buffer), ioOuts) =>
    // Connect the outputs to the ioOutput skip buffers
    val interVar = conn.dstOrInterVar
    val srcPes = conn.ioPoints.map(p => pes(p.coords))

    conn.bypass match {
      case Some(bypassConn) =>
        val bypass = getOptSkipBuffer(bypassConn)
        val data = OptimisticSkipBuffer.bypassedData(conn, bypass, srcPes.map(_.getPe2PeOutput(conn.dstOrInterVar)))
        buffer.io.ins := data

      case None =>
        srcPes.zip(buffer.io.ins).foreach { case (srcPE, bufferIn) =>
          val srcPEOut = srcPE.getPe2PeOutput(interVar)
          bufferIn.data := srcPEOut.data
          bufferIn.coords := DontCare
        }
    }

    ioOuts := buffer.io.ins
    buffer.io.outs.foreach(_.coords := DontCare)
  }

  // Connect the "last" IO signals
  val infiniteTime = ((BigInt(1) << 31)-1).toInt
  val unBoundedTimeAxes = transform.timeTr.map { tr =>
    its.indices.zip(tr).exists { case (ind, tr) => ind.isUnbounded && tr != 0 }
  }.zipWithIndex.collect { case (true, i) => i }
  var timeIsUnbounded = false

  ioVars.zip(io.last_in_or_out).foreach { case (ioVar, ioLasts) =>
    // TODO Some variables might not be re-used during the unbounded time segment, so we can issue the "last" signal for them even before the OpCount totally ends
    val lastTime = unBoundedTimeAxes.foldLeft(ioLastTimes(ioVar)) { (acc, x) => acc.updated(x, infiniteTime) }

    ioLasts.zip(pesSorted).foreach { case (ioLast, pe) =>
      val isLastTime = if (transform.nTimeAxes == 0) {
        true.B
      } else if (transform.nTimeAxes > 0 && unBoundedTimeAxes.isEmpty && pe.maxTimeForPe < lastTime) {
        false.B
      } else {
        val isEnding = if (pe == endingPe) pe.io.isEnding else false.B
        val timeMatches = if (lastTime.contains[Int](infiniteTime)) false.B else vecEquals(pe.io.time, lastTime.map(_.S))
        val timeLessThan = if (lastTime.exists(_ != infiniteTime)) compareVecsSigned(pe.io.time, lastTime.map(_.S), {_ < _}) else true.B

        if (lastTime.contains[Int](infiniteTime))
          timeIsUnbounded = true

        timeMatches || isEnding && timeLessThan
      }

      ioLast.valid := pe.io.busy && !stall_full && isLastTime
      ioLast.bits := pe.io.runningOpCounter
    }
  }
  dontTouch(io.last_in_or_out)
  pesSorted.foreach { pe =>
    dontTouch(pe.io.end)
    dontTouch(pe.io.endInnermostTimeAxis)
    dontTouch(pe.io.isEnding)
    dontTouch(pe.io.allInputsAndOutputsAreInvalid)
  }

  // Start execution on PEs based on opCounter values
  val minFillCounter = minOf(io.rfFillCounters:_*)
  pesSorted.foreach { pe =>
    pe.io.start := alwaysStart.B || (minFillCounter > pe.io.opCounter) && (!io.clFillCounter.valid || io.clFillCounter.bits > pe.io.opCounter)
  }

  // Make sure that PEs stay in sync with each other, even when reg-files aren't being accessed at full throughput
  pesSorted.foreach { pe =>
    val earlierPeOpt = pesSorted.filter(_.minTimeForPe < pe.minTimeForPe).maxByOption(_.minTimeForPe)
    earlierPeOpt match {
      case Some(earlierPe) =>
        pe.io.nextOpTime.valid := earlierPe.io.busy && earlierPe.io.runningOpCounter > pe.io.runningOpCounter
        pe.io.nextOpTime.bits := earlierPe.io.time

        if (withAsserts) assert(earlierPe.io.runningOpCounter >= pe.io.runningOpCounter &&
          earlierPe.io.runningOpCounter <= pe.io.runningOpCounter + 1.U,
          "earlier PE is at an op-counter that makes it impossible for us to use it to keep PEs in sync")

      case None =>
        pe.io.nextOpTime.valid := false.B
        pe.io.nextOpTime.bits := DontCare
    }
  }

  io.ending.valid := endingPe.io.isEnding && !stall_full
  io.ending.bits := endingPe.io.runningOpCounter

  val endIsExplicitlySet = its.pointAssignments.map(_.assignment.dst.variable).exists {
    case interVar: Intermediate => interVar.signalsLocalEnding || interVar.signalsEnding || interVar.signalsEndingInnermostTimeAxis
    case _ => false
  }
  val allInputsAndOutputsInvalid = if (timeIsUnbounded && !endIsExplicitlySet && transform.nTimeAxes == 1) !stall && // any(endingPe.io.time.map(_ > 0.S)) && endingPe.io.allInputsAndOutputsAreInvalid
    all(pesSorted.map { pe =>
      any(pe.io.time.map(_ > 0.S)) && pe.io.allInputsAndOutputsAreInvalid
    }) else
      false.B

  if (endIsExplicitlySet)
    pesSorted.foreach { pe =>
      pe.io.endInnermostTimeAxis := endingPe.io.triggeringEarlyEndOfInnermostTimeAxis && (endingPe.io.runningOpCounter === pe.io.runningOpCounter)
      pe.io.end := endingPe.io.triggeringEarlyEnd && (endingPe.io.runningOpCounter === pe.io.runningOpCounter)
    }
  else
    pesSorted.foreach { pe =>
      pe.io.endInnermostTimeAxis := allInputsAndOutputsInvalid
      pe.io.end := allInputsAndOutputsInvalid
    }
  assert(endingPe.io.triggeringEarlyEndOfInnermostTimeAxis || !pesSorted.map(_.io.triggeringEarlyEndOfInnermostTimeAxis).reduce(_ || _), "Currently, we only allow early ending from the ending-PE. There are a few different places (like the code for setting the spatial-array's 'io.ending' signal) which assume that the endingPE signals the end of every op-count")
  assert(endingPe.io.triggeringEarlyEnd || !pesSorted.map(_.io.triggeringEarlyEnd).reduce(_ || _), "Currently, we only allow early ending from the ending-PE. There are a few different places (like the code for setting the spatial-array's 'io.ending' signal) which assume that the endingPE signals the end of every op-count")

  if (withAsserts)
    for ((pe, i) <- pesSorted.zipWithIndex) {
      for (pe2 <- pesSorted.drop(i+1)) {
        when (pe.io.busy && pe2.io.busy && pe.io.runningOpCounter === pe2.io.runningOpCounter) {
          assert(vecEquals(pe.io.time, pe2.io.time), "pes have gotten out of sync")
        }
      }
    }

  io.busy := pesSorted.map(_.io.busy).reduce(_ || _)

  // TODO remove this once we have a better way to start and stop spatial array executions
  when (reset.asBool) {
    io.ins.foreach(_.valid := false.B)
    io.outs.foreach(_.valid := false.B)
  }
}
