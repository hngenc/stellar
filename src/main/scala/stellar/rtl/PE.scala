package stellar.rtl

import scala.math.Ordering.Implicits._

import chisel3._
import chisel3.util._
import chisel3.experimental._

import stellar._

import ChiselUtil._
import MatrixChiselUtil._

class PE(pointAssignments: Iterable[PointAssignment], ioConns: Iterable[IOConn],
         domainIndices: Seq[stellar.Index], domainIndUpperBounds: Seq[Int], pointMappings: Seq[PointsMapping],
         val spaceCoords: Seq[Int], val minTimeForPe: Seq[Int], val maxTimeForPe: Seq[Int], maxTimeForSpArray: Seq[Int],
         transform: Transform, canEndEarly: Boolean, inVarsThatAreNeverPopped: Set[stellar.Input], dataWidthBits: Int) extends Module
{
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  override val desiredName = s"PE_${spaceCoords.mkString("_")}"

  private val nDomainIndices = domainIndices.size
  private val nTimeAxes = transform.nTimeAxes

  private val assignments = pointAssignments.map(_.assignment).toSet.toSeq

  private val relevantPointsMappings = pointMappings.filter { pm =>
    val tos: Set[Seq[Int]] = pm.tos.map(_.coords).map(transform.spaceTransform)
    val froms: Set[Seq[Int]] = pm.froms.map(_.coords).map(transform.spaceTransform)

    tos.contains(spaceCoords) || froms.contains(spaceCoords)
  }
  private val pointsMappingsThatAffectBias = pointMappings.intersect(pointAssignments.flatMap(_.mapping.toSeq).toSeq)

  private val dsts = assignments.map(_.dst.variable).distinct.sortBy(_.name)
  private val srcs = (assignments.flatMap(asg => Passes.extractIndexed(asg.src)) ++
    assignments.flatMap(asg => asg.dst.indices.flatMap(Passes.extractIndexed))).distinct.sortBy(_.toString)

  def inIsCoordOf(in: Indexed): Option[Indexed] = {
    in match {
      case Indexed(inVar: Input, indices) if inVar.isCoordOfOpt.nonEmpty =>
        inVar.isCoordOfOpt match {
          case Some((baseInVar: Input, _, false)) => srcs.collectFirst { case ind @ Indexed(`baseInVar`, `indices`) => ind }
          case Some((baseInVar: Input, _, true)) =>
            val result = srcs.collect { case ind @ Indexed(`baseInVar`, _) => ind }
            require(result.size <= 1, s"Unclear which baseInVar to force connection to")
            result.headOption

          case _ => throw new Exception("UNREACHABLE")
        }

      case _ => None
    }
  }

  private val ioSrcs = srcs.filter(_.isIOInput).filter(inIsCoordOf(_).isEmpty).toSeq
  private val ioDsts = dsts.collect { case v: Output => v }.toSeq

  private val ioSrcsThatAreCoordsOfOtherSrcs = srcs.filter(_.isIOInput).diff(ioSrcs).toSeq

  private val pe2peSrcs = srcs.filter(_.isIntermediate).toSeq
  private val pe2peDsts = dsts.collect { case v: Intermediate => v }.toSeq

  assert((ioSrcs ++ pe2peSrcs ++ ioSrcsThatAreCoordsOfOtherSrcs).toSet == srcs.toSet, "some PE inputs were ignored")
  assert((ioDsts ++ pe2peDsts).toSet == dsts.toSet, "some PE outputs were ignored")

  val skipFuncs = (assignments.flatMap(asg => Passes.extractSkipFuncs(asg.src)) ++
    assignments.flatMap(asg => asg.dst.indices.flatMap(Passes.extractSkipFuncs))).distinct.sortBy(_.toString)

  val debug_should_print = false // ioDsts.isEmpty // spaceCoords.forall(_ == 0) // spaceCoords.head == 0

  val debug_cycle = RegInit(0.U(32.W))
  debug_cycle := debug_cycle + 1.U

  val io = IO(new Bundle {
    val ioIns = MixedVec(ioSrcs.map(ios => new SpatialArrayInPort(ios.indices.size, dataWidthBits = dataWidthBits)))

    val ioOuts = MixedVec(ioDsts.map { iod =>
      val nCoords = assignments.filter(_.dst.variable == iod).map(_.dst.indices.size).toSet
      assert(nCoords.size == 1, "output indexed differently multiple times")
      Decoupled(new SpatialArrayOutPort(nCoords.head, nDomainIndices, dataWidthBits = dataWidthBits))
    })

    val pe2peIns = chisel3.Input(Vec(pe2peSrcs.size, new DataElement(nDomainIndices, dataWidthBits = dataWidthBits)))
    val pe2peOuts = chisel3.Output(Vec(pe2peDsts.size, new DataElement(nDomainIndices, dataWidthBits = dataWidthBits)))

    val loadBalancingMappings = new LoadBalancingMappings(pointMappings.size)

    // We create one compressed2ExpandedMapping for each possible index instantiated in each IO, as well as one so that
    // the output pe2pe elements' indices can be calculated properly
    val baseCompressed2ExpandedMappings = new Compressed2ExpandedMapping(nDomainIndices, relevantIndices = domainIndices.toSet)
    val ioCompressed2ExpandedMappings = Vec(skipFuncs.size, new Compressed2ExpandedMapping(nDomainIndices))

    // The ports below are used to check when a PE should begin operation, and to synchronize PEs across the spatial
    // array and the broader accelerator
    val opCounter = chisel3.Output(OpCount())
    val start = chisel3.Input(Bool())
    val stall = chisel3.Input(Bool())
    val outs_stalling = chisel3.Input(Bool()) // This tells us specifically whether the output-ports are stalling or not, and is useful for spatial arrays where stallForOutputs == false
    val endInnermostTimeAxis = chisel3.Input(Bool())
    val end = chisel3.Input(Bool())

    val isStarting = chisel3.Output(Bool())
    val isEnding = chisel3.Output(Bool())
    val triggeringEarlyEndOfInnermostTimeAxis = chisel3.Output(Bool())
    val triggeringEarlyEnd = chisel3.Output(Bool())
    val allInputsAndOutputsAreInvalid = chisel3.Output(Bool())

    val busy = chisel3.Output(Bool())

    val runningOpCounter = chisel3.Output(OpCount()) // This port is only used to handle special cases where we care if a 'start' signal is being asserted at the same time that we're reading an 'opcounter' value

    val time = chisel3.Output(Vec(nTimeAxes max 1, SInt(32.W))) // TODO magic number
    val nextOpTime = Flipped(Valid(Vec(nTimeAxes max 1, SInt(32.W)))) // TODO magic number
  })

  def getIOInput(src: Indexed): SpatialArrayInPort = {
    val index = ioSrcs.indexOf(src)
    io.ioIns(index)
  }

  def getPe2PeInput(src: Indexed): DataElement = {
    assert(pe2peSrcs.contains(src), s"$src is not in PE_${spaceCoords.mkString("_")}\n\tExisting srcs:\n\t\t${pe2peSrcs.mkString("\n\t\t")}")
    val index = pe2peSrcs.indexOf(src)
    io.pe2peIns(index)
  }

  def getIOOutput(dst: Output): DecoupledIO[SpatialArrayOutPort] = {
    val index = ioDsts.indexOf(dst)
    io.ioOuts(index)
  }

  def getPe2PeOutput(dst: Intermediate): DataElement = {
    val index = pe2peDsts.indexOf(dst)
    assert(index >= 0, s"$dst is not an output of PE_${spaceCoords.mkString("_")}")
    io.pe2peOuts(index)
  }

  def getIOInputs(v: stellar.Input): Seq[SpatialArrayInPort] = {
    ioSrcs.zip(io.ioIns).collect {
      case (Indexed(`v`, _), inport) => inport
    }
  }

  def getIOOutputs(v: stellar.Output): Seq[DecoupledIO[SpatialArrayOutPort]] = {
    ioDsts.zip(io.ioOuts).collect {
      case (`v`, outport) => outport
    }
  }

  def getAllIOInputs: Iterable[(stellar.Input, SpatialArrayInPort)] = ioSrcs.zip(io.ioIns).map {
    case (Indexed(v: Input, _), inport) => (v, inport)
    case _ => throw new Exception("UNREACHABLE")
  }

  def getAllIOOutputs: Iterable[(stellar.Output, DecoupledIO[SpatialArrayOutPort])] = ioDsts.zip(io.ioOuts)

  def getCompressed2ExpandedMapping(indexSkipFunc: IndexSkipFunc): Compressed2ExpandedMapping = {
    val index = skipFuncs.indexOf(indexSkipFunc)
    io.ioCompressed2ExpandedMappings(index)
  }

  io.ioCompressed2ExpandedMappings.zip(skipFuncs).foreach { case (c2eMapping, skipFunc) =>
    c2eMapping.relevantIndices = skipFunc.indicesInExpr
  }

  io.ioIns.foreach(_.valid := false.B)
  io.ioOuts.foreach(_.valid := false.B)

  io.ioIns.foreach(_.pop.valid := false.B)
  io.ioIns.foreach(_.pop.bits := DontCare)
  io.ioIns.foreach(_.last_in_axis.foreach(_ := false.B))

  io.ioIns.foreach(_.coords := DontCare)

  io.ioOuts.foreach(_.bits.last_in_axis.foreach(_ := false.B))
  io.ioOuts.foreach(_.bits.axis_spans.foreach(_.valid := false.B))
  io.ioOuts.foreach(_.bits.axis_spans.foreach(_.bits := DontCare))

  // State transitions for the PE (to trigger beginning and end of execution)
  object State extends ChiselEnum {
    val startingOrIdling, running = Value
  }
  val state = RegInit(State.startingOrIdling)
  val opCounter = RegInit(OpCount(0.U))

  val startingOrRunning = state === State.running || io.start

  val timeBitwidth = 32
  val time = RegInit(VecInit.fill(transform.nTimeAxes max 1)(0.S(timeBitwidth.W)))
  val (incrementedTime, timeWraparound) = if (nTimeAxes > 0) wrappingIncrement(time, maxTimeForSpArray, minTimeForPe, maxTimeForPe, forceInnermostWraparound = io.endInnermostTimeAxis) else (VecInit(0.S), true.B)
  val timeCompleted = if (domainIndices.exists(_.isUnbounded)) false.B else timeWraparound
  val completed = WireInit(timeCompleted || io.end)

  val nextOpTime = Reg(Vec(nTimeAxes max 1, SInt(32.W)))
  def invalidateNextOpTime(): Unit = { nextOpTime.head := (-1).S }
  def incNextOpTime(t: Vec[SInt]) = if (nTimeAxes > 0) wrappingIncrement(t, maxTimeForSpArray, Seq.fill(nTimeAxes)(0), maxTimeForSpArray)._1 else VecInit(0.S(32.W))
  val incrementedNextOpTime = Mux(io.nextOpTime.fire, io.nextOpTime.bits,
    Mux(nextOpTime.head >= 0.S, incNextOpTime(nextOpTime), nextOpTime))

  require(minTimeForPe.reverse <= maxTimeForPe.reverse, s"minTime ($minTimeForPe) is larger than maxTime ($maxTimeForPe)")

  io.busy := startingOrRunning

  io.isStarting := io.start && state === State.startingOrIdling
  io.isEnding := io.busy && completed
  io.triggeringEarlyEndOfInnermostTimeAxis := false.B
  io.triggeringEarlyEnd := false.B

  val allInputsAreInvalid = all(io.ioIns.zip(ioSrcs).collect { case (in, Indexed(inVar: Input, _)) if !inVarsThatAreNeverPopped.contains(inVar) => !in.valid || in.unavailable })
  val outputsAreInvalid = VecInit.fill(io.ioOuts.size max 1)(true.B)
  io.allInputsAndOutputsAreInvalid := allInputsAreInvalid && all(outputsAreInvalid) && time.head >= ioConns.map(_.time.head).min.S

  io.opCounter := opCounter
  io.runningOpCounter := opCounter + (state === State.startingOrIdling && io.start)
  io.time := time

  io.ioIns.foreach(_.op_count := opCounter - (state === State.running))
  io.ioOuts.foreach(_.bits.op_count := opCounter - (state === State.running))

  (io.baseCompressed2ExpandedMappings +: io.ioCompressed2ExpandedMappings).foreach(_.op_count := opCounter - (state === State.running))

  io.baseCompressed2ExpandedMappings.valid := io.busy && domainIndices.exists(_.isSkipped).B
  io.ioCompressed2ExpandedMappings.foreach(_.valid := false.B)

  io.loadBalancingMappings.opCount := io.runningOpCounter - 1.U

  // State transitions and updates
  when (!io.stall) {
    switch (state) {
      is (State.startingOrIdling) {
        when (io.start) {
          state := Mux(completed, State.startingOrIdling, State.running)
          time := incrementedTime
          nextOpTime := incrementedNextOpTime
          opCounter := opCounter + 1.U
        }
      }

      is (State.running) {
        time := incrementedTime
        nextOpTime := incrementedNextOpTime

        when(completed) {
          state := State.startingOrIdling
          time := incNextOpTime(incrementedNextOpTime)
          invalidateNextOpTime()
          if (nTimeAxes > 0)
            assert(compareVecsSigned(incNextOpTime(incrementedNextOpTime), minTimeForPe.map(_.S), {_ < _}, orEquals = true), "skipping time steps")
        }
      }
    }
  }

  // Generate the indices (in the domain-space, i.e. i,j,k rather than x,y,t)
  val spaceTimeCoords = spaceCoords.map(_.S).toSeq ++ time.take(nTimeAxes)

  private val inverse = transform.inverse.map(_.map(_.S))
  private val domainCompressedIndicesBeforeLoadBalancing = matvecS(inverse, spaceTimeCoords).toSeq

  io.loadBalancingMappings.configs.foreach(_.ready := false.B)
  private val loadBalancingMappings =
    if (relevantPointsMappings.isEmpty) {
      // Chisel freaks out if we instantiate an empty Vec, so we have to have a special case here for when the
      // loadBalancingMappings vec is empty
      VecInit(false.B)
    } else {
      VecInit((io.loadBalancingMappings.configs zip pointMappings).map {
        case (ioMapping, pm) if relevantPointsMappings.contains(pm) =>
          val mappingIsRelevantAtThisTime = any((pm.tos ++ pm.froms).map(_.coords.map(_.S)).map(
            vecEquals(domainCompressedIndicesBeforeLoadBalancing, _)
          ))

          ioMapping.ready := io.busy && mappingIsRelevantAtThisTime
          ioMapping.valid && ioMapping.bits && mappingIsRelevantAtThisTime

        case _ => false.B
      })
    }

  private val loadBalancingsToChooseFrom = if (pointsMappingsThatAffectBias.isEmpty) VecInit(false.B) else {
    loadBalancingMappings.zip(pointMappings).map {
      case (lbm, pm) if pointsMappingsThatAffectBias.contains(pm) => lbm
      case _ => false.B
    }
  }
  private val chosenLoadBalancingMapping = PriorityEncoder(loadBalancingsToChooseFrom)
  private val chosenLoadBalancingMappingValid = any(loadBalancingsToChooseFrom)
  private val loadBalancingBias = if (pointMappings.nonEmpty) {
    Mux(chosenLoadBalancingMappingValid,
      VecInit(pointMappings.map(pm => VecInit(pm.bias.map(_.S(32.W)))))(chosenLoadBalancingMapping),
      VecInit(Seq.fill(domainIndices.size)(0.S(32.W))))
    } else {
      Seq.fill(domainIndices.size)(0.S(32.W))
    }
  assert(PopCount(loadBalancingsToChooseFrom) <= 1.U, "we don't yet have a way to handle multiple simultaneous load-balancings")

  private val allLoadBalancingMappingsCalculated = all((io.loadBalancingMappings.configs zip pointMappings).collect {
    case (ioMapping, pm) if relevantPointsMappings.contains(pm) =>
      ioMapping.valid
  })
  private val notWaitingForAllLoadBalancingMappings = allLoadBalancingMappingsCalculated &&
    all(io.loadBalancingMappings.configs.zip(pointMappings).collect {
      case (ioMapping, pm) if relevantPointsMappings.contains(pm) =>
        val points = (pm.tos ++ pm.froms).map(_.coords).map(transform.spaceTimeTransform).filter { spaceTimeCoords =>
          val spaceCoords_ = spaceTimeCoords.dropRight(nTimeAxes)
          spaceCoords == spaceCoords_
        }
        val executionTimes = points.map(_.takeRight(nTimeAxes))
        val startTime = executionTimes.minBy(_.reverse).map(_.S)

        !ioMapping.bits || compareVecsSigned(startTime, time, {_ < _}, orEquals = true)
    })
  assert(!io.endInnermostTimeAxis && !io.end || notWaitingForAllLoadBalancingMappings, "ending PE's operation early while there are still loadBalancing mappings we're waiting for")

  private val domainCompressedIndices = addvS(domainCompressedIndicesBeforeLoadBalancing, loadBalancingBias)
  private val domainExpandedIndices =
    domainIndices.zip(io.baseCompressed2ExpandedMappings.compressed).zip(io.baseCompressed2ExpandedMappings.expanded).zip(domainCompressedIndices).map { case (((ind, ioMappingCompressed), ioMappingExpanded), compressedInd) =>
      ioMappingCompressed := compressedInd

      if (ind.isSkipped) ioMappingExpanded
      else compressedInd
    }

  val should_debug = false // spaceCoords == Seq(1,1,3,1)
  if (should_debug) {
    println(s"$desiredName")
    println(s"\tioSrcs = $ioSrcs")
    println(s"\t\tFull ioSrcs size = ${ioSrcs.size} | Set ioSrcs size = ${ioSrcs.toSet.size} | Optimized ioSrcs size = ${ioSrcs.map(Passes(_)).toSet.size}")
    println(s"\tioDsts = $ioDsts")
    println(s"\tpe2peSrcs = ${pe2peSrcs.zipWithIndex}")
    println(s"\tpe2peDsts = ${pe2peDsts.zipWithIndex}")
    println(s"\tskipFuncs = $skipFuncs")
    println(s"\tspaceTimeCoords = ${spaceTimeCoords.map(_.litOption)}")
    println(s"\tinverse = ${inverse.map(_.map(_.litOption))}")
    println(s"\tdomainCompressedIndicesBeforeLoadBalancing = ${domainCompressedIndicesBeforeLoadBalancing.map(_.litOption)}")
    println(s"\tloadBalancingBias = ${loadBalancingBias.map(_.litOption)}")
    println(s"\tdomainCompressedIndices = ${domainCompressedIndices.map(_.litOption)}")
    println(s"\tminTimeForPe = $minTimeForPe")
    println(s"\tmaxTimeForPe = $maxTimeForPe")
    println(s"\tdomainIndUpperBounds = $domainIndUpperBounds")
  }

  private val idle = (if (nTimeAxes > 0) compareVecsSigned(time, minTimeForPe.map(_.S), {_ < _}) else false.B) ||
    /* The "any" statement below assumes that the load-balancer never generates conflicting load-balanced mappings. So
       the "any" statement is simply meant to make sure that if the default, non-load-balanced domainIndices of this PE
       conflict with another PE which is being load-balanced, then the load-balanced PE takes precedence. */
    any(io.loadBalancingMappings.configs.zip(pointMappings).zipWithIndex.map { case ((ioMapping, pm), ioMappingId) =>
      ioMapping.valid && ioMapping.bits &&
        (!chosenLoadBalancingMappingValid || chosenLoadBalancingMapping =/= ioMappingId.U) && // TODO if we assume that the load-balancer never generates conflicting load-balanced mappings, then is the "|| chosenLoadBalancingMapping =/= ioMappingId.U" check actually required?
        any(pm.froms.map(_.coords.map(_.S)).map(vecEquals(_, domainCompressedIndices)))
    })

  val maximum_val = maxVal(SInt(dataWidthBits.W))

  private val dataMap = ((ioSrcs zip io.ioIns.map(_.data)) ++ (ioSrcs.map(_.found) zip io.ioIns.map(_.found.zext)) ++ (ioSrcs.map(_.unavailable) zip io.ioIns.map(_.unavailable.zext))
    ++ ioSrcs.zip(io.ioIns).flatMap { case (ioSrc, ioIn) => ioSrc.indices.indices.map(d => ioSrc.axisSpan(d) -> ioIn.axis_spans(d).bits.zext) }
    ++ ioSrcs.zip(io.ioIns).flatMap { case (ioSrc, ioIn) => ioIn.expanded_coords.zipWithIndex.map { case (c, i) => CoordOf(ioSrc, i) -> Mux(ioIn.unavailable, maximum_val, c) } }
    ++ (pe2peSrcs zip io.pe2peIns.map(_.data))
    ++ (domainIndices zip domainCompressedIndices)
    ++ skipFuncs.zip(io.ioCompressed2ExpandedMappings).map { case (skipFunc, ioMapping) => skipFunc -> ioMapping.expanded(domainIndices.indexOf(skipFunc.index)) }
    ++ ioSrcsThatAreCoordsOfOtherSrcs.map {
      case ind @ Indexed(inVar: Input, _) =>
        val Some(baseInd) = inIsCoordOf(ind)
        val Some((_, coordId, _)) = inVar.isCoordOfOpt
        ind -> Mux(getIOInput(baseInd).unavailable, maximum_val, getIOInput(baseInd).expanded_coords(coordId))
      case _ => throw new Exception("UNREACHABLE")
    }
    ++ Seq(OutputPortsStalling -> io.outs_stalling.zext, MaxVal -> maximum_val)
  ).toMap

  io.ioCompressed2ExpandedMappings.zip(skipFuncs).foreach { case (ioMapping, IndexSkipFunc(index, indexExpr, dependencies, _)) =>
    val depInds = domainIndices.filter(index.dependencies.contains).toSeq
    ioMapping.compressed.zip(domainIndices).foreach { case (mappingCompressed, ind) =>
      mappingCompressed := {
        if (ind == index) ChiselConverter(indexExpr, dataMap, domainIndices.zip(domainIndUpperBounds).toMap)
        else if (depInds.contains(ind)) ChiselConverter(dependencies(depInds.indexOf(ind)), dataMap, domainIndices.zip(domainIndUpperBounds).toMap)
        else DontCare
      }
    }
  }

  // Wire up all the output assignments
  private val sortedAssignments = pointAssignments.groupBy(asgn => (asgn.assignment, asgn.priority, asgn.mapping)).
    values.map(asgns => (asgns.head, asgns.map(_.originalPoint.coords).toSet)).toSeq.sortBy(_._1.priority).reverse

  if (should_debug) {
    println(s"\tAssignments:")
    println(s"\t\t${sortedAssignments.filter(_._1.assignment.dst.variable.name=="outerCoord").mkString("\n\t\t")}")
  }

  for (((debug_pa @ PointAssignment(_, _, assignment, _, pointMapping), requiredIndices), sortedAsgnId) <- sortedAssignments.zipWithIndex) {
    val domainCompressedInds = noPrefix(domainCompressedIndices.zip(domainIndices).zip(domainIndUpperBounds).map {
      case ((chiselInd, stellarInd), upperBound) if stellarInd.isUnbounded =>
        val denom = upperBound / 2; require(upperBound % 2 == 0)
        val shifted = (chiselInd % denom.S) +& denom.S; require(isPow2(upperBound))
        minOf(chiselInd, shifted)

      case ((chiselInd, _), _) => chiselInd
    })

    val indicesMatch = noPrefix(any(requiredIndices.map(requiredInds => vecEquals(domainCompressedInds, requiredInds.map(_.S)))))

    val loadBalancingMappingMatches = pointMapping match {
      case Some(pm) => loadBalancingMappings(pointMappings.indexOf(pm))
      case None if relevantPointsMappings.nonEmpty => loadBalancingMappings.asUInt === 0.U
      case None => true.B
    }

    val cond = indicesMatch && loadBalancingMappingMatches

    // The weird "when" clause generators below were just added for the sake of some manual const-propping
    val is_default = !sortedAssignments.take(sortedAsgnId).map(_._1).exists { asgn =>
      asgn.assignment.dst.isIOOutput == assignment.dst.isIOOutput && (
        if (assignment.dst.isIOOutput) asgn.assignment.dst.outVariable == assignment.dst.outVariable
        else (asgn.assignment.dst.interVariable == assignment.dst.interVariable))
    }
    def outerWhen(foo: => Unit) = if (is_default) { foo } else { when (cond) { foo } }
    def innerWhen(foo: => Unit) = if (!is_default) { foo } else { when (cond) { foo } }

    if (should_debug && false) {
      println(s"debug_pa = $debug_pa")
      println(s"\trequiredIndices = $requiredIndices")
      println(s"\tdomainCompressedInds lit ${domainCompressedInds.map(_.litOption)}")
      println(s"\tloadBalancingMappingMatches lit = ${loadBalancingMappingMatches.litOption}")
      println(s"\tindicesMatch lit = ${indicesMatch.litOption}")
    }

    outerWhen {
      if (assignment.dst.isIOOutput) {
        // Wire up IO outputs
        val in = assignment.src match {
          case Indexed(v: Intermediate, _) => getPe2PeOutput(v)
          case _ => throw new Exception("can only handle outputs that are assigned to a single indexed intermediate (for now)")
        }

        val out = getIOOutput(assignment.dst.outVariable)

        out.bits.element := in
      } else {
        // Wire up pe2pe outputs
        val in = ChiselConverter(assignment.src, dataMap, domainIndices.zip(domainIndUpperBounds).toMap)
        val out = getPe2PeOutput(assignment.dst.interVariable)

        out.data := in
        // out.domainCoords := domainExpandedIndices.toSeq
        out.domainCoords := domainCompressedIndices.toSeq

        innerWhen {
          val interVar = assignment.dst.variable.asInstanceOf[Intermediate]

          def earlyEnding =
            // TODO should this be 'time > minTime' or 'time =/= 0'? And should 'minTime' always be 0 when triggering early ends is possible? (Otherwise, we may get out-of-sync errors)
            out.data =/= 0.S && !idle && compareVecsSigned(time, minTimeForPe.map(_.S), {_ > _}) && // TODO right now, we don't permit early-ending on the very first cycle
            notWaitingForAllLoadBalancingMappings

          if (interVar.signalsEndingInnermostTimeAxis || interVar.signalsEnding) {
            val endingPort = if (interVar.signalsEndingInnermostTimeAxis) io.triggeringEarlyEndOfInnermostTimeAxis else if (interVar.signalsEnding) io.triggeringEarlyEnd else throw new Exception("UNREACHABLE")
            when (earlyEnding) {
              endingPort := true.B
            }
          }
          if (interVar.signalsLocalEnding)
            when (earlyEnding) {
              completed := true.B
            }
        }

        if (debug_should_print && false)
          when (io.busy && !idle && !io.stall) {
            printf(p"\nPe-2-pe output from PE_${spaceCoords.mkString("_")} @$debug_cycle\n")
            printf(p"\t$assignment\n")
            printf(p"\tportId = ${io.pe2peOuts.indexOf(out)}\n")
            printf(p"\tdomainCompressedInds = ${VecInit(domainCompressedInds)}\n")
            printf(p"\trunningOpCounter = ${io.runningOpCounter}\n")
            printf(p"\ttime = $time\n")
            printf(p"\tidle = $idle | stall = ${io.stall}\n")
            printf(p"\t$out\n")
            printf(p"\tdataMap = \n")
            dataMap.toSeq.foreach { case (expr, value) =>
              printf(p"\t\t$expr = $value\n")
            }
          }
      }
    }
  }

  // Wire up all the IO coords, valid, and pop signals
  val iocs = ioConns.filter(_.point.coords == spaceCoords.toSeq).groupBy { ioc =>
    val dstInterVar = ioc.ioVar match {
      // Some input pop conds depend on the exact interVar which this IoConn is writing to. For those, we need to preserve the exact interVar
      case inVar: stellar.Input if inVar.popConds.exists(_._2.nonEmpty) => Some(ioc.interVar)
      case _ => None
    }
    (ioc.ioIndexed, ioc.mapping, dstInterVar)
  }.values.map(iocs_ => (iocs_.head, iocs_.map(_.time).toSet)).toSeq
  if (should_debug && false) {
    println(s"ioc outs = ${ioConns.filter(_.point.coords == spaceCoords.toSeq).filter(_.isOutput)})")
    println(s"ioc ins  = ${ioConns.filter(_.point.coords == spaceCoords.toSeq).filterNot(_.isOutput)})")
  }

  /*
  val maxTimeForIocs = iocs.flatMap(_._2).flatten.max // This variable is just used as an optimization to reduce the number of bits in "time" that we need to check. We could also set this to (2<<31-1) without breaking correctness
  val iocTime = if (!minTimeForPe.exists(_ < 0)) {
    time.map(t => reduceWidth(t.asUInt, log2Up(maxTimeForIocs)).asSInt)
  } else {
    // I'm 99% sure it's fine to perform this optimization even when time can be negative, but I neglected to do so for now
    time
  }
  val reachedMaxTime = RegInit(false.B)
  */

  for (((ioc @ IOConn(_, _, ioIndexed, interVar, _, pointMapping), ts), iocId) <- iocs.zipWithIndex) {
    val timeMatches = noPrefix((transform.nTimeAxes == 0).B || any(ts.map(t => vecEquals(time, t.map(_.S)))) || {
      val mayBeShiftedInTime = ioIndexed.variable match {
        case inVar: Input => inVar.unBoundedAccessesPermitted // TODO this should be calculated automatically, not set manually by the user
        case _ => true
      }

      if (mayBeShiftedInTime) {
        // TODO Does this code work when dealing with BOTH load-balancing and time-unbounding?
        val domainIndsShiftedInTime = noPrefix(domainCompressedIndices.zip(domainIndices).map {
          case (chiselInd, stellarInd) if stellarInd.isUnbounded =>
            val upperBound = domainIndUpperBounds(domainIndices.indexOf(stellarInd))
            require(isPow2(upperBound))
            val denom = upperBound / 2
            minOf(chiselInd, (chiselInd % denom.S) +& denom.S).suggestName("disit")

          case (chiselInd, _) => chiselInd
        })

        val timeTr = transform.timeTr.map(_.map(_.S))
        val shiftedTime = matvecS(timeTr, domainIndsShiftedInTime).toSeq

        any(ts.map(t => vecEquals(shiftedTime, t.map(_.S))))
      } else {
        false.B
      }
    })

    val loadBalancingMatches = pointMapping match {
      case Some(pm) => loadBalancingMappings(pointMappings.indexOf(pm))
      case None => loadBalancingMappings.asUInt === 0.U // TODO do we have to check whether the whole vector is 0, or just whether one specific element of it is zero?
    }

    val cond = timeMatches && loadBalancingMatches && startingOrRunning

    // The weird "when" clause generators below were just added for the sake of some manual const-propping
    val is_default = !iocs.take(iocId).map(_._1).exists { ioconn =>
      ioconn.isOutput == ioc.isOutput && (
        if (ioc.isOutput) ioconn.ioIndexed.outVariable == ioc.ioIndexed.outVariable
        else ioconn.ioIndexed == ioc.ioIndexed)
    }
    def outerWhen(foo: => Unit) = if (is_default) { foo } else { when (cond) { foo } }
    def innerWhen(foo: => Unit) = if (!is_default) { foo } else { when (cond) { foo } }

    outerWhen {
      if (ioc.isOutput) {
        val out = getIOOutput(ioIndexed.outVariable)

        val coords = ioIndexed.indices.map { id => ChiselConverter(id, dataMap, domainIndices.zip(domainIndUpperBounds).toMap) }
        out.bits.coords := coords

        if (is_default) {
          (0 until out.bits.nIOCoords).foreach { i =>
            out.bits.hardCodedCoords(i) = coords(i).litOption.flatMap(c => Some(c.toInt))
          }
        } else {
          (0 until out.bits.nIOCoords).foreach { i =>
            out.bits.hardCodedCoords(i) = (out.bits.hardCodedCoords(i), coords(i).litOption) match {
              case (Some(c1), Some(c2)) if c1 == c2 => Some(c1)
              case _ => None
            }
          }
        }

        val coordsAreInvalid = any(coords.map(c => c === maximum_val))

        innerWhen {
          val relevantSkipFuncs = Passes.extractSkipFuncs(ioIndexed)
          io.ioCompressed2ExpandedMappings.zip(skipFuncs).collect { case (mapping, skipFunc) if relevantSkipFuncs.contains(skipFunc) =>
            mapping.valid := true.B
          }

          val lastInAxisConds = ioIndexed.variable.asInstanceOf[stellar.Output].lastInAxisConds
          lastInAxisConds.foreach { case (cond, axisId) =>
            out.bits.last_in_axis(axisId) := getPe2PeOutput(cond).data =/= 0.S
          }
          if (lastInAxisConds.isEmpty) {
            out.bits.last_in_axis.head := coordsAreInvalid
          }

          val axisSpanVariables = ioIndexed.variable.asInstanceOf[stellar.Output].axisSpanVariables
          axisSpanVariables.foreach { case (span, axisId) =>
            out.bits.axis_spans(axisId).valid := true.B
            out.bits.axis_spans(axisId).bits := getPe2PeOutput(span).data.asUInt
          }

          val validCondOpt = ioIndexed.variable.asInstanceOf[stellar.Output].validCond
          val valid = validCondOpt match {
            case Some(validCond) => getPe2PeOutput(validCond).data =/= 0.S
            case _ => !coordsAreInvalid // true.B
          }

          out.valid := valid

          outputsAreInvalid(io.ioOuts.indexOf(out)) := !valid // && ChiselUtil.all(coords.zip(ioIndexed.indices).collect { case (coord, expr) if Passes.extractIndexed(expr).nonEmpty => coord === maximum_val })

          if (debug_should_print && false)
            when (io.busy && !idle && !io.stall) {
              printf(p"\nIO output from PE_${spaceCoords.mkString("_")} @$debug_cycle\n")
              printf(p"\t$ioc\n")
              printf(p"\tportId = ${io.ioOuts.indexOf(out)}\n")
              printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
              printf(p"\trunningOpCounter = ${io.runningOpCounter}\n")
              printf(p"\ttime = $time\n")
              printf(p"\tidle = $idle | stall = ${io.stall}\n")
              printf(p"\t$out\n")
              printf(p"\tdataMap = \n")
              dataMap.toSeq.foreach { case (expr, value) =>
                printf(p"\t\t$expr = $value\n")
              }
            }
        }
      } else if (inIsCoordOf(ioIndexed).isEmpty) {
        val in = getIOInput(ioIndexed)

        val coords = {
          val indices = if (ioIndexed.variable.asInstanceOf[Input].useCompressedCoords) {
            val skipFuncs = ioIndexed.indices.flatMap(Passes.extractSkipFuncs).toSet
            val dict = skipFuncs.map(skipFunc => (skipFunc: Expr, skipFunc.indexExpr)).toMap
            ioIndexed.indices.map(Passes.replaceExprs(_, dict))
          } else {
            ioIndexed.indices
          }

          val optimized_indices = {
            /* This is just used to help with const-propping in cases where coordinates that might look like
                "0 * foo(t-1)" could instead be replaced with "0". These lines could be removed without affecting
                correctnes of the synthesized hardware. */
            indices.zip(indices.map(Passes(_))).map {
              case (_, c: Const) => c
              case (ind, _) => ind
            }
          }

          optimized_indices.map { id => ChiselConverter(id, dataMap, domainIndices.zip(domainIndUpperBounds).toMap) }
        }
        in.coords := coords

        if (is_default) {
          (0 until in.nIOCoords).foreach { i =>
            in.hardCodedCoords(i) = coords(i).litOption.flatMap(c => Some(c.toInt))
          }
        } else {
          (0 until in.nIOCoords).foreach { i =>
            in.hardCodedCoords(i) = (in.hardCodedCoords(i), coords(i).litOption) match {
              case (Some(c1), Some(c2)) if c1 == c2 => Some(c1)
              case _ => None
            }
          }
        }

        innerWhen {
          val relevantSkipFuncs = Passes.extractSkipFuncs(ioIndexed)
          val relevantC2EMappings = io.ioCompressed2ExpandedMappings.zip(skipFuncs).collect { case (mapping, skipFunc) if relevantSkipFuncs.contains(skipFunc) => mapping }
          relevantC2EMappings.foreach(_.valid := true.B)

          val c2eMappingsUnavailable = any(relevantC2EMappings.map(c2e => c2e.valid && c2e.unavailable.get))

          in.valid := !c2eMappingsUnavailable && all(ioIndexed.variable.asInstanceOf[stellar.Input].validConds.map {
            case (v, Some(whenCoord0Equals)) => getPe2PeOutput(v).data(0) || in.coords.head =/= getPe2PeOutput(whenCoord0Equals).data
            case (v, None) => getPe2PeOutput(v).data(0)
          })

          ioIndexed.variable.asInstanceOf[stellar.Input].popConds.collect {
            case (popCond, None | Some(`interVar`), None) =>
              when (getPe2PeOutput(popCond).data(0)) {
                in.pop.valid := true.B
              }

            case (popCond, None | Some(`interVar`), Some(coord0MustEqual)) =>
              when (getPe2PeOutput(popCond).data(0) && in.coords.head === getPe2PeOutput(coord0MustEqual).data) {
                in.pop.valid := true.B
              }
          }

          val lastInAxisOpt = ioIndexed.variable.asInstanceOf[stellar.Input].lastInAxis
          lastInAxisOpt match {
            case Some(cond) =>
              in.last_in_axis.head := getPe2PeOutput(cond).data(0) || // TODO We only support last-in-axis flags for the innermost axis for now
                io.isEnding // TODO We should add seperate options for whether or not we also want to signal 'last-in-axis' if the spatial array ends early
            case _ =>
          }
        }

        if (debug_should_print && ioIndexed.variable.name.contains("artial")) {
          when (io.busy && !idle && !io.stall) {
            printf(p"\nIO input from PE_${spaceCoords.mkString("_")} @$debug_cycle\n")
            printf(p"\t$ioc\n")
            printf(p"\tioIndexed = $ioIndexed\n")
            printf(p"\tportId = ${io.ioIns.indexOf(in)}\n")
            printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
            printf(p"\trunningOpCounter = ${io.runningOpCounter}\n")
            printf(p"\ttime = $time\n")
            // printf(p"\tidle = $idle | stall = ${io.stall}\n")
            // printf(p"\tis_default = $is_default | cond = $cond\n")
            printf(p"\t$in\n")
            printf(p"\tdataMap = \n")
            dataMap.toSeq.collect { case (expr, value) if expr.toString.contains("earl") =>
              printf(p"\t\t$expr = $value\n")
            }
          }
        }
      }
    }
  }

  when (idle) {
    io.ioIns.foreach(_.valid := false.B)
    io.ioOuts.foreach(_.valid := false.B)
    (io.baseCompressed2ExpandedMappings +: io.ioCompressed2ExpandedMappings).foreach(_.valid := false.B)
  }

  when (idle || io.stall) {
    io.ioOuts.foreach(_.bits.last_in_axis.foreach(_ := false.B))
  }

  io.ioOuts.foreach(out => out.bits.valid_copy := out.valid)
  io.ioOuts.foreach(_.bits.increment_sticky_coord := false.B)

  when (reset.asBool) {
    invalidateNextOpTime()
  }

  // These variables are just meant to help with waveform debugging
  /*if (false /* spaceCoords.tail.forall(_ == 0) */)*/ {
    val named_pe2peOuts = Seq.tabulate(pe2peDsts.size)(i => WireInit(io.pe2peOuts(i).data).suggestName(s"named_p2pOut_${i}__" + pe2peDsts(i).name))
    named_pe2peOuts.foreach(dontTouch(_))

    /*if (false)*/ {
      def clean(s: String) = s.replace("+", "_PLUS_").replace("-", "_MINUS_").replace("[", "_LEFT_").replace("]", "_RIGHT_").replace(",", "_COMMA_").replace("*", "_MULT_").replace("?", "_QUESTION_").replace(":", "_COLON_").replace("==", "_EQ_").replace("%", "_MOD_").replace(".", "_DOT_")

      val named_ioIns = Seq.tabulate(ioSrcs.size)(i => WireInit(io.ioIns(i)).suggestName(s"named_ioIn_${i}__" + clean(Passes(ioSrcs(i)).toString)))
      val named_ioOuts = Seq.tabulate(ioDsts.size)(i => WireInit(io.ioOuts(i)).suggestName(s"named_ioOut_${i}__" + ioDsts(i).name))
      val named_pe2peIns = Seq.tabulate(pe2peSrcs.size)(i => WireInit(io.pe2peIns(i).data).suggestName(s"named_p2pIn_${i}__" + clean(Passes(pe2peSrcs(i)).toString)))
      named_ioIns.foreach(dontTouch(_))
      named_ioOuts.foreach(dontTouch(_))
      named_pe2peIns.foreach(dontTouch(_))

      val not_found_ports = io.ioIns.map(in => in.valid && !(in.found || in.unavailable)).toSeq
      val not_found_ioIn_port_id = WireInit(if (io.ioIns.isEmpty) 0.U else PriorityEncoder(not_found_ports))
      dontTouch(not_found_ioIn_port_id)
      not_found_ports.foreach(dontTouch(_))
    }
  }

  io.ioOuts.foreach { out =>
    domainCompressedIndices.zipWithIndex.collect {
      case (domainInd, i) if domainInd.litOption.nonEmpty => out.bits.hardCodedDomainCoords(i) = Some(domainInd.litValue.toInt)
    }
  }

  io.ioIns.zipWithIndex.foreach { case (in, inPortId) =>
    assert(!in.valid || all(in.coords.zip(in.hardCodedCoords).collect {
      case (c, Some(hc)) => c === hc.S
    }), p"Input port $inPortId's hardcoded coords do not match\n\thardcoded coords = ${in.hardCodedCoords}\n\tactual coords = ${in.coords}")
  }
  io.ioOuts.zipWithIndex.foreach { case (out, outPortId) =>
    assert(!out.valid || all(out.bits.coords.zip(out.bits.hardCodedCoords).collect {
      case (c, Some(hc)) => c === hc.S
    }), p"Output port $outPortId's hardcoded coords do not match\n\thardcoded coords = ${out.bits.hardCodedCoords}\n\tactual coords = ${out.bits.coords}")
  }

  if (debug_should_print && false) {
    when (io.busy && !io.stall && !idle) {
      when (io.baseCompressed2ExpandedMappings.valid) {
        printf(p"\nBase c2e mapping in PE_${spaceCoords.mkString("_")} is being requested @$debug_cycle\n")
        printf(p"\t${io.baseCompressed2ExpandedMappings}\n")
        printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
        printf(p"\ttime = $time\n")
        printf(p"\tidle = $idle | stall = ${io.stall}\n")
      }

      io.ioCompressed2ExpandedMappings.zip(skipFuncs).zipWithIndex.foreach { case ((c2e, skipFunc), portId) =>
        when (c2e.valid) {
          printf(p"\nIO c2e mapping in PE_${spaceCoords.mkString("_")} is being requested @$debug_cycle\n")
          printf(p"\t$skipFunc\n")
          printf(p"\tportId = $portId\n")
          printf(p"\t${c2e}\n")
          printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
          printf(p"\ttime = $time\n")
          printf(p"\trunningOpCounter = ${io.runningOpCounter}\n")
          printf(p"\tidle = $idle | stall = ${io.stall}\n")
        }
      }
    }

    when (io.busy && io.stall) {
      /*
      when (io.baseCompressed2ExpandedMappings.valid && !io.baseCompressed2ExpandedMappings.found.get) {
        printf(p"\nBase c2e mapping in PE_${spaceCoords.mkString("_")} is not found @$debug_cycle\n")
        printf(p"\t${io.baseCompressed2ExpandedMappings}\n")
        printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
        printf(p"\ttime = $time\n")
        printf(p"\tidle = $idle | stall = ${io.stall}\n")
      }

      io.inputIOcompressed2ExpandedMappings.zip(ioSrcs).zipWithIndex.foreach { case ((c2es, ioSrc), srcId) =>
        c2es.zipWithIndex.foreach { case (c2e, indId) =>
          when (c2e.valid && !c2e.found.get) {
            printf(p"\nInput c2e mapping in PE_${spaceCoords.mkString("_")} is not found @$debug_cycle\n")
            printf(p"\t$ioSrc\n")
            printf(p"\tsrcId = $srcId | indId = $indId\n")
            printf(p"\t${c2e}\n")
            printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
            printf(p"\ttime = $time\n")
            printf(p"\tidle = $idle | stall = ${io.stall}\n")
          }
        }
      }

      io.outputIOcompressed2ExpandedMappings.zip(ioDsts).zipWithIndex.foreach { case ((c2es, ioDst), dstId) =>
        c2es.zipWithIndex.foreach { case (c2e, indId) =>
          when (c2e.valid && !c2e.found.get) {
            printf(p"\nOutput c2e mapping in PE_${spaceCoords.mkString("_")} is not found @$debug_cycle\n")
            printf(p"\t$ioDst\n")
            printf(p"\tdstId = $dstId | indId = $indId\n")
            printf(p"\t${c2e}\n")
            printf(p"\tdomainCompressedIndices = ${VecInit(domainCompressedIndices.toSeq)}\n")
            printf(p"\ttime = $time\n")
            printf(p"\tidle = $idle | stall = ${io.stall}\n")
          }
        }
      }
       */
    }
  }
}
