package stellar.rtl

import scala.math.Ordering.Implicits._
import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._
import chisel3.experimental._

import stellar.RfEntryExitOption
import stellar.RfEntryExitOption._
import stellar.Util.{crossJoin, without}

import ChiselUtil._

class NElemsInAxisLookup(nCoords: Int) extends Bundle {
  val address = Input(Vec(nCoords, SInt(32.W))) // TODO magic number
  val nElems = Output(UInt(32.W)) // TODO magic number
}

class ChiselRegFile(nElems: Int, val nInPorts: Int, val nOutPorts: Int, nUpdatePorts: Int,
                    nIOCoords: Int, nDomainCoords: Int,
                    nPipelinesOpt: Option[Int] = None,
                    triangular: Option[Boolean] = None, // Some(pipe-length increasing when true, decreasing when true)

                    val nSubArrays: Int = 1,

                    lastInAxisFlags: Seq[(Int, Int)] = Seq.empty, // Seq[(nFlags, coordId)]
                    lastInAxisFlagsFromOutPorts: Boolean = true, // this param is just used to help with manual const-propping
                    separateLastInAxisFlagsForEachPipeline: Boolean = false,
                    resetLastInAxisFlagsBasedOnTravellingOpCounts: Boolean = false,
                    dontCheckExtraLastInAxisFlag: Boolean = false, // TODO can we just make this true whenever 'separateLastInAxisFlagsForEachPipeline' is true?
                    unavailableIfSmallerOpt: Option[(Int, Boolean, Set[Int])] = None, // Option(coordId, unavailableIfSmallerThanAny, coordsToIgnore)

                    popIfEqual: Boolean = true,
                    popIfSmallerThanHead: Seq[Int] = Seq.empty, // coords to select when determining when an element's coords are "lower" than the "head's" coords

                    entryOption: RfEntryExitOption = Incrementing(), exitOption: RfEntryExitOption = Anywhere,
                    subPipelineOpt: Option[(Int, Int, Boolean, Boolean)] = None, // (nSubCounters, subCounterCoord, fromDomainCoords, hardCodeCoords) // TODO replace this with a more general "pipeliningOpt" variable which allows pipelining with _multiple_ coords rather than a single 'subCounterCoord' coord
                    transposingPipelinesOpt: Option[Boolean] = None, // Some(postDelay if true, preDelay if false). If None, no transposing pipelines // TODO "transposingPipelines" should be inferred by the entry and exit options
                    lockstepIns: Boolean = false,
                    lockstepOuts: (Int, Int) = (0, 0), // (lockSteppedPortGroupSize, nLockSteppedPortGroups)
                    groupedInPortStride: Int = 1,

                    constantCoordsForInputs: Seq[Seq[Option[Int]]] = Seq.empty, // This is for input ports which have coordinates that never change
                    constantCoordsForOutputs: Seq[Seq[Option[Int]]] = Seq.empty,
                    constantCoordsForNElemsLookups: Seq[Seq[Option[Int]]] = Seq.empty,
                    coordIsDivisibleByForPipelines: Seq[Seq[Option[(Int /* denom */, Int /* bias */)]]] = Seq.empty, // This is for input ports which have coordinates that are always divisible by some value
                    incrementingCoordsForInputs: Set[(Int, Boolean)] = Set.empty, // Set[(coordId, hardCodeCoords)]
                    maxOutCoordOpt: Option[Int] = None, coordsToIgnoreForOutputs: Set[Int] = Set.empty,
                    domainCoordsToUseForOutputs: stellar.Util.SMap[Int, Int] = scala.collection.Map.empty, // Map[(outCoordId,domainCoordId)]
                    checkOpCountForOuts: Boolean = true,

                    allElemsWillBePoppedBeforeLastOut: Boolean = false,
                    getLastInFromInOpCounts: Boolean = false,
                    getTravellingOpCountFromInPorts: Boolean = false, // TODO I think it might be fine for this to always be true
                    getTravellingCoordsFromInPorts: Set[Int] = Set.empty,

                    stickyOutPortCoords: Option[(Set[Int /* coordId */], Int /* nStickyRegSets */, Int /* pipelineIdDivider */)] = None,
                    stickyInPortCoords: Set[Int /* coordId */] = Set.empty,

                    coordsThatNeedExpanding: Set[Int] = Set.empty, // This variable is just used to help with const-prop. ChiselAccelerator will pass in a superset of coords which might need expanding, but ChiselRegFile might be able to narrow them down further. We do not bother to do that currently, but it could be worth doing later for even better const-prop

                    dataWidthBits: Int,

                    nElemsLookupPorts: Int = 1,
                    nElemsLookupPortsCoordsOpt: Option[Seq[Int]] = None, // if None, then use the flattenedFlagId. Otherwise, compare the coords with the coordIds given

                    dummyData: Boolean = false, withAsserts: Boolean = true, nameOpt: Option[String] = None) extends Module {
  require(nElems > 0)

  val shortenNames = nOutPorts >= 16 || nElems > 200
  def withoutPrefix[T](f: => T): T = if (shortenNames) noPrefix(f) else f
  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  def inPortType = Flipped(Decoupled(new SpatialArrayOutPort(nIOCoords, nDomainCoords, dataWidthBits = dataWidthBits)))
  def outPortType = Flipped(new SpatialArrayInPort(nIOCoords, dataWidthBits = dataWidthBits))
  def updatePortType = Flipped(Valid(new RegfileUpdatePort(nIOCoords)))

  val (lockSteppedPortGroupSize, nLockSteppedPortGroups) = lockstepOuts
  val shouldLockstepOuts = lockSteppedPortGroupSize > 0 && nLockSteppedPortGroups > 0

  val io = IO(new Bundle {
    val ins = Vec(nInPorts, inPortType)
    val outs = Vec(nOutPorts, outPortType)

    val updates = Vec(nUpdatePorts, updatePortType)

    val last_in = Input(Bool())
    val last_out = Input(Bool())

    val fillCount = Output(OpCount())
    val emptyCount = Output(OpCount())

    val isEmpty = Output(Bool())

    val n_elems_in_axis_lookups = Vec(nElemsLookupPorts.max(1), new NElemsInAxisLookup(nIOCoords)).suggestName("neloopkup")

    val busy = Output(Bool())
  })

  val elems = Reg(Vec(nElems, new SpatialArrayOutPort(nIOCoords, nDomainCoords, dataWidthBits = dataWidthBits))).suggestName("el")
  val elem_valids = RegInit(VecInit.fill(nElems, nSubArrays)(false.B)).suggestName("ev")

  val nPipelines = nPipelinesOpt.getOrElse(nInPorts)
  val elemsPerPipeline = nElems / nPipelines

  val fillCount = RegInit(OpCount(0.U))
  val emptyCount = RegInit(OpCount(0.U))
  if (withAsserts) OpCount.assertInSync(fillCount, emptyCount)

  val nLastInAxisFlags = if (lastInAxisFlags.nonEmpty) lastInAxisFlags.map(_._1).product else 0
  val lastInAxisFlagsOpt = Option.when(nLastInAxisFlags > 0)(RegInit(VecInit.fill(if (separateLastInAxisFlagsForEachPipeline) nPipelines else 1, nLastInAxisFlags)(false.B)).suggestName("liaf"))
  val flagIdSizes = lastInAxisFlags.map(_._1)
  val flagIdRanges = flagIdSizes.map(0 until _)
  val flagIdAddrs = if (lastInAxisFlags.isEmpty) Seq.empty else crossJoin(flagIdRanges).toSeq.map(_.toSeq.reverse).sorted.map(_.reverse) // We don't actually need the 'flagIdAddrs' to be sorted like this (it would be sufficient to just write 'crossJoin(flagIdRanges)' and nothing else), but this method orders them in a way that's easier for me to read in waveform viewers
  def flattenedFlagId(coords: Seq[UInt], assertGuard: Bool = true.B): UInt = withoutPrefix {
    if (lastInAxisFlags.isEmpty) {
      0.U
    } else if (lastInAxisFlags.size == 1) {
      val Seq((_, coordId)) = lastInAxisFlags
      coords(coordId)
    } else {
      val flagIds = lastInAxisFlags.map { case (nFlags, coordId) => reduceWidth(coords(coordId), log2Up(nFlags)) }

      val result = if (flagIds.forall(_.isLit)) {
        val result2 = flagIdAddrs.indexOf(flagIds.map(_.litValue.toInt))
        assert(result2 >= 0)
        result2.U
      } else {
        MuxCase(0.U, flagIdAddrs.zipWithIndex.map { case (addr, flag_id) =>
          vecEqualsU(flagIds, addr.map(_.U)) -> flag_id.U
        })
      }

      if (withAsserts) {
        assert(!assertGuard || result < nLastInAxisFlags.U)

        // The 'correct' calculation below is how we were originally calculating flag-id, before the SFC's poor
        // const-prop drove us to this other method instead
        val volumes = flagIdSizes.scanLeft(1)(_ * _)
        val correct = flagIds.zip(volumes).foldLeft(0.U) { case (acc, (f,v)) => acc +& (f * v.U)}
        assert(!assertGuard || result === correct)
      }

      result
    }
  }
  def hardcodedFlattenedFlagId(coords: Seq[Option[Int]]): Option[Int] = {
    val flagIds = lastInAxisFlags.map { case (_, coordId) => coords(coordId) }
    if (lastInAxisFlags.isEmpty)
      Some(0)
    else if (flagIds.forall(_.nonEmpty)) {
      val result = flagIdAddrs.indexOf(flagIds.map(_.get))
      assert(result >= 0, s"hardcoded flag id not found:\n\tlastInAxisFlags = $lastInAxisFlags\n\tflagIds = $flagIds\n\tflagIdAddrs = $flagIdAddrs")
      Some(result)
    } else
      None
  }

  val pipedLastInAxisFlags = RegInit(VecInit.fill(nElems, nLastInAxisFlags max 1)(false.B)).suggestName("pdFl")
  val lastInAxisFlagIsBeingSetByIn = lastInAxisFlagsOpt.map(x => VecInit.fill(x.size, x.head.size)(false.B).suggestName("liafibsbi"))
  val lastInAxisFlagIsBeingSetByOut = lastInAxisFlagsOpt.map(x => VecInit.fill(x.size, x.head.size)(false.B).suggestName("liafibsbo"))

  // lastInAxisFlagIsBeingSetByIn.foreach(x => dontTouch(x))

  val isBeingCleared = VecInit.fill(nElems)(false.B).suggestName("ibc")
  val isRunningAhead = VecInit(elems.map(_.op_count > fillCount)).suggestName("ira") // This lets us know whether an elem is being set by an in-port to an opCount that is larger than the current fillCount
  val opCountIsBeingSetTo = VecInit(elems.map(_.op_count)).suggestName("ocibst") // This helps us know if an in-port is setting an element to a higher op-count than what it was previously. This signal is useful for regfiles with grouped-in-ports in particular

  val nSubPipelines = subPipelineOpt.map(_._1: Int).getOrElse(1)

  val last_in = if (getLastInFromInOpCounts) {
    all(io.ins.map(_.bits.op_count > fillCount))
  } else {
    io.last_in
  }
  dontTouch(last_in)
  dontTouch(io.last_in)
  dontTouch(fillCount)
  dontTouch(emptyCount)

  println(s"\tconstantCoordsForInputs = ${constantCoordsForInputs.zipWithIndex}")
  println(s"\tcoordsThatNeedExpanding = ${coordsThatNeedExpanding}")

  // Handle piping
  println(s"\tDEBUG Handle piping")
  val transposingPipelines = transposingPipelinesOpt.nonEmpty
  val transposePostDelay = transposingPipelinesOpt.contains(true)
  val transposePreDelay = transposingPipelinesOpt.contains(false)
  val transposeSwitch = if (transposingPipelines) RegInit(false.B) else false.B

  val defaultPipelineElemIds = triangular match {
    case Some(increasing) =>
      val n_rectangular_elems = nElems - nSubPipelines * (1 to nPipelines).sum
      val base = n_rectangular_elems / (nPipelines * nSubPipelines); require(n_rectangular_elems % (nPipelines * nSubPipelines) == 0)
      if (increasing)
        (0 until nSubPipelines).map(subpipelineId =>
          (0 until nPipelines).map { pipelineId =>
            val len = pipelineId + 1 + base
            val start = (0 until pipelineId).map(_ + 1 + base).sum * nSubPipelines + subpipelineId * len
            (0 until len).map(i => start + i)
          })
      else
        (0 until nSubPipelines).map(subpipelineId =>
          (0 until nPipelines).map { pipelineId =>
            val len = nPipelines - pipelineId + base
            val start = (0 until pipelineId).map(nPipelines - _ + base).sum * nSubPipelines + subpipelineId * len
            (0 until len).map(i => start + i)
          })

    case None =>
      (0 until nSubPipelines).map(subpipelineId =>
        (0 until nPipelines).map(pipelineId =>
          (0 until (elemsPerPipeline/nSubPipelines)).map(i =>
            pipelineId * elemsPerPipeline + subpipelineId * (elemsPerPipeline/nSubPipelines) + i)))
  }

  val (squarePipelineElemIds, transposedPipelineElemIds) = {
    // These are only used when doing transpositions
    val square = defaultPipelineElemIds.map(_.map(x => (if (transposePostDelay) x.take(_) else x.takeRight(_))(nPipelines)))
    val squareT = if (transposingPipelines) square.map(_.transpose.map(_.reverse).reverse) else square // The if-statement here is just meant to avoid exceptions with triangular regfiles
    val post = defaultPipelineElemIds.map(_.map(x => x.drop(nPipelines)))
    val pre = defaultPipelineElemIds.map(_.map(x => x.dropRight(nPipelines)))
    (square, (if (transposePostDelay) squareT.zip(post) else pre.zip(squareT)).map { case (x, y) =>
      x.zip(y).map { case (a, b) => a ++ b }
    })
  }
  val transposedPipelineIsCompletelySquare = squarePipelineElemIds.flatten.flatten.size == nElems

  val transposePreDelayBufferOpCountBeingSetToOpt = Option.when(transposePreDelay && !transposedPipelineIsCompletelySquare)(VecInit.tabulate(nSubPipelines, nPipelines) { (subPipelineId, pipelineId) =>
    val elem_id = squarePipelineElemIds(subPipelineId)(pipelineId).head - 1
    assert(elem_id > 0, "we currently need at least one register in every level of the pre-delay")
    opCountIsBeingSetTo(elem_id)
  })
  val transposeSquareOpCount = Option.when(transposePreDelay && !transposedPipelineIsCompletelySquare)(RegInit(OpCount(0.U)))

  def pipelines(transposed: Bool): Seq[Seq[Seq[(UInt, Set[Int])]]] = {
    // This function returns the elemIds of the elements in each pipeline, based on the transposeSwitches
    // Return format: [SubPipelineId][PipelineId][ElemId]
    // We also return a set of the possible values the elemId can be (just to help with const-prop)
    if (transposed.litToBooleanOption.contains(false)) {
      defaultPipelineElemIds.map(_.map(_.map(x => (x.U, Set(x)))))
    } else if (transposed.litToBooleanOption.contains(true)) {
      transposedPipelineElemIds.map(_.map(_.map(x => (x.U, Set(x)))))
    } else {
      defaultPipelineElemIds.zip(transposedPipelineElemIds).map { case (defs, trans) =>
        require(defs.size == trans.size)
        defs.zip(trans).map { case (ds, ts) =>
          require(ds.size == ts.size)
          ds.zip(ts).map {
            case (d, t) if d == t => (d.U, Set(d))
            case (d, t) => (Mux(transposeSwitch, t.U, d.U), Set(d,t))
          }
        }
      }
    }
  }

  val isBalanced = defaultPipelineElemIds.flatten.map(_.size).distinct.size == 1 // We check here to see if every pipeline is the same length
  require(!transposingPipelines || defaultPipelineElemIds.map(_.map(_.size)) == transposedPipelineElemIds.map(_.map(_.size)), "transposed pipelines have a different size than the default pipeline")
  require((0 until nPipelines).forall(pipelineId => defaultPipelineElemIds.map(_(pipelineId).size).distinct.size == 1), "some pipelines have sub-pipelines which are not the same length")
  require(defaultPipelineElemIds.flatten.flatten.size == nElems, "not all elems fit into their pipelines")
  require(defaultPipelineElemIds.forall(x => x.nonEmpty && x.forall(_.nonEmpty)))

  val elemsTravel = Seq(entryOption, exitOption).exists(eo => eo.isInstanceOf[Edge] || eo == PerpendicularEdge)
  val elemsTravelPerpendicularly = Seq(entryOption, exitOption).contains(PerpendicularEdge) || transposingPipelines
  require(!elemsTravelPerpendicularly || nPipelines == nInPorts, "i'm not sure yet if perpendicular travelling works with grouped-in-ports")

  val inPipelines = pipelines(transposeSwitch)
  val outPipelines = pipelines(if (elemsTravelPerpendicularly) {
    transposeSwitch.litToBooleanOption match {
      case Some(b) => (!b).B
      case None => !transposeSwitch
    }
  } else transposeSwitch)

  val pipedHeadIsBeingClearedOpt = if (elemsTravel) Some(VecInit.fill(nSubPipelines, nPipelines)(false.B).suggestName("phbc")) else None

  val pipelineIdOfInPort = {
    // Format: [(pipelineId, inPortGroupPos)]
    val result = ArrayBuffer.fill(nInPorts)((-1, -1))
    val inPortGroupSize = nInPorts / nPipelines; require (nInPorts % nPipelines == 0)
    var start = 0
    for (pipelineId <- 0 until nPipelines) {
      start = result.indexOf((-1,-1), from=start)
      for (inPortGroupPos <- 0 until inPortGroupSize) {
        val id = start + inPortGroupPos * groupedInPortStride
        assert(result(id) == (-1,-1))
        result(id) = (pipelineId, inPortGroupPos)
      }
    }
    assert(!result.contains((-1,-1)))
    assert(result.distinct.size == result.size)
    result.toSeq
  }
  println(s"\tPipelineId of in-port [((pipelineId, inPortGroupPos), inPortId)] =\n\t\t${pipelineIdOfInPort.zipWithIndex.mkString("\n\t\t")}")

  if (elemsTravel) {
    require(nSubArrays == 1, "currently, we only support pipelines with just one subarray")
    val allFalseLastInAxisFlags = VecInit.fill(nLastInAxisFlags max 1)(false.B).suggestName("ff") // The only reason we define this variable up here is to reduce the FIRRTL LOC generated later

    val shiftIfAnyFreeSpot = entryOption == Edge(shiftIfAnyFreeSpot=true)
    require(!shiftIfAnyFreeSpot || !transposingPipelines, "i haven't checked yet if this shifting method works with transposing deadlines (although I think it should be fine as-is except for tranposing rfs with post-delays)")

    outPipelines.zipWithIndex.foreach { case (pipelines_, subPipelineId) =>
      pipelines_.zipWithIndex.foreach { case (pipeline, pipelineId) =>
        val evclros = pipeline.map { case (elemId, possibleElemIds) => (possibleElemIds, elemId, elems(elemId), elem_valids(elemId), isBeingCleared(elemId), pipedLastInAxisFlags(elemId), isRunningAhead(elemId), opCountIsBeingSetTo(elemId)) }

        val (_, _, _, tailIsValid, tailIsBeingCleared, _, _, _) = evclros.last
        lazy val (sqTailPossibleElemIds, sqTailElemId, _, sqTailIsValid, sqTailIsBeingCleared, _, _, _) = evclros(nPipelines-1) // This is only used when doing transposes

        val (_, _, headElem, headValid, _, headLastInAxisFlags, headIsRunningAhead, _) = evclros.head
        val headShouldShift = WireInit(noPrefix(!evclros.tail.headOption.map { case (_, _, _, v, _, _, _, _) => any(v) }.getOrElse(true.B) || !any(tailIsValid) || tailIsBeingCleared)).suggestName("hss")
        when(headShouldShift) {
          headValid.foreach(_ := false.B)
        }

        // Note: This is not quite as "performant" as a real pipeline with a long, fully combinational "ready" signal.
        //   We only check whether the last element (the "tail") of the pipeline is being popped, even though this can
        //   result in stalling, bubble cycles sometimes, because we believe this will minimize the length of the
        //   combinational paths.
        evclros.zip(evclros.tail).zipWithIndex.reverse.foreach { case (((_, _, e, v, c, l, _, _), (_, _, ne, nv, _, nl, _, no)), i) =>
          val isInSquare = transposingPipelines && i < nPipelines && transposePostDelay &&
            !transposedPipelineIsCompletelySquare // No need for a distinction between the "square" and everything else if everything is in the square anyways
          val isTailOfSquare = isInSquare && i == nPipelines - 1
          val (tid, tibc) = if (isInSquare && !isTailOfSquare) (sqTailIsValid, sqTailIsBeingCleared) else (tailIsValid, tailIsBeingCleared)

          val nextSlotIsFree = noPrefix(if (shiftIfAnyFreeSpot) any(evclros.drop(i+1).dropRight(1).map { case (_, _, _, fv, _, _, _, _) => !any(fv) }) else !any(nv)).suggestName("nsif")

          val shouldShift = noPrefix(all(Seq(nextSlotIsFree || !any(tid) || tibc) ++
            Option.when(isTailOfSquare)(e.op_count < fillCount))).suggestName("ss")

          when (shouldShift) {
            ne := e
            nv := (if (exitOption.isInstanceOf[Edge] || exitOption == PerpendicularEdge) v else Seq(v.head && !c)); require(nSubArrays == 1)
            if (nPipelines == nInPorts) {
              // This if-clause is just meant to reduce FIRRTL LOC. We should be able to replace it with just the "else"
              // clause below without breaking correctness
              nl := l
            } else {
              nl := Mux(no > e.op_count, allFalseLastInAxisFlags, l) // TODO should we also have a special case for if "no > e.op_count && no === fillCount"?
            }
            v.foreach(_ := false.B)

            if (isTailOfSquare) {
              // The code below was just supposed to be this one line, but that made FIRRTL's comb-path checker return false positives:
              //   sqTailIsBeingCleared := true.B
              // We've replaced it with the equivalent code below:
              sqTailPossibleElemIds.foreach { possibleElemId =>
                when (sqTailElemId === possibleElemId.U) {
                  isBeingCleared(possibleElemId) := true.B
                }
              }
            }

            val shiftingIntoTailOfSquare = transposingPipelines && !transposedPipelineIsCompletelySquare && transposePreDelay &&
              i == evclros.size - nPipelines - 2
            if (shiftingIntoTailOfSquare) {
              transposePreDelayBufferOpCountBeingSetToOpt.foreach(_(subPipelineId)(pipelineId) := e.op_count)
            }
          }

          if (i == 0 && (shiftIfAnyFreeSpot || transposingPipelines && !transposedPipelineIsCompletelySquare)) {
            // For transposed RFs with padding, or aggressively shifting RFs, the "headShouldShift" variable above is
            //   incorrect because it will look at the tail rather than the square's tail. We set it correctly in this
            //   if-statement:
            headShouldShift := shouldShift
          } else if (i == 0 && withAsserts)
            assert(shouldShift === headShouldShift, "'headShouldShift' above should be identical to the head pipeline elem's 'shouldShift' value. we only brought it out of this loop so that we could support the case where 'elemsPerPort' is 1")
        }

        if (nPipelines == nInPorts)
          pipedHeadIsBeingClearedOpt.foreach(_(subPipelineId)(pipelineId) := headShouldShift)
        else {
          pipedHeadIsBeingClearedOpt.foreach { phibc =>
            val groupedHead = nInPorts / nPipelines - 1; require(nInPorts % nPipelines == 0 && groupedHead > 0, s"groupedHead=$groupedHead")
            phibc(subPipelineId)(pipelineId) := !any(evclros.take(groupedHead).map { case (_, _, _, v, _, _, _, _) => any(v) }) &&
              (!(evclros.lift(groupedHead+1).map { case (_, _, _, v, _, _, _, _) => any(v) }.getOrElse(true.B)) || !any(tailIsValid) || tailIsBeingCleared)
          }
        }

        when(headShouldShift || !any(headValid)) {
          if (getTravellingOpCountFromInPorts) {
            val inPortId = pipelineIdOfInPort.indexOf((pipelineId, nInPorts / nPipelines - 1))
            headElem.op_count := io.ins(inPortId).bits.op_count
            headIsRunningAhead := io.ins(inPortId).bits.op_count > fillCount
          } else {
            headElem.op_count := fillCount // TODO I'm not sure if this works with shifted parallelogram-like inputs
          }
          lastInAxisFlagsOpt.zip(lastInAxisFlagIsBeingSetByIn).zip(lastInAxisFlagIsBeingSetByOut).foreach {
            case ((flagsVec, setByInVec), setByOutVec) =>
              val Seq(flags, setByIn, setByOut) = Seq(flagsVec, setByInVec, setByOutVec).map(_(if (separateLastInAxisFlagsForEachPipeline) pipelineId else 0))

              if (resetLastInAxisFlagsBasedOnTravellingOpCounts) {
                val inPortId = pipelineIdOfInPort.indexOf((pipelineId, nInPorts / nPipelines - 1))
                headLastInAxisFlags := Mux(io.ins(inPortId).bits.op_count =/= RegNext(io.ins(inPortId).bits.op_count),
                  setByIn, vecOr(flags, setByIn, setByOut))
              } else {
                headLastInAxisFlags := Mux(headIsRunningAhead, allFalseLastInAxisFlags, vecOr(flags, setByIn, setByOut))
              }
          }
        }
      }
    }
  }

  val hardCodedCoords: Seq[Seq[Option[Int]]] = {
    // For each element in the regfile, this variable will tell you which of its coords we have determined can be
    // hardcoded. This is simply used to help with const-prop.
    val result = ArrayBuffer.fill(nElems, nIOCoords)(None: Option[Int])

    if (!transposingPipelines && !elemsTravelPerpendicularly && groupedInPortStride == 1 /* TODO support hardcoding with bigger groupedInPortStrides as well */) {
      defaultPipelineElemIds.foreach(_.zipWithIndex.foreach { case (elemIdss, pipelineId) =>
        elemIdss.foreach { elemId =>
          val inPortIds = pipelineIdOfInPort.zipWithIndex.collect { case ((`pipelineId`, _ /*inPortGroupPos*/), inPortId) => inPortId }
          Seq.tabulate(nIOCoords) { coordId =>
            val constSetForAllInPorts = inPortIds.forall(inPortId => constantCoordsForInputs.lift(inPortId).exists(_(coordId).nonEmpty))
            val constOpts = inPortIds.flatMap(inPortId => constantCoordsForInputs.lift(inPortId).map(_(coordId))).distinct
            if (constSetForAllInPorts && constOpts.size == 1) {
              assert(constOpts.nonEmpty)
              println(s"\tHardcoding elemId=$elemId coord-$coordId to ${constOpts.head.get} | pipeline=$pipelineId")
              result(elemId)(coordId) = constOpts.head
            }
          }
        }
      })
    }

    subPipelineOpt match { case Some((nSubCounters, coordId, false, true)) =>
      require(isBalanced, "the code below assumes square regfiles")
      result.grouped(elemsPerPipeline).foreach { hcsGroupedByPort =>
        hcsGroupedByPort.grouped(elemsPerPipeline / nSubCounters).zipWithIndex.foreach { case (hcsGroupedBySubCounter, subCounterId) =>
          hcsGroupedBySubCounter.foreach(_(coordId) = Some(subCounterId))
        }
      }

      case Some((_, _, true, true)) => throw new Exception("Are you sure you want to hard-code expanded addresses?")

      case _ =>
    }

    result.map(_.toSeq).toSeq
  }
  def elemCoordsWithHardcoding[T <: Data](elemId: Int, t: T): Seq[T] = {
    elems(elemId).coords.zip(hardCodedCoords(elemId)).map {
      case (_, Some(const)) =>
        t match {
          case _: SInt => const.S
          case _: UInt => const.U
          case _ => throw new Exception("Incorrect type")
        }

      case (coord, None) =>
        t match {
          case _: SInt => coord
          case _: UInt => coord.asUInt
          case _ => throw new Exception("Incorrect type")
        }
    }.map(_.asInstanceOf[T])
  }
  val fullyHardcodedCoordIds = Seq.tabulate(nIOCoords) { coordId =>
    hardCodedCoords.forall(_(coordId).nonEmpty)
  }

  println("\tDEBUG Pipeline ids =")
  for ((subPipeline, subPipelineId) <- pipelines(false.B).zipWithIndex)
    for ((pipeline, pipelineId) <- subPipeline.zipWithIndex)
      for ((_, elemId) <- pipeline)
        println(s"\t\telemId=$elemId | pipelineId=$pipelineId | subPipelineId=$subPipelineId | hardcodedCoords=${hardCodedCoords(elemId.head)}")

  // Handle counters and last-signals
  println(s"\tDEBUG Handle counters and last-signals")
  io.fillCount := transposeSquareOpCount.getOrElse(fillCount)
  io.emptyCount := emptyCount

  if (lockstepIns && !transposingPipelines) {
    // TODO it should be possible to optimize isEmpty like this (without needing a giant combinational path from all
    //   registers in the RegFile) for cases where lockStepIns is not true as well
    val isEmpty = RegInit(true.B)
    when (io.last_in) {
      isEmpty := true.B
    }.elsewhen(io.ins.head.fire) {
      isEmpty := false.B
    }
    io.isEmpty := isEmpty
  } else {
    io.isEmpty := transposingPipelines.B || // Note: when we're transposing, we don't attempt early-starts, because we need to wait for the inputs to travel through the entire spatial array anyways
      fillCount >= emptyCount && all(elems.zip(elem_valids).map { case (e, v) =>
        !any(v) || e.op_count =/= fillCount
      })
  }

  val debug_cycle = RegInit(0.U(32.W))
  debug_cycle := debug_cycle + 1.U

  lastInAxisFlagsOpt.zip(lastInAxisFlagIsBeingSetByIn).foreach { case (lastInAxisFlagRegsVec, flagIsBeingSetVec) =>
    io.ins.zipWithIndex.foreach { case (in, inPortId) =>
      val (pipelineId, _) = pipelineIdOfInPort(inPortId)
      val lastInAxisFlagRegs = lastInAxisFlagRegsVec(if (separateLastInAxisFlagsForEachPipeline) pipelineId else 0)
      val flagIsBeingSet = flagIsBeingSetVec(if (separateLastInAxisFlagsForEachPipeline) pipelineId else 0)

      val op_count = in.bits.op_count
      if (withAsserts) assert(!any(in.bits.last_in_axis) || op_count === fillCount, "the last-in-axis signal (from inputs) is arriving early; we don't currently have a way of buffering it until the fillCount catches up")

      if (resetLastInAxisFlagsBasedOnTravellingOpCounts) {
        when (in.bits.op_count =/= RegNext(in.bits.op_count)) {
          assert(separateLastInAxisFlagsForEachPipeline) // TODO remove this restriction
          lastInAxisFlagRegsVec(pipelineId).foreach(_ := false.B)
        }
      }

      when (in.bits.last_in_axis.head) {
        val flag_id = flattenedFlagId(in.bits.coords.map(_.asUInt))
        lastInAxisFlagRegs(flag_id) := true.B
        flagIsBeingSet(flag_id) := true.B
      }

      in.bits.last_in_axis.take(lastInAxisFlags.size + (if (dontCheckExtraLastInAxisFlag) 0 else 1) /* TODO I'm not sure if the "+1" always works when we have banked SRAMs feeding this regfile. What if one bank sets the last_in_axis(1) while the second bank is still issuing outputs for those coordinates? */).zipWithIndex.tail.foreach { case (last_in_axis, axisId) =>
        when (last_in_axis) {
          val ranges = lastInAxisFlags.take(axisId).map(_._1) match {
            case nFlagss :+ nFlags => nFlagss.map(0 until _) :+ (1 until nFlags)
          }
          val offsets = crossJoin(ranges)

          offsets.foreach { offset =>
            val (size, coordId) = lastInAxisFlags(axisId-1)
            when (offset.toSeq.last.U > (in.bits.coords(coordId).asUInt % size.U)) {
              val coords = WireInit(in.bits.coords)
              offset.zip(lastInAxisFlags.map(_._2)).foreach { case (off, coordId) =>
                coords(coordId) := off.S
              }
              val flag_id = flattenedFlagId(coords.map(_.asUInt))
              lastInAxisFlagRegs(flag_id) := true.B
              flagIsBeingSet(flag_id) := true.B
            }
          }
        }
      }
    }
  }

  when (last_in) {
    fillCount := fillCount + 1.U
    if (!resetLastInAxisFlagsBasedOnTravellingOpCounts)
      lastInAxisFlagsOpt.foreach(_.foreach(_.foreach(_ := false.B)))
    if (transposingPipelines && !(transposePreDelay && !transposedPipelineIsCompletelySquare)) {
      // TODO I don't think this works with parallelograms
      // TODO I don't know if this works with matrices that need to be padded
      transposeSwitch := !transposeSwitch
    }
  }

  if (transposingPipelines && transposePreDelay && !transposedPipelineIsCompletelySquare) {
    when (all(transposePreDelayBufferOpCountBeingSetToOpt.get.flatten.map(_ > transposeSquareOpCount.get))) {
      transposeSquareOpCount.get := transposeSquareOpCount.get + 1.U
      transposeSwitch := !transposeSwitch
    }
  }

  if (lastInAxisFlagsFromOutPorts) lastInAxisFlagsOpt.zip(lastInAxisFlagIsBeingSetByOut).foreach { case (lastInAxisFlagRegsVec, flagIsBeingSetVec) =>
    // TODO consolidate this code with the "last-in-axis" for inputs code block above, to reduce duplicated code
    val lastInAxisFlagRegs = lastInAxisFlagRegsVec.head; require(!separateLastInAxisFlagsForEachPipeline)
    val flagIsBeingSet = flagIsBeingSetVec.head; require(!separateLastInAxisFlagsForEachPipeline)

    io.outs.foreach { out =>
      // TODO does this work when elemsTravel? I'm especially worried about how we directly set "valids" here instead of setting "isBeingCleared"
      val op_count = out.op_count
      if (withAsserts) assert(!any(out.last_in_axis) || op_count <= fillCount, "the last-in-axis signal (from outputs) is arriving early; we don't currently have a way of buffering it until the fillCount catches up")

      when (out.last_in_axis.head) {
        val flag_id = flattenedFlagId(out.coords.map(_.asUInt))
        when (op_count === fillCount && !last_in) {
          lastInAxisFlagRegs(flag_id) := true.B
          flagIsBeingSet(flag_id) := true.B
        }

        elems.zip(elem_valids).foreach { case (elem, valids) =>
          val elem_flag_id = flattenedFlagId(elem.coords.map(_.asUInt))
          when (any(valids) && elem_flag_id === flag_id && elem.op_count === op_count) {
            if (withAsserts) assert((valids.size == 1).B, s"We don't yet support last-in-axis signals with sub-arrays")
            valids.foreach(_ := false.B)
          }
        }
      }

      out.last_in_axis.take(lastInAxisFlags.size).zipWithIndex.tail.foreach { case (last_in_axis, axisId) =>
        when (last_in_axis) {
          val ranges = lastInAxisFlags.take(axisId).map(_._1) match {
            case nFlagss :+ nFlags => nFlagss.map(0 until _) :+ (1 until nFlags)
          }
          val offsets = crossJoin(ranges)

          offsets.foreach { offset =>
            when (offset.toSeq.last.S > out.coords(lastInAxisFlags(axisId - 1)._2)) {
              val coords = offset.zip(lastInAxisFlags.map(_._2)).foldLeft(out.coords.map(_.asUInt)) {
                case (acc, (off, coordId)) => acc.updated(coordId, off.U)
              }

              val flag_id = flattenedFlagId(coords)
              when (op_count === fillCount && !last_in) {
                lastInAxisFlagRegs(flag_id) := true.B
                flagIsBeingSet(flag_id) := true.B
              }

              elems.zip(elem_valids).foreach { case (elem, valids) =>
                val elem_flag_id = flattenedFlagId(elem.coords.map(_.asUInt))
                when (any(valids) && elem_flag_id === flag_id && elem.op_count === op_count) {
                  if (withAsserts) assert((valids.size == 1).B, s"We don't yet support last-in-axis signals with sub-arrays")
                  valids.foreach(_ := false.B)
                }
              }
            }
          }
        }
      }
    }
  }

  when (io.last_out) {
    emptyCount := emptyCount + 1.U
    if (!allElemsWillBePoppedBeforeLastOut)
      elems.zip(elem_valids).zip(isBeingCleared).foreach { case ((e, v), isCleared) =>
        when (e.op_count === emptyCount) {
          if (!elemsTravel) {
            // If we are piping, then we want to just let the pipe logic determine what the "elem_valid" will be in the
            // next cycle
            v.foreach(_ := false.B)
          }
          isCleared := true.B
        }
      }
  }

  // Handling inputs
  println(s"\tDEBUG Handling inputs")
  val inPortCounters = Seq.tabulate(nPipelines)(pipelineId => RegInit(VecInit.fill(nSubPipelines)(0.U(log2Up(defaultPipelineElemIds.head(pipelineId).size / nSubPipelines).W))))
  val elemIdsForInPortCounters = inPortCounters.zipWithIndex.map { case (cntrs, i) =>
    def foo(subCounter: UInt): UInt = entryOption match {
      case Edge(_) =>
        val heads = inPipelines.map(_(i).head).map(_._1)
        selectFrom(heads, subCounter)

      case Incrementing(_) =>
        require(isBalanced)
        val cntr = cntrs(subCounter)
        (i*elemsPerPipeline).U + subCounter*(elemsPerPipeline/nSubPipelines).U +& cntr

      case _ => throw new Exception(s"Invalid entry-option used: $entryOption")
    }
    foo(_)
  }
  val incrementedInPortCounters = inPortCounters.zipWithIndex.map { case (cntrs, pipelineId) =>
    VecInit(cntrs.zipWithIndex.map { case (cntr, subPipelineId) =>
      val elemsPerSubPipeline = defaultPipelineElemIds(subPipelineId)(pipelineId).size

      if (entryOption == Incrementing(skipOccupiedElems = true)) {
        require(nInPorts == nPipelines, "haven't added support for certain types of RFs with this entry option")
        val eids = inPipelines(subPipelineId)(pipelineId).map(_._1).zipWithIndex
        val current_eid = elemIdsForInPortCounters(pipelineId)(subPipelineId.U)
        val default_next_cntr = Mux(cntr + 1.U >= elemsPerSubPipeline.U, 0.U, cntr + 1.U)
        MuxCase(default_next_cntr, eids.map { case (eid, i) => (!any(elem_valids(eid)) && eid =/= current_eid) -> i.U })
      } else {
        val inPortGroupSize = nInPorts / nPipelines; require(nInPorts % nPipelines == 0)
        // This if-statement is just here to help with const-prop. We could replace the whole if-statement with just the "else" clause
        if (elemsPerSubPipeline == inPortGroupSize) 0.U
        else if (isPow2(elemsPerSubPipeline) && cntr.getWidth == log2Ceil(elemsPerSubPipeline)) cntr +% inPortGroupSize.U
        else if (inPortGroupSize == 1) Mux(cntr + 1.U === elemsPerSubPipeline.U, 0.U, cntr + 1.U)
        else {
          // TODO it would be more performant to give each inPort sharing an inPipeline its own inPortCounter
          require(elemsPerSubPipeline >= inPortGroupSize)
          Mux(cntr + inPortGroupSize.U >= elemsPerSubPipeline.U, (cntr + inPortGroupSize.U) - elemsPerSubPipeline.U, cntr + inPortGroupSize.U)
        }
      }
    })
  }

  def efficientAccess[T <: Data](has_default: Boolean, id: UInt, idRange: Iterable[Int], values: Vec[T])(foo: T => Any): Unit = {
    // This function only exists to help with const-prop
    for (id_ <- idRange) {
      def whenClause(bar: => Any) = if (has_default && id_ == idRange.head) bar
      else when(id_.U === id) { bar }

      whenClause {
        foo(values(id_))
      }
    }
  }

  if (entryOption.isInstanceOf[Incrementing] && !lockstepIns && !shouldLockstepOuts) {
    // By default, the inPortCounters just keep rotating whenever they're on an already-occupied entry
    inPortCounters.zip(incrementedInPortCounters).zip(elemIdsForInPortCounters).zipWithIndex.foreach { case (((cntrs, incr_cntrs), elem_ids), pipelineId) =>
      cntrs.zip(incr_cntrs).zipWithIndex.foreach { case ((cntr, incr_cntr), subPipelineId) =>
        val elem_id = elem_ids(subPipelineId.U)

        val rangeToCheck = defaultPipelineElemIds(subPipelineId)(pipelineId); require(!transposingPipelines, "this line assumes that we're not transposing")

        /* We had to remove the commented-out code because it was making Firesim's const-prop very angry. We replaced it
         * instead with the equivalent when-statement below (which doesn't use our "efficientAccess" convenience wrapper).
        efficientAccess(false, elem_id, rangeToCheck, elem_valids) { evalid =>
          when (any(evalid)) {
            cntr := incr_cntr
          }
        }
        */
        when (any(rangeToCheck.map(eid => any(elem_valids(eid)) && elem_id === eid.U))) {
          cntr := incr_cntr
        }
      }
    }
  }

  val inPort0IncrementedCntr = Wire(Bool()) // This is used for lockstep inputs

  val in_port_writing_to = Wire(Vec(nInPorts, UInt(log2Up(nElems).W))) // This is a debugging signal
  // val in_port_arrived_late = Wire(Vec(nInPorts, Bool())) // This is a debugging signal
  // Seq(in_port_writing_to/*, in_port_arrived_late*/).foreach(dontTouch(_))

  io.ins.zipWithIndex.foreach { case (in, inPortId) =>
    val inPortGroupSize = nInPorts / nPipelines; require(nInPorts % nPipelines == 0)
    val (pipelineId, inPortGroupPos) = pipelineIdOfInPort(inPortId)
    val inPortGroupOffset = inPortGroupSize - inPortGroupPos - 1

    val subCounter = subPipelineOpt match {
      case Some((_, coordId, false, _)) => (in.bits.coords(coordId) % nSubPipelines.S).asUInt
      case Some((_, coordId, true, _)) => (in.bits.element.domainCoords(coordId) % nSubPipelines.S).asUInt
      case None => 0.U
    }
    val cntr = inPortCounters(pipelineId)(subCounter) // TODO it would be more performant to give each inPort sharing an inPipeline its own inPortCounter
    val incr_cntr = incrementedInPortCounters(pipelineId)(subCounter)
    val elem_id = elemIdsForInPortCounters(pipelineId)(subCounter) + inPortGroupOffset.U

    println(s"\tDEBUG inPortId=$inPortId pipelineId=$pipelineId inPortGroupPos=$inPortGroupPos nElems=$nElems")

    val elem = elems(elem_id)

    in_port_writing_to(inPortId) := elem_id

    // Normally, we would just set "evalid" with the code below:
    //   val evalid = elem_valids(elem_id)
    // However, Chisel's const-prop doesn't recognize that there's only a limited range of values that 'elem_id' can
    // take, which makes the generated Verilog excessively long. To get around this issue, we manually loop over the
    // restricted set of values that "elem_id" can take, using the helper functions below
    val rangeToCheck = if (entryOption.isInstanceOf[Edge]) {
      (Seq(defaultPipelineElemIds) ++ Option.when(transposingPipelines)(transposedPipelineElemIds)).flatMap(_.map(_(pipelineId)(inPortGroupOffset)))
    } else if (entryOption.isInstanceOf[Incrementing] && !transposingPipelines) {
      (Seq(defaultPipelineElemIds) ++ Option.when(transposingPipelines)(transposedPipelineElemIds)).flatMap(_.flatMap(_(pipelineId)))
    } else {
      0 until nElems
    }
    def accessElemValid(has_default: Boolean, _elem_id: UInt = elem_id)(foo: Vec[Bool] => Any): Unit =
      efficientAccess(has_default, _elem_id, rangeToCheck, elem_valids)(foo)
    def accessElem(_elem_id: UInt = elem_id)(foo: SpatialArrayOutPort => Any): Unit =
      efficientAccess(false, _elem_id, rangeToCheck, elems)(foo)
    def accessRunningAhead(foo: Bool => Any): Unit =
      efficientAccess(false, elem_id, rangeToCheck, isRunningAhead)(foo)
    def accessOpCountIsBeingSetTo(foo: OpCount => Any): Unit =
      efficientAccess(false, elem_id, rangeToCheck, opCountIsBeingSetTo)(foo)

    accessElemValid (true) { evalid =>
      in.ready := any(Seq(!any(evalid)) ++ Option.when(elemsTravel)(pipedHeadIsBeingClearedOpt.get(subCounter)(pipelineId)))
    }
    if (exitOption != Anywhere && inPortGroupOffset > 0) {
      val precedingInPort = (0 until nInPorts).find { i =>
        val (_pipelineId, _inPortGroupPos) = pipelineIdOfInPort(i)
        _pipelineId == pipelineId && _inPortGroupPos == inPortGroupPos + 1
      }.get
      when (!io.ins(precedingInPort).ready) {
        in.ready := false.B
      }
    }

    val arrivedLate = if (allElemsWillBePoppedBeforeLastOut) false.B else {
      val flag_id = if (lastInAxisFlags.nonEmpty) flattenedFlagId(in.bits.coords.map(_.asUInt), in.valid) else 0.U
      val op_count_is_behind_empty_count = if (allElemsWillBePoppedBeforeLastOut) false.B else (in.bits.op_count < (emptyCount + io.last_out))
      op_count_is_behind_empty_count || in.bits.op_count === fillCount && (
        lastInAxisFlagsOpt.map(_(if (separateLastInAxisFlagsForEachPipeline) pipelineId else 0)(flag_id)).getOrElse(false.B) ||
        lastInAxisFlagIsBeingSetByOut.map(_(if (separateLastInAxisFlagsForEachPipeline) pipelineId else 0)(flag_id)).getOrElse(false.B))
    }

    // in_port_arrived_late(inPortId) := arrivedLate

    when (in.fire && !arrivedLate) {
      accessElem()(_ := in.bits)
      accessElemValid(false)(_.foreach(_ := true.B))
      accessRunningAhead(_ := in.bits.op_count > fillCount)
      if (nPipelines != nInPorts || transposePreDelay && !transposedPipelineIsCompletelySquare) accessOpCountIsBeingSetTo(_ := in.bits.op_count) // The "if" is just to help reduce FIRRTL LOC. We should be able to run this line in all cases without breaking correctness.

      val constCoords = if (inPortId < constantCoordsForInputs.size) constantCoordsForInputs(inPortId) else Seq.empty
      elem.coords.zip(constCoords).foreach {
        case (coord, Some(const)) => coord := const.S
        case _ =>
      }

      elem.coords.zipWithIndex.filter(incrementingCoordsForInputs.map(_._1) contains _._2).map(_._1).foreach(_ := maxOutCoordOpt.map(cntr.zext % _.S).getOrElse(cntr.zext)) // TODO we need a separate counter for the "incrementing coords". Using the inPortCounter will lead to errors sometimes, especially for padded inputs
      require(incrementingCoordsForInputs.isEmpty || nInPorts == nPipelines, "i haven't updated the incrementing coords code to support grouped-in-ports yet")

      if (withAsserts && !elemsTravel)
        assert(!elem_valids.zip(elems).map { case (v, e) => any(v) && all(e.coords.zip(in.bits.coords).map(t => t._1 === t._2)) && e.op_count === in.bits.op_count }.reduce(_ || _),
          "overwriting existing element in regfile")
    }

    if (getTravellingCoordsFromInPorts.nonEmpty && inPortGroupPos == inPortGroupSize-1) withoutPrefix {
      val in_coords = if (stickyInPortCoords.nonEmpty) WireInit(in.bits.coords) else (in.bits.coords)

      if (stickyInPortCoords.nonEmpty) {
        val stuckCoords = RegInit(VecInit.fill(nSubPipelines, in_coords.size)(0.U(in_coords.head.getWidth.W))).suggestName(s"stuckCoords_port$inPortId")
        val stuckOpCount = RegInit(VecInit.fill(nSubPipelines)(OpCount(0.U))).suggestName(s"stuckOpCount_port$inPortId")

        when (in.bits.op_count === stuckOpCount(subCounter)) {
          stuckCoords(subCounter) := stuckCoords(subCounter).zip(in.bits.coords).map { case (sc, ic) =>
            maxOf(sc, ic.asUInt +& in.bits.increment_sticky_coord)
          }
          stickyInPortCoords.foreach { coordId =>
            in_coords(coordId) := maxOf(in.bits.coords(coordId).asUInt, stuckCoords(subCounter)(coordId)).asSInt
          }
        }.elsewhen(in.bits.op_count > stuckOpCount(subCounter)) {
          stuckCoords(subCounter) := in.bits.coords.map(_.asUInt +& in.bits.increment_sticky_coord)
          stuckOpCount(subCounter) := in.bits.op_count
        }

        for (_subCounter <- 0 until nSubPipelines) {
          when (_subCounter.U =/= subCounter) { // Write to the other sub-pipelines' coords as well, rather than just writing to the in-port's current sub-pipeline
            val _elem_id = elemIdsForInPortCounters(pipelineId)(_subCounter.U) + (inPortGroupSize - inPortGroupPos - 1).U
            val _in_ready = WireInit(false.B)
            accessElemValid (true, _elem_id) { _evalid =>
              _in_ready := any(Seq(!any(_evalid)) ++ Option.when(elemsTravel)(pipedHeadIsBeingClearedOpt.get(_subCounter)(pipelineId)))
            }
            when (_in_ready) {
              accessElem(_elem_id) { _elem =>
                stickyInPortCoords.foreach { coordId =>
                  _elem.coords(coordId) := Mux(getTravellingOpCountFromInPorts.B && in.bits.op_count =/= stuckOpCount(_subCounter),
                    0.S, // If we can get the travelling op-count from the in-port, then we need to account for the possibility that the elem's op-count will diverge from the stuckOpCount
                    stuckCoords(_subCounter)(coordId).asSInt)
                }
              }
            }
          }
        }
      }

      when (in.ready && !arrivedLate) {
        accessElem() { _elem =>
          getTravellingCoordsFromInPorts.foreach { coordId =>
            _elem.coords(coordId) := in_coords(coordId)
          }
        }
      }
    }

    val should_increment_cntr = if (lockstepIns && inPortId > 0) inPort0IncrementedCntr else {
      val result = in.fire && !arrivedLate
      inPort0IncrementedCntr := result
      result
    }
    when (should_increment_cntr) {
      cntr := incr_cntr
    }
  }

  // Handling outputs
  println(s"\tDEBUG Handling outputs")
  val headOutPortMatchingIds = Wire(Vec(nLockSteppedPortGroups max 1, UInt(log2Up(nElems).W))) // This is only used for lockstep outputs
  val headOutPortFounds = Wire(Vec(nLockSteppedPortGroups max 1, Bool())) // This is only used for lockstep outputs

  headOutPortMatchingIds := DontCare
  headOutPortFounds := DontCare

  val debug_out_matchingIds = Wire(Vec(nOutPorts, UInt(log2Up(nElems).W)))

  // val out_port_not_found = any(io.outs.map(out => out.valid && !out.found && !out.unavailable)); dontTouch(out_port_not_found) // Debugging signal
  // val not_found_out_port_id = PriorityEncoder(io.outs.map(out => out.valid && !out.found && !out.unavailable)); dontTouch(not_found_out_port_id) // Debugging signal
  // val not_found_out_port_checking = VecInit.fill(if (shortenNames) 1 else nElems)(false.B); dontTouch(not_found_out_port_checking) // Debugging signal
  val debug_out_port_checking_id = 0
  val debug_out_port_checking = VecInit.fill(nElems)(false.B)
  // dontTouch(debug_out_port_checking)

  val pipelineExits: Seq[(UInt, Int /* PipelineId */, Int /* SubPipelineId */)] = if (exitOption.isInstanceOf[Edge] || exitOption == PerpendicularEdge) {
    outPipelines.zipWithIndex.flatMap { case (pipeline_, subPipelineId) => pipeline_.map(_.map(_._1)).map(_.last).zipWithIndex.map { case (exit, pipeId) => (exit, pipeId, subPipelineId) }}
  } else if (exitOption == Anywhere) {
    outPipelines.zipWithIndex.flatMap { case (pipeline_, subPipelineId) => pipeline_.map(_.map(_._1)).zipWithIndex.flatMap { case (exit, pipeId) => exit.map((_, pipeId, subPipelineId)) }}
  } else {
    throw new Exception(s"Unsupported exit option: $exitOption")
  }

  io.outs.zipWithIndex.foreach { case (out, outPortId) =>
    val (outCoords, outCoordsU) = {
      val result = out.coords.zipWithIndex.map { case (coord, coordId) =>
        val _coord = Option.when(constantCoordsForOutputs.size > outPortId)(constantCoordsForOutputs(outPortId)(coordId)).flatten.map(c => (c.S, c.U)).getOrElse(coord, coord.asUInt)

        maxOutCoordOpt match {
          case Some(maxCoord) if _coord._1.isLit => ((_coord._1.litValue % maxCoord).S, (_coord._2.litValue % maxCoord).U)
          case Some(maxCoord) => (_coord._1 % maxCoord.S, _coord._2 % maxCoord.U)
          case None => _coord
        }
      }

      (result.map(_._1), result.map(_._2))
    }

    type ElemToCheckT = (((SpatialArrayOutPort /* Elem */, Vec[Bool] /* Valid */), UInt /* ElemId */), Bool /* ShouldCheck */)
    val elemsToCheck: Seq[ElemToCheckT] = {
      val withoutSticky = pipelineExits.map { case (elemId, pipelineId, subPipelineId) => (elems(elemId), elem_valids(elemId), elemId, pipelineId, subPipelineId) }.filter {
        case (_, _, elemId, _, _) =>
          !elemId.isLit || constantCoordsForOutputs.size <= outPortId || hardCodedCoords(elemId.litValue.toInt).zip(constantCoordsForOutputs(outPortId)).forall {
            case (Some(c1), Some(c2)) => c1 == c2
            case _ => true
          }
      }.map {
        case (e, v, elemId, pipelineId, subPipelineId) if !transposingPipelines && exitOption != PerpendicularEdge =>
          val constCoordsMatch = {
            val constCoords = if (pipelineId < constantCoordsForInputs.size && nInPorts == nPipelines) constantCoordsForInputs(pipelineId) else Seq.empty
            vecEquals(outCoords.zip(constCoords).collect {
              case (outCoord, Some(constCoord)) => (outCoord, constCoord.S)
            }).suggestName("ccm")
          }

          val subPipelineMatches = subPipelineOpt match {
            case Some((_, subCounterCoord,  false /*fromDomainCoords*/, true /*hardCodeCoords*/)) =>
              if (outCoords(subCounterCoord).isLit) (outCoords(subCounterCoord).litValue == subPipelineId).B // This line is just here to help with const-prop. We could replace it with the "else" clause without affecting the generated hardware
              else (outCoords(subCounterCoord) === subPipelineId.S).suggestName("spm")

            case Some((nSubCounters, subCounterCoord,  false /*fromDomainCoords*/, false /*hardCodeCoords*/)) =>
              ((outCoords(subCounterCoord) % nSubCounters.S) === subPipelineId.S).suggestName("spm")

            case _ => true.B
          }

          val coordsAreDivisible = if (coordIsDivisibleByForPipelines.nonEmpty) withoutPrefix {
            val divisibleBy = coordIsDivisibleByForPipelines(pipelineId)
            all(outCoords.zip(divisibleBy).collect {
              case (outCoord, Some((divisBy, bias))) => outCoord % divisBy.S === bias.S
            }).suggestName("cd")
          } else true.B

          val matches = all(Seq(constCoordsMatch, subPipelineMatches, coordsAreDivisible))

          (((e, v), (elemId, pipelineId)), matches)

        case (e, v, elemId, pipelineId, subPipelineId) if !transposingPipelines && exitOption == PerpendicularEdge =>
          val subPipelineMatches = subPipelineOpt match {
            case Some((_, subCounterCoord,  false /*fromDomainCoords*/, true /*hardCodeCoords*/)) =>
              outCoords(subCounterCoord) === subPipelineId.S

            case Some((nSubCounters, subCounterCoord,  false /*fromDomainCoords*/, false /*hardCodeCoords*/)) =>
              (outCoords(subCounterCoord) % nSubCounters.S) === subPipelineId.S

            case _ => true.B
          }

          (((e, v), (elemId, pipelineId)), subPipelineMatches)

        case (e, v, elemId, pipelineId, _) if transposingPipelines =>
          require(!(transposingPipelines && exitOption == PerpendicularEdge), "i'm not sure if the code below will work if we are transposing but ALSO exiting only from perpendicular edges")

          val incrementingCoordsMatch = all(outCoords.zipWithIndex.collect {
            // TODO I think this code works for 2D matrices, but I have no idea really if it works for higher-dim tensors
            case (outCoord, coordId) if incrementingCoordsForInputs.map(_._1).contains(coordId) => outCoord === pipelineId.S
          })
          require(incrementingCoordsForInputs.isEmpty || nIOCoords == 2, "I think the 'incrementingCoordsMatch' code works for 2D matrices, but I have no idea really if it works for higher-dim tensors")

          (((e, v), (elemId, pipelineId)), incrementingCoordsMatch)

        case (e, v, elemId, pipelineId, _) =>
          (((e, v), (elemId, pipelineId)), true.B)
      }.map {
        case (((e, v), (elemId, pipelineId)), shouldCheck) if shouldLockstepOuts && outPortId < (nLockSteppedPortGroups * lockSteppedPortGroupSize) =>
          val _shouldCheck = if (lockstepIns && shouldLockstepOuts) {
            require(isBalanced)
            require(nLockSteppedPortGroups == 1, "we haven't yet added support for partial output lock-stepping for this case")
            if (outPortId == 0) shouldCheck && elemId < elemsPerPipeline.U
            else if (outPortId == pipelineId) (shouldCheck && elemId === headOutPortMatchingIds.head + (elemsPerPipeline * outPortId).U)
            else false.B
          } else {
            require(entryOption.isInstanceOf[Edge] && exitOption == Anywhere && nSubPipelines == 1, "for now, we only support locksteps-only-for-outputs in very specific circumstances (basically when we are trying to do a krste-style transpose)")
            require(isBalanced)
            if (outPortId % lockSteppedPortGroupSize == 0) {
              shouldCheck && elemId === ((pipelineId+1) * elemsPerPipeline - 1).U
            } else {
              shouldCheck && elemId === headOutPortMatchingIds(outPortId / lockSteppedPortGroupSize) - (outPortId % lockSteppedPortGroupSize).U &&
                elemId === ((pipelineId+1) * elemsPerPipeline - 1 - (outPortId % lockSteppedPortGroupSize)).U
            }
          }

          (((e, v), elemId), _shouldCheck)

        case (((e, v), (elemId, _)), shouldCheck) => (((e, v), elemId), shouldCheck)
      }

      stickyOutPortCoords match {
        case Some((stickyCoordIds, nStickySets, pipelineIdDivider)) => withoutPrefix {
          val stickyRegSelectorCoordId = 0

          val prevValid = RegInit(VecInit.fill(nStickySets)(false.B))
          val prevCoords = Reg(Vec(nStickySets, getChiselType(elems.head.coords)))
          val prevElemId = Reg(Vec(nStickySets, UInt(log2Up(nElems).W)))
          val prevOpCount = Reg(Vec(nStickySets, OpCount()))

          val stickyRegSelect = (outCoordsU(stickyRegSelectorCoordId) >> 1).asUInt % nStickySets.U

          if (outPortId % 2 == 1) {
            when (io.outs(outPortId-1).valid && io.outs(outPortId-1).found) {
              val otherStickyRegSelect = 0
              prevValid(otherStickyRegSelect) := true.B
              prevCoords(otherStickyRegSelect) := io.outs(outPortId-1).coords
              prevOpCount(otherStickyRegSelect) := io.outs(outPortId-1).op_count
              prevElemId(otherStickyRegSelect) := debug_out_matchingIds(outPortId-1)
            }
          }

          when (out.valid && out.found) {
            prevValid(stickyRegSelect) := true.B
            prevCoords(stickyRegSelect) := outCoords
            prevOpCount(stickyRegSelect) := out.op_count
            prevElemId(stickyRegSelect) := debug_out_matchingIds(outPortId)
          }

          val stickyMatches = prevValid(stickyRegSelect) && prevOpCount(stickyRegSelect) === out.op_count &&
            prevCoords(stickyRegSelect)(stickyRegSelectorCoordId) === outCoords(stickyRegSelectorCoordId) &&
            all(stickyCoordIds.map(coordId => prevCoords(stickyRegSelect)(coordId) === outCoords(coordId)))

          val elemIdDivider = elemsPerPipeline * pipelineIdDivider; require(isBalanced)

          withoutPrefix(withoutSticky.map { case (((e, v), i), shouldCheck) =>
            (((e, v), i), shouldCheck && (!stickyMatches || (i / elemIdDivider.U) === (prevElemId(stickyRegSelect) / elemIdDivider.U)))
          })
        }

        case None => withoutSticky
      }
    }.filterNot { case (((e, v), i), shouldCheck) => shouldCheck.litToBooleanOption.contains(false) }

//    if (!shortenNames)
//      when (outPortId.U === not_found_out_port_id) {
//        elemsToCheck.map { case (((e, v), i), shouldCheck) =>
//          when (shouldCheck) {
//            not_found_out_port_checking(i) := true.B
//          }
//        }
//      }

    println(s"\tDEBUG outPortId=$outPortId | elemsToCheck = ${elemsToCheck.size} | outCoords = ${outCoords.map(_.litOption)}")

    if (outPortId == debug_out_port_checking_id) {
      elemsToCheck.map { case (((e, v), i), shouldCheck) =>
        debug_out_port_checking(i) := shouldCheck
      }
    }

    // This helper muxing function was only created so that we could avoid indexing into "elems" when setting
    // "out.data", which helps us to reduce Verilog lines
    def search(foo: ElemToCheckT => Unit): Unit = elemsToCheck.foreach { case x @ (((e, v), i), shouldCheck) =>
      val isLockSteppedOutPort = shouldLockstepOuts && outPortId < (nLockSteppedPortGroups * lockSteppedPortGroupSize) && outPortId % lockSteppedPortGroupSize != 0

      val coordsMatch = if (isLockSteppedOutPort) true.B else withoutPrefix {
        // Apply hardcoding
        val e_coords = i.litOption.map(_.toInt).map(elemCoordsWithHardcoding(_, SInt())).getOrElse(e.coords)

        // Use domain-coords when needed
        val elem_coords = domainCoordsToUseForOutputs.foldLeft(e_coords.toIndexedSeq) { case (acc, (outCoordId, domainCoordId)) =>
          acc.updated(outCoordId, e.element.domainCoords(domainCoordId))
        }

        // Remove coords which can be ignored
        val coords = elem_coords.zip(outCoords).zipWithIndex.filterNot(coordsToIgnoreForOutputs contains _._2).map(_._1)

        // Finally, check that coords match
        vecEquals(coords)
      }.suggestName("cm")

      val opCountMatches = if (!checkOpCountForOuts || lockstepIns && isLockSteppedOutPort) true.B
        else (e.op_count === out.op_count).suggestName("om")

      val inLockStep = if (isLockSteppedOutPort) headOutPortFounds(outPortId / lockSteppedPortGroupSize) else true.B // If out-port-0 couldn't actually find anything, then none of the lock-stepped out-ports that depend on it can find anything either

      val cond = all(Seq(shouldCheck, any(v), opCountMatches, coordsMatch, inLockStep))

      if (!cond.litToBooleanOption.contains(false))
        when (cond) {
          foo(x)
        }
    }

    val matchingId = WireInit(nElems.U).suggestName("mid")

    out.data := 0.S
    out.expanded_coords := out.coords
    out.axis_spans := DontCare

    search { case (((e, _), i), _) =>
      matchingId := i

      if (!dummyData) out.data := e.element.data
      out.expanded_coords.zipWithIndex.collect { case (expanded_coord, coordId) if coordsThatNeedExpanding.contains(coordId) =>
        expanded_coord := e.coords(coordId)
      }
      out.axis_spans := e.axis_spans
    }

    out.found := matchingId < nElems.U
    debug_out_matchingIds(outPortId) := matchingId

    if (shouldLockstepOuts && outPortId % lockSteppedPortGroupSize == 0 && outPortId < nLockSteppedPortGroups * lockSteppedPortGroupSize) {
      headOutPortMatchingIds(outPortId / lockSteppedPortGroupSize) := matchingId
      headOutPortFounds(outPortId / lockSteppedPortGroupSize) := out.found
    }

    out.unavailable := withoutPrefix {
      // TODO There are several cases where we have special behavior for krste-style transpositions, as in OuterSPACE's
      //  regScattered_MatmulOutput regfile. However, we should double-check that this logic is more generalizable, and
      //  that it applies to other types of regfiles as well. Eventually, we should get rid of the
      //  'krste_style_transpose' variable entirely
      val krste_style_transpose = entryOption.isInstanceOf[Edge] && exitOption == Anywhere && shouldLockstepOuts && !lockstepIns

      var all_op_counts_checked = false // This is just used to reduce the lines of FIRRTL code generated. It shouldn't actually affect the synthesized hardware
      var all_last_in_axis_flags_checked = false // This is just used to reduce the lines of FIRRTL code generated. It shouldn't actually affect the synthesized hardware

      val coordsAreBehind = withoutPrefix {
        def stripIrrelevantCoords(coords: Seq[SInt], ignore: Set[Int]): Seq[SInt] =
          coords.zipWithIndex.collect { case (c, i) if !ignore.contains(i) => c }

        unavailableIfSmallerOpt match {
          case Some((coordId, true, ignore)) =>
            val relevant_out_coords = stripIrrelevantCoords(outCoords, ignore + coordId)
            any(elemsToCheck.map { case (((e, v), i), shouldCheck) =>
              val e_coords = i.litOption.map(_.toInt).map(elemCoordsWithHardcoding(_, SInt())).getOrElse(e.coords)
              val relevant_e_coords = stripIrrelevantCoords(e_coords, ignore + coordId)
              shouldCheck && any(v) && e.op_count === out.op_count &&
                outCoords(coordId) < e_coords(coordId) &&
                vecEquals(relevant_e_coords, relevant_out_coords)
            })

          case Some((coordId, false, ignore)) =>
            all_op_counts_checked = true
            all_last_in_axis_flags_checked = true

             val relevant_out_coords = stripIrrelevantCoords(outCoords, ignore + coordId)
             val flag_id = flattenedFlagId(outCoordsU /*, out.valid && !out.found && !opCountBehind && all(out.coords.map(c => c < maxVal(c))) */) // For SpArch mergers, checking the "elem_flag_id" should be sufficient, but we should make sure later that it generalizes well to do so

            all(elemsToCheck.map { case (((e, v), i), shouldCheck) =>
              val e_coords = i.litOption.map(_.toInt).map(elemCoordsWithHardcoding(_, SInt())).getOrElse(e.coords)
              val relevant_e_coords = stripIrrelevantCoords(e_coords, ignore + coordId)

              val flag = i.litOption.map(_.toInt) match {
                // This whole thing should really just be:
                //   pipedLastInAxisFlags(i)(flattenedFlagId(e.coords.map(_.asUInt)))
                // but the need for manual const-prop forced us to write it like this instead:
                case Some(constI) =>
                  hardcodedFlattenedFlagId(hardCodedCoords(constI)) match {
                    case Some(constFlagId) => pipedLastInAxisFlags(constI)(constFlagId)
                    case None =>
                      val e_coordsU = elemCoordsWithHardcoding(constI, UInt())
                      val elem_flag_id = flattenedFlagId(e_coordsU)
                      pipedLastInAxisFlags(constI)(elem_flag_id)
                  }
                case None =>
                  val elem_flag_id = flattenedFlagId(e.coords.map(_.asUInt))
                  pipedLastInAxisFlags(i)(elem_flag_id)
              }

              !shouldCheck ||
                e.op_count > out.op_count ||
                e.op_count === out.op_count && (flag || outCoords(coordId) < e_coords(coordId) && vecEquals(relevant_e_coords, relevant_out_coords))
            })

          case None => false.B
        }
      }.suggestName("cb")

      val notInAxis = withoutPrefix (lastInAxisFlagsOpt match {
        case Some(_) if krste_style_transpose =>
          val flag_id = flattenedFlagId(outCoordsU /*, out.valid && !out.found && !opCountBehind && all(out.coords.map(c => c < maxVal(c))) */)
          any(elemsToCheck.collect { case (((e, v), i), shouldCheck) =>
            val elem_coords = i.litOption.map(_.toInt).map(elemCoordsWithHardcoding(_, UInt())).getOrElse(e.coords.map(_.asUInt))
            val elem_flag_id = flattenedFlagId(elem_coords /*, out.valid && !out.found && !opCountBehind && all(out.coords.map(c => c < maxVal(c))) */)

            val flag = pipedLastInAxisFlags(i)(elem_flag_id)
            shouldCheck && elem_flag_id === flag_id && (out.op_count === e.op_count && flag ||
              out.op_count < e.op_count)
          })

        case Some(lastInAxisFlagRegsVec) if exitOption == Anywhere =>
          val lastInAxisFlagRegs = lastInAxisFlagRegsVec.head; require(!separateLastInAxisFlagsForEachPipeline)
          lastInAxisFlagRegs(flattenedFlagId(outCoordsU /*, out.valid && !out.found && !opCountBehind && all(out.coords.map(c => c < maxVal(c)))*/)) && out.op_count === fillCount

        case Some(_) if exitOption.isInstanceOf[Edge] || exitOption == PerpendicularEdge =>
          all_op_counts_checked = true

          if (all_last_in_axis_flags_checked) {
            false.B
          } else {
            val onlyCheckElemFlagId = lastInAxisFlags.map(_._2).forall(coordId => fullyHardcodedCoordIds(coordId)) // This variable is just used for manual const-prop. If its just always set to "false", the generated hardware shouldn't change

            val flag_id_opt = Option.unless(onlyCheckElemFlagId)(flattenedFlagId(outCoordsU)) // The only reason this is "opt" is to reduce the number of FIRRTL LOC generated when "flag_id" is not needed

            all(elemsToCheck.collect { case (((e, v), i), shouldCheck) =>
              val elem_flag_id_opt = Option.when(onlyCheckElemFlagId)(withoutPrefix {
                val e_coordsU = i.litOption.map(_.toInt).map(elemCoordsWithHardcoding(_, UInt())).getOrElse(e.coords.map(_.asUInt))
                flattenedFlagId(e_coordsU)
              })

              val flag = pipedLastInAxisFlags(i)(if (onlyCheckElemFlagId) elem_flag_id_opt.get else flag_id_opt.get)
              !shouldCheck || out.op_count < e.op_count || out.op_count === e.op_count && flag
            })
          }

        case Some(_) => throw new Exception(s"we don't support last-in-axis-flags when exitOption is: $exitOption")

        case None => false.B
      }).suggestName("nia")

      val opCountBehind = if (!checkOpCountForOuts) false.B else withoutPrefix(if (exitOption != Anywhere || krste_style_transpose) {
        all(elemsToCheck.collect { case (((e, _), _), shouldCheck) =>
          if (all_op_counts_checked) {
            false.B
          } else {
            !shouldCheck || out.op_count < e.op_count
          }
        })
      } else {
        val op_count_is_behind_empty_count = if (allElemsWillBePoppedBeforeLastOut) false.B else (out.op_count < emptyCount)
        out.op_count < fillCount || op_count_is_behind_empty_count
      }).suggestName("opb")

      !out.found && any(Seq(opCountBehind, notInAxis, coordsAreBehind)) // The only reason we don't write "!out.found && (opCountBehind || notInAxis || coordsAreBehind)" here is to do some manual const-propping
    }

    if (popIfEqual) {
      when (out.valid && out.found && out.pop.valid) {
        if (!elemsTravel) {
          // If we are piping, then we want to just let the pipe logic determine what the "elem_valid" will be in the next
          // cycle
          elem_valids(matchingId)(out.pop.bits) := false.B
        }
        require(!elemsTravel || nSubArrays == 1, "we don't currently support popping from pipelined reg-files where different sub-arrays might pop at different times")
        isBeingCleared(matchingId) := true.B
      }
    }

    if (popIfSmallerThanHead.nonEmpty && out == io.outs.head) {
      require(nSubArrays == 1, "i'm not sure if pop-from-smaller-coords makes sense when we have multiple sub-arrays")
      require(!elemsTravel, "we don't currently support pop-from-smaller-coords options when pipelining regfiles")

      elems.zip(elem_valids).zipWithIndex.foreach { case ((elem, valids), elemId) =>
        val elem_coords = popIfSmallerThanHead.map(elem.coords(_))
        val out_coords = popIfSmallerThanHead.map(outCoords(_))
        when (out.pop.valid && valids(out.pop.bits) && compareVecsSigned(elem_coords, out_coords, {_ < _}) && elem.op_count === out.op_count) {
          valids(out.pop.bits) := false.B
          isBeingCleared(elemId) := true.B
        }
      }
    }
  }

  // Handling updates
  println(s"\tDEBUG Handling updates")
  io.updates.foreach { update =>
    val from = update.bits.from
    val to = update.bits.to

    when (update.fire) {
      val matchingId = MuxCase(nElems.U, elems.zip(elem_valids).zipWithIndex.map { case ((e, v), i) =>
        (any(v) && e.op_count === from.op_count && e.coords.zip(from.coords).map { case (c1, c2) => c1 === c2 }.reduce(_ && _)) -> i.U
      })
      if (withAsserts) assert(matchingId < nElems.U)

      elems(matchingId).op_count := to.op_count
      elems(matchingId).coords := to.coords

      if (withAsserts) assert(!elemsTravel, "not yet sure if piping works with this updating code")
    }
  }

  // Hardcode some coords based on optimizations
  elems.zip(hardCodedCoords).foreach { case (elem, hcs) =>
    elem.coords.zip(hcs).collect { case (c, Some(hc)) =>
      c := hc.S
    }
  }

  elems.grouped(elemsPerPipeline).foreach { elemsInPipeline =>
    elemsInPipeline.grouped(elemsPerPipeline / nSubPipelines).foreach { elemsInSubPipeline =>
      elemsInSubPipeline.zipWithIndex.foreach { case (e, incrementingCoord) =>
        val coords = if (transposingPipelines) e.coords.reverse else e.coords
        coords.zipWithIndex.collect { case (coord, coordId) if incrementingCoordsForInputs.contains((coordId, true)) =>
          // TODO add some kind of assertion to the input code to check that this hardcoding is correct
          require(isBalanced)
          coord := incrementingCoord.S
        }
      }
    }
  }

  // Lookup the number of elements in an axis
  println(s"\tDEBUG Lookup the number of elements in an axis")
  if (nElemsLookupPorts > 0) {
    // The SpArch mergers take a lot of time to generate with Chisel/FIRRTL (and sometimes produce so many lines of
    // Verilog that they overflow their bounds before ordinary FIRRTL const-prop can remove them). The SpArch special
    // cases below won't affect the actual generated hardware, but they will reduce the number of lines of intermediate
    // FIRRTL which are generated, making the elaboration faster and less likely to overflow the 2GB-of-FIRRTL-text
    // limitation.
    val for_expensive_sparch_merger = nameOpt.exists(_.startsWith("sparch_")) && nElemsLookupPorts > 1 && nIOCoords == 2 && entryOption.isInstanceOf[Edge] && exitOption.isInstanceOf[Edge]
    val for_cheap_sparch_merger = nameOpt.exists(_.startsWith("sparch_")) && nElemsLookupPorts > 1 && nIOCoords == 3 && entryOption.isInstanceOf[Edge] && exitOption.isInstanceOf[Edge]
    val cheap_sparch_outermost_read_bandwidth_flex_space = nInPorts / nPipelines
    val saved_pipes = withoutPrefix(outPipelines.map(_.map(_.map(_._1))))

    def addrWithHardcoding(portId: Int): Seq[UInt] = {
      val addr = io.n_elems_in_axis_lookups(portId).address
      if (constantCoordsForNElemsLookups.size > portId) {
        addr.zip(constantCoordsForNElemsLookups(portId)).map {
          case (_, Some(c)) => c.U
          case (a, _) => a.asUInt
        }
      } else
        addr.map(_.asUInt)
    }

    def getFlagId(portId: Int): Either[Int, UInt] = {
      def default = Right(flattenedFlagId(addrWithHardcoding(portId), assertGuard = false.B))

      if (constantCoordsForNElemsLookups.size > portId) {
        val hardcoded_flag_id = hardcodedFlattenedFlagId(constantCoordsForNElemsLookups(portId))
        hardcoded_flag_id match {
          case Some(c) => Left(c)
          case _ => default
        }
      } else {
        default
      }
    }

    io.n_elems_in_axis_lookups.zipWithIndex.foreach { case (n_elems_in_axis_lookup, portId) =>
      if (for_cheap_sparch_merger && constantCoordsForNElemsLookups.isEmpty) withoutPrefix {
        val pipelineId = portId / nSubPipelines
        val subPipelineId = portId % nSubPipelines

        n_elems_in_axis_lookup.nElems := withoutPrefix(PopCount(saved_pipes(subPipelineId)(pipelineId).map(_.litValue.toInt).map { elemId => any(elem_valids(elemId)) && elems(elemId).op_count === fillCount }))
      } else if (for_cheap_sparch_merger && constantCoordsForNElemsLookups.nonEmpty) withoutPrefix {
        require(cheap_sparch_outermost_read_bandwidth_flex_space == 1)

        val pipelineId = portId / nSubPipelines
        val subPipelineId = constantCoordsForNElemsLookups(portId).head.get

        val headElemId = saved_pipes(subPipelineId)(pipelineId).head.litValue.toInt

        n_elems_in_axis_lookup.nElems := Mux(!any(elem_valids(headElemId)) || pipedHeadIsBeingClearedOpt.get(subPipelineId)(pipelineId), 0.U, (elemsPerPipeline / nSubPipelines).U); require(isBalanced)
      } else if (for_expensive_sparch_merger && constantCoordsForNElemsLookups.nonEmpty) withoutPrefix {
        val subPipelineId = constantCoordsForNElemsLookups(portId).head.get; require(subPipelineId < nSubPipelines)
        val headElemIds = saved_pipes(subPipelineId).map(_.head.litValue.toInt)

        n_elems_in_axis_lookup.nElems := withoutPrefix(Mux(all(headElemIds.zipWithIndex.map { case (headElemId, pipelineId) =>
          !any(elem_valids(headElemId)) || pipedHeadIsBeingClearedOpt.get(subPipelineId)(pipelineId)
        }), 0.U, (nElems/nSubPipelines).U))
      } else if (for_expensive_sparch_merger && constantCoordsForNElemsLookups.isEmpty) withoutPrefix {
        val subPipelineId = portId; require(subPipelineId < nSubPipelines)
        n_elems_in_axis_lookup.nElems := withoutPrefix(PopCount(saved_pipes(subPipelineId).flatten.map(_.litValue.toInt).map { elemId => any(elem_valids(elemId)) && elems(elemId).op_count === fillCount }))
      } else if (nElemsLookupPortsCoordsOpt.isEmpty) withoutPrefix {
        val flag_id = getFlagId(portId)

        def mightMatch(elemId: Int) = flag_id match {
          case Left(c1) =>
            val hardcoded_elem_flag_id = hardcodedFlattenedFlagId(hardCodedCoords(elemId))
            hardcoded_elem_flag_id match {
              case Some(c2) => c1 == c2
              case _ => true
            }
          case _ => true
        }

        var debug_elems_checked = 0

        n_elems_in_axis_lookup.nElems := withoutPrefix(PopCount(elems.zip(elem_valids).zipWithIndex.collect { case ((elem, valids), elemId) if mightMatch(elemId) =>
          debug_elems_checked += 1

          val hardcoded_elem_flag_id = hardcodedFlattenedFlagId(hardCodedCoords(elemId))

          (flag_id, hardcoded_elem_flag_id) match {
            case (Left(c1), Some(c2)) =>
              assert(c1 == c2, "we shouldn't even be checking this case")
              all(Seq(any(valids), elem.op_count === fillCount))

            case (_, _) =>
              val elem_flag_id = hardcoded_elem_flag_id.map(_.U).getOrElse(flattenedFlagId(elemCoordsWithHardcoding(elemId, UInt()), assertGuard = false.B))
              all(Seq(any(valids), elem.op_count === fillCount, elem_flag_id === flag_id.left.map(_.U).merge))
          }
        }))

        println(s"\t\tlookupPortId = $portId | elemsChecked = $debug_elems_checked | flag_id = $flag_id")
      } else withoutPrefix {
        assert(nElemsLookupPortsCoordsOpt.nonEmpty && nElemsLookupPortsCoordsOpt.get.forall(_ < nIOCoords))
        val lookupCoordIds = nElemsLookupPortsCoordsOpt.get

        n_elems_in_axis_lookup.nElems := withoutPrefix(PopCount(elems.zip(elem_valids).zipWithIndex.collect { case ((elem, valids), elemId) =>
          val elem_coords = elemCoordsWithHardcoding(elemId, SInt())
          any(valids) && all(lookupCoordIds.map { coord_id =>
            n_elems_in_axis_lookup.address(coord_id) === elem_coords(coord_id)
          })
        }))
      }
    }
  } else {
    io.n_elems_in_axis_lookups.foreach(_.nElems := DontCare)
  }

  // Busy logic
  println(s"\tDEBUG Busy logic")
  if (shortenNames) {
    io.busy := true.B
  } else {
    io.busy := any(elem_valids.flatten)
  }

  // Assertions
  if (withAsserts)
    io.outs.zip(constantCoordsForOutputs).zipWithIndex.foreach { case ((out, constCoords), outPortId) =>
      assert(!out.valid || all(out.coords.zip(constCoords).collect { case (coord, Some(const)) => coord === const.S }), s"outPortId = $outPortId | constCoords = $constCoords")
    }

  // Reset logic
  println(s"\tDEBUG Reset logic")
  when (reset.asBool) {
    if (exitOption != Anywhere) {
      elems.foreach(_.op_count := OpCount(0.U)) // This is necessary for the "unavailable" logic to work
    }
    if (unavailableIfSmallerOpt.nonEmpty) {
      val Some((_, _, ignoreCoords)) = unavailableIfSmallerOpt
      elems.foreach(_.coords.zipWithIndex.collect { case (c,i) if !ignoreCoords.contains(i) => c }.foreach(_ := 0.S))
    }
  }

  // io.ins.foreach(x => dontTouch(x.bits.coords))
  // io.ins.foreach(x => dontTouch(x.bits.op_count))
  // elems.foreach(x => dontTouch(x.coords))
  // elems.foreach(x => dontTouch(x.op_count))
  // io.ins.foreach(x => dontTouch(x.bits.last_in_axis))
  // io.ins.foreach(x => dontTouch(x.bits.increment_sticky_coord))
  io.outs.foreach(x => dontTouch(x.coords))
  io.outs.foreach(x => dontTouch(x.op_count))
  io.outs.foreach(x => dontTouch(x.found))
  io.outs.foreach(x => dontTouch(x.unavailable))

  val debug_utilization = PopCount(elem_valids.map(any))
  // dontTouch(debug_utilization)

  println(s"\tDEBUG Done with regfile")
}
