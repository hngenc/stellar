package stellar.rtl

import chisel3._
import chisel3.util._
import chisel3.experimental._
import stellar.{Const, Expr, Index, IndexSkipFunc, IterationSpace, PointsMapping, Transform}
import stellar.Util.{SMap, without}
import ChiselUtil._

class LoadBalancerConfiguration(nAxes: Int, rfInAddrsT: => MixedVec[Vec[Vec[UInt]]], clInAddrsT: => Vec[Vec[UInt]]) extends Bundle {
  val axis_sizes = Vec(nAxes, UInt(32.W)) // TODO magic number

  val rf_in_strides = rfInAddrsT
  val cl_in_strides = clInAddrsT
}

class ChiselLoadBalancer(nAxes: Int, nOpCounts: Int, nOutputPorts: Int,
                         rfInPorts: SMap[stellar.Input, (SpatialArrayOutPort, Int)], rfUpdatePorts: SMap[stellar.Input, (RegfileUpdatePort, Int)],
                         nClInPortsPerIndex: Int, nClUpdatePortsPerIndex: Int,
                         its: IterationSpace, transform: Transform,
                         cl_in_innermost_axes: SMap[Index, Index] = Seq.empty.toMap,
                         nonWaitingInVars: Set[stellar.Input] = Set.empty, instantUpdateIndices: Set[Index] = Set.empty,
                         loadbalancingAcrossOpCounts: Boolean = true) extends Module {
  val indices = its.indices.toSeq
  val inVars = its.ioConns.map(_.ioVar).collect { case in: stellar.Input => in }.toSeq.sortBy(_.name)
  val pointsMappings = its.sortedPointsMappings

  def rfInAddrsT = MixedVec(inVars.map { inVar =>
    val nCoords = rfInPorts(inVar)._1.coords.size
    Vec(nCoords, Vec(nAxes, UInt(32.W))) // TODO magic number
  })
  def clInAddrsT = Vec(indices.size, Vec(nAxes, UInt(32.W)))

  require(nClUpdatePortsPerIndex > 0, "no cl-update ports specified")

  val io = IO(new Bundle {
    val rfIns = MixedVec(inVars.map(rfInPorts.apply).map { case (t,n) => Vec(n, Flipped(Decoupled(getChiselType(t)))) })
    val rf_last_ins = Input(Vec(inVars.size, Bool()))
    val rf_lasts_in_axes = Input(Vec(inVars.size, Bool()))

    val clIns = Vec(indices.size, Vec(nClInPortsPerIndex, new Compressed2ExpandedMapping(nCoords=indices.size, snooping=true)))
    val cl_last_ins = Input(Vec(indices.size, Bool()))
    val cl_lasts_in_axes = Input(Vec(indices.size, Bool()))

    val config = Flipped(Valid(new LoadBalancerConfiguration(nAxes, rfInAddrsT, clInAddrsT)))

    val mappings = Vec(nOutputPorts, Flipped(new LoadBalancingMappings(pointsMappings.size)))
    val last_out = Input(Bool())

    val rfUpdates = MixedVec(inVars.map(rfUpdatePorts.apply).map { case (t,n) => Vec(n, Valid(getChiselType(t))) })
    val clUpdates = Vec(indices.size, Vec(nClUpdatePortsPerIndex, Valid(new RegfileUpdatePort(indices.size))))
  })

  def getRfInsForInVar(inVar: stellar.Input) = {
    val ind = inVars.indexOf(inVar)
    (io.rfIns(ind), io.rf_last_ins(ind), io.rf_lasts_in_axes(ind))
  }

  def getRfUpdatesForInVar(inVar: stellar.Input) = {
    val ind = inVars.indexOf(inVar)
    io.rfUpdates(ind)
  }

  def getClInsForIndex(index: Index) = {
    val ind = indices.indexOf(index)
    (io.clIns(ind), io.cl_last_ins(ind), io.cl_lasts_in_axes(ind))
  }

  def getClUpdatesForIndex(index: Index) = {
    val ind = indices.indexOf(index)
    io.clUpdates(ind)
  }

  val rf_op_counts = inVars.map(_ -> RegInit(OpCount(0.U))).toMap
  val cl_op_counts = indices.map(_ -> RegInit(OpCount(0.U))).toMap
  val out_op_count = RegInit(OpCount(0.U))

  val rf_in_strides = RegEnable(io.config.bits.rf_in_strides, io.config.fire)
  val cl_in_strides = RegEnable(io.config.bits.cl_in_strides, io.config.fire)

  val axis_sizes = RegEnable(io.config.bits.axis_sizes, io.config.fire)

  val rf_axis_its = inVars.map(_ -> RegInit(VecInit(Seq.fill(nAxes)(0.U(32.W))))).toMap // TODO magic number // TODO remove the reset signal from this
  val cl_axis_its = indices.map(_ -> RegInit(VecInit(Seq.fill(nAxes)(0.U(32.W))))).toMap // TODO magic number // TODO remove the reset signal from this
  val out_axis_its = RegInit(VecInit(Seq.fill(nAxes)(0.U(32.W)))) // TODO magic number // TODO remove the reset signal from this

  var instant_values: Seq[SMap[Expr, (SInt, Bool)]] = Seq.empty

  object InputAvailablity extends ChiselEnum {
    val waiting, available, unavailable = Value

    def reduced(avails: Iterable[InputAvailablity.Type]) = MuxCase(waiting, Seq(
      any(avails.map(_ === unavailable)) -> unavailable,
      ChiselUtil.all(avails.map(_ === available)) -> available
    ))
  }
  import InputAvailablity.{waiting, available, unavailable}

  class Coord(val stellarCoord: Expr) extends Bundle {
    val value = SInt(32.W) // TODO magic number
    val availability = InputAvailablity()

    val deps = MixedVec(stellarCoord match {
      case IndexSkipFunc(_, indexExpr, dependencies, _) => (indexExpr +: dependencies).map(d => new Coord(d))
      case Const(_) => Seq()
    })

    val default_location = new OpCountAndCoords(indices.size)
    val updated_cl = Bool()

    def depsAvailabilty: InputAvailablity.Type = {
      if (deps.isEmpty)
        available
      else
        InputAvailablity.reduced(deps.map(_.depsAvailabilty))
    }

    def setSkipFuncAvailabilities(index: Index, compressedCoords: Either[(Seq[SInt], Boolean), (Seq[SInt], Seq[SInt])], setTo: InputAvailablity.Type, expandedCoord: Option[SInt], def_loc: Option[OpCountAndCoords], opId: Int): Unit = {
      deps.foreach(_.setSkipFuncAvailabilities(index, compressedCoords, setTo, expandedCoord, def_loc, opId))

      stellarCoord match {
        case IndexSkipFunc(`index`, _, _, _) =>
          val indexInd = indices.indexOf(index)
          def withoutIndex(coords: Seq[SInt]) = indices.zipWithIndex.collect { case (ind, i) if ind != index && index.dependencies.contains(ind) => coords(i) }

          val matches = compressedCoords match {
            case Left((compCoords, alongAxis)) =>
              val chiselIndexExpr = compCoords(indexInd)
              val chiselDeps = withoutIndex(compCoords)
              assert(chiselDeps.size == deps.size-1)

              val orderedIndices = index +: (indices.filter(index.dependencies.contains).filter(_ != index))
              all((chiselIndexExpr +: chiselDeps).zip(deps.map(_.value)).zip(orderedIndices).map { case ((c, d), ind) =>
                if (alongAxis && cl_in_innermost_axes.get(index).contains(ind)) {
                  true.B
                } else {
                  c === d
                }
              })

            case Right((lowerBounds, upperBounds)) =>
              val lowerIndexExpr = lowerBounds(indexInd)
              val upperIndexExpr = upperBounds(indexInd)

              val lowerDeps = withoutIndex(lowerBounds)
              val upperDeps = withoutIndex(upperBounds)

              assert(lowerDeps.size == deps.size-1 && upperDeps.size == deps.size-1)
              assert(lowerIndexExpr < upperIndexExpr)
              assert(all(lowerDeps.zip(upperDeps).map { case (l,u) => l <= u }), s"upper bounds are not higher than lower bounds for $index")

              lowerIndexExpr <= deps.head.value && deps.head.value < upperIndexExpr &&
                all(lowerBounds.zip(upperBounds).zip(deps.tail.map(_.value)).map { case ((l,u), c) => l <= c && c < u || l === u && c === l })
          }

          when (availability === waiting && matches) {
            assert(all(deps.map(_.availability === available)), "trying to mark a skipFunc as available when its deps aren't yet available")
            expandedCoord.foreach(ec => value := ec)
            availability := setTo
            def_loc.foreach(dl => default_location := dl)

            expandedCoord.foreach { ec =>
              instant_values(opId).get(stellarCoord).foreach { case (inst_val, inst_valid) =>
                inst_val := ec
                inst_valid := setTo === available
              }
            }
          }

        case _ =>
      }
    }

    def updateSkipFuncAvailabilities(index: Index, compressedCoords: Seq[SInt], expandedCoord: SInt, def_loc: OpCountAndCoords, opId: Int): Unit = {
      setSkipFuncAvailabilities(index, Left((compressedCoords, false)), available, Some(expandedCoord), Some(def_loc), opId)
    }

    def updateSkipFuncAvailabilitiesInRange(index: Index, lowerCompressedCoords: Seq[SInt], upperCompressedCoords: Seq[SInt], setTo: InputAvailablity.Type, opId: Int): Unit = {
      setSkipFuncAvailabilities(index, Right(lowerCompressedCoords, upperCompressedCoords), setTo, None, None, opId)
    }

    def updateFollowingSkipFuncAvailabilites(index: Index, compressedCoords: Seq[SInt], setTo: InputAvailablity.Type, opId: Int): Unit = {
      setSkipFuncAvailabilities(index = index, Left((compressedCoords, true)), setTo, None, None, opId)
    }

    def setWaitingSkipFuncAvailabilities(index: Index, setTo: InputAvailablity.Type): Unit = {
      deps.foreach(_.setWaitingSkipFuncAvailabilities(index, setTo))

      stellarCoord match {
        case IndexSkipFunc(`index`, _, _, _) =>
          when (availability === waiting) {
            availability := setTo
          }

        case _ =>
      }
    }

    def innerSkipFuncs: Seq[Coord] = deps.flatMap(_.innerSkipFuncs).toSeq ++ {
      stellarCoord match {
        case _: IndexSkipFunc => Seq(this)
        case _ => Seq()
      }
    }

    def instantValue(opId: Int): (SInt, InputAvailablity.Type) = {
      instant_values(opId).get(stellarCoord) match {
        case Some((inst_val, inst_valid)) => (Mux(inst_valid, inst_val, value), Mux(inst_valid, available, availability))
        case _ => (value, availability)
      }
    }

    def reset(): Unit = {
      deps.foreach(_.reset())

      updated_cl := false.B

      stellarCoord match {
        case Const(c) =>
          value := c.S
          availability := available

        case _ =>
          availability := waiting
      }
    }
  }

  class OpCountAndCoords(val nCoords: Int) extends Bundle {
    val op_count = OpCount()
    val coords = Vec(nCoords, SInt(32.W)) // TODO magic number

    def ===(other: OpCountAndCoords): Bool = {
      op_count === other.op_count && (nCoords == other.nCoords).B &&
        all(coords.zip(other.coords).map(t => t._1 === t._2))
    }
  }

  class Entry(stellarCoords: Seq[Expr]) extends Bundle {
    val nCoords = stellarCoords.size

    val availability = InputAvailablity()

    val coords = MixedVec(stellarCoords.map(ec => new Coord(ec)))
    def coordsAvailibity(opId: Int, withoutLast: Boolean = false) = {
      val cs = if (withoutLast) coords.init else coords
      InputAvailablity.reduced(cs.map(_.instantValue(opId)._2))
    }

    val default_location = new OpCountAndCoords(nCoords)
    val updated_rf = Bool()

    def reset(): Unit = {
      availability := waiting
      updated_rf := false.B
      coords.foreach(_.reset())
    }

    override def toString: String = s"[${stellarCoords.mkString(",")}]"
  }

  class PotentialMapping(defIns: Map[stellar.Input, Seq[Seq[Expr]]], otherIns: Map[stellar.Input, Seq[Seq[Expr]]], val otherPoints: Seq[Seq[Int]]) extends Bundle {
    def genEntries(inMap: Map[stellar.Input, Seq[Seq[Expr]]]) = MixedVec(inVars.map { inVar =>
      val ins = inMap(inVar)
      MixedVec(ins.map(in => new Entry(in)))
    })

    val defaultInputs = genEntries(defIns)
    val otherInputs = genEntries(otherIns)

    val otherPointsTaken = Vec(otherPoints.size, Bool())

    def setMatchingOrInRange(inVar: stellar.Input, inds: Either[(Seq[SInt], Boolean), (Seq[SInt], Seq[SInt])], setTo: InputAvailablity.Type, defLocOpt: Option[OpCountAndCoords], includeDefaults: Boolean, opId: Int): Unit = {
      val inVarInd = inVars.indexOf(inVar)

      val inputss = Seq(otherInputs) ++ {
        if (includeDefaults) Seq(defaultInputs) else Seq()
      }

      for (inputs <- inputss) {
        val availablities = inputs(inVarInd)
        availablities.foreach { avail =>
          val matches = inds match {
            case Left((equalInds, alongAxis)) =>
              all(avail.coords.init.map(_.instantValue(opId)._1).zip(equalInds).map(t => t._1 === t._2) :+ {
                if (alongAxis) {
                  true.B
                } else {
                  avail.coords.last.instantValue(opId)._1 === equalInds.last
                }
              })

            case Right((lowerBounds, upperBounds)) =>
              assert(all(upperBounds.zip(lowerBounds).map { case (u, l) => u >= l }), s"upper bounds aren't larger than lower bounds for $inVar")
              all(avail.coords.map(_.instantValue(opId)._1).zip(lowerBounds.zip(upperBounds)).map { case (c, (l, u)) => c >= l && c < u || l === u && c === l })
          }

          val coordsAvailability = inds match {
            case Left((_, true)) => avail.coordsAvailibity(opId, withoutLast = true)
            case _ => avail.coordsAvailibity(opId)
          }

          when (avail.availability === waiting && (coordsAvailability === available && matches || coordsAvailability === unavailable && setTo === unavailable)) {
            avail.availability := setTo
            defLocOpt.foreach(def_loc => avail.default_location := def_loc)
          }
        }
      }
    }

    def setMatching(inVar: stellar.Input, inds: Seq[SInt], setTo: InputAvailablity.Type, def_loc: OpCountAndCoords, opId: Int): Unit = {
      setMatchingOrInRange(inVar, Left((inds, false)), setTo, Some(def_loc), includeDefaults = true, opId)
    }

    def setOtherInRange(inVar: stellar.Input, lowerBounds: Seq[SInt], upperBounds: Seq[SInt], setTo: InputAvailablity.Type, opId: Int): Unit = {
      setMatchingOrInRange(inVar, Right((lowerBounds, upperBounds)), setTo, None, includeDefaults = false, opId)
    }

    def setFollowing(inVar: stellar.Input, inds: Seq[SInt], setTo: InputAvailablity.Type, opId: Int): Unit = {
      setMatchingOrInRange(inVar, Left((inds, true)), setTo, None, includeDefaults = true, opId)
    }

    def coordsWaiting(inVar: stellar.Input): Bool = {
      val inVarInd = inVars.indexOf(inVar)
      any((0 until nOpCounts).flatMap(opId => (defaultInputs(inVarInd) ++ otherInputs(inVarInd)).map(_.coordsAvailibity(opId) === waiting)))
    }

    def skipFuncDepsWaiting(index: Index): Bool = {
      val coords = (defaultInputs ++ otherInputs).flatMap(_.flatMap(_.coords.flatMap(_.innerSkipFuncs))).filter(_.stellarCoord.asInstanceOf[IndexSkipFunc].index == index)
      any(coords.map(_.depsAvailabilty === waiting))
    }

    def setAllWaitingDefaults(inVar: stellar.Input, setTo: InputAvailablity.Type): Unit = {
      defaultInputs(inVars.indexOf(inVar)).foreach { avail =>
        when (avail.availability === waiting) {
          avail.availability := setTo
        }
      }
    }

    def setAllWaitingOthers(inVar: stellar.Input, setTo: InputAvailablity.Type): Unit = {
      otherInputs(inVars.indexOf(inVar)).foreach { avail =>
        when (avail.availability === waiting) {
          avail.availability := setTo
        }
      }
    }

    def updateSkipFuncs(index: Index, compressedCoords: Seq[SInt], expanded: SInt, def_loc: OpCountAndCoords, opId: Int): Unit = {
      for (inputs <- Seq(defaultInputs, otherInputs)) {
        inputs.flatten.foreach { entry =>
          entry.coords.foreach(_.updateSkipFuncAvailabilities(index, compressedCoords, expanded, def_loc, opId))
        }
      }
    }

    def setOtherSkipFuncsInRange(index: Index, lowerCompressedCoords: Seq[SInt], upperCompressedCoords: Seq[SInt], setTo: InputAvailablity.Type, opId: Int): Unit = {
      otherInputs.flatten.foreach { entry =>
        entry.coords.foreach(_.updateSkipFuncAvailabilitiesInRange(index, lowerCompressedCoords, upperCompressedCoords, setTo, opId))
      }
    }

    def setFollowingSkipFuncs(index: Index, compressedCoords: Seq[SInt], setTo: InputAvailablity.Type, opId: Int): Unit = {
      (defaultInputs ++ otherInputs).flatten.foreach { entry =>
        entry.coords.foreach(_.updateFollowingSkipFuncAvailabilites(index, compressedCoords, setTo, opId))
      }
    }

    def setAllWaitingDefaultSkipFuncs(index: Index, setTo: InputAvailablity.Type): Unit = {
      defaultInputs.flatten.foreach { entry =>
        entry.coords.foreach(_.setWaitingSkipFuncAvailabilities(index, setTo))
      }
    }

    def setAllWaitingOtherSkipFuncs(index: Index, setTo: InputAvailablity.Type): Unit = {
      otherInputs.flatten.foreach { entry =>
        entry.coords.foreach(_.setWaitingSkipFuncAvailabilities(index, setTo))
      }
    }

    def entriesToUpdate(opCount: OpCount) = inVars.zipWithIndex.map { case (inVar, inVarId) =>
      val entries = otherInputs(inVarId)
      val updatePorts = io.rfUpdates(inVarId)

      // Check if any relevant inputs from future op-counts in the reg-files are awaiting updates
      val mustBeUpdated = entries.map(x => !x.updated_rf && x.availability === available && x.default_location.op_count =/= opCount)
      val updatePortIds = mustBeUpdated.scanLeft(0.U)(_ +& _).tail

      inVar -> entries.zip(mustBeUpdated).zip(updatePortIds).map { case ((entry, mustUpdate), portId) =>
        (entry, mustUpdate && portId <= updatePorts.size.U, updatePorts(portId - 1.U))
      }
    }.toMap

    def skipFuncsToUpdate(opCount: OpCount) = indices.zip(io.clUpdates).map { case (index, updatePorts) =>
      val skipFuncs = otherInputs.flatten.flatMap(_.coords.flatMap(_.innerSkipFuncs)).
        filter(_.stellarCoord.asInstanceOf[IndexSkipFunc].index == index).
        sortBy(_.hashCode()). // This sortBy is just used to ensure a consistent sorting
        distinctBy(_.stellarCoord)
      val mustBeUpdated = skipFuncs.map(x => !x.updated_cl && x.availability === available && x.default_location.op_count =/= opCount)
      val updatePortIds = mustBeUpdated.scanLeft(0.U)(_ +& _).tail

      index -> skipFuncs.zip(mustBeUpdated).zip(updatePortIds).map { case ((skipFunc, mustUpdate), portId) =>
        (skipFunc, mustUpdate && portId <= updatePorts.size.U, updatePorts(portId - 1.U))
      }
    }.toMap

    val is_frozen = Bool()
    val frozen_mapping = Bool()

    def waitingForDefault = any(defaultInputs.flatten.map(_.availability === waiting))
    def waitingForOther = any(otherInputs.flatten.map(_.availability === waiting))
    def waitingForUpdates(opCount: OpCount) = any(entriesToUpdate(opCount).values.flatMap(_.map(_._2)) ++ skipFuncsToUpdate(opCount).values.flatMap(_.map(_._2)))
    def canBeReMapped = defaultIsIdle && !otherIsIdle && !all(otherPointsTaken)

    def connectMapping(opCount: OpCount, ioMapping: DecoupledIO[Bool], overlappingPrior: Bool): Unit = {
      val remapped = canBeReMapped && !overlappingPrior
      ioMapping.valid := is_frozen || !waitingForDefault && (!remapped || !waitingForOther)
      ioMapping.bits := Mux(is_frozen, frozen_mapping, remapped)

      when (!is_frozen && !waitingForDefault && remapped && waitingForUpdates(opCount)) {
        ioMapping.valid := false.B
      }

      when (ioMapping.fire && !is_frozen) {
        is_frozen := true.B
        frozen_mapping := ioMapping.bits
      }
    }

    def updateTrackersAndRfsAndCls(opCount: OpCount, trackerOpCounts: Seq[OpCount], overlappingPrior: Bool, potMapId: Int): Unit = {
      val remapped = canBeReMapped && !overlappingPrior
      val canUpdate = WireInit(!is_frozen && !waitingForDefault && remapped)
      for ((trackers, trackerOpCount) <- input_trackers.zip(trackerOpCounts)) {
        val trackerMightStillGetInputs = minOf(inVars.map(inVar => rf_op_counts(inVar)):_*) <= trackerOpCount
        val trackerWaitingForUpdates = any(trackers.map(_.waitingForUpdates(opCount)))
        when (trackerOpCount < opCount && (trackerMightStillGetInputs || trackerWaitingForUpdates)) {
          canUpdate := false.B
        }
      }

      when (canUpdate) {
        // Invalidate the relevant index-points from the same op-count within the load-balancer
        //   Note: I'm assuming that we don't need to delay setting ioMapping.valid to true while waiting for this
        //         update to happen, because it should take only one cycle anyways
        // TODO should we do this across different op-counts as well?
        input_trackers.zip(trackerOpCounts).foreach { case (trackers, trackerOpCount) =>
          when (opCount === trackerOpCount) {
            val otherTrackers = trackers.zipWithIndex.filter(_._2 != potMapId).map(_._1)
            otherTrackers.foreach { tracker =>
              otherPoints.foreach { otherPoint =>
                val otherPointInd = tracker.otherPoints.indexOf(otherPoint)
                if (otherPointInd >= 0)
                  tracker.otherPointsTaken(otherPointInd) := true.B
              }
            }
          }
        }

        if (loadbalancingAcrossOpCounts) {
          for (((inVar, inVarId), loadBalancedInputs) <- inVars.zipWithIndex.zip(otherInputs)) {
            // Invalidate the relevant inputs from future op-counts in the load-balancer
            //   Note: I'm assuming that we don't need to delay setting ioMapping.valid to true while waiting for this
            //         update to happen, because it should take only one cycle anyways
            input_trackers.zip(trackerOpCounts).foreach { case (trackers, trackerOpCount) =>
              when(trackerOpCount > opCount) {
                trackers.foreach { potMap =>
                  (potMap.defaultInputs(inVarId) ++ potMap.otherInputs(inVarId)).foreach { entry =>
                    when(any(loadBalancedInputs.map(_.default_location === entry.default_location))) {
                      entry.availability := unavailable
                    }
                  }
                }
              }
            }

            // Update the relevant inputs from future op-counts in the reg-files
            for ((entry, mustUpdate, updatePort) <- entriesToUpdate(opCount)(inVar)) {
              when (mustUpdate) {
                updatePort.valid := true.B

                updatePort.bits.from.op_count := entry.default_location.op_count
                updatePort.bits.from.coords := entry.default_location.coords

                updatePort.bits.to.op_count := out_op_count
                updatePort.bits.to.coords := entry.coords.map(_.value).toSeq

                when (updatePort.fire) {
                  entry.updated_rf := true.B
                }
              }
            }
          }

          for (index <- indices) {
            for ((entry, mustUpdate, updatePort) <- skipFuncsToUpdate(opCount)(index)) {
              when(mustUpdate) {
                updatePort.valid := true.B

                updatePort.bits.from.op_count := entry.default_location.op_count
                updatePort.bits.from.coords := entry.default_location.coords

                updatePort.bits.to.op_count := out_op_count
                updatePort.bits.to.coords := {
                  val axis_its = scalarVecAdd(entry.default_location.op_count - out_op_count, out_axis_its, axis_sizes)
                  val centered_axis_its = axis_its.zip(out_axis_its).map { case (x, y) => x.zext -& y.zext }
                  entry.default_location.coords.zip(cl_in_strides).map { case (coord, strides) =>
                    coord +& centered_axis_its.zip(strides).map { case (it, stride) => it * stride }.reduce(_ +& _)
                  }
                }

                when(updatePort.fire) {
                  entry.updated_cl := true.B
                }
              }
            }
          }
        }
      }
    }

    private def isIdle(inputs: Iterable[Iterable[Entry]]): Bool = !all(inputs.map(inputsForOneVar => any(inputsForOneVar.map(_.availability === available))))
    def defaultIsIdle = isIdle(defaultInputs)
    def otherIsIdle = isIdle(otherInputs)

    def reset(): Unit = {
      defaultInputs.flatten.foreach(_.reset())
      otherInputs.flatten.foreach(_.reset())
      otherPointsTaken.foreach(_ := false.B)
      is_frozen := false.B
    }
  }

  val input_trackers = Seq.fill(nOpCounts)(pointsMappings.map { pointsMapping => Reg({
    def getMap(defPoints: Option[Set[stellar.Point]]) = {
      // Note: We only need to pass in "defPoints" if we want to generate the default-input-ioConns map
      inVars.map { inVar =>
        val ioConns = defPoints match {
          case Some(points) =>
            its.ioConns.filter(ioc => ioc.ioVar == inVar && ioc.mapping.isEmpty).filter(ioConn => {
              val spacetime = ioConn.point.coords ++ ioConn.time; require(transform.nTimeAxes > 0 || ioConn.time.isEmpty)
              val originalPoint = stellar.MatrixUtil.matvec(transform.inverse, spacetime).toSeq
              points.map(_.coords).contains(originalPoint)
            }).toSeq

          case None => its.ioConns.filter(ioc => ioc.ioVar == inVar && ioc.mapping.contains(pointsMapping)).toSeq
        }
        val indices = ioConns.map(_.ioIndex)

        inVar -> indices
      }.toMap
    }

    new PotentialMapping(getMap(Some(pointsMapping.tos)), getMap(None), pointsMapping.froms.map(_.coords).toSeq)
  })})

  // Initialize the "instant" values
  instant_values = Seq.fill(nOpCounts)(input_trackers.flatten.flatMap(t => t.defaultInputs ++ t.otherInputs).flatten.flatMap(_.coords.flatMap(_.innerSkipFuncs)).map(_.stellarCoord).collect {
    case skipFunc @ IndexSkipFunc(index, _, _, _) if instantUpdateIndices.contains(index) => skipFunc -> (WireInit(0.S(32.W)), WireInit(false.B))
  }.toMap)

  // Select the best mapping, and update the load-balancer, regfiles, and coord-lookups based on the mapping
  io.rfUpdates.flatten.foreach { clUpdatePort =>
    clUpdatePort.valid := false.B
    clUpdatePort.bits := DontCare
  }

  io.clUpdates.flatten.foreach { clUpdatePort =>
    clUpdatePort.valid := false.B
    clUpdatePort.bits := DontCare
  }

  val trackerOpCounts = VecInit((0 until nOpCounts).map { op_id =>
    require(isPow2(nOpCounts))

    val out_op_id = out_op_count.bits % nOpCounts.U
    val op_count = MuxCase(out_op_count, Seq(
      (op_id.U > out_op_id) -> (out_op_count + (op_id.U - out_op_id)),
      (op_id.U < out_op_id) -> (out_op_count + (nOpCounts.U - out_op_id) + op_id.U)
    ))

    op_count
  })

  def mappingsOverlap(x: PointsMapping, y: PointsMapping): Boolean = {
    x.tos.map(_.coords).intersect(y.tos.map(_.coords)).nonEmpty ||
      x.froms.map(_.coords).intersect(y.froms.map(_.coords)).nonEmpty
  }

  input_trackers.zip(trackerOpCounts).foreach { case (trackers, opCount) =>
    trackers.zipWithIndex.foreach { case (tracker, i) =>
      val overlappingPrior = any(trackers.take(i).zip(pointsMappings).collect {
        case (priorTracker, priorMapping) if mappingsOverlap(pointsMappings(i), priorMapping) =>
          priorTracker.is_frozen && priorTracker.frozen_mapping || priorTracker.canBeReMapped
      })
      tracker.updateTrackersAndRfsAndCls(opCount, trackerOpCounts, overlappingPrior, i)
    }
  }

  io.mappings.flatMap(_.configs).foreach { config =>
    config.valid := false.B
    config.bits := DontCare
  }
  io.mappings.foreach { ioMappingConfigs =>
    input_trackers.zip(trackerOpCounts).foreach { case (trackers, opCount) =>
      when (opCount === ioMappingConfigs.opCount) {
        trackers.zip(ioMappingConfigs.configs).zipWithIndex.foreach { case ((tracker, ioMapping), i) =>
          val overlappingPrior = any(trackers.take(i).zip(pointsMappings).collect {
            case (priorTracker, priorMapping) if mappingsOverlap(pointsMappings(i), priorMapping) =>
              priorTracker.is_frozen && priorTracker.frozen_mapping || priorTracker.canBeReMapped
          })
          tracker.connectMapping(opCount, ioMapping, overlappingPrior)
        }
      }
    }
  }

  // TODO consider whether or not to add this assertion back in
//  io.mappings.map(_.configs).foreach { ioMappingConfigs =>
//    ioMappingConfigs.zip(pointsMappings).zipWithIndex.foreach { case ((ioMapping, pointsMapping), i) =>
//      ioMappingConfigs.zip(pointsMappings).take(i).collect { case (priorIoMapping, priorPointsMapping) if mappingsOverlap(pointsMapping, priorPointsMapping) =>
//        assert(!(ioMapping.valid && priorIoMapping.valid), "we don't yet support cases where multiple pointsMappings might overlap")
//      }
//    }
//  }

  // Increment the output op-counter
  when (io.last_out) {
    out_op_count := out_op_count + 1.U
    val op_id = out_op_count.bits % nOpCounts.U; require(isPow2(nOpCounts))
    input_trackers.zipWithIndex.foreach { case (trackers, trackerId) => when (op_id === trackerId.U) { trackers.foreach(_.reset()) }} // This line is equivalent to calling "input_trackers(op_id).foreach(_.reset())"
    out_axis_its := scalarVecAdd(1.U, out_axis_its, axis_sizes)
  }

  // Snoop on RF inputs
  inVars.zip(io.rf_last_ins).foreach { case (inVar, last_in) =>
    // Mark inputs in the trackers as unavailable based on "last_in"
    val rf_op_count = rf_op_counts(inVar)

    when (last_in) {
      rf_op_count := rf_op_count + 1.U
      rf_axis_its(inVar) := scalarVecAdd(1.U, rf_axis_its(inVar), axis_sizes)
    }

    input_trackers.zip(trackerOpCounts).zipWithIndex.foreach { case ((trackers, tracker_op_count), trackerId) =>
      when (tracker_op_count < (rf_op_count + last_in)) {
        trackers.foreach(_.setAllWaitingDefaults(inVar, unavailable))

        if (loadbalancingAcrossOpCounts) {
          val Seq(lowerBounds, upper_bounds) = {
            val tracker_axis_its = scalarVecAdd(tracker_op_count - out_op_count, out_axis_its, axis_sizes)
            val centered_rf_axis_its = rf_axis_its(inVar).zip(tracker_axis_its).map { case (x, y) => x.zext -& y.zext }

            val stridess = rf_in_strides(inVars.indexOf(inVar))

            for (off <- Seq(0, 1)) yield stridess.map { strides =>
              centered_rf_axis_its.zip(strides).map { case (it, stride) => (it +& off.S) * stride.zext }.reduce(_ +& _)
            }
          }
          trackers.foreach(_.setOtherInRange(inVar, lowerBounds, upper_bounds, unavailable, trackerId))
        } else {
          trackers.foreach(_.setAllWaitingOthers(inVar, unavailable))
        }
      }
    }
  }

  inVars.zip(io.rf_lasts_in_axes).foreach { case (inVar, last_in_axis) =>
    // Mark inputs in the trackers as unavailable based on "last_it_in_axis"
    val rf_op_count = rf_op_counts(inVar) // Note: this only works if we're getting "last_it_in_axis" from an SRAM. Otherwise, the "rfIn.opCount" might not equal "rf_op_counts(inVar)" all the time

    // TODO add some kind of assertion to make sure that "compressed_coords" are all along a 1D vector that spans the innermost dimension
    // TODO "compressed_coords" should come directly from some port associated with "last_in_axis". we shouldn't just assume that it matches the very first "rfIn" port's coords
    val compressed_coords = io.rfIns(inVars.indexOf(inVar)).head.bits.coords
    when (last_in_axis) {
      assert({
        val rfIns = io.rfIns(inVars.indexOf(inVar))
        val outer_coords = rfIns.map(rfIn => Cat(rfIn.bits.coords.init))
        all(outer_coords.map(_ === outer_coords.head))
      }, "to fix this assertion, look at the two TODOs above")
    }

    for (((trackers, tracker_op_count), trackerId) <- input_trackers.zip(trackerOpCounts).zipWithIndex) {
      val op_count_matches = if (loadbalancingAcrossOpCounts) (tracker_op_count >= rf_op_count) else (tracker_op_count === rf_op_count)

      when (last_in_axis && op_count_matches) {
        val unrolledInds = {
          val centered_rf_axis_its = if (loadbalancingAcrossOpCounts) {
            val _rf_axis_its = rf_axis_its(inVar)
            val tracker_axis_its = scalarVecAdd(tracker_op_count - out_op_count, out_axis_its, axis_sizes)
            _rf_axis_its.zip(tracker_axis_its).map { case (x, y) => x.zext -& y.zext }
          } else {
            rf_axis_its(inVar).map(_.zext)
          }

          val stridess = rf_in_strides(inVars.indexOf(inVar))
          compressed_coords.zip(stridess).map { case (coord, strides) =>
            coord +& centered_rf_axis_its.zip(strides).map { case (it, stride) => it * stride }.reduce(_ +& _)
          }
        }

        trackers.foreach(_.setFollowing(inVar, unrolledInds, unavailable, trackerId))
      }
    }
  }

  io.rfIns.zip(inVars).foreach { case (rfIns, inVar) =>
    val op_count = rf_op_counts(inVar)
    val op_id = op_count.bits % nOpCounts.U; require(isPow2(nOpCounts))
    val is_ready = trackerOpCounts(op_id) === op_count && {
      if (nonWaitingInVars.contains(inVar)) {
        true.B
      } else {
        !any(input_trackers.zipWithIndex.map { case (trackers, trackerId) => trackerId.U === op_id && any(trackers.map(_.coordsWaiting(inVar))) })
      }
    }

    rfIns.foreach(_.ready := is_ready)
  }

  io.rfIns.zip(inVars).foreach { case (rfIns, inVar) =>
    // Mark inputs in the trackers as available
    val rf_op_count = rf_op_counts(inVar)

    rfIns.foreach { rfIn =>
      when (rfIn.fire) {
        for (((trackers, tracker_op_count), trackerId) <- input_trackers.zip(trackerOpCounts).zipWithIndex) {
          val op_count_matches = if (loadbalancingAcrossOpCounts) (rfIn.bits.op_count >= tracker_op_count) else (rfIn.bits.op_count === tracker_op_count)

          when (op_count_matches) {
            val unrolledInds = {
              val _rf_axis_its = scalarVecAdd(rfIn.bits.op_count - rf_op_count, rf_axis_its(inVar), axis_sizes)

              val centered_rf_axis_its = if (loadbalancingAcrossOpCounts) {
                val tracker_axis_its = scalarVecAdd(tracker_op_count - out_op_count, out_axis_its, axis_sizes)
                _rf_axis_its.zip(tracker_axis_its).map { case (x, y) => x.zext -& y.zext }
              } else {
                _rf_axis_its.map(_.zext)
              }

              val stridess = rf_in_strides(inVars.indexOf(inVar))
              rfIn.bits.coords.zip(stridess).map { case (coord, strides) =>
                coord +& centered_rf_axis_its.zip(strides).map { case (it, stride) => it * stride.zext }.reduce(_ +& _)
              }
            }

            val default_location = Wire(new OpCountAndCoords(rfIn.bits.coords.size))
            default_location.op_count := rfIn.bits.op_count
            default_location.coords := rfIn.bits.coords

            trackers.foreach(_.setMatching(inVar, unrolledInds, available, default_location, trackerId))
          }
        }
      }
    }
  }

  // Snoop on CL inputs
  indices.zip(io.cl_last_ins).foreach { case (index, last_in) =>
    // Mark as unavailable based on "last_ins"
    val cl_op_count = cl_op_counts(index)

    val final_it = WireInit(false.B)

    when (last_in) {
      cl_op_count := cl_op_count + 1.U
      val new_axis_its = scalarVecAdd(1.U, cl_axis_its(index), axis_sizes)
      cl_axis_its(index) := new_axis_its
      final_it := all(new_axis_its.map(_ === 0.U))
    }

    input_trackers.zip(trackerOpCounts).zipWithIndex.foreach { case ((trackers, tracker_op_count), trackerId) =>
      when(tracker_op_count < (cl_op_count + last_in)) {
        trackers.foreach(_.setAllWaitingDefaultSkipFuncs(index, unavailable))

        if (loadbalancingAcrossOpCounts) {
          when (final_it) {
            trackers.foreach(_.setAllWaitingOtherSkipFuncs(index, unavailable))
          }

          val Seq(lowerBounds, upper_bounds) = {
            val tracker_axis_its = scalarVecAdd(tracker_op_count - out_op_count, out_axis_its, axis_sizes)
            val centered_cl_axis_its = cl_axis_its(index).zip(tracker_axis_its).map { case (x, y) => x.zext -& y.zext }

            for (off <- Seq(0, 1)) yield cl_in_strides.map { strides =>
              centered_cl_axis_its.zip(strides).map { case (it, stride) => (it +& off.S) * stride }.reduce(_ +& _)
            }
          }

          trackers.foreach(_.setOtherSkipFuncsInRange(index, lowerBounds, upper_bounds, unavailable, trackerId))
        } else {
          trackers.foreach(_.setAllWaitingOtherSkipFuncs(index, unavailable))
        }
      }
    }
  }

  indices.zip(io.cl_lasts_in_axes).foreach { case (index, last_in_axis) =>
    // Mark as unavailable based on "last_it_in_axis"
    val cl_op_count = cl_op_counts(index) // Note: this only works if we're getting "last_it_in_axis" from an SRAM. Otherwise, the "rfIn.opCount" might not equal "rf_op_counts(inVar)" all the time

    // TODO add some kind of assertion to make sure that "compressed_coords" are all along a 1D vector that spans the innermost dimension
    // TODO "compressed_coords" should come directly from some port associated with "last_in_axis". we shouldn't just assume that it matches the very first "clIn" port's coords
    val compressed_coords = io.clIns(indices.indexOf(index)).head.compressed

    for (((trackers, tracker_op_count), trackerId) <- input_trackers.zip(trackerOpCounts).zipWithIndex) {
      val op_count_matches = if (loadbalancingAcrossOpCounts) (tracker_op_count >= cl_op_count) else (tracker_op_count === cl_op_count)

      when (last_in_axis && op_count_matches) {
        val unrolledInds = {
          val centered_cl_axis_its = if (loadbalancingAcrossOpCounts) {
            val _cl_axis_its = cl_axis_its(index)
            val tracker_axis_its = scalarVecAdd(tracker_op_count - out_op_count, out_axis_its, axis_sizes)
            _cl_axis_its.zip(tracker_axis_its).map { case (x, y) => x.zext -& y.zext }
          } else {
            cl_axis_its(index).map(_.zext)
          }

          compressed_coords.zip(cl_in_strides).map { case (coord, strides) =>
            coord +& centered_cl_axis_its.zip(strides).map { case (it, stride) => it * stride }.reduce(_ +& _)
          }
        }

        trackers.foreach(_.setFollowingSkipFuncs(index, unrolledInds, unavailable, trackerId))
      }
    }
  }

  io.clIns.zip(indices).foreach { case (clIns, index) =>
    val op_count = cl_op_counts(index)
    val op_id = op_count.bits % nOpCounts.U; require(isPow2(nOpCounts))
    val is_ready = trackerOpCounts(op_id) === op_count &&
      !any(input_trackers.zipWithIndex.map { case (trackers, trackerId) => trackerId.U === op_id && any(trackers.map(_.skipFuncDepsWaiting(index))) })

    clIns.foreach(_.ready.foreach(_ := is_ready))
  }

  io.clIns.zip(indices).foreach { case (clIns, index) =>
    // Mark as available based on inputs
    val cl_op_count = cl_op_counts(index)

    clIns.foreach { clIn =>
      for (((trackers, tracker_op_count), trackerId) <- input_trackers.zip(trackerOpCounts).zipWithIndex) {
        when (clIn.fire) {
          val op_count_matches = if (loadbalancingAcrossOpCounts) (clIn.op_count >= tracker_op_count) else (clIn.op_count === tracker_op_count)

          when (op_count_matches) {
            val unrolledInds = {
              val _cl_axis_its = scalarVecAdd(clIn.op_count - cl_op_count, cl_axis_its(index), axis_sizes)

              val centered_cl_axis_its = if (loadbalancingAcrossOpCounts) {
                val tracker_axis_its = scalarVecAdd(tracker_op_count - out_op_count, out_axis_its, axis_sizes)
                _cl_axis_its.zip(tracker_axis_its).map { case (x, y) => x.zext -& y.zext }
              } else {
                _cl_axis_its.map(_.zext)
              }

              clIn.compressed.zip(cl_in_strides).map { case (coord, strides) =>
                coord +& centered_cl_axis_its.zip(strides).map { case (it, stride) => it * stride }.reduce(_ +& _)
              }
            }

            val expandedCoord = clIn.expanded(indices.indexOf(index))

            val default_location = Wire(new OpCountAndCoords(indices.size))
            default_location.op_count := clIn.op_count
            default_location.coords := clIn.compressed

            trackers.foreach(_.updateSkipFuncs(index, unrolledInds, expandedCoord, default_location, trackerId))
          }
        }
      }
    }
  }

  when (reset.asBool) {
    input_trackers.flatten.foreach(_.reset())
  }
}
