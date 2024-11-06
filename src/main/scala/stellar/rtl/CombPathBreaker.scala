package stellar.rtl

import chisel3._
import chisel3.util._
import ChiselUtil._
import stellar.rtl.CombPathBreaker.{DelayPort, pipeDecoupledMultiStage}

class CombPathBreaker[ModT <: Module, IoT <: Bundle](innerGen: => ModT, name: String)
                                                    (ioGetter: ModT => IoT)
                                                    (delayPorts: IoT => Seq[CombPathBreaker.DelayPort], busyPorts: IoT => Map[String, Bool], postProcess: Seq[ModT => Unit], hardCodedValues: IoT => stellar.Util.SMap[Data, Data]) extends Module {
  import CombPathBreaker.pipeDecoupled

  val inner = Module(innerGen)

  val io = IO(getChiselType(ioGetter(inner)))
  io <> ioGetter(inner)

  val innerPorts = delayPorts(ioGetter(inner))
  val outerPorts = delayPorts(io).map(_.port)

  def alwaysDo(affectsBusy: Map[String, Data => Bool], onResetOpt: Option[Data => Unit]): Option[(Data, Bool) => Unit] = {
    if (affectsBusy.isEmpty && onResetOpt.isEmpty) {
      None
    } else Some({ case (bits, valid) =>
      affectsBusy.foreach { case (busyName, busyFoo) =>
        when (valid && busyFoo(bits)) {
          busyPorts(io)(busyName) := true.B
        }
      }

      onResetOpt.map { onReset =>
        when (reset.asBool) {
          onReset(bits)
        }
      }
    })
  }

  def delaySeq[T <: Data](outerPort: Seq[DecoupledIO[T]], innerPort: Seq[DecoupledIO[T]], isOutput: Boolean, affectsBusy: Map[String, Data => Bool], onResetOpt: Option[Data => Unit]): Unit = {
    val alwaysDoOpt = alwaysDo(affectsBusy, onResetOpt)

    outerPort.zip(innerPort).foreach { case (o, i) =>
      if (isOutput) {
        o <> pipeDecoupled(i, alwaysDoOpt)
      } else {
        i <> pipeDecoupled(o, alwaysDoOpt)
      }
    }
  }

  (outerPorts zip innerPorts).foreach {
    case (_, DelayPort(_, _, _, 0, _)) => // We don't actually need to do anything if this is a direct connection

    case (outerPort: DecoupledIO[_], DelayPort(innerPort: DecoupledIO[_], true, affectsBusy, 1, onReset)) =>
      outerPort <> pipeDecoupled(innerPort, alwaysDo(affectsBusy, onReset))
    case (outerPort: DecoupledIO[_], DelayPort(innerPort: DecoupledIO[_], false, affectsBusy, 1, onReset)) =>
      innerPort <> pipeDecoupled(outerPort, alwaysDo(affectsBusy, onReset))
    case (outerPort: DecoupledIO[_], DelayPort(innerPort: DecoupledIO[_], true, affectsBusy, stages, onReset)) =>
      outerPort <> pipeDecoupledMultiStage(innerPort, stages, alwaysDo(affectsBusy, onReset))
    case (outerPort: DecoupledIO[_], DelayPort(innerPort: DecoupledIO[_], false, affectsBusy, stages, None)) =>
      innerPort <> pipeDecoupledMultiStage(outerPort, stages, alwaysDo(affectsBusy, None))

    case (outerPort: ValidIO[_], DelayPort(innerPort: ValidIO[_], true, affectsBusy, 1, None)) if affectsBusy.isEmpty =>
      outerPort <> Pipe(innerPort)
    case (outerPort: ValidIO[_], DelayPort(innerPort: ValidIO[_], false, affectsBusy, 1, None)) if affectsBusy.isEmpty =>
      innerPort <> Pipe(outerPort)

    case (outerPort: MixedVec[_], DelayPort(innerPort: MixedVec[_], isOutput, affectsBusy, 1, onReset)) if outerPort.forall(_.isInstanceOf[DecoupledIO[_]]) =>
      val castedOuterPort = outerPort.asInstanceOf[MixedVec[DecoupledIO[Data]]]
      val castedInnerPort = innerPort.asInstanceOf[MixedVec[DecoupledIO[Data]]]
      delaySeq(castedOuterPort.toSeq, castedInnerPort.toSeq, isOutput, affectsBusy, onReset)

    case (outerPort: Vec[_], DelayPort(innerPort: Vec[_], isOutput, affectsBusy, 1, onReset)) if outerPort.forall(_.isInstanceOf[DecoupledIO[_]]) =>
      val castedOuterPort = outerPort.asInstanceOf[Vec[DecoupledIO[Data]]]
      val castedInnerPort = innerPort.asInstanceOf[Vec[DecoupledIO[Data]]]
      delaySeq(castedOuterPort, castedInnerPort, isOutput, affectsBusy, onReset)

    case (outerPort: MixedVec[_], DelayPort(innerPort: MixedVec[_], isOutput, affectsBusy, 1, onReset)) if outerPort.forall(_.isInstanceOf[Vec[_]]) && outerPort.forall(_.asInstanceOf[Vec[_]].forall(_.isInstanceOf[DecoupledIO[_]])) =>
      val castedOuterPort = outerPort.asInstanceOf[MixedVec[Vec[DecoupledIO[Data]]]]
      val castedInnerPort = innerPort.asInstanceOf[MixedVec[Vec[DecoupledIO[Data]]]]
      castedOuterPort.zip(castedInnerPort).foreach { case (outer, inner) =>
        delaySeq(outer, inner, isOutput, affectsBusy, onReset)
      }

    case x => throw new Exception(s"port cannot be delayed: $x")
  }

  hardCodedValues(ioGetter(inner)).foreach { case (d, s) =>
    d := s
  }

  postProcess.foreach(_(inner))

  override def desiredName: String = name
}

object CombPathBreaker {
  import scala.language.implicitConversions

  case class DelayPort(port: Data, isOutput: Boolean, affectsBusy: Map[String, Data => Bool] = Map.empty[String, Data => Bool], stages: Int = 1, onReset: Option[Data => Unit] = None)
  implicit def delayPortFromTuple2(tuple2: (Data, Boolean)): DelayPort = DelayPort(tuple2._1, tuple2._2)
  implicit def delayPortFromTuple3_Busy(tuple3: (Data, Boolean, Map[String, Data => Bool])): DelayPort = DelayPort(tuple3._1, tuple3._2, tuple3._3)
  implicit def delayPortFromTuple3_Stages(tuple3: (Data, Boolean, Int)): DelayPort = DelayPort(tuple3._1, tuple3._2, stages=tuple3._3)

  def apply[ModT <: Module, IoT <: Bundle](innerGen: => ModT, name: String = "CombPathBreaker")(ioGetter: ModT => IoT)(delayPorts: IoT => Seq[DelayPort], busyPorts: IoT => Map[String, Bool] = {_: IoT => Map.empty[String, Bool]}, postProcess: Seq[ModT => Unit] = Seq.empty, hardCodedValues: IoT => stellar.Util.SMap[Data, Data] = {_: IoT => scala.collection.Map.empty[Data,Data]}) =
    Module(new CombPathBreaker(innerGen, name)(ioGetter)(delayPorts, busyPorts, postProcess, hardCodedValues))

  private def pipeDecoupled[T <: Data](in: DecoupledIO[T], alwaysDoOpt: Option[(Data, Bool) => Unit] = None): DecoupledIO[T] = {
    // Stellar uses a lot of nested Vecs, which appear to cause Vec-indexing in FIRRTL to really freak out. The default
    //   Chisel Queue indexes Vecs (I think) so we create our own replacement for it here
    val in_counter = RegInit(0.U(1.W))
    val out_counter = RegInit(0.U(1.W))

    val valid0 = RegInit(false.B)
    val valid1 = RegInit(false.B)

    val bits0 = RegEnable(in.bits, !valid0)
    val bits1 = RegEnable(in.bits, !valid1)

    in.ready := !valid0 || !valid1
    when (in.fire) {
      in_counter := in_counter + 1.U
      when (in_counter === 0.U) {
        assert(!valid0)
        valid0 := true.B
      }.otherwise {
        assert(!valid1)
        valid1 := true.B
      }
    }

    val out = Wire(Decoupled(getChiselType(in.bits)))
    out.valid := valid0 || valid1
    when (out_counter === 0.U) {
      out.bits := bits0
    }.otherwise {
      out.bits := bits1
    }
    when (out.fire) {
      out_counter := out_counter + 1.U
      when (out_counter === 0.U) {
        assert(valid0)
        valid0 := false.B
      }.otherwise {
        assert(valid1)
        valid1 := false.B
      }
    }

    alwaysDoOpt.foreach { alwaysDo =>
      alwaysDo(bits0, valid0)
      alwaysDo(bits1, valid1)
    }

    out
  }

  private def pipeDecoupledMultiStage[T <: Data](in: DecoupledIO[T], stages: Int, alwaysDoOpt: Option[(Data, Bool) => Unit] = None): DecoupledIO[T] = {
    if (stages == 0) {
      in
    } else {
      pipeDecoupledMultiStage(pipeDecoupled(in, alwaysDoOpt), stages-1, alwaysDoOpt)
    }
  }
}
