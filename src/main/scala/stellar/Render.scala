package stellar

import slack3d.graphics._
import slack3d.algebra._
import slack3d.graphics.shape._
import slack3d.graphics.shape.line._
import slack3d.graphics.colour._

import scala.concurrent.duration.DurationInt

import Util.SMap

object Render {
  class PE(box: Box, text: Text) extends Shape {
    override type Self = PE

    override def map(f: Vector3[Double] => Vector3[Double]): PE =
      new PE(box = box.map(f), text = text.map(f))

    override def buildMesh(mesh: Mesh[Point, LineOrRay, Triangle]): Unit = {
      box buildMesh mesh
      text buildMesh mesh
    }
  }

  class ArrayConnection(line: Line, text: Text) extends Shape {
    override type Self = ArrayConnection

    override def map(f: Vector3[Double] => Vector3[Double]): ArrayConnection =
      new ArrayConnection(line = line.map(f), text = text.map(f))

    override def buildMesh(mesh: Mesh[Point, LineOrRay, Triangle]): Unit = {
      line buildMesh mesh
      text buildMesh mesh
    }
  }

  class GroupedInterArrayConnection(lines: Iterable[Line], box: Box, text: Text) extends Shape {
    override type Self = GroupedInterArrayConnection

    override def map(f: Vector3[Double] => Vector3[Double]): GroupedInterArrayConnection =
      new GroupedInterArrayConnection(lines = lines.map(_.map(f)), box = box.map(f), text = text.map(f))

    override def buildMesh(mesh: Mesh[Point, LineOrRay, Triangle]): Unit = {
      lines.foreach(_ buildMesh mesh)
      box buildMesh mesh
      text buildMesh mesh
    }
  }

  object PE {
    def apply(point: stellar.Point): PE = {
      val coords = point.coords.padTo(3, 0).map(_.toDouble)
      val box = Box(colour = Colour.DarkSlateGray, widths=Vector3(0.1, 0.1, 0.05))
      val text = Text("(" + coords.take(point.dim).map(_.toInt).mkString(",") + ")", Colour.White) + Vector3(-0.08, -0.01, 0.07)
      val pe = new PE(box, text)
      pe + Vector3(coords.toArray) * 0.7
    }
  }

  object IAC {
    def apply(iac: Point2PointConn, iacSrcVars: Seq[Intermediate]): ArrayConnection = {
      val src = iac.src.coords.padTo(3, 0).map(_.toDouble)
      val dst = iac.dst.coords.padTo(3, 0).map(_.toDouble)
      val v = Vector3(MatrixUtil.addvd(dst, src.map(-_)).toArray)

      val line = Line(
        to=v * 0.85,
        colour = Colour.Red,
        showCone = true,
        showVectorInfo = false,
      )

      val textShift = iacSrcVars.indexOf(iac.srcVar) - iacSrcVars.size / 2
      val text = Text(s"${iac.srcVar}", colour = Colour.White) * 1.5 + v / 2 +
        Vector3(0, 0.05, 0) * textShift

      (new ArrayConnection(line = line, text = text) + Vector3(src.toArray)) * 0.7
    }
  }

  object IOC {
    def apply(point: stellar.Point, portVar: Variable, portCount: Int, ioVars: Seq[Variable]): ArrayConnection = {
      val src = point.coords.padTo(3, 0).map(_.toDouble)
      val v = Vector3(-0.2, 0, 0.15)

      val line = Line(
        to=v,
        colour = Colour.Blue,
        showCone = true,
        showVectorInfo = false,
      )

      val textShift = ioVars.indexOf(portVar) - ioVars.size / 2
      val portCountText = if (portCount > 1) s",$portCount" else ""
      val text = Text(s"|$portVar$portCountText|", colour = Colour.White) + v -
        Vector3(0, 0.05, 0) * textShift -
        Vector3(-0.015, 0, 0)

      new ArrayConnection(line = line, text = text) + Vector3(src.toArray) * 0.7
    }
  }

  object GroupedConn {
    private def midpointOf(syncSkipConn: OptimisticSkipConn): Seq[Double] = {
      val allPoints = if (!syncSkipConn.isIO) {
        syncSkipConn.srcPoints ++ syncSkipConn.dstPoints
      } else {
        syncSkipConn.ioPoints
      }

      val midp = allPoints.map(_.coords).reduce(MatrixUtil.addv)
        .map(_.toDouble / allPoints.size).padTo(3, 0.0)

      if (!syncSkipConn.isIO) {
        midp
      } else if (syncSkipConn.isOutput) {
        MatrixUtil.addvd(midp, Seq(0.05, 0.0, 0.0))
      } else /*if (syncSkipConn.isInput)*/ {
        MatrixUtil.subvd(midp, Seq(0.05, 0.0, 0.0))
      }
    }

    def apply(syncSkipConn: OptimisticSkipConn, interVars: Seq[Intermediate], ioVars: Seq[Variable]): GroupedInterArrayConnection = {
      val allPoints = syncSkipConn.srcPoints ++ syncSkipConn.dstPoints

      val midpoint = midpointOf(syncSkipConn)

      val srcLines = syncSkipConn.srcPoints.map(_.coords.map(_.toDouble)).map { coords =>
        Line(
          to=Vector3(MatrixUtil.subvd(midpoint, coords).toArray),
          colour = Colour.Purple,
          showCone = true,
          showVectorInfo = false,
        ) * 0.85 + Vector3(coords.toArray)
      }

      val dstLines = syncSkipConn.dstPoints.map(_.coords.map(_.toDouble)).map { coords =>
        Line(
          to=Vector3(MatrixUtil.subvd(coords, midpoint).toArray),
          colour = Colour.Purple,
          showCone = true,
          showVectorInfo = false,
        ) * 0.8 + Vector3(midpoint.toArray)
      }

      val bypassLines = syncSkipConn.bypass.map { b =>
        val m = midpointOf(b)
        Line(
          to=Vector3(MatrixUtil.subvd(m, midpoint).toArray),
          colour = Colour.Brown,
          showCone = true,
          showVectorInfo = false,
        ) * 0.8 + Vector3(midpoint.toArray)
      }.toSeq

      val textShift = if (syncSkipConn.isIO) {
        interVars.indexOf(syncSkipConn.srcOrIOVar) - interVars.size / 2
      } else {
        ioVars.indexOf(syncSkipConn.srcOrIOVar) - ioVars.size / 2
      }
      val text = Text(s"${syncSkipConn.srcOrIOVar}", colour = Colour.White) + Vector3(midpoint.toArray) -
        Vector3(0, 0.05, 0) * textShift -
        Vector3(-0.015, 0, 0)

      val boxColor = if (syncSkipConn.bypass.isEmpty) Colour.LightGoldenrodYellow else Colour.Yellow
      val box = Box(colour = boxColor, widths=Vector3(0.05, 0.05, 0.05)) + Vector3(midpoint.toArray)

      new GroupedInterArrayConnection(srcLines ++ dstLines ++ bypassLines, box, text) * 0.7
    }
  }

  def apply(its: IterationSpace, ioPorts: SMap[stellar.Point, SMap[Variable, Int]]): Unit = {
    assert(its.dim <= 3)

    Slack3D("wooo it's in 3d").foreach(interval = 10000.seconds) { _ =>
      val pes = its.points.map(p => PE(p))

      val interVars = its.p2pConns.map(_.srcVar).toSet.toSeq
      val ioVars = its.ioConns.map(_.ioVar).toSet.toSeq

      val iacs = its.topP2pConns.map(iac => IAC(iac, interVars))

      val iocs = ioPorts.toSeq.flatMap { case (p, ports) =>
        ports.toSeq.map { case (variable, n) =>
          IOC(p, variable, n, ioVars)
        }
      }

      val groupConns = its.topOptimisticSkipConns.filter(!_.isIO).map(c => GroupedConn(c, interVars, ioVars))

      pes ++ iacs ++ iocs ++ groupConns
    }
  }
}
