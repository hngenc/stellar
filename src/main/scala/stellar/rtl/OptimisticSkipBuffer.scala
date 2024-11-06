package stellar.rtl

import scala.math.pow
import chisel3._
import chisel3.util.{MixedVec, MuxCase}
import stellar._

class OptimisticSkipInPort(nCoords: Int) extends Bundle {
  val data = SInt(32.W)
  val coords = Vec(nCoords, SInt(32.W)) // These are only the skipped coords
}

class OptimisticSkipOutPort(nCoords: Int) extends Bundle {
  val data = chisel3.Output(SInt(32.W))
  val coords = chisel3.Input(Vec(nCoords, SInt(32.W))) // These are only the skipped coords
}

class OptimisticSkipBuffer(val optimisticSkip: OptimisticSkipConn, indicesDim: Int, val compressedDim: Int, val expandedDim: Int) extends Module {
  // Note: We are assuming here that "compressedDim" and "expandedDim" are the same for all indices. If that is not
  // true, then we may need to change the dimensions of "io.ins" and "io.outs". TODO remove this assumption

  val dataDim = optimisticSkip.nonSharedDim
  val ioInsDim = pow(expandedDim, dataDim).toInt
  val ioOutsDim = pow(compressedDim, dataDim).toInt

  val isIO = optimisticSkip.isIO

  val io = IO(new Bundle {
    val ins = chisel3.Input(Vec(ioInsDim, new OptimisticSkipInPort(dataDim)))
    val outs = Vec(ioOutsDim, new OptimisticSkipOutPort(dataDim))
  })

  io.outs.foreach { out =>
    val insId = MuxCase(0.U, io.ins.zipWithIndex.map { case (in, i) =>
      val coordsMatch = (in.coords zip out.coords).map { case (ic, oc) => ic === oc }.foldLeft(true.B)(_ && _)
      coordsMatch -> i.U
    })

    out.data := io.ins(insId).data
  }
}

object OptimisticSkipBuffer {
  def notSharedIndices(domainIndices: Seq[SInt], optimisticSkip: OptimisticSkipConn): Vec[SInt] = {
    val nonSharedInds = optimisticSkip.nonSharedInds
    val result = domainIndices.zipWithIndex.collect { case (ind, i) if nonSharedInds.contains(i) => ind }
    VecInit(result)
  }

  def bypassedData(optimisticSkip: OptimisticSkipConn, bypass: OptimisticSkipBuffer, peOuts: Iterable[DataElement]) = {
    val result = WireInit(bypass.io.ins)

    peOuts.foreach { peOut =>
      val inds = notSharedIndices(peOut.domainCoords, optimisticSkip)

      for (r <- result) {
        val indsMatch = (inds zip r.coords).map { case (i1, i2) => i1 === i2 }.reduce(_ && _)
        when (indsMatch) {
          r.data := peOut.data
        }
      }
    }

    result
  }
}
