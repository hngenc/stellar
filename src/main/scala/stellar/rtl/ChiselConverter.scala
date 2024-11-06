package stellar.rtl

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.ShiftRegister
import stellar._
import stellar.Util.SMap

object ChiselConverter {
  // This converts Stellar expressions to Chisel expressions

  implicit val removeComments: chisel3.internal.sourceinfo.SourceInfo = chisel3.internal.sourceinfo.UnlocatableSourceInfo

  def apply(expr: Expr, stellarExpr2Chisel: SMap[Expr, SInt], indexUpperBounds: SMap[Index, Int]): SInt = {
    def recurse(e: Expr) =
      apply(e, stellarExpr2Chisel, indexUpperBounds)

    expr match {
      case e if stellarExpr2Chisel.contains(e) => stellarExpr2Chisel(e)

      case Const(n) => n.S

      case IndexUpperBound(ind) if ind.isSkipped => (indexUpperBounds(ind) * 2 /* TODO the * 2 works for A100-style skipping, but we should make the multiplier a parameter */).S
      case IndexUpperBound(ind) => indexUpperBounds(ind).S
      case IndexLowerBound(_) => (-1).S

      case Not(b) => (!recurse(b)(0)).zext

      case a: Add => MatrixChiselUtil.sumvS(a.ops.map(o => recurse(o)))
      case m: Multiply =>
        val break_up_multiply = false // This is done to help Genus with retiming. Unfortunately, it breaks functional correctness because it causes the matmul results to arrive out-of-sync with the coordinates. Only use this for P&D area/timing evaluations
        if (break_up_multiply && m.ops.size == 2) {
          val Seq(m1, m2) = m.ops.map(recurse)
          val nStages = 4
          val bitsPerStage = m2.getWidth / nStages
          val regsPerStage = 1

          (0 until nStages).foldLeft(0.S) { (acc, stageId) =>
            val _m1 = ShiftRegister(m1, regsPerStage * (stageId+1))
            val _m2 = ShiftRegister((m2 / (BigInt(1) << (stageId * bitsPerStage)).S) % (BigInt(1) << bitsPerStage).S, regsPerStage * (stageId+1)) << (stageId * bitsPerStage)
            val m3 = (_m1 * _m2.asTypeOf(SInt(32.W))).asTypeOf(SInt(32.W)) // TODO magic number
            (ShiftRegister(acc, regsPerStage) + m3).asTypeOf(SInt(32.W)) // TODO magic number
          }.asSInt
        } else {
          m.ops.map(o => recurse(o)).reduce(_ * _)
        }

      case Divide(numer, denom) => recurse(numer) / recurse(denom)
      case Modulo(numer, Const(2)) => recurse(numer)(0).zext
      case Modulo(numer, denom) => recurse(numer) % recurse(denom)

      case Equality(left, right) => (recurse(left) === recurse(right)).zext
      case LessThan(left, right) => (recurse(left) < recurse(right)).zext
      case GreaterThan(left, right) => (recurse(left) > recurse(right)).zext
      case LessThanOrEq(left, right) => (recurse(left) <= recurse(right)).zext
      case GreaterThanOrEq(left, right) => (recurse(left) >= recurse(right)).zext

      case Or(left, right) => (recurse(left)(0) || recurse(right)(0)).zext
      case And(left, right) => (recurse(left)(0) && recurse(right)(0)).zext

      case Select(cond, iftrue, iffalse) => Mux(recurse(cond)(0), recurse(iftrue), recurse(iffalse))

      case True => true.B.zext
      case False => false.B.zext

      case Custom(ops, toChisel, _) => toChisel(ops.map(recurse))

      /*
      case IndexSkipFunc(index, indexExpr, dependencies, _) =>
        val depInds = domainIndices.filter(index.dependencies.contains).toSeq

        val compressed2ExpandedMapping = compressed2ExpandedMappings(index)
        /* TODO remove this
        (compressed2ExpandedMapping.compressed zip domainIndices).foreach { case (mappingCompressed, ind) =>
          mappingCompressed := {
            if (ind == index) recurse(indexExpr)
            else if (depInds.contains(ind)) recurse(dependencies(depInds.indexOf(ind)))
            else DontCare
          }
        }
         */

        compressed2ExpandedMapping.expanded(domainIndices.indexOf(index))
       */

      case _ => throw new Exception(s"Converter doesn't support this operation yet: $expr")
    }
  }

  def convertToString(expr: Expr, stellarExpr2Chisel: SMap[Expr, SInt], domainIndices: Seq[Index] = Seq(),
                      compressed2ExpandedMappings: SMap[Index, Vec[Compressed2ExpandedMapping]] = Seq().toMap): String = {
    // This function just exists to help when debugging the ChiselConverter

    def recurse(e: Expr) =
      convertToString(e, stellarExpr2Chisel, domainIndices, compressed2ExpandedMappings)

    expr match {
      case Const(n) => s"$n"

      case a: Add => "("+ a.ops.map(o => recurse(o)).mkString(" + ") + ")"
      case m: Multiply => "(" + m.ops.map(o => recurse(o)).mkString(" * ") + ")"

      case Equality(left, right) => (s"(${recurse(left)} === ${recurse(right)})")
      case LessThan(left, right) => (s"(${recurse(left)} < ${recurse(right)})")
      case GreaterThan(left, right) => (s"(${recurse(left)} > ${recurse(right)})")
      case LessThanOrEq(left, right) => (s"(${recurse(left)} <= ${recurse(right)})")
      case GreaterThanOrEq(left, right) => (s"(${recurse(left)} >= ${recurse(right)})")

      case Or(left, right) => s"(${recurse(left)} || ${recurse(right)})"
      case And(left, right) => s"(${recurse(left)} && ${recurse(right)})"

      case Select(cond, iftrue, iffalse) => s"Mux(${recurse(cond)}, ${recurse(iftrue)}, ${recurse(iffalse)})"

      case ind: Index if stellarExpr2Chisel.contains(ind) => stellarExpr2Chisel(ind).toString()

      case indexed: Indexed if stellarExpr2Chisel.contains(indexed) => stellarExpr2Chisel(indexed).toString()

      case _ => throw new Exception(s"Converter doesn't support this operation yet: $expr")
    }
  }

  def allPEs(its: IterationSpace, indices: Seq[Index], upperBounds: Seq[Int], pointTimespans: SMap[Point, (Seq[Int], Seq[Int])], maxTimePerAxis: Seq[Int], transform: Transform, canEndEarly: Boolean): SMap[Seq[Int], (Int, Set[stellar.Input]) => PE] = {
    its.points.map { p =>
      val pointAsgns = its.pointAssignments.filter(_.point == p)
      val ioConns = its.ioConns.filter(_.point == p)
      val (minTimeForPe, maxTimeForPe) = pointTimespans(p)

      (p.coords, (dataWidthBits: Int, inVarsThatAreNeverPopped: Set[stellar.Input]) => new PE(pointAsgns, ioConns, indices, upperBounds, its.sortedPointsMappings, p.coords, minTimeForPe, maxTimeForPe, maxTimePerAxis, transform, canEndEarly, inVarsThatAreNeverPopped = inVarsThatAreNeverPopped, dataWidthBits = dataWidthBits))
    }.toMap
  }

  def spatialArrayModule(genPEs: SMap[Seq[Int], (Int, Set[stellar.Input]) => PE], its: IterationSpace, transform: Transform, maxTimePerAxis: Seq[Int], alwaysStart: Boolean, stallIfAnyInputsAreFound: Boolean, onlyCheckIfOutputsAreReady: Boolean, stallForOutputs: Boolean, ignoreBaseC2E: Boolean, dataWidthBits: Int, withAsserts: Boolean, hasSubArrays: Boolean) =
    new ChiselSpatialArray(genPEs, its, transform, maxTimePerAxis, ignoreBaseC2E, alwaysStart, stallIfAnyInputsAreFound, onlyCheckIfOutputsAreReady, stallForOutputs, dataWidthBits, withAsserts, hasSubArrays)

  def emitSpatialArrayVerilog(genPEs: SMap[Seq[Int], (Int, Set[stellar.Input]) => PE], its: IterationSpace, transform: Transform, maxTimePerAxis: Seq[Int]): Unit = {
    /* TODO commented this out for Chipyard, but should add it back in for main branch
    val chiselStage = new ChiselStage
    chiselStage.emitVerilog(spatialArrayModule(genPEs, its, transform, maxTimePerAxis, false, false, false, false), args = Array("--target-dir", "verilog/"))
     */
  }

  def emitAcceleratorVerilog(accelerator: Accelerator): Unit = {
    /* TODO commented this out for Chipyard, but should add it back in for main branch
    val chiselStage = new ChiselStage
    chiselStage.emitVerilog(accelerator.toChiselModule, args = Array("--target-dir", "verilog/"))
     */
  }
}
