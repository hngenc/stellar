package stellar

import chisel3.experimental.BundleLiterals._
import stellar.rtl.{ChiselSRAMPipelineData, ChiselSRAMReadReq, ChiselSRAMWriteReq, ChiselUtil}
import Util.SMap

class SparseDenseConv1D extends SpatialArray {
  // This performs a 1D convolution, where the weight kernel is a sparse vector

  val i = new Index(name="i")
  val j = new Index(name="j")

  val X = new Input(name = "X")
  val W = new Input(name = "W")
  val Y = new Output(name = "Y")

  val x = new Intermediate(name = "x")
  val w = new Intermediate(name = "w")
  val y = new Intermediate(name = "y")

  // Inputs
  x(i, j.lowerBound) := X(i)
  x(i.upperBound, j) := X(i.upperBound + j)
  y(i, j.lowerBound) := 0
  w(i.lowerBound, j) := W(j)

  // Intermediate calculations
  x(i,j) := x(i+1, j-1)
  w(i,j) := w(i-1, j)
  y(i,j) := y(i, j-1) + w(i-1, j) * x(i+1, j-1)

  // Outputs
  Y(i) := y(i, j.upperBound)

  // Space-time transform
  Transform(
    1, 0,
    0, 1,
  )

  // Sparsity
  Skip(j) where (W(j) === 0)
}

class KeyMatcher extends SpatialArray {
  // This matches rows/columns of matrices along the shared dimension, so that they can be fed into an inner-product
  // matmul unit afterwards

  val n = new Index(name="n")

  val A = new Input(name="A")
  val B = new Input(name="B")
  val M = new Output(name="M")
  val C = new Output(name="C")

  val matches = new Intermediate(name="matches")
  val c = new Intermediate(name="c")
  val ka = new Intermediate(name="ka")
  val kb = new Intermediate(name="kb")

  ka(n.lowerBound) := 0
  kb(n.lowerBound) := 0

  matches(n) := A(ka(n-1)) === B(kb(n-1))
  c(n) := A(ka(n-1))
  ka(n) := ka(n-1) + (A(ka(n-1)) <= B(kb(n-1)))
  kb(n) := kb(n-1) + (A(ka(n-1)) >= B(kb(n-1)))

  M(n) := matches(n)
  C(n) := c(n)

  // Transform(
  Transform.withoutTime(
    1
  )
}

class SortedCsrMatricesMerger(rows: Int, smallCompressedCols: Int, largeCompressedCols: Int) extends SpatialArray(smallCompressedCols + largeCompressedCols, Some(Seq(rows, smallCompressedCols + largeCompressedCols))) {
  // This merges two CSR matrices, one of which is a small matrix resulting from a matmul. The other is a large matrix
  // of partial sums which were produced by previous matmuls

  val row = new Index(name=s"row")
  val col = new Index(name=s"col")

  val MatmulResult = new Input(name="MatmulResult")
  val MatmulResultCoords = new Input(name="MatmulResultCoords")
  val ExistingMatrix = new Input(name="ExistingMatrix")
  val ExistingMatrixCoords = new Input(name="ExistingMatrixCoords")

  val OutputMatrix = new Output(name=s"OutputMatrix")

  val matmulResultIt = new Intermediate(name=s"matmulResultIt")
  val existingMatrixIt = new Intermediate(name=s"existingMatrixIt")

  val matmulResultData = new Intermediate(name="matmulResultData")
  val existingMatrixData = new Intermediate(name="existingMatrixData")
  val matmulResultCoord = new Intermediate(name="matmulResultCoord")
  val existingMatrixCoord = new Intermediate(name="existingMatrixCoord")

  val outVal = new Intermediate(name="outVal")
  val outCoord = new Intermediate(name="outCoord")
  val outValid = new Intermediate(name="outValid")

  matmulResultData(row, col) := MatmulResult(row, matmulResultIt(row, col))
  existingMatrixData(row, col) := ExistingMatrix(row, existingMatrixIt(row, col))
  matmulResultCoord(row, col) := MatmulResultCoords(row, matmulResultIt(row, col))
  existingMatrixCoord(row, col) := ExistingMatrixCoords(row, existingMatrixIt(row, col))

  outValid(row, col.lowerBound) := 0

  outValid(row, col) := matmulResultIt(row, col-1) < smallCompressedCols || existingMatrixIt(row, col-1) < largeCompressedCols
  outCoord(row, col) := Select(matmulResultCoord(row, col-1) < existingMatrixCoord(row, col-1) && matmulResultIt(row, col-1) < smallCompressedCols,
    matmulResultCoord(row, col-1), existingMatrixCoord(row, col-1))
  outVal(row, col) := Select(matmulResultCoord(row, col-1) === existingMatrixCoord(row, col-1) && matmulResultIt(row, col-1) < smallCompressedCols,
    matmulResultData(row, col-1) + existingMatrixData(row, col-1),
    Select(matmulResultCoord(row, col-1) < existingMatrixCoord(row, col-1) && matmulResultIt(row, col-1) < smallCompressedCols,
      matmulResultData(row, col-1), existingMatrixData(row, col-1)))

  OutputMatrix(row, outCoord(row, col)) := outVal(row, col)
  OutputMatrix.setValidCond(outValid)

  matmulResultIt(row, col.lowerBound) := 0
  existingMatrixIt(row, col.lowerBound) := 0

  matmulResultIt(row, col) := matmulResultIt(row, col-1) + ((matmulResultCoord(row, col-1) <= existingMatrixCoord(row, col-1) || existingMatrixIt(row, col-1) >= largeCompressedCols) && matmulResultIt(row, col-1) < smallCompressedCols)
  existingMatrixIt(row, col) := existingMatrixIt(row, col-1) + ((existingMatrixCoord(row, col-1) <= matmulResultCoord(row, col-1) || matmulResultIt(row, col-1) >= smallCompressedCols) && existingMatrixIt(row, col-1) < largeCompressedCols)

  Transform(
    1, 0,
    0, 1,
  )
}

class CsrMatrixSorter(val size: Int = 2, hasBias: Boolean = false, name: String = "merger") extends SpatialArray(
  size = size, boundsOverride = Some(Seq(size, size * 2, size + (if (hasBias) 1 else 0))), hasSubArrays = true, alwaysStart = true, name = Some(name)) {
  val maxInJ = size

  val i = new Index(name="i")
  val j = new Index(name="j", isUnbounded = true)
  val k = new Index(name="k")

  val ScatteredMatrix = new Input(name="ScatteredMatrix")
  val BiasOpt = if (hasBias) Some(new Input(name="Bias")) else None
  val MergedMatrix = new Output(name="MergedMatrix")

  val scatteredMatrixJ = new Intermediate(name="scatteredMatrixJ")
  val mergedMatrixData = new Intermediate(name="mergedMatrixData")
  val mergedMatrixJ = new Intermediate(name="mergedMatrixJ")

  val coords = new Intermediate(name="coords")
  val data = new Intermediate(name="data")

  val pop = new Intermediate(name="pop")
  ScatteredMatrix.setPopCond(pop)
  BiasOpt.foreach(_.setPopCond(pop))

  scatteredMatrixJ(i, j, k.lowerBound) := 0
  scatteredMatrixJ(i, j.lowerBound, k) := 0

  mergedMatrixJ(i, j, k.lowerBound) := MaxVal
  mergedMatrixJ(i, j.lowerBound, k) := MaxVal

  pop(i, j.lowerBound, k) := False

  val maxK = if (hasBias) size + 1 else size
  for (k_ <- -1 until maxK) {
    val k__ = k.as(k_)
    val use_bias = hasBias && k_ == maxK - 1

    val newScatteredMatrixJ = scatteredMatrixJ(i, j-1, k__) + pop(i, j-1, k__)

    coords(i, j, k__) := (if (use_bias) BiasOpt.get(i, newScatteredMatrixJ).coord(1) else ScatteredMatrix(i, newScatteredMatrixJ, k__).coord(1))
    data(i, j, k__) := (if (use_bias) BiasOpt.get(i, newScatteredMatrixJ) else ScatteredMatrix(i, newScatteredMatrixJ, k__))
    scatteredMatrixJ(i, j, k__) := newScatteredMatrixJ

    pop(i, j, k__) := mergedMatrixJ(i, j, k.upperBound - 1) === coords(i, j, k__) // TODO I feel like this could set up some unnecessarily long combinational wires
  }

  mergedMatrixJ(i, j, k) := Util.min(mergedMatrixJ(i, j, k-1), coords(i, j, k))
  mergedMatrixData(i, j, k) := Select(mergedMatrixJ(i, j, k-1) < coords(i, j, k),
    mergedMatrixData(i, j, k-1), Select(mergedMatrixJ(i, j, k-1) === coords(i, j, k),
      mergedMatrixData(i, j, k-1) + data(i, j, k), data(i, j, k)
    )
  )

  MergedMatrix(i, mergedMatrixJ(i, j, k)) := mergedMatrixData(i, j, k.upperBound - 1)

  Transform(
    1, 0, 0,
    0, 0, 1,
    0, 1, 0,
  )
}

class Filter(max_I: Int = 2, max_J: Int = 2, max_K: Int = 2) extends SpatialArray(size = max_I.min(max_J).min(max_K), boundsOverride = Some(Seq(max_I, max_J, max_K))) {
  // Filters out columns of A to match the rows of a CSC B

  val i = new Index(name="i")
  val j = new Index(name="j")
  val k = new Index(name="k", isUnbounded = true)

  val A = new Input(name="A")
  val Bk = new Input(name="Bk")

  val Out = new Output(name="Out")

  val a = new Intermediate(name="a")

  val bk = new Intermediate(name="bk")
  val bkIt = new Intermediate(name="bkIt")

  val popA = new Intermediate(name="popA")
  val popB = new Intermediate(name="popB")

  val valid = new Intermediate(name="valid")

  val lastAcross = new Intermediate(name="lastAcross")
  val last = new Intermediate(signalsEndingInnermostTimeAxis=true, name="last")

  a(i, j.lowerBound, k) := A(i, k)
  a(i, j.as(0), k) := A(i, k) // TODO we shouldn't need this line, but it allows us to avoid errors when we time-unbound the 'k' dimension
  a(i, j, k) := a(i, j - 1, k)
  popA(i, j, k) := True // TODO this should only be true on "j.lowerBound"
  A.setPopCond(popA)

  popB(i, j, k.lowerBound) := False
  popB(i.lowerBound, j, k) := bk(i.lowerBound, j, k) === k
  popB(i.as(0), j, k) := bk(i.as(0), j, k) === k // TODO we shouldn't need this line, but the "i.lowerBound" line above just gets cut completely out of scope and replaced with DontCares
  popB(i, j, k) := popB(i-1, j, k)
  Bk.setPopCond(popB)

  bkIt(i, j, k.lowerBound) := 0
  bkIt(i.lowerBound, j, k) := bkIt(i.lowerBound, j, k-1) + popB(i.lowerBound, j, k-1)
  bkIt(i.as(0), j, k) := bkIt(i.as(0), j, k-1) + popB(i.as(0), j, k-1) // TODO we shouldn't need this line, but the "i.lowerBound" line above just gets cut completely out of scope and replaced with DontCares
  bkIt(i, j, k) := bkIt(i-1, j, k)

  bk(i.lowerBound, j, k) := Bk(bkIt(i.lowerBound, j, k), j)
  bk(i.as(0), j, k) := Bk(bkIt(i.as(0), j, k), j) // TODO we shouldn't need this line, but the "i.lowerBound" line above just gets cut completely out of scope and replaced with DontCares
  bk(i, j, k) := bk(i-1, j, k)

  valid(i, j.lowerBound, k) := popB(i, j.lowerBound, k)
  valid(i, j, k) := popB(i, j, k) || valid(i, j-1, k)

  Out(i,k) := a(i, j.upperBound - 1, k)
  Out.setValidCond(valid)

  lastAcross(i, j.lowerBound, k) := True
  lastAcross(i, j, k) := bk(i, j, k) >= MaxVal && lastAcross(i, j-1, k)
  last(i.upperBound - 1, j.upperBound - 1, k) := lastAcross(i.upperBound - 1, j.upperBound - 1, k)
  last(i, j, k) := False

  Transform(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  )
}

class MatrixAdder(size: Int = 2, alwaysStart: Boolean = false, unbounded: Boolean = false, override val dataWidthBitsOpt: Option[Int] = None) extends SpatialArray(size = size, alwaysStart = alwaysStart) {
  // Calculates C + D = E

  val i = new Index(name="i", isUnbounded = unbounded)
  val j = new Index(name="j")

  val C = new Input(name = "C")
  val D = new Input(name = "D")
  val E = new Output(name = "E")

  val c = new Intermediate(name="c")
  val d = new Intermediate(name="d")
  val e = new Intermediate(name="e")

  c(i,j) := C(i,j)
  d(i,j) := D(i,j)

  e(i,j) := c(i,j) + d(i,j)

  E(i,j) := e(i,j)

  // Valid and last signals
  val unavailable = Option.when(unbounded)(new Intermediate(name="unavailable"))
  val valid = Option.when(unbounded)(new Intermediate(name="valid"))
  val last = Option.when(unbounded)(new Intermediate(name="last", signalsEnding = true))

  unavailable.foreach(_(i,j) := D(i,j).unavailable) // We assume that if D is unavailable, then C is also unavailable
  valid.foreach(_(i,j.as(0)) := !unavailable.get(i,j))
  valid.foreach(v => v(i,j) := v(i,j-1))
  valid.foreach(E.setValidCond(_))
  valid.foreach(C.setValidCond(_))

  last.foreach(_(i,j.upperBound-1) := !valid.get(i,j))
  last.foreach(_(i,j) := False)

  Transform(
    0, 1,
    1, 0
  )
}

class CsrMatrixAdder(val size: Int = 2, maxAJ: Int = -1, maxBJ: Int = -1, isVector: Boolean = false) extends SpatialArray(size = size, boundsOverride = Some((if (isVector) Seq() else Seq(size)) ++ Seq((size*size).max(maxAJ).max(maxBJ)))) {
  // This array adds two CSR matrices (A and B) together to output another CSR matrix (C)

  val maxAJ_ = if (maxAJ >= 0) maxAJ else size
  val maxBJ_ = if (maxBJ >= 0) maxBJ else size

  val iOpt = if (isVector) None else Some(new Index(name="i"))
  def i = iOpt.get
  val j = new Index(name="j", isUnbounded = true)

  def wI(j_ : Expr): Vector[Expr] = iOpt.toVector ++ Vector(j_)

  val Adata = new Input(name="Adata")
  val Bdata = new Input(name="Bdata")

  val Acoords = new Input(name="Acoords")
  val Bcoords = new Input(name="Bcoords")

  val C = new Output(name="C")

  val aj = new Intermediate(name="aj")
  val bj = new Intermediate(name="bj")

  val ad = new Intermediate(name="ad")
  val bd = new Intermediate(name="bd")
  val cd = new Intermediate(name="cd")

  val ac = new Intermediate(name="ac")
  val bc = new Intermediate(name="bc")
  val cc = new Intermediate(name="cc")

  val valid = new Intermediate(name="valid")

  val lasta = new Intermediate(name="lasta")
  val lastb = new Intermediate(name="lastb")
  val lastcAcross = new Intermediate(name="lastcAcross")
  val lastc = new Intermediate(signalsEnding=true, name="lastc")

  aj(wI(j.lowerBound)) := 0
  bj(wI(j.lowerBound)) := 0

  if (!isVector) {
    valid(i.lowerBound, j) := False
  }
  valid(wI(j.lowerBound)) := False

  ad(wI(j)) := Adata(wI(aj(wI(j-1))))
  bd(wI(j)) := Bdata(wI(bj(wI(j-1))))

  ac(wI(j)) := Acoords(wI(aj(wI(j-1))))
  bc(wI(j)) := Bcoords(wI(bj(wI(j-1))))

  aj(wI(j)) := aj(wI(j-1)) + (ac(wI(j)) <= bc(wI(j)))
  bj(wI(j)) := bj(wI(j-1)) + (ac(wI(j)) >= bc(wI(j)))

  cc(wI(j)) := Util.min(ac(wI(j)), bc(wI(j)))
  cd(wI(j)) := Select(ac(wI(j)) === bc(wI(j)), ad(wI(j)) + bd(wI(j)), Select(ac(wI(j)) < bc(wI(j)), ad(wI(j)), bd(wI(j))))

  val maxVal = 1000000000 // TODO we need to set this without using magic numbers

  // valid(wI(j)) := ((aj(wI(j-1)) < maxAJ_) || (bj(wI(j-1)) < maxBJ_)) && cc(wI(j)) < maxVal
  valid(wI(j)) := cc(wI(j)) < maxVal
  /* TODO
     When specifying the "valid" signal, we should also add a " && cd(wI(j)) =/= 0" condition. However, this currently
     causes errors, because the coordLookup may end up having missing elements, since coordLookups are currently filled
     based on spatial array indices, rather than by PE outputs. We should fix that in the future
   */

  C(wI(cc(wI(j)))) := cd(wI(j))
  C.setValidCond(valid)

  lasta(wI(j)) := ac(wI(j)) <= bc(wI(j))
  lastb(wI(j)) := ac(wI(j)) >= bc(wI(j))
  Adata.setPopCond(lasta)
  Bdata.setPopCond(lastb)

  val isLastOut = cc(wI(j)) >= maxVal
  if (isVector) {
    lastc(j.lowerBound) := False
    lastc(j) := isLastOut
  } else {
    lastcAcross(i.lowerBound, j.lowerBound) := False
    lastcAcross(i.lowerBound, j) := True
    lastcAcross(i, j.lowerBound) := False
    lastcAcross(i, j) := isLastOut && (lastcAcross(i-1, j) =/= 0)

    lastc(i.upperBound - 1, j) := lastcAcross(i.upperBound - 1, j)
    lastc(i, j) := False
  }

  if (isVector) {
    Transform(
      1
    )
  } else {
    Transform(
      1, 0,
      0, 1,
    )
  }
}

class GustavsonsMatmul extends SpatialArray {
  // This performs a sparse matmul with the Gustavson dataflow (like in Gamma and MatRaptor)

  val i = new Index(name="i")
  val j = new Index(name="j")
  val k = new Index(name="k")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)
  c(i, j, k.lowerBound) := 0

  // Intermediate calculations
  a(i, j, k) := a(i, j-1, k)
  b(i, j, k) := b(i-1, j, k)
  c(i, j, k) := c(i, j, k-1) + a(i, j-1, k) * b(i-1, j, k)

  // Outputs
  C(i,j) := c(i, j, k.upperBound - 1)

  // Space-time transform
  Transform(
    1, 0, 0,
    0, 0, 1,
    1, 1, 1,
  )

  // Sparsity
  Skip(k) where (A(i,k) === 0)
  Skip(j) where (B(k,j) === 0)
}

class OuterMatmul(size: Int = 2, isLoadBalanced: Boolean = true, isUnbounded: Boolean = true, shouldPopA: Boolean = true, travellingCoords: Boolean = false, fewerInPorts: Boolean = false, skipA: Boolean = true, useCompressedCoordsForB: Boolean = false, alwaysStart: Boolean = false, outputDelay: Int = 0, onlyCheckIfOutputsAreReady: Boolean = false, name: String = "matmul", isDummy: Boolean = false) extends SpatialArray(
  hasSubArrays = true, size = size, boundsOverride = if (isUnbounded) Some(Seq(size, size*2, size)) else None, ignoreBaseC2e = true, alwaysStart = alwaysStart, outputDelay = outputDelay, onlyCheckIfOutputsAreReady = onlyCheckIfOutputsAreReady, name = Some(name)) {
  // This performs a sparse matmul with the outer-product dataflow (like in OuterSpace)

  require(!(isLoadBalanced && isUnbounded), "the load-balanced matmul can't assert the last-signal for some reason, so we can't make it unbalanced right now") // TODO fix this issue

  val i = new Index(name="i")
  val j = new Index(name="j", isUnbounded = isUnbounded)
  val k = new Index(name="k")

  val A = new Input(unBoundedAccessesPermitted = false, name = "A")
  val B = new Input(useCompressedCoords = useCompressedCoordsForB, name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)
  c(i, j, k.lowerBound) := 0

  // Intermediate calculations
  a(i, j, k) := a(i, j - 1, k)
  b(i, j, k) := b(i - 1, j, k)
  c(i, j, k) := (if (isDummy) Const(0) else { a(i, j - 1, k) * b(i - 1, j, k) })

  // Outputs
  C(i, j, k) := c(i, j, k)

  // Axis spans.
  /* This isn't necessary for functional correctness, but it can make your accelerator's DMA faster by helping it know
     how long adjacent axes will be before your matmul has finished executing. That makes it easier for the DMA to
     overlap a matmul with the mvout of the result of that matmul. */
  // TODO We should add an optimization pass that lets Stellar set this automatically, without the user needing to do so
  val j_span = new Intermediate(name="j_span")
  j_span(i.as(0), j, k) := B(k, j).axisSpan()
  j_span(i, j, k) := j_span(i-1, j, k)
  C.setAxisSpan(j_span)

  // Space-time transform
  Transform(
    0, 0, 1,
    1, 0, 0,
    1, 1, 0,
  )

  // Sparsity
  if (skipA) {
    Skip(i) where (A(i,k) === 0)
  }
  Skip(j) where (B(k,j) === 0)

  // Load-balancing
  if (isLoadBalanced) {
    Map(i.upperBound->, j, k) to ((i.lowerBound+1) -> i.upperBound, j, k + 1)
    // Map(i, j, k.upperBound->) to (i + 1, j, (k.lowerBound+1) -> k.upperBound) // This is a more unusual load-balancing option
  }
}

class DiagonalMatmul extends SpatialArray {
  // This performs a matmul (A * B = C) where B is diagonal

  val i = new Index(name="i")
  val j = new Index(name="j")
  val k = new Index(name="k")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)
  c(i, j, k.lowerBound) := 0

  // Intermediate calculations
  a(i, j, k) := a(i, j - 1, k)
  b(i, j, k) := b(i - 1, j, k)
  c(i, j, k) := c(i, j, k - 1) + a(i, j - 1, k) * b(i - 1, j, k)

  // Outputs
  C(i,j) := c(i, j, k.upperBound - 1)

  // Space-time transform
  Transform(
    0, 0, 1,
    0, 1, 0,
    1, 1, 1,
  )

  Skip(k) where (k =/= j)
  Skip(j) where (k =/= j)
}

class DenseMatmul(size: Int = 2, hasTransform: Boolean = true, dataflow: String = "is", unbounded: Boolean = false, override val dataWidthBitsOpt: Option[Int] = None, alwaysStart: Boolean = false) extends SpatialArray(size=size, alwaysStart=alwaysStart) {
  // This performs a dense matmul

  val i = new Index(name="i", isUnbounded = unbounded)
  val j = new Index(name="j")
  val k = new Index(name="k")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)
  c(i, j, k.lowerBound) := 0

  // Intermediate calculations
  a(i, j, k) := a(i, j - 1, k)
  b(i, j, k) := b(i - 1, j, k)
  c(i, j, k) := c(i, j, k - 1) + a(i, j - 1, k) * b(i - 1, j, k)

  // Outputs
  C(i,j) := c(i, j, k.upperBound - 1)

  // Last signals
  val aUnavailable = Option.when(unbounded)(new Intermediate(name="aUnavailable"))
  val last = Option.when(unbounded)(new Intermediate(name="last", signalsLocalEnding = true))
  val valid = Option.when(unbounded)(new Intermediate(name="valid"))
  aUnavailable.foreach(_(i, j.lowerBound, k) := A(i, k).unavailable)
  aUnavailable.foreach(_(i, j, k) := aUnavailable.get(i, j - 1, k))
  valid.foreach(_(i, j, k) := !aUnavailable.get(i,j,k))
  valid.foreach(C.setValidCond)
  last.foreach(_(i.as(0),j,k) := False)
  last.foreach(_(i,j,k) := aUnavailable.get(i,j,k))

  // Space-time transform
  if (hasTransform) {
    if (dataflow == "is") {
      Transform(
        // 2, 1, 0,
        // 1, 1, 1,
        // 0, 0, 1,

        // 1, 0, 0,
        // 1, 1, 1,
        // 0, 1, 0,

        // 1, 0, 0,
        // 0, 1, 0,
        // 1, 1, 1,

        0, 0, 1,
        1, 0, 0,
        1, 1, 1,
      )
    } else if (dataflow == "ws") {
      Transform(
        0, 0, 1,
        0, 1, 0,
        1, 1, 1,
      )
    } else {
      throw new Exception("Unknown dataflow")
    }
  }
}

class DenseSparseMatmul extends SpatialArray {
  // This multiplies a dense matrix by a sparse matrix

  val i = new Index(name="i")
  val j = new Index(name="j")
  val k = new Index(name="k")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)
  c(i, j, k.lowerBound) := 0

  // Intermediate calculations
  a(i, j, k) := a(i, j-1, k)
  b(i, j, k) := b(i-1, j, k)
  c(i, j, k) := c(i, j, k-1) + a(i, j-1, k) * b(i-1, j, k)

  // Outputs
  C(i,j) := c(i, j, k.upperBound - 1)

  // Space-time transform
  Transform(
    1, 0, 0,
    0, 0, 1,
    1, 1, 1,
  )

  // Sparsity
  Skip(j) where (B(k,j) === 0)
}

class A100DenseMatmul extends SpatialArray {
  // This performs a dense A100-style matmul

  val i = new Index(name="i")
  val j = new Index(name="j")
  val ko = new Index(name="ko")
  val ki = new Index(name="ki")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, ko, ki) := A(i, ko * ki.upperBound + ki)
  b(i.lowerBound, j, ko, ki) := B(ko * ki.upperBound + ki, j)

  // Intermediate calculations
  a(i, j, ko, ki) := a(i, j - 1, ko, ki)
  b(i, j, ko, ki) := b(i - 1, j, ko, ki)
  c(i, j, ko, ki.upperBound - 1) := c(i, j, ko, ki.upperBound - 2) + c(i, j, ko - 1, ki.upperBound - 1) +
    a(i, j - 1, ko, ki.upperBound - 1) * b(i - 1, j, ko, ki.upperBound - 1)
  c(i, j, ko, ki) := c(i, j, ko, ki - 1) + a(i, j - 1, ko, ki) * b(i - 1, j, ko, ki)

  // Outputs
  C(i,j) := c(i, j, ko.upperBound - 1, ki.upperBound - 1)

  // Space-time transform
  Transform(
    // OS
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 0, 1,
    1, 1, 1, 0,
  )
}

class A100Test extends SpatialArray {
  // This performs a dense matmul with an output-stationary dataflow

  val i = new Index(name="i")
  val j = new Index(name="j")
  val ko = new Index(name="ko")
  val ki = new Index(name="ki")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, ko, ki) := A(i, ko * ki.upperBound + ki)
  b(i.lowerBound, j, ko, ki) := B(ko * ki.upperBound + ki, j)
  c(i, j, ko, ki.lowerBound) := 0

  // Intermediate calculations
  a(i, j, ko, ki) := a(i, j - 1, ko, ki)
  b(i, j, ko, ki) := b(i - 1, j, ko, ki)
  c(i, j, ko, ki.upperBound - 1) := c(i, j, ko, ki - 1) + c(i, j, ko - 1, ki) +
    a(i, j - 1, ko, ki) * b(i - 1, j, ko, ki)
  c(i, j, ko, ki) := c(i, j, ko, ki - 1) + a(i, j - 1, ko, ki) * b(i - 1, j, ko, ki)

  // Outputs
  C(i,j) := c(i, j, ko.upperBound - 1, ki.upperBound - 1)

  // Space-time transform
  Transform(
    // OS
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 0, 1,
    1, 1, 1, 0,
  )

  OptimisticSkip(ki) where (A(i,ko,ki) === 0)
}

class A100TiledMatmul extends SpatialArray {
  // This performs a sparse A100-style matmul

  val i = new Index(name="i")
  val j = new Index(name="j")
  val k = new Index(name="k", nSubtiles = 1)

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)

  // Intermediate calculations
  a(i, j, k) := a(i, j - 1, k)
  b(i, j, k) := b(i - 1, j, k)
  c(i, j, k) := c(i, j, k - 1) + a(i, j - 1, k) * b(i - 1, j, k)

  // Outputs
  C(i,j) := c(i, j, k.upperBound - 1)

  // Space-time transform
  Transform(
    // OS
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 0, 1,
    1, 1, 1, 0,

    // WS
//    0, 0, 1, 0,
//    1, 0, 0, 0,
//    0, 0, 0, 1,
//    1, 1, 1, 0,
  )

  // Sparsity
  OptimisticSkip(k(1)) where (A(i,k(0),k(1)) === 0)
}

class OuterISorter(size: Int = 2, name: String = "sorter") extends SpatialArray(size = size, boundsOverride = Some(Seq(size*size, size)), name = Some(name)) {
  // Sorts the "expanded-i" coming from a CSC A matrix in the OuterSpace-like accelerator
  val i = new Index("i", isUnbounded = true)
  val k = new Index("k")

  val ExpandedI = new Input(name="ExpandedI")

  val SortedI = new Output(name="SortedI")

  val it = new Intermediate(name="it")
  val sortedI = new Intermediate(name="sortedI")
  val expandedI = new Intermediate(name="expandedI")

  val pop = new Intermediate(name="pop")
  val valid = new Intermediate(name="valid")
  val last = new Intermediate(signalsEndingInnermostTimeAxis=true, name="last")

  val maxVal = 1000000000 // TODO we need to set this without using magic numbers

  it(i.lowerBound, k) := 0
  it(i, k) := it(i - 1, k) + pop(i - 1, k)

  expandedI(i, k) := ExpandedI(it(i, k), Const(0), k) // We insert a 0 in the ExpandedI coords here just because the ExpandedI values come from elements with (i,j,k) coords
  // ExpandedI.setPopCond(pop) // We can't pop here because the contents of RegScatteredC need to be feed into SramScatteredC later

  sortedI(i, k.lowerBound) := maxVal
  sortedI(i, k) := Select(sortedI(i, k-1) < expandedI(i, k), sortedI(i, k-1), expandedI(i, k))

  pop(i.lowerBound, k) := False
  for (k_ <- 0 until size) {
    val k__ = k.as(k_)
    pop(i, k__) := expandedI(i, k__) === sortedI(i, k.upperBound - 1) // TODO this doesn't work when using multiple time axes
  }

  SortedI(sortedI(i, k.upperBound - 1)) := sortedI(i, k.upperBound - 1)
  SortedI.setValidCond(valid)

  valid(i,k) := sortedI(i, k) < maxVal

  last(i, k.upperBound - 1) := !valid(i, k.upperBound - 1)
  last(i, k) := False

  // TODO we should use 2 time axes, instead of unrolling 'k' spatially
  Transform.withMultipleTimeAxes(1)(transform =
    0, 1,
    1, 0,
  )
}

class SCNNMultArray(nBanks: Int, rowSize: Int, maxI: Int, maxF: Int, maxWt: Int, maxHt: Int, name: String = "scnn") extends SpatialArray(size = maxI min maxF, boundsOverride = Some(Seq(maxI, maxF, 4, maxWt, maxHt)), hasSubArrays = true, alwaysStart = true, name = Some(name)) {
  // Indices
  val i = new Index("i") // Represents pixels spatially
  val f = new Index("f") // Represents filter spatially
  val w = new Index("w", isUnbounded = true) // Represents filter temporally

  val wt = new Index(name = "wt") // A 3D tiling factor
  val ht = new Index(name = "ht") // A 3D tiling factor

  // Variables
  val Image = new Input(name="Image")
  val ImageWCoord = new Input(name="ImageWCoord")
  val ImageHCoord = new Input(name="ImageHCoord")

  val Filter = new Input(name="Filter")
  val FilterWCoord = new Input(name="FilterWCoord")
  val FilterHCoord = new Input(name="FilterHCoord")
  val FilterOutChannelCoord = new Input(name="FilterOutChannelCoord")

  val PartialSums = new Output(name="PartialSums")

  val Params = new Input(name = "Params")

  // Intermediates
  val input = new Intermediate(name="input")
  val weight = new Intermediate(name="weight")

  val partial = new Intermediate(name="partial")
  val partial_channel_coord = new Intermediate(name="partial_channel_coord")
  val partial_w_coord = new Intermediate(name="partial_w_coord")
  val partial_h_coord = new Intermediate(name="partial_h_coord")

  val input_w_coord = new Intermediate(name="input_w_coord")
  val input_h_coord = new Intermediate(name="input_h_coord")
  val weight_out_channel_coord = new Intermediate(name="weight_out_channel_coord")
  val weight_w_coord = new Intermediate(name="weight_w_coord")
  val weight_h_coord = new Intermediate(name="weight_h_coord")

  val valid = new Intermediate(name="valid")

  val params_nc = new Intermediate(name="params_nc")
  val params_nw = new Intermediate(name="params_nw")
  val params_nh = new Intermediate(name="params_nh")

  // Inputs
  params_nc(i,f,w,wt,ht) := Params(Const(0))
  params_nw(i,f,w,wt,ht) := Params(Const(1))
  params_nh(i,f,w,wt,ht) := Params(Const(2))

  val nc = params_nc(i,f,w,wt,ht)
  val nw = params_nw(i,f,w,wt,ht)
  val nh = params_nh(i,f,w,wt,ht)

  Seq((input, Image), (input_w_coord, ImageWCoord), (input_h_coord, ImageHCoord)).foreach { case (x,y) =>
    val coords = Seq(wt * maxHt + ht, i)
    x(i,f.as(0),w.as(0),wt,ht) := y(coords:_*)
  }

  Seq((weight, Filter), (weight_w_coord, FilterWCoord), (weight_h_coord, FilterHCoord), (weight_out_channel_coord, FilterOutChannelCoord)).foreach { case (x,y) =>
    x(i.as(0),f,w,wt,ht) := y(w * maxF + f)
  }

  // Intermediate calculations
  Seq(input, input_w_coord, input_h_coord).foreach { x =>
    x(i,f,w.as(0),wt,ht) := x(i,f-1,w,wt,ht)
    x(i,f,w,wt,ht) := x(i,f,w-1,wt,ht)
  }

  Seq(weight, weight_w_coord, weight_h_coord, weight_out_channel_coord).foreach { x =>
    x(i,f,w,wt,ht) := x(i-1, f, w, wt, ht)
  }

  partial(i,f,w,wt,ht) := input(i,f,w,wt,ht) * weight(i,f,w,wt,ht)
  partial_channel_coord(i,f,w,wt,ht) := weight_out_channel_coord(i,f,w,wt,ht)
  partial_w_coord(i,f,w,wt,ht) := input_w_coord(i,f,w,wt,ht) - weight_w_coord(i,f,w,wt,ht) + 11
  partial_h_coord(i,f,w,wt,ht) := input_h_coord(i,f,w,wt,ht) - weight_h_coord(i,f,w,wt,ht) + 11

  // Outputs
  /* TODO This spatial array combines the tasks of both SCNN's multiplier array and the scatter network used to route
          partial sums to the right SRAM bank. In the future though, we should update the scatter-SRAMs so that they
          can identify which partial sums they need to read on their own, without a scatter-gather network like this one
          to issue them from the mult-array. */
  val address = partial_channel_coord(i,f,w,wt,ht) * nw * nh + partial_w_coord(i,f,w,wt,ht) * nh + partial_h_coord(i,f,w,wt,ht)
  val bank = address % nBanks

  PartialSums(bank, address) := partial(i,f,w,wt,ht)

  valid(i,f,w,wt,ht) := Seq(
    input_w_coord(i,f,w,wt,ht),
    input_h_coord(i,f,w,wt,ht),
    weight_out_channel_coord(i,f,w,wt,ht),
    weight_w_coord(i,f,w,wt,ht),
    weight_h_coord(i,f,w,wt,ht)
  ).map(_ < MaxVal).foldLeft(True: BoolExpr)(_ && _)

  PartialSums.setValidCond(valid)

  // Transform
  Transform(
    0, 0, 0, 1, 0, // outer W
    0, 0, 0, 0, 1, // outer H
    1, 0, 0, 0, 0, // inner I
    0, 1, 0, 0, 0, // inner F
    0, 0, 1, 0, 0, // time
  )
}

class ParallelSorter(throughputPerDimPerChunk: Int, nChunksPerDim: Int, nTimeMultiplexedInFiberPairs: Int, nOuterFibers: Int, popDuplicates: Boolean = false, shouldPop: Boolean = true, expandOuterCoord: Boolean = true, outputWhichFiberPops: Boolean = false, checkWhichFiberPopped: Boolean = false, stallForInCoords: Boolean = true, maxElemsPerFiber: Option[Int] = None, isDummy: Boolean = false, name: String = "sorter") extends SpatialArray(boundsOverride = Some(Seq(nOuterFibers, 4, 2 * nChunksPerDim - 1, throughputPerDimPerChunk + 1, throughputPerDimPerChunk + 1)), name = Some(name), alwaysStart = true, hasSubArrays = true, stallForOutputs = false) {
  // This merger mimic SpArch's high-throughput parallel merger

  val nChunks = 2 * nChunksPerDim - 1

  val outer_fiber = new Index("out_fiber")
  val t = new Index("t", isUnbounded = true)
  val chunk = new Index("chunk")
  val comparator_row = new Index(s"comparator_row")
  val comparator_col = new Index(s"comparator_col")

  val InnerCoord = new Input(name="InnerCoord")
  val OuterCoordIn = new Input(name="OuterCoordIn")

  val SortedInnerCoord = new Output(name="SortedInnerCoord")

  val ElemsPerFiberOpt = Option.when(maxElemsPerFiber.nonEmpty)(new Input(name="ElemsPerFiber"))

  val itAs = Seq.tabulate(nTimeMultiplexedInFiberPairs)(i => new Intermediate(name=s"itA$i"))
  val itBs = Seq.tabulate(nTimeMultiplexedInFiberPairs)(i => new Intermediate(name=s"itB$i"))
  val itMergeds = Seq.tabulate(nTimeMultiplexedInFiberPairs)(i => new Intermediate(name=s"itMerged$i"))

  val pair = new Intermediate(name="pair")
  val pairIsRunning = Seq.tabulate(nTimeMultiplexedInFiberPairs)(i => new Intermediate(name=s"pairIsRunning$i"))
  val lastForPair = new Intermediate(name="lastForPair")

  val outerCoordItsOpt = Option.when(nOuterFibers > 1)(Seq.tabulate(nTimeMultiplexedInFiberPairs)(i => new Intermediate(name=s"outerCoordIt$i")))
  val outerCoordOpt = Option.when(nOuterFibers > 1)(new Intermediate(name=s"outerCoord"))
  val outerCoordIsValidOpt = Option.when(nOuterFibers > 1)(new Intermediate(name=s"outerCoordIsValid"))
  val outerCoordIsValidForAOpt = Option.when(nOuterFibers > 1 && checkWhichFiberPopped)(new Intermediate(name="outerCoordIsValidForA"))
  val outerCoordIsValidForBOpt = Option.when(nOuterFibers > 1 && checkWhichFiberPopped)(new Intermediate(name="outerCoordIsValidForB"))
  val outerCoordFoundOpt = Option.when(nOuterFibers > 1)(new Intermediate(name=s"outerCoordFound"))

  val mainInCoordA = new Intermediate(name="mainInCoordA")
  val mainInCoordB = new Intermediate(name="mainInCoordB")
  val mainInCoordAFound = new Intermediate(name="mainInCoordAFound")
  val mainInCoordBFound = new Intermediate(name="mainInCoordBFound")

  val mainComparison = new Intermediate(name="mainComparison")
  val mainBoundary = new Intermediate(name="mainBoundary")

  val mainChunkAcrossA = new Intermediate(name="mainChunkAcrossA")
  val mainChunkAcrossB = new Intermediate(name="mainChunkAcrossB")

  val mainChunkA = new Intermediate(name="mainChunkA")
  val mainChunkB = new Intermediate(name="mainChunkB")

  val mainChunkChosen = new Intermediate(name="mainChunkChosen")

  val chunkInCoordA = new Intermediate(name="chunkInCoordA")
  val chunkInCoordB = new Intermediate(name="chunkInCoordB")

  val chunkComparison = new Intermediate(name="chunkComparison")
  val chunkEqualsOpt = Option.when(popDuplicates)(new Intermediate(name="chunkEquals"))
  val chunkBoundary = new Intermediate(name="chunkBoundary")

  val chunkMin = new Intermediate(name="chunkMin")
  val chunkMax = new Intermediate(name="chunkMax")

  val sortedCoord = new Intermediate(name="sortedCoord")

  val popA = new Intermediate(name="popA")
  val popB = new Intermediate(name="popB")
  val popEither = new Intermediate(name="popEither")
  val popMerged = new Intermediate(name="popMerged")
  val diagPopA = new Intermediate(name="diagPopA")
  val diagPopB = new Intermediate(name="diagPopB")
  val diagPopEither = new Intermediate(name="diagPopEither")
  val diagPopMerged = new Intermediate(name="diagPopMerged")
  val runningPopsA = new Intermediate(name="runningPopsA")
  val runningPopsB = new Intermediate(name="runningPopsB")
  val runningPopsEither = new Intermediate(name="runningPopsEither")
  val runningPopsMerged = new Intermediate(name="runningPopsMerged")
  val totalPopA = new Intermediate(name="totalPopA")
  val totalPopB = new Intermediate(name="totalPopB")
  val totalPopMerged = new Intermediate(name="totalPopMerged")

  val popOuterCoordOpt = Option.when(nOuterFibers > 1)(new Intermediate(name="popOuterCoord"))

  val elemsPerFiberInRf = Seq.tabulate(if (maxElemsPerFiber.nonEmpty) nTimeMultiplexedInFiberPairs else 0)(i => new Intermediate(name=s"elemsPerFiberInRf$i"))

  val valid = new Intermediate(name="valid")

  val last = new Intermediate(signalsEndingInnermostTimeAxis=true, name="last")

  val outs_stalling = new Intermediate(name="outs_stalling")

  def now(inter: Intermediate): Indexed = inter(outer_fiber, t, chunk, comparator_row, comparator_col)
  def prev(inter: Intermediate): Indexed = inter(outer_fiber, t-1, chunk, comparator_row, comparator_col)

  def broadCast(inter: Intermediate): Unit = {
    inter(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col) := inter(outer_fiber, t, chunk, comparator_row, comparator_col-1)
    inter(outer_fiber, t, chunk.as(0), comparator_row, comparator_col) := inter(outer_fiber, t, chunk, comparator_row-1, comparator_col)
    inter(outer_fiber, t, chunk, comparator_row, comparator_col) := inter(outer_fiber, t, chunk-1, comparator_row, comparator_col)
  }

  def setSpatialLowerBounds(inter: Intermediate, setTo: Int): Unit = {
    inter(outer_fiber, t, chunk.lowerBound, comparator_row, comparator_col) := setTo
    inter(outer_fiber, t, chunk, comparator_row.lowerBound, comparator_col) := setTo
    inter(outer_fiber, t, chunk, comparator_row, comparator_col.lowerBound) := setTo
  }

  def inputs_stalling(timing: Intermediate => Expr = now) = if (stallForInCoords) False else {
    !outerCoordFoundOpt.map(now).getOrElse(True) || Seq((mainInCoordAFound, outerCoordIsValidForAOpt), (mainInCoordBFound, outerCoordIsValidForBOpt)).map { case (mainInCoordFound, outerCoordIsValidOpt) =>
      (!timing(mainInCoordFound) && outerCoordIsValidOpt.map(now).getOrElse(True)): BoolExpr
    }.reduce(_ || _)
  }
  def stalling(timing: Intermediate => Expr = now) = timing(outs_stalling) || inputs_stalling(timing)

  val poppingOuterCoord = popOuterCoordOpt.zip(outerCoordIsValidOpt).map { case (pop, v) => now(pop) && now(v) }.getOrElse(False)

  lastForPair(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := False
  now(lastForPair) := now(totalPopA) === 0 && now(totalPopB) === 0

  pairIsRunning.zipWithIndex.foreach { case (isRunning, pairId) =>
    isRunning(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := True
    isRunning(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := prev(isRunning) &&
      !(!stalling() && prev(pair) === pairId && outerCoordIsValidOpt.map(x => !now(x)).getOrElse(now(lastForPair)))
    broadCast(isRunning)
  }

  pair(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := 0
  pair(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := {
    val pairRunningAndReady = pairIsRunning.map(prev).zipWithIndex.map { case (isRunning, pairId) =>
      maxElemsPerFiber match {
        case Some(maxElems) => isRunning && now(elemsPerFiberInRf(pairId)) + (throughputPerDimPerChunk * nChunksPerDim) <= maxElems
        case _ => isRunning
      }
    }

    val higherPairIdExists = pairRunningAndReady.zipWithIndex.map { case (wasRunning, pairId) => (prev(pair) < pairId && wasRunning): BoolExpr }.reduce(_ || _)
    val lowerPairIdExists = pairRunningAndReady.zipWithIndex.map { case (wasRunning, pairId) => (prev(pair) > pairId && wasRunning): BoolExpr }.reduce(_ || _)

    def getHigherOrLowerPair(compFoo: (Indexed, Const) => BoolExpr): Expr =
      pairRunningAndReady.zipWithIndex.map { case (wasRunning, pairId) => ((compFoo(prev(pair), Const(pairId)) && wasRunning): BoolExpr, pairId) }.
        foldRight(Const(nTimeMultiplexedInFiberPairs-1): Expr) { case ((choose, pairId), chosenId) =>
          Select(choose, pairId, chosenId)
        }

    val higherPair = getHigherOrLowerPair(_ < _)
    val lowerPair = getHigherOrLowerPair(_ > _)

    Select(higherPairIdExists, higherPair, Select(lowerPairIdExists, lowerPair, prev(pair)))
  }
  broadCast(pair)

  for ((its, totalPop) <- Seq((itAs, totalPopA), (itBs, totalPopB), (itMergeds, totalPopMerged))) {
    its.zipWithIndex.foreach { case (it, pairId) =>
      it(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := 0
      setSpatialLowerBounds(it, 0)
      it(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) :=
        Select(prev(pair) === pairId && !stalling(), Select(poppingOuterCoord, 0, prev(it) + now(totalPop)), prev(it))
      broadCast(it)
    }
  }

  def forPair(vars: Seq[Intermediate], timing: Intermediate => Expr = prev): Expr = {
    val exprs = vars.map(timing)
    exprs.zipWithIndex.foldLeft(exprs.head: Expr) { case (acc, (it, pairId)) => Select(prev(pair) === pairId, it, acc) }
  }

  val Seq(itA, itB, itMerged) = Seq(itAs, itBs, itMergeds).map { its =>
    val it = new Intermediate(name=its.head.name.filterNot(_.isDigit))
    it(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := 0
    setSpatialLowerBounds(it, 0)
    it(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := forPair(its)
    broadCast(it)
    now(it)
  }

  for (((mainInCoord, found, it, offset), inner_fiber) <- Seq((mainInCoordA, mainInCoordAFound, itA, comparator_col), (mainInCoordB, mainInCoordBFound, itB, comparator_row)).zipWithIndex) {
    val inner_coord_coords = Seq(prev(pair) * 2 + inner_fiber) ++ outerCoordOpt.map(now).toSeq ++ Seq(it + (offset+1) * throughputPerDimPerChunk - 1)

    val padded_indices = indices.toSeq.map(ind => if (ind == offset) ind.upperBound-1 else ind)
    val base_indices = Seq(outer_fiber, t, chunk.as(0)) ++ Seq(comparator_row, comparator_col).map(ind => if (ind != offset) ind.as(0) else ind)
    val propagating_indices = Seq(outer_fiber, t, chunk.as(0)) ++ Seq(comparator_row, comparator_col).map(ind => if (ind != offset) ind-1 else ind)

    mainInCoord(padded_indices:_*) := MaxVal
    mainInCoord(base_indices:_*) := InnerCoord(inner_coord_coords:_*)
    mainInCoord(outer_fiber, t, chunk.as(0), comparator_row, comparator_col) := mainInCoord(propagating_indices:_*)
    now(mainInCoord) := mainInCoord(outer_fiber, t, chunk-1, comparator_row, comparator_col)

    if (stallForInCoords) {
      now(found) := True
    } else {
      require(nChunksPerDim == 1, "i haven't updated the 'found' code yet to work with bigger main mergers")
      found(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := InnerCoord(inner_coord_coords:_*).found
      broadCast(found)
    }
  }

  mainComparison(outer_fiber, t, chunk.as(0), comparator_row.lowerBound, comparator_col) := False
  mainComparison(outer_fiber, t, chunk.as(0), comparator_row, comparator_col.lowerBound) := True
  mainComparison(outer_fiber, t, chunk.as(0), comparator_row, comparator_col) := now(mainInCoordA) < now(mainInCoordB)
  now(mainComparison) := mainComparison(outer_fiber, t, chunk-1, comparator_row, comparator_col)

  mainBoundary(outer_fiber, t, chunk, comparator_row.upperBound-1, comparator_col) := False
  mainBoundary(outer_fiber, t, chunk, comparator_row, comparator_col.upperBound-1) := False
  mainBoundary(outer_fiber, t, chunk, comparator_row.lowerBound, comparator_col) := False
  mainBoundary(outer_fiber, t, chunk, comparator_row, comparator_col.lowerBound) := False
  mainBoundary(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := True
  mainBoundary(outer_fiber, t, chunk.as(0), comparator_row, comparator_col) := !now(mainComparison) && mainComparison(outer_fiber, t, chunk, comparator_row, comparator_col-1) ||
    now(mainComparison) && !mainComparison(outer_fiber, t, chunk, comparator_row-1, comparator_col)
  now(mainBoundary) := mainBoundary(outer_fiber, t, chunk-1, comparator_row, comparator_col)

  for ((mainChunk, mainChunkAcross, chunkId) <- Seq((mainChunkA, mainChunkAcrossA, comparator_col), (mainChunkB, mainChunkAcrossB, comparator_row))) {
    mainChunkAcross(outer_fiber, t, chunk, comparator_row.lowerBound, comparator_col) := MaxVal

    mainChunkAcross(outer_fiber, t, chunk.as(0), comparator_row, comparator_col.upperBound-1) := MaxVal
    mainChunkAcross(outer_fiber, t, chunk.as(0), comparator_row, comparator_col) := Select(now(mainBoundary), chunkId, mainChunkAcross(outer_fiber, t, chunk, comparator_row-1, comparator_col+1))

    now(mainChunkAcross) := mainChunkAcross(outer_fiber, t, chunk-1, comparator_row, comparator_col)

    for (chunk_ <- 0 until nChunks) {
      val (comparator_row_to_check, comparator_col_to_check) = if (chunk_ < nChunksPerDim) (chunk_, 0) else (nChunksPerDim-1, chunk_ - nChunksPerDim + 1)
      mainChunk(outer_fiber, t, chunk.as(chunk_), comparator_row.as(0), comparator_col.as(0)) := mainChunkAcross(outer_fiber, t, chunk, comparator_row.as(comparator_row_to_check), comparator_col.as(comparator_col_to_check))
      mainChunk(outer_fiber, t, chunk.as(chunk_), comparator_row.as(0), comparator_col) := mainChunk(outer_fiber, t, chunk, comparator_row, comparator_col-1)
      mainChunk(outer_fiber, t, chunk.as(chunk_), comparator_row, comparator_col) := mainChunk(outer_fiber, t, chunk, comparator_row-1, comparator_col)
    }
  }

  now(mainChunkChosen) := now(mainChunkA) < MaxVal && now(mainChunkB) < MaxVal && outerCoordIsValidOpt.map(now).getOrElse(True)

  for (((chunkInCoord, mainInCoord, mainChunk, it, offset), inner_fiber) <- Seq((chunkInCoordA, mainInCoordA, mainChunkA, itA, comparator_col), (chunkInCoordB, mainInCoordB, mainChunkB, itB, comparator_row)).zipWithIndex) {
    val chosenChunk = Select(now(mainChunkChosen), now(mainChunk), 0)
    val inner_coord_coords = Seq(prev(pair) * 2 + inner_fiber) ++ outerCoordOpt.map(now).toSeq ++ Seq(it + chosenChunk * throughputPerDimPerChunk + offset)

    val padded_indices = indices.toSeq.map(ind => if (ind == offset) ind.upperBound-1 else ind)
    val base_indices = Seq(outer_fiber, t, chunk) ++ Seq(comparator_row, comparator_col).map(ind => if (ind != offset) ind.as(0) else ind)
    val propagating_indices = Seq(outer_fiber, t, chunk) ++ Seq(comparator_row, comparator_col).map(ind => if (ind != offset) ind-1 else ind)

    chunkInCoord(padded_indices:_*) := MaxVal
    chunkInCoord(base_indices:_*) := (if (throughputPerDimPerChunk * nChunksPerDim == 1) now(mainInCoord) else InnerCoord(inner_coord_coords:_*))
    now(chunkInCoord) := chunkInCoord(propagating_indices:_*)
  }

  chunkComparison(outer_fiber, t, chunk, comparator_row.lowerBound, comparator_col) := False
  chunkComparison(outer_fiber, t, chunk, comparator_row, comparator_col.lowerBound) := True
  now(chunkComparison) := now(chunkInCoordA) < now(chunkInCoordB)

  chunkEqualsOpt.foreach { chunkEquals =>
    setSpatialLowerBounds(chunkEquals, 0)
    now(chunkEquals) := now(chunkInCoordA) === now(chunkInCoordB)
  }

  chunkBoundary(outer_fiber, t, chunk, comparator_row, comparator_col.lowerBound) := False
  chunkBoundary(outer_fiber, t, chunk, comparator_row.as(0), comparator_col.as(0)) := True
  chunkBoundary(outer_fiber, t, chunk, comparator_row.upperBound-1, comparator_col.upperBound-1) := False
  now(chunkBoundary) := !now(chunkComparison) && chunkComparison(outer_fiber, t, chunk, comparator_row, comparator_col-1) ||
    now(chunkComparison) && !chunkComparison(outer_fiber, t, chunk, comparator_row-1, comparator_col) &&
      chunkEqualsOpt.map(chunkEquals => !chunkEquals(outer_fiber, t, chunk, comparator_row-1, comparator_col)).getOrElse(True)

  now(sortedCoord) := Select(now(chunkComparison) =/= 0, now(chunkInCoordA), now(chunkInCoordB))

  chunkMin(outer_fiber, t, chunk.as(0), comparator_row, comparator_col) := 0
  chunkMin(outer_fiber, t, chunk, comparator_row.as(0), comparator_col.as(0)) := Select(mainChunkA(outer_fiber, t, chunk-1, comparator_row, comparator_col) =/= now(mainChunkA),
    chunkInCoordA(outer_fiber, t, chunk, comparator_row, comparator_col.as(0)),
    chunkInCoordB(outer_fiber, t, chunk, comparator_row.as(0), comparator_col))
  chunkMin(outer_fiber, t, chunk, comparator_row.as(0), comparator_col) := chunkMin(outer_fiber, t, chunk, comparator_row, comparator_col-1)
  chunkMin(outer_fiber, t, chunk, comparator_row, comparator_col) := chunkMin(outer_fiber, t, chunk, comparator_row-1, comparator_col)

  chunkMax(outer_fiber, t, chunk.upperBound-1, comparator_row, comparator_col) := MaxVal
  now(chunkMax) := Select(mainChunkChosen(outer_fiber, t, chunk+1, comparator_row, comparator_col), chunkMin(outer_fiber, t, chunk+1, comparator_row, comparator_col), MaxVal)

  val validWithoutRunningPops = now(sortedCoord) >= now(chunkMin) && now(sortedCoord) < now(chunkMax) && now(chunkBoundary) && now(mainChunkChosen) && !inputs_stalling()
  now(valid) := validWithoutRunningPops && now(runningPopsEither) < nChunksPerDim * throughputPerDimPerChunk

  now(popA) := {
    var result: BoolExpr = now(valid) && now(chunkComparison)
    if (popDuplicates) {
      result ||= now(popB) && now(chunkEqualsOpt.get)
    }
    result
  }
  now(popB) := now(valid) && !now(chunkComparison)
  now(popEither) := validWithoutRunningPops
  now(popMerged) := now(valid)

  for ((diagPop, pop) <- Seq((diagPopA, popA), (diagPopB, popB), (diagPopEither, popEither), (diagPopMerged, popMerged))) {
    setSpatialLowerBounds(diagPop, 0)
    diagPop(outer_fiber, t, chunk, comparator_row, comparator_col.upperBound-1) := now(pop)
    now(diagPop) := now(pop) || diagPop(outer_fiber, t, chunk, comparator_row-1, comparator_col+1)
  }

  for ((runningPops, diagPop) <- Seq((runningPopsA, diagPopA), (runningPopsB, diagPopB), (runningPopsEither, diagPopEither), (runningPopsMerged, diagPopMerged))) {
    setSpatialLowerBounds(runningPops, 0)

    for (diag <- 0 to throughputPerDimPerChunk * 2) {
      val (comparator_row_to_check, comparator_col_to_check) = if (diag <= throughputPerDimPerChunk) (diag, 0) else (throughputPerDimPerChunk, diag - throughputPerDimPerChunk)
      val (prev_chunk_to_check, prev_row_to_check, prev_col_to_check) = if (diag == 0) (chunk-1, throughputPerDimPerChunk, throughputPerDimPerChunk) else if (diag <= throughputPerDimPerChunk) (chunk, comparator_row_to_check-1, comparator_col_to_check) else (chunk, comparator_row_to_check, comparator_col_to_check-1)

      runningPops(outer_fiber, t, chunk, comparator_row.as(comparator_row_to_check), comparator_col.as(comparator_col_to_check)) :=
        diagPop(outer_fiber, t, prev_chunk_to_check, comparator_row.as(prev_row_to_check), comparator_col.as(prev_col_to_check)) +
          runningPops(outer_fiber, t, prev_chunk_to_check, comparator_row.as(prev_row_to_check), comparator_col.as(prev_col_to_check))
    }

    runningPops(outer_fiber, t, chunk, comparator_row.upperBound-1, comparator_col.upperBound-1) := runningPops(outer_fiber, t, chunk, comparator_row, comparator_col-1)
    runningPops(outer_fiber, t, chunk, comparator_row, comparator_col) := runningPops(outer_fiber, t, chunk, comparator_row+1, comparator_col-1)
  }

  for ((totalPop, runningPops) <- Seq((totalPopA, runningPopsA), (totalPopB, runningPopsB), (totalPopMerged, runningPopsMerged))) {
    totalPop(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := 0
    totalPop(outer_fiber, t, chunk.upperBound-1, comparator_row.upperBound-1, comparator_col.upperBound-1) := now(runningPops)
    totalPop(outer_fiber, t, chunk.upperBound-1, comparator_row.upperBound-1, comparator_col) := totalPop(outer_fiber, t, chunk, comparator_row, comparator_col+1)
    totalPop(outer_fiber, t, chunk.upperBound-1, comparator_row, comparator_col) := totalPop(outer_fiber, t, chunk, comparator_row+1, comparator_col)
    totalPop(outer_fiber, t, chunk, comparator_row, comparator_col) := totalPop(outer_fiber, t, chunk+1, comparator_row, comparator_col)
  }

  val isLast = now(lastForPair) && !outerCoordIsValidOpt.map(now).getOrElse(False) && !pairIsRunning.map(x => now(x).asBool).reduce(_ || _) && !inputs_stalling()
  last(outer_fiber, t, chunk.upperBound-1, comparator_row.upperBound-1, comparator_col.upperBound-1) := isLast
  now(last) := False

  import SpArchMerger.{poppedAIndex, poppedBIndex, removePoppedDenom}
  // Seq(poppedAIndex, poppedBIndex, removePoppedDenom).foreach(x => require(x > 4e6 && chisel3.util.isPow2(x), "not big enough or not pow-2"))

  val outerCoordItOpt = outerCoordItsOpt.map { its =>
    val it = new Intermediate(name=its.head.name.filterNot(_.isDigit))
    it(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := 0
    setSpatialLowerBounds(it, 0)
    it(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := forPair(its)
    broadCast(it)
    now(it)
  }

  val sorted_inner_coord_outer_coord = (if (expandOuterCoord) outerCoordOpt.map(now) else outerCoordItOpt)
  val sorted_inner_coord_coords = Seq(prev(pair)) ++ sorted_inner_coord_outer_coord.toSeq ++ Seq(itMerged + now(runningPopsEither))
  val sorted_inner_coord_inter_coords = Seq(outer_fiber, t, chunk) ++ Seq(comparator_row, comparator_col).map(ind => if (throughputPerDimPerChunk * nChunksPerDim == 1) /* We want to avoid generating output ports along the padded dimensions for mergers with a throughput of just 1 */ ind.as(0) else ind)
  val sortedOutput = new Intermediate(name="sortedOutput")
  now(sortedOutput) := (if (isDummy) Const(0) else {
    var result: Expr = now(sortedCoord)
    if (outputWhichFiberPops) {
      result += now(popA) * poppedAIndex + now(popB) * poppedBIndex
    }
    result
  })
  SortedInnerCoord(sorted_inner_coord_coords:_*) := sortedOutput(sorted_inner_coord_inter_coords:_*)
  SortedInnerCoord.setValidCond(valid)

  val last_in_axis = new Intermediate(name="last_in_axis")
  now(last_in_axis) := now(lastForPair) && !outerCoordIsValidOpt.map(now).getOrElse(False) && !inputs_stalling()
  SortedInnerCoord.setLastInAxis(last_in_axis)
  if (nOuterFibers > 1) {
    val increment_outer_axis = new Intermediate(name="increment_outer_axis")
    increment_outer_axis(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := False
    now(increment_outer_axis) := now(lastForPair) && !inputs_stalling()
    SortedInnerCoord.setLastInAxis(increment_outer_axis, 1)
  }

  val ACoordEquals = new Intermediate(name="ACoordEquals") // This is used to specify when we want to pop (or even just read) A
  val BCoordEquals = new Intermediate(name="BCoordEquals") // This is used to specify when we want to pop (or even just read) B
  now(ACoordEquals) := prev(pair) * 2
  now(BCoordEquals) := prev(pair) * 2 + 1

  if (nOuterFibers > 1) {
    val outerCoordIts = outerCoordItsOpt.get
    val outerCoord = outerCoordOpt.get
    val outerCoordIsValid = outerCoordIsValidOpt.get
    val outerCoordFound = outerCoordFoundOpt.get
    val popOuterCoord = popOuterCoordOpt.get

    val outerCoordFull = new Intermediate(name=s"outerCoordFull")
    outerCoordFull(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := OuterCoordIn(prev(pair), outerCoordItOpt.get)
    broadCast(outerCoordFull)

    outerCoordFound(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := OuterCoordIn(prev(pair), outerCoordItOpt.get).found
    broadCast(outerCoordFound)

    now(outerCoord) := {
      var result: Expr = now(outerCoordFull)
      if (checkWhichFiberPopped)
        result %= removePoppedDenom
      Select(now(outerCoordIsValid), result, 0)
    }

    outerCoordIts.zipWithIndex.foreach { case (outerCoordIt, pairId) =>
      outerCoordIt(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := outer_fiber
      outerCoordIt(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := prev(outerCoordIt) + Select(now(popOuterCoord).asBool && prev(pair) === pairId, nOuterFibers, 0)
      broadCast(outerCoordIt)
    }

    popOuterCoord(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := False
    now(popOuterCoord) := !stalling() && now(lastForPair)
    OuterCoordIn.setPopCond(popOuterCoord)

    now(outerCoordIsValid) := now(outerCoordFull) < MaxVal

    InnerCoord.setValidCond(outerCoordIsValid)
    if (checkWhichFiberPopped) {
      val outerCoordIsValidForA = outerCoordIsValidForAOpt.get
      val outerCoordIsValidForB = outerCoordIsValidForBOpt.get
      now(outerCoordIsValidForA) := (now(outerCoordFull) / poppedAIndex) % 2
      now(outerCoordIsValidForB) := (now(outerCoordFull) / poppedBIndex) % 2
      InnerCoord.setValidCond(outerCoordIsValidForA, Some(ACoordEquals))
      InnerCoord.setValidCond(outerCoordIsValidForB, Some(BCoordEquals))
    }
  }

  if (shouldPop) {
    val popACond = new Intermediate(name="popACond")
    val popBCond = new Intermediate(name="popBCond")

    popACond(outer_fiber, t, chunk, comparator_row.as(0), comparator_col) := (0 to throughputPerDimPerChunk).map(off => popA(outer_fiber, t, chunk, comparator_row.as(off), comparator_col).asBool).reduce(_ || _)
    popBCond(outer_fiber, t, chunk, comparator_row, comparator_col.as(0)) := (0 to throughputPerDimPerChunk).map(off => popB(outer_fiber, t, chunk, comparator_row, comparator_col.as(off)).asBool).reduce(_ || _)
    now(popACond) := False
    now(popBCond) := False

    InnerCoord.setPopCond(popACond, dstInterVar = Option.unless(throughputPerDimPerChunk * nChunksPerDim == 1)(chunkInCoordA), coord0MustEqual = Some(ACoordEquals))
    InnerCoord.setPopCond(popBCond, dstInterVar = Option.unless(throughputPerDimPerChunk * nChunksPerDim == 1)(chunkInCoordB), coord0MustEqual = Some(BCoordEquals))
  }

  outs_stalling(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := False
  now(outs_stalling) := OutputPortsStalling

  elemsPerFiberInRf.zipWithIndex.foreach { case (elemsPerFiber, pairId) =>
    elemsPerFiber(outer_fiber, t.lowerBound, chunk, comparator_row, comparator_col) := 0
    elemsPerFiber(outer_fiber, t, chunk.as(0), comparator_row.as(0), comparator_col.as(0)) := ElemsPerFiberOpt.get((pairId +: Seq.fill(if (nOuterFibers > 1) 2 else 1)(0)).map(Const):_*)
    broadCast(elemsPerFiber)
  }

  Transform(
    /* out_fiber, t, chunk, comparator_row, comparator_col */
    1, 0, 0, 0, 0,
    0, 0, 1, 0, 0,
    0, 0, 0, 1, 0,
    0, 0, 0, 0, 1,
    0, 1, 0, 0, 0,
  )
}

class SparseDenseMatmul(isLoadBalanced: Boolean = false, size: Int = 2, maxJOpt: Option[Int] = None,
                        hasSubArrays: Boolean = false, shouldPopA: Boolean = true,
                       ) extends SpatialArray(size=size, boundsOverride = Some(Seq(size, maxJOpt.getOrElse(size), size)), hasSubArrays = hasSubArrays) {
  val i = new Index(name="i")
  val j = new Index(name="j", isUnbounded = true)
  val k = new Index(name="k")

  val A = new Input(name = "A")
  val B = new Input(name = "B")
  val C = new Output(name = "C")

  val a = new Intermediate(name="a")
  val b = new Intermediate(name="b")
  val c = new Intermediate(name="c")

  val bj = new Intermediate(name="bj")

  val valid = new Intermediate(name="valid")

  val popA = new Intermediate(name="popA")
  val popB = new Intermediate(name="popB")

  val lastAcross = new Intermediate(name="lastAcross")
  val last = new Intermediate(signalsEnding=true, name="last")

  // Inputs
  a(i, j.lowerBound, k) := A(i, k)
  a(i, j.as(0), k) := A(i, k)
  b(i.lowerBound, j, k) := B(k, j)
  b(i.as(0), j, k) := B(k, j)
  c(i, j, k.lowerBound) := 0

  bj(i.lowerBound, j, k) := B(k, j).coord(1)
  bj(i.as(0), j, k) := B(k, j).coord(1)

  // Intermediate calculations
  a(i, j, k) := a(i, j-1, k)
  b(i, j, k) := b(i-1, j, k)

  c(i, j, k) := a(i, j-1, k) * b(i-1, j, k)

  bj(i, j, k) := bj(i-1, j, k)

  // Outputs
  C(i, j, k) := c(i, j, k)

  valid(i, j, k) := bj(i-1, j, k) < MaxVal
  C.setValidCond(valid)

  popA(i, j.lowerBound, k) := True
  popA(i, j, k) := True
  if (shouldPopA)
    A.setPopCond(popA)

  popB(i.lowerBound, j, k) := True
  popB(i, j, k) := True
  B.setPopCond(popB)

  lastAcross(i.lowerBound, j, k) := True
  lastAcross(i, j, k.lowerBound) := True
  if (!hasSubArrays) {
    lastAcross(i.upperBound - 1, j, k) := lastAcross(i.upperBound - 1, j, k - 1) && lastAcross(i.upperBound - 2, j, k) && !valid(i.upperBound - 1, j, k)
  }
  lastAcross(i, j, k) := lastAcross(i - 1, j, k) && !valid(i, j, k)

  val lastk = if (hasSubArrays) k else (k.upperBound - 1)
  last(i.upperBound - 1, j, lastk) := lastAcross(i.upperBound - 1, j, lastk)
  last(i, j, k) := False

  // Space-time transform
  Transform(
    1, 0, 0,
    0, 0, 1,
    1, 1, 0,
  )

  // Sparsity
  Skip(j) where (B(k,j) === 0)

  // Load-balancing
  if (isLoadBalanced) {
    Map(i, (j + size) -> , k) to (i, j -> (j + size), k+1) sweep(k, max = size-1) step(j, size) sweep(j, size)
    Map(i, (j + size) -> , k) to (i, j -> (j + size), k) sweep(k, max = size-1) step(j, size) sweep(j, size)
  }
}


class SpDenseMMAccelerator(iTiles: Int, jTiles: Int, kTiles: Int, isLoadBalanced: Boolean,
                           val matmulDimSize: Int = 2, sramAElemsOpt: Option[Int] = None,
                           sramBElemsOpt: Option[Int] = None, sramCElemsOpt: Option[Int] = None
                          ) extends Accelerator(withAsserts = matmulDimSize <= 2) {

  import chisel3._

  val maxOutputCols = matmulDimSize * matmulDimSize // these are the output-cols per matmul
  val maxTotalOutputCols = jTiles * matmulDimSize * matmulDimSize // these are the total number of output-cols in the accumulator SRAM
  val nAxes = 5

  // Spatial arrays
  val matmul = MakeSpatialArray(new SparseDenseMatmul(isLoadBalanced = isLoadBalanced, size = matmulDimSize,
    maxJOpt = if (isLoadBalanced) Some(matmulDimSize * jTiles) else None,
    hasSubArrays = true, shouldPopA = !isLoadBalanced))

  val merger = MakeSpatialArray(new CsrMatrixSorter(size = matmulDimSize))

  val adder = MakeSpatialArray(new CsrMatrixAdder(size = matmulDimSize))

  // SRAMs
  val sramA = new SRAM(elemT = SInt(32.W), nElems = sramAElemsOpt.getOrElse(iTiles * matmulDimSize * kTiles * matmulDimSize),
    elemsPerRead = matmulDimSize, elemsPerWrite = matmulDimSize, axes = Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas = Seq.fill(nAxes)(DenseMetadata))

  val nSramBElems = sramBElemsOpt.getOrElse(kTiles * matmulDimSize * jTiles * matmulDimSize).max(2048)
  val nSramBOuter = (kTiles * matmulDimSize * jTiles + 1).max(256)
  val sramB = new SRAM(elemT = SInt(32.W), nElems = nSramBElems, elemsPerRead = matmulDimSize, elemsPerWrite = matmulDimSize,
    axes = FiberTreeAxis.Compressed +: Seq.fill(nAxes - 1)(FiberTreeAxis.Dense),
    metadatas = CompressedMetadata(nOuter = nSramBOuter, nInnerOpt = CompressedMetadata.InnerBuffer(Some({
      // TODO for some reason, we have to divide by 2 here to get Chipyard to compile the SRAM. I think it sometimes fails to deduplicate identical SRAMs?
      nSramBElems / 2
    }))) +: Seq.fill(nAxes - 1)(DenseMetadata),
    branchSizes = Seq(matmulDimSize),
  )

  val sramCNodeSize = 16
  val nSramCElems = {
    // TODO the '.max(1024) * 4' is just meant to get around the fact that Chipyard can't elaborate very small SRAMs
    sramCElemsOpt.getOrElse(iTiles * matmulDimSize * maxTotalOutputCols.max(sramCNodeSize)).max(1024) * 4 * {
      if (isLoadBalanced) 2 else 1 // TODO This multiplication is just caused the need to get around some Chisel SRAM de-duplication errors
    }
  }
  val nSramCNodes = (nSramCElems + sramCNodeSize - 1) / sramCNodeSize
  val nSramCHeads = (iTiles * matmulDimSize).max(256)
  val sramC = new SRAM(elemT = SInt(32.W), nElems = nSramCElems, elemsPerRead = matmulDimSize, elemsPerWrite = matmulDimSize,
    axes = FiberTreeAxis.LinkedList +: Seq.fill(nAxes - 1)(FiberTreeAxis.Dense),
    metadatas = LinkedListMetadata(nHeads = nSramCHeads, nNodes = nSramCNodes, nodeSize = 16) +: Seq.fill(nAxes - 1)(DenseMetadata),
    branchSizes = Seq(matmulDimSize))

  // Regfiles
  val regA = new RegFile(nElems = matmulDimSize * matmulDimSize, nUpdatePorts = if (isLoadBalanced) matmulDimSize else 0, nIOCoords = 2, nDomainCoords = 3, nameOpt=Some("regA"))
  val regB = new RegFile(nElems = matmulDimSize * matmulDimSize * (if (isLoadBalanced) 2 else 1), nUpdatePorts = if (isLoadBalanced) matmulDimSize else 0, nIOCoords = 2, nDomainCoords = 3, lastInAxisFlags = Seq((matmulDimSize, 0)), nameOpt=Some("regB"))
  val regScatteredC = new RegFile(nElems = matmulDimSize * matmulDimSize * matmulDimSize * (if (isLoadBalanced) 2 else 1), nIOCoords = 3, nDomainCoords = 3, nameOpt = Some("regScatteredC"))
  val regMergedC = new RegFile(nElems = matmulDimSize * maxOutputCols, nIOCoords = 2, nDomainCoords = 3, nameOpt=Some("regMergedC"))
  val regD = new RegFile(nElems = matmulDimSize * matmulDimSize, nIOCoords = 2, nDomainCoords = 2, nameOpt=Some("regD"))
  val regE = new RegFile(nElems = matmulDimSize * matmulDimSize, nIOCoords = 2, nDomainCoords = 2, nameOpt=Some("regE"))

  // Coord-Lookups
  val coordLookupB = new CoordLookup(nElems = regB.nElems * 2, nUpdatePorts = if (isLoadBalanced) matmulDimSize else 0, nCoords = 2, lastInAxisFlags = Seq((matmulDimSize, 1)), nSubArrays=matmulDimSize, subPipelineOpt=Some((matmulDimSize,1,false,false)), name = Some("coordLookupB"))
  val coordLookupC = new CoordLookup(nElems = regMergedC.nElems, nCoords = 2, name = Some("coordLookupC"))
  val coordLookupD = new CoordLookup(nElems = regD.nElems, nCoords = 2, name = Some("coordLookupD"))
  val coordLookupE = new CoordLookup(nElems = regE.nElems, nCoords = 2, name = Some("coordLookupE"))

  // Load-Balancers
  val loadBalancer = Option.when(isLoadBalanced)(new LoadBalancer(
    matmul, nAxes = nAxes - 2,
    nOpCounts = 2,
    cl_in_innermost_axes = Seq((matmul.k, matmul.j)).toMap,
    nonWaitingInVars = Set(matmul.B), instantUpdateIndices = Set(matmul.j),
    loadbalancingAcrossOpCounts = false,
  ))

  // Connections between spatial arrays and regfiles
  connectVarToRegFile(matmul.A, regA)
  connectVarToRegFile(matmul.B, regB)
  connectVarToRegFile(matmul.C, regScatteredC)
  connectVarToRegFile(merger.ScatteredMatrix, regScatteredC)
  connectVarToRegFile(merger.MergedMatrix, regMergedC)
  connectVarToRegFile(adder.Adata, regMergedC)
  connectVarToRegFile(adder.Acoords, regMergedC, CoordConn(1))
  connectVarToRegFile(adder.Bdata, regD)
  connectVarToRegFile(adder.Bcoords, regD, CoordConn(1))
  connectVarToRegFile(adder.C, regE)

  // Connections between srams and regfiles
  connectSRAMtoRegFile(sramA, regA)
  connectSRAMtoRegFile(sramB, regB)
  connectSRAMtoRegFile(sramC, regD)
  connectRegFileToSRAM(regE, sramC)

  // Coord-lookup fills
  fillCoordLookupFromSram(sramB, coordLookupB)
  fillCoordLookupFromOutput(merger.MergedMatrix, coordLookupC, Seq(merger.i, merger.j))
  fillCoordLookupFromSram(sramC, coordLookupD)
  fillCoordLookupFromOutput(adder.C, coordLookupE, Seq(adder.i, adder.j))

  // Coord-lookup lookups
  connectIndex2CoordLookup(coordLookupB,
    indicesToInputIntoCoordLookup = Seq((matmul.j, false), (matmul.k, false)),
    indicesToInputIntoSpatialArray = Seq((matmul.j, 0)),
  )

  connectIo2CoordLookup(merger.ScatteredMatrix, coordLookupB,
    compressedCoords=Seq(
      (1, 2, true), // k
      (0, 1, true)), // j
    expandedCoords=Seq((0, 1)), // j
  )

  for (inVar <- Seq(adder.Adata, adder.Acoords)) {
    connectIo2CoordLookup(inVar, coordLookupC,
      compressedCoords=Seq(
        (0, 0, true), // i
        (1, 1, true), // j
      ),
      expandedCoords=Seq((1, 1)), // j
    )
  }

  for (inVar <- Seq(adder.Bdata, adder.Bcoords)) {
    connectIo2CoordLookup(inVar, coordLookupD,
      compressedCoords=Seq(
        (1, 0, true), // i
        (0, 1, true), // j
      ),
      expandedCoords=Seq((0, 1)), // j
    )
  }

  connectSram2CoordLookup(sramC, coordLookupE,
    compressedCoords=Seq((0, 1, true), (1, 0, true)),
    expandedCoords=Seq((1, 0)),
  )

  // Coord-lookup empties
  /* TODO Based on the exact space-time transform chosen, it's possible that these aren't actually the last
     "emptying" variables. We need to be able to specify MULTIPLE variables here, and have Stellar pick the "last"
     one of them */
  emptyCoordLookupWithVariable(coordLookupB, merger.ScatteredMatrix)
  emptyCoordLookupWithVariable(coordLookupC, adder.Adata)
  emptyCoordLookupWithVariable(coordLookupD, adder.Bdata)
}

class CsrMatrixAdderAccelerator extends Accelerator {
  import chisel3._

  val size = 2
  val nAxes = 2

  val Acols = 16
  val Bcols = 256

  val adder = new CsrMatrixAdder(size = size, maxAJ = Acols, maxBJ = Bcols, isVector = true)
  registerSpatialArray(adder)

  val regA = new RegFile(nElems=size, nIOCoords=1, nDomainCoords=1)
  val regB = new RegFile(nElems=size*2, nIOCoords=1, nDomainCoords=1)
  val regC = new RegFile(nElems=size,  nIOCoords=1, nDomainCoords=1)

  val nSramElems = 1024*1024

  val sramA = new SRAM(elemT=SInt(32.W), nElems=nSramElems, elemsPerRead=size, elemsPerWrite=size,
    axes=FiberTreeAxis.Compressed +: Seq.fill(nAxes-1)(FiberTreeAxis.Dense),
    metadatas=CompressedMetadata(nOuter=nSramElems, nInnerOpt=CompressedMetadata.InnerBuffer(Some(nSramElems))) +: Seq.fill(nAxes-1)(DenseMetadata)) // TODO we don't need nearly this much metadata storage
  val sramC = new SRAM(elemT=SInt(32.W), nElems=nSramElems, elemsPerRead=size, elemsPerWrite=size,
    axes=FiberTreeAxis.LinkedList +: Seq.fill(nAxes-1)(FiberTreeAxis.Dense),
    metadatas=LinkedListMetadata(nHeads=nSramElems, nNodes=nSramElems, nodeSize=16) +: Seq.fill(nAxes-1)(DenseMetadata)) // TODO we don't need nearly this much metadata storage

  val coordLookupA = new CoordLookup(nElems=size, nCoords=1)
  val coordLookupB = new CoordLookup(nElems=size*2, nCoords=1)
  val coordLookupC = new CoordLookup(nElems=size, nCoords=1)

  connectVarToRegFile(adder.Adata, regA)
  connectVarToRegFile(adder.Acoords, regA, CoordConn(0))
  connectVarToRegFile(adder.Bdata, regB)
  connectVarToRegFile(adder.Bcoords, regB, CoordConn(0))
  connectVarToRegFile(adder.C, regC)

  connectSRAMtoRegFile(sramA, regA)
  connectSRAMtoRegFile(sramC, regB)
  connectRegFileToSRAM(regC, sramC)

  fillCoordLookupFromSram(sramA, coordLookupA)
  fillCoordLookupFromSram(sramC, coordLookupB)
  fillCoordLookupFromOutput(adder.C, coordLookupC, Seq(adder.j))

  for ((inVars, coordLookup) <- Seq((Seq(adder.Adata, adder.Acoords), coordLookupA), (Seq(adder.Bdata, adder.Bcoords), coordLookupB))) {
    for (inVar <- inVars) {
      connectIo2CoordLookup(inVar, coordLookup,
        compressedCoords=Seq(
          (0, 0, true), // j
        ),
        expandedCoords=Seq((0, 0)),
      )
    }
  }

  connectSram2CoordLookup(sramC, coordLookupC,
    compressedCoords=Seq((0, 0, true)),
    expandedCoords=Seq((0, 0)),
  )

  emptyCoordLookupWithVariable(coordLookupA, adder.Adata)
  emptyCoordLookupWithVariable(coordLookupB, adder.Bdata)
}

class OuterSpace(size: Int = 2, hasMatmul: Boolean = true, hasMerger: Boolean = true, leaveOutCoordLookupA: Boolean = true, leaveOutCoordLookupB: Boolean = true, leaveOutMergerCoordLookups: Boolean = true, withAsserts: Boolean = true) extends Accelerator(withAsserts = withAsserts) {
  import chisel3._
  import RfEntryExitOption._

  val nOuterAxes = 1
  val nAxes = 4

  val nMaxI = if (size == 2) 16 else 256
  val nMaxK = if (size == 2) 16 else 256
  val nMaxJ = if (size == 2) 16 else 256

  val elemT = SInt(32.W)

  // Spatial arrays
  val matmulOpt = Option.when(hasMatmul)(MakeSpatialArray(new OuterMatmul(isLoadBalanced = false, size = size, travellingCoords = true, fewerInPorts = true, skipA = true, useCompressedCoordsForB = leaveOutCoordLookupB, alwaysStart = true, outputDelay = 4, onlyCheckIfOutputsAreReady = true, isDummy = false)))
  val mergerOpt = Option.when(hasMerger)(MakeSpatialArray(new CsrMatrixSorter(size = size, hasBias = false)))

  def matmul = matmulOpt.get
  def merger = mergerOpt.get

  // Regfiles and coord-lookups
  val flex_space_cl_b = if (hasMatmul) {
    // TODO Ideally, we wouldn't need this flex space in the coord-lookup because we wouldn't need to lookup the same entries out of it multiple times
    // Note: In theory, this flex-space should only apply to CoordLookupB. However, the 'maxElemsInRf' option for SramB
    //   can cause issues for small reg-files if RegB.size =/= CoordLookupB.size. Therefore, we apply this flex-space to
    //   both
    val matmul = matmulOpt.get
    require(matmul.block.transform.get.nTimeAxes == 1)
    size * (size-1) * matmul.block.transform.get.timeTr.head.head + // The extra elems that need to be stored for the coordLookup entries that are re-used at the beginning and end of the spatial array rows
      size // The extra elems that need to be stored so that the SRAM can do it's coord-lookups while the spatial arrays continue executing
  } else 0

  val sramScatteredC_read_multiplier = 2 // This just helps with perf later on in the sramScatteredC module

  val regAOpt = Option.when(hasMatmul)(new RegFile(nElems = size * size, nIOCoords = 2, nDomainCoords = 3, nameOpt = Some("regA"), automaticallyOptimize = true))
  val regBOpt = if (hasMatmul) {
    var nElems = size * size + flex_space_cl_b
    while ((nElems / size) % size != 0) nElems += 1
    Some(new RegFile(nElems = nElems, nIOCoords = 2, nDomainCoords = 3, domainCoordsToUseForOutputs = (if (leaveOutCoordLookupB) Seq(1 -> 0) else Seq.empty).toMap, nameOpt = Some("regB"), automaticallyOptimize = true))
  } else None
  val regScattered_MatmulOutputOpt = Option.when(hasMatmul)(new RegFile(nElems = size * size * size, nIOCoords = 3, nDomainCoords = 3, domainCoordsToUseForOutputs = Seq(0 -> 0).toMap, nameOpt = Some("regScattered_MatmulOutput"), automaticallyOptimize = true)

  val regScattered_MergerInputOpt = Option.when(hasMerger)(new RegFile(nElems = size * size * size * sramScatteredC_read_multiplier, nIOCoords = 3, nDomainCoords = 3, dontCheckExtraLastInAxisFlag = true, nameOpt = Some("regScattered_MergerInput"), automaticallyOptimize = true))
  val regMergedOpt = Option.when(hasMerger)(new RegFile(nElems = size * 2, nIOCoords = 2, nDomainCoords = 3, nameOpt = Some("regMerged"), getTravellingOpCountFromInPorts = true, automaticallyOptimize = true))

  def regA = regAOpt.get
  def regB = regBOpt.get
  def regScattered_MatmulOutput = regScattered_MatmulOutputOpt.get
  def regScattered_MergerInput = regScattered_MergerInputOpt.get
  def regMerged = regMergedOpt.get

  // SRAMs
  def hardCodedValues(x: ChiselSRAMPipelineData[SInt]): SMap[Data, Data] = Seq(
    x.write_req.should_trail_reads -> false.B, x.read_req.axis -> 0.U,
    x.write_req.from_regfile_last_axis -> 1.U,
  ).toMap
  def hardCodedValuesAB(x: ChiselSRAMPipelineData[SInt]): SMap[Data, Data] = Seq(
    x.write_req.from_regfile_metadata -> VecInit.fill(x.write_req.from_regfile_metadata.size, x.write_req.from_regfile_metadata.head.size)((new stellar.rtl.ChiselSRAMWriteReq.FromRegfileMetadata).Lit(_.valid -> false.B)),
    x.read_req.to_regfile_last_axis -> 1.U, x.read_req.address(0) -> 0.U,
    x.read_req.interleave.should_push -> false.B, x.read_req.interleave.should_pop -> false.B,
    x.write_req.interleave.should_push -> false.B, x.write_req.interleave.should_pop -> false.B,
  ).toMap
  def hardCodedValuesScatteredC(x: ChiselSRAMPipelineData[SInt]): SMap[Data, Data] = Seq(
    x.write_req.from_regfile_metadata -> {
      val result = VecInit.fill(x.write_req.from_regfile_metadata.size, x.write_req.from_regfile_metadata.head.size)((new stellar.rtl.ChiselSRAMWriteReq.FromRegfileMetadata).Lit(_.valid -> false.B))
      result(0)(LinkedListMetadata.coord_buffer_id).valid := true.B
      result(0)(LinkedListMetadata.coord_buffer_id).coord := 1.U
      result
    },
    x.read_req.to_regfile_last_axis -> 2.U, x.read_req.address(0) -> 0.U,
    x.metadata_addrs(0)(LinkedListMetadata.coord_buffer_id) -> 0.U,
    x.data_addr -> 0.U,
    x.write_req.data_strides -> VecInit(Seq.fill(x.write_req.data_strides.size-1)(0.U) :+ 1.U),
    x.read_req.data_strides -> VecInit(Seq.fill(x.read_req.data_strides.size-1)(0.U) :+ 1.U),
    x.read_req.interleave.should_push -> false.B, x.read_req.interleave.should_pop -> false.B,
    x.write_req.interleave.should_push -> false.B, x.write_req.interleave.should_pop -> false.B,
    x.write_req.from_regfile_last_axis_log_size -> chisel3.util.log2Ceil(size).U,
  ).toMap ++ Seq(x.read_req.metadata_strides, x.write_req.metadata_strides).flatMap { metadata_strides =>
      metadata_strides.zipWithIndex.map { case (outerStrides, outerAxis) =>
        outerStrides.zipWithIndex.map { case (innerStrides, innerAxis) =>
          innerStrides.zipWithIndex.collect {
            case (stride, metadata_buffer_id) if (outerAxis == 3 && innerAxis == 1 && metadata_buffer_id == CompressedMetadata.inner_metadata_buffer_id) => stride -> size.U
            case (stride, metadata_buffer_id) if (outerAxis == 2 && innerAxis == 1 && metadata_buffer_id == CompressedMetadata.outer_metadata_buffer_id) => stride -> 1.U
            case (stride, metadata_buffer_id) if (outerAxis == 1 && innerAxis == 1 && metadata_buffer_id == CompressedMetadata.inner_metadata_buffer_id) => stride -> 1.U
            case (stride, metadata_buffer_id) if (outerAxis == 0 && innerAxis == 0 && metadata_buffer_id == LinkedListMetadata.coord_buffer_id) => stride -> 1.U
            case (stride, metadata_buffer_id) if (!(outerAxis == 1 && innerAxis == 1 && metadata_buffer_id == CompressedMetadata.outer_metadata_ends_buffer_id)) => stride -> 0.U
          }
        }
      }
    }.flatten.flatten.toMap

  val ll_node_size = size * 8 // making the ll-node-size large can make SRAM accesses faster by reducing the number of pointer lookups which may stall due to bank conflicts

  val sramAOpt = Option.when(hasMatmul)(new SRAM(elemT = elemT, nElems = nMaxI * nMaxK, elemsPerRead = size, elemsPerWrite = size,
    axes = FiberTreeAxis.Compressed +: Seq.fill(nAxes - 1)(FiberTreeAxis.Dense),
    metadatas = CompressedMetadata(nOuter = nMaxK * 2, CompressedMetadata.InnerBuffer(None, expandForRf = false), nonInfiniteSpan = Some(size)) +: Seq.fill(nAxes - 1)(DenseMetadata),
    branchSizes = Seq(size), elemsPerRowMultiplier = 4,
    hardCodedValues = hardCodedValuesAB, name = Some("sramA"),
  ))

  val sramBOpt = Option.when(hasMatmul)(new SRAM(elemT = elemT, nElems = nMaxK * nMaxJ, elemsPerRead = size, elemsPerWrite = size,
    axes = FiberTreeAxis.Compressed +: Seq.fill(nAxes - 1)(FiberTreeAxis.Dense),
    metadatas = CompressedMetadata(nOuter = nMaxK  * 2) +: Seq.fill(nAxes - 1)(DenseMetadata),
    branchSizes = Seq(size), maxElemsInRf = Some(regB.nElems / size), elemsPerRowMultiplier = 4,
    hardCodedValues = hardCodedValuesAB, name = Some("sramB"),
  ))

  val sramScatteredC = new SRAM(elemT = elemT, nElems = nMaxK.max(ll_node_size) * nMaxI.max(ll_node_size) * nMaxJ.max(ll_node_size),
    elemsPerRead = size * sramScatteredC_read_multiplier,
    elemsPerWrite = size, elemsPerWriteRf = if (hasMerger) 1 else -1,
    axes = Seq(FiberTreeAxis.LinkedList, FiberTreeAxis.Compressed, FiberTreeAxis.Dense) ++ Seq.fill(nAxes - 3)(FiberTreeAxis.Dense),
    metadatas = Seq(
      LinkedListMetadata(nMaxK.max(ll_node_size) * nMaxI.max(ll_node_size), (nMaxK.max(ll_node_size) * nMaxI.max(ll_node_size) * nMaxJ.max(ll_node_size)) / ll_node_size, ll_node_size, resetRunningLenAutomatically = true, headPtrIsHeadAddr = true, initialLenIs0ForRfWrites = true, nextNodeIsNullForRfWrites = true, decrementingFreeNodePointer = true, useAddrForSpanTraversed = hasMatmul, supportsImbalancedSpanTraversal = hasMerger),
        CompressedMetadata(nMaxK.max(nMaxI) * 2, CompressedMetadata.DontExpandInner(Some(nMaxK * nMaxI)), canReadFromStage = false, canReadReverse = hasMerger),
      DenseMetadata) ++
      Seq.fill(nAxes - 3)(DenseMetadata),
    branchSizes = Seq(size), writeBranchSizes = Option.when(hasMerger){assert(!hasMatmul); Seq(1)},
    nBanks = size,
    readBankingStrategies = (if (hasMatmul) sramC_matmul_read_banking_strategies else Seq.empty) ++ (if (hasMerger) sramC_merge_read_banking_strategies else Seq.empty),
    writeBankingStrategies = if (hasMerger) sramC_merge_write_banking_strategies else Seq.empty,
    maxElemsInRf = if (hasMerger) Some(regScattered_MergerInput.nElems / (size * size)) else None,
    elemsPerRowMultiplier = ll_node_size / size,
    hardCodedValues = if (!hasMerger) hardCodedValuesScatteredC else {_: ChiselSRAMPipelineData[SInt] => scala.collection.Map.empty[Data,Data]},
    name = Some(if (hasMatmul) "outerspace_sramScatteredC" else "outerspace_merge_sram"),
  )

  def sramA = sramAOpt.get
  def sramB = sramBOpt.get

  // Fill and empty regfiles and coord-lookups
  {
    // Matmul phase
    if (hasMatmul) {
      connectSRAMtoRegFile(sramA, regA, reorderCoords = Seq(1,0))

      connectSRAMtoRegFile(sramB, regB)

      connectVarToRegFile(matmul.A, regA)
      connectVarToRegFile(matmul.B, regB)
      connectVarToRegFile(matmul.C, regScattered_MatmulOutput)

      connectRegFileToSRAM(regScattered_MatmulOutput, sramScatteredC, reorderCoords = Seq(2, 0, 1))
    }

    // Merge phase
    if (hasMerger) {
      connectSRAMtoRegFile(sramScatteredC, regScattered_MergerInput, reorderCoords = Seq(1, 0))

      connectVarToRegFile(merger.ScatteredMatrix, regScattered_MergerInput)
      connectVarToRegFile(merger.MergedMatrix, regMerged)

      connectRegFileToSRAM(regMerged, sramScatteredC, reorderCoords = Seq(0, 2))
    }
  }

  // Post-processing
  (sramAOpt.toSeq ++ sramBOpt ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.use_running_state_for_metadata.foreach(_.foreach(_ := false.B)))
    mod.io.write.bits.use_running_state_for_metadata.foreach(_.foreach(_ := false.B))

    mod.io.read_reqs.foreach(_.bits.iteration_strides.foreach(_ := 1.U))
    mod.io.write.bits.iteration_strides.foreach(_ := 1.U)
  })

  if (!hasMerger)
    (sramBOpt.toSeq ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
      mod.io.read_reqs.foreach(_.bits.to_dma := false.B)
    })

  sramBOpt.foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.to_regfile := true.B)
  })

  (sramAOpt.toSeq ++ sramBOpt).foreach(_.postProcess += { mod =>
    mod.io.write.bits.from_regfile.foreach(_ := false.B)
  })

  Seq(sramScatteredC).foreach(_.postProcess += { mod =>
    mod.io.write.bits.from_regfile.tail.foreach(_ := false.B)
  })

  (sramAOpt.toSeq ++ sramBOpt).foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.to_regfile_last_axis := 1.U)
    mod.io.write.bits.from_regfile_last_axis := 1.U
  })

  Seq(sramScatteredC).foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.to_regfile_last_axis := (if (hasMerger) 1 else 2).U)
    mod.io.write.bits.from_regfile_last_axis := 1.U
    mod.io.write.bits.from_regfile_last_axis_log_size := chisel3.util.log2Ceil(size).U
    if (hasMerger) {
      mod.io.read_reqs.foreach(_.bits.to_regfile_last_axis_log_size := chisel3.util.log2Ceil(size).U)
      mod.io.write.bits.from_regfile_last_axis_log_size := 0.U
    }
  })

  if (!hasMerger)
    (sramAOpt.toSeq ++ sramBOpt ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
      mod.io.read_reqs.foreach(_.bits.interleave.should_pop := false.B)
      mod.io.write.bits.interleave.should_push := false.B
    })

  if (!hasMerger)
    (sramAOpt.toSeq ++ sramBOpt ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
      mod.io.read_reqs.foreach(_.bits.interleave.should_push := false.B)
      mod.io.write.bits.interleave.should_pop := false.B
      mod.io.write.bits.should_trail_reads := false.B
    })

  (sramAOpt.toSeq ++ sramBOpt ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.axis := 0.U)
  })

  (sramAOpt.toSeq ++ sramBOpt).foreach(_.postProcess += { mod =>
    mod.io.write.bits.axis := 0.U
  })

  (sramAOpt.toSeq ++ sramBOpt).foreach(_.postProcess += { mod =>
    mod.io.write.bits.address.drop(2).foreach(_ := 0.U)
    mod.io.read_reqs.foreach(_.bits.address.foreach(_ := 0.U))
  })

  Seq(sramScatteredC).foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.address.head := 0.U)

    mod.io.write.bits.address.drop(3).foreach(_ := 0.U)
    mod.io.read_reqs.foreach(_.bits.address.drop(3).foreach(_ := 0.U))
  })

  sramAOpt.foreach(_.postProcess += { mod =>
    mod.io.write.bits.spans(2) := 1.U
  })

  sramBOpt.foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.spans(0) := ChiselUtil.maxVal(mod.io.read_reqs.head.bits.spans(0)))
    mod.io.write.bits.spans(2) := 1.U
  })

  (sramAOpt.toSeq ++ sramBOpt ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
    mod.io.write.bits.data_strides.foreach(_ := 0.U)
    mod.io.write.bits.data_strides(0) := 1.U
  })

  (sramBOpt.toSeq ++ Seq(sramScatteredC)).foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.data_strides.foreach(_ := 0.U))
    mod.io.read_reqs.foreach(_.bits.data_strides(0) := 1.U)
  })

  sramAOpt.foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.data_strides.foreach(_ := 0.U))
    mod.io.read_reqs.foreach(_.bits.data_strides(0) := 1.U)
    mod.io.read_reqs.foreach(_.bits.data_strides(2) := size.U)
  })

  (sramAOpt.toSeq ++ sramBOpt).foreach(_.postProcess += { mod =>
    mod.io.write.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U)))
    mod.io.read_reqs.foreach(_.bits.metadata_strides_by_addr.foreach(_.foreach(_.foreach(_ := 0.U))))

    mod.io.write.bits.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U)))
    mod.io.write.bits.metadata_strides(0)(0)(CompressedMetadata.inner_metadata_buffer_id) := 1.U;
    require(CompressedMetadata.inner_metadata_buffer_id == LinkedListMetadata.coord_buffer_id)
    mod.io.write.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id) := 1.U;
    require(CompressedMetadata.outer_metadata_buffer_id == LinkedListMetadata.head_ptr_buffer_id)
  })

  sramBOpt.foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.metadata_strides.foreach(_.foreach(_.foreach(_ := 0.U))))
    mod.io.read_reqs.foreach(_.bits.metadata_strides(0)(0)(CompressedMetadata.inner_metadata_buffer_id) := 1.U);
    require(CompressedMetadata.inner_metadata_buffer_id == LinkedListMetadata.coord_buffer_id)
    mod.io.read_reqs.foreach(_.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id) := 1.U);
    require(CompressedMetadata.outer_metadata_buffer_id == LinkedListMetadata.head_ptr_buffer_id)
  })

  sramAOpt.foreach(_.postProcess += { mod =>
    mod.io.read_reqs.foreach(_.bits.metadata_strides.zipWithIndex.foreach { case (outerStrides, outerAxis) =>
      outerStrides.zipWithIndex.foreach { case (innerStrides, innerAxis) =>
        innerStrides.zipWithIndex.foreach { case (stride, metadata_buffer_id) =>
          if (!(outerAxis == 0 && innerAxis == 0 && metadata_buffer_id == CompressedMetadata.outer_metadata_ends_buffer_id))
            stride := 0.U
        }
      }
    })

    mod.io.read_reqs.foreach(_.bits.metadata_strides(0)(0)(CompressedMetadata.inner_metadata_buffer_id) := 1.U);
    require(CompressedMetadata.inner_metadata_buffer_id == LinkedListMetadata.coord_buffer_id)
    mod.io.read_reqs.foreach(_.bits.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id) := 1.U);
    require(CompressedMetadata.outer_metadata_buffer_id == LinkedListMetadata.head_ptr_buffer_id)
    mod.io.read_reqs.foreach(_.bits.metadata_strides(2)(0)(CompressedMetadata.inner_metadata_buffer_id) := size.U)
  })

  Seq(sramScatteredC).foreach(_.postProcess += { mod =>
    mod.io.write.bits.metadata_strides_by_addr(0).foreach(_.foreach(_ := 0.U))
    mod.io.write.bits.metadata_strides_by_addr(1)(0).zipWithIndex.collect { case (s, i) if i != LinkedListMetadata.head_ptr_buffer_id => s := 0.U }
    mod.io.write.bits.metadata_strides_by_addr(1).drop(1).foreach(_.foreach(_ := 0.U))
    mod.io.write.bits.metadata_strides_by_addr.drop(2).foreach(_.foreach(_.foreach(_ := 0.U)))

    mod.io.write.bits.metadata_strides.zipWithIndex.foreach { case (stridess, outerAxis) =>
      stridess.zipWithIndex.foreach { case (strides, innerAxis) =>
        strides.zipWithIndex.foreach { case (stride, bufferId) =>
          if (!(outerAxis == 1 && innerAxis == 1 && bufferId == CompressedMetadata.outer_metadata_ends_buffer_id)) {
            stride := 0.U
          }
        }
      }
    }
    mod.io.write.bits.metadata_strides(3)(1)(CompressedMetadata.inner_metadata_buffer_id) := size.U
    mod.io.write.bits.metadata_strides(2)(1)(CompressedMetadata.outer_metadata_buffer_id) := 1.U
    mod.io.write.bits.metadata_strides(1)(1)(CompressedMetadata.inner_metadata_buffer_id) := 1.U
    mod.io.write.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id) := 1.U

    mod.io.read_reqs.foreach(_.bits.metadata_strides_by_addr(0).foreach(_.foreach(_ := 0.U)))
    mod.io.read_reqs.foreach(_.bits.metadata_strides_by_addr(1)(0).zipWithIndex.collect { case (s, i) if i != LinkedListMetadata.head_ptr_buffer_id => s := 0.U })
    mod.io.read_reqs.foreach(_.bits.metadata_strides_by_addr(1).drop(1).foreach(_.foreach(_ := 0.U)))
    mod.io.read_reqs.foreach(_.bits.metadata_strides_by_addr.drop(2).foreach(_.foreach(_.foreach(_ := 0.U))))

    mod.io.read_reqs.foreach(_.bits.metadata_strides.zipWithIndex.foreach { case (outerStrides, outerAxis) =>
      outerStrides.zipWithIndex.foreach { case (innerStrides, innerAxis) =>
        innerStrides.zipWithIndex.foreach { case (stride, metadata_buffer_id) =>
          if (!(outerAxis == 1 && innerAxis == 1 && metadata_buffer_id == CompressedMetadata.outer_metadata_ends_buffer_id))
            stride := 0.U
        }
      }
    })
    mod.io.read_reqs.foreach(_.bits.metadata_strides(3)(1)(CompressedMetadata.inner_metadata_buffer_id) := size.U)
    mod.io.read_reqs.foreach(_.bits.metadata_strides(2)(1)(CompressedMetadata.outer_metadata_buffer_id) := 1.U)
    mod.io.read_reqs.foreach(_.bits.metadata_strides(1)(1)(CompressedMetadata.inner_metadata_buffer_id) := 1.U)
    mod.io.read_reqs.foreach(_.bits.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id) := 1.U)
  })

  (sramAOpt.toSeq ++ sramBOpt).foreach(_.postProcess += { mod =>
    mod.io.write.bits.from_regfile_metadata := DontCare
    mod.io.write.bits.from_regfile_metadata.foreach(_.foreach(_.valid := false.B))
  })

  Seq(sramScatteredC).foreach(_.postProcess += { mod =>
    mod.io.write.bits.from_regfile_metadata := DontCare
    mod.io.write.bits.from_regfile_metadata.foreach(_.foreach(_.valid := false.B))
    mod.io.write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).valid := true.B
    mod.io.write.bits.from_regfile_metadata(0)(LinkedListMetadata.coord_buffer_id).coord := 1.U
  })

  if (hasMerger)
    Seq(sramScatteredC).foreach(_.postProcess += { mod =>
      mod.io.read_reqs.foreach(_.bits.recursive_dim(1) := size.U)
    })

  def sramC_matmul_read_banking_strategies = Seq(ReadBankingStrategy({ x => Seq.tabulate(size) { bankId =>
    (!x.should_read_data && x.metadata_buffer_id === LinkedListMetadata.coord_buffer_id.U && x.spans(2) > 1.U, { y: rtl.ChiselSRAMReadReq =>
      y.adjacent := true.B
      y.update_running_len := true.B
      y.address(2) := bankId.U
      y.spans(2) := 1.U
      y.should_read_metadata := true.B
      y.should_read_data := true.B
    })
  }}))

  def sramC_merge_read_banking_strategies = Seq(ReadBankingStrategy({ x => Seq.tabulate(size) { bankId =>
    (Mux(!x.to_regfile && x.should_read_metadata && x.metadata_buffer_id === LinkedListMetadata.head_ptr_buffer_id.U, x.spans.head, x.spans(2)) > 1.U, { y: rtl.ChiselSRAMReadReq =>
      y.address(2) := bankId.U
      y.spans(2) := 1.U

      when (!x.to_regfile) {
        when (x.spans(3) > 1.U) {
          y.spans(3) := 1.U
          y.should_read_data := true.B
          y.should_read_metadata := true.B
        }.elsewhen (x.should_read_metadata && x.metadata_buffer_id === LinkedListMetadata.head_ptr_buffer_id.U) {
          y.spans.head := 1.U
          y.adjacent := x.spans.head > 1.U
          y.fused := true.B
        }.otherwise {
          // assert(y.independent)
          y.adjacent := x.spans(2) > 1.U
          y.update_running_len := x.should_read_data
        }
      }
    })
  }}))

  def sramC_merge_write_banking_strategies = Seq(
    WriteBankingStrategy({ x: ChiselSRAMWriteReq[SInt] => Seq.tabulate(size) { bankId =>
      ({
        val addr = x.address(2) - 1.U
        x.axis === 1.U && x.address(2) > 0.U && addr <= bankId.U && bankId.U < (addr +& x.spans(1))
      }, { y: rtl.ChiselSRAMWriteReq[SInt] =>
        y.address(2) := (bankId+1).U
        y.spans(1) := 1.U
        y.spans(2) := 1.U

        val addr = x.address(2) - 1.U
        y.data.head := x.data(bankId.U - addr)
      })
    }}),

    WriteBankingStrategy({ x: ChiselSRAMWriteReq[SInt] => Seq.tabulate(size) { bankId =>
      ({
        x.axis === 0.U && !x.reset_running_state &&
          !x.is_data && x.metadata_buffer_id === LinkedListMetadata.head_ptr_buffer_id.U &&
          x.address(2) <= bankId.U && bankId.U < (x.address(2) +& x.spans(2))
      }, { y: rtl.ChiselSRAMWriteReq[SInt] =>
        y.address(2) := bankId.U
        y.spans(2) := 1.U
      })
    }}),

    WriteBankingStrategy({ x: ChiselSRAMWriteReq[SInt] => Seq.tabulate(size) { bankId =>
      ({
        x.axis === 0.U && !x.reset_running_state &&
          !x.from_regfile(0) &&
          x.address(2) === bankId.U
      }, { y: rtl.ChiselSRAMWriteReq[SInt] =>
        y.address(2) := bankId.U
        y.address(0) := x.address.head / 2.U

        y.spans(2) := 1.U
        y.spans(0) := x.spans.head / 2.U

        y.is_both_data_and_metadata := true.B
      })
    }}),

    WriteBankingStrategy({ x: ChiselSRAMWriteReq[SInt] => Seq.tabulate(size) { bankId =>
      ({
        x.axis === 0.U && !x.reset_running_state &&
          x.from_regfile(0) &&
          x.address(2) <= bankId.U && bankId.U < (x.address(2) +& x.spans(2))
      }, { y: rtl.ChiselSRAMWriteReq[SInt] =>
        y.address(2) := bankId.U
        y.spans(2) := 1.U
      })
    }}),
  )
}

object SCNN {
  val nAxes = 6
}
class SCNN(maxI: Int, maxF: Int, maxWt: Int, maxHt: Int,
           maxInputChannels: Int, maxOutputChannels: Int,
           nBanks: Int, rowSize: Int,
           nSramImageElems: Int, nSramFilterElems: Int, nSramParamsElems: Int, nSramResultElems: Int,
           withAsserts: Boolean = false) extends Accelerator(withAsserts = withAsserts) {
  import SCNN._
  import chisel3._

  val elemT = SInt(32.W)

  // Spatial arrays
  val mult = MakeSpatialArray(new SCNNMultArray(maxI=maxI, maxF=maxF, maxWt=maxWt, maxHt=maxHt, nBanks=nBanks, rowSize=rowSize))

  // Regfiles
  val Seq(regImageValueAndH, regImageW) = Seq.fill(2)(new RegFile(nElems = maxWt * maxHt * maxI * 4, nIOCoords = 2, nDomainCoords = 2, nameOpt = Some("regImage"), domainCoordsToUseForOutputs = Seq(1 -> 0).toMap))
  val Seq(regFilterValueAndH, regFilterWAndOutChannel) = Seq.fill(2)(new RegFile(nElems = maxF * 8, nIOCoords = 1, nDomainCoords = 1, nSubArrays = maxWt * maxHt, nameOpt = Some("regFilter"), domainCoordsToUseForOutputs = Seq(0 -> 0).toMap))
  val regPartialSum = new RegFile(nElems = maxWt * maxHt * maxI * maxF * nBanks * 2, nIOCoords = 2, nDomainCoords = 5,
    // lastInAxisFlags = Seq((maxI, 0), (maxF, 1), (maxWt, 3), (maxHt, 4)), /*lastInAxisFlagsFromOutPorts = false,*/
    nameOpt = Some("regPartial"),
    // entryOption = Edge(), exitOption = Edge(),
    subPipelineOpt = Some((nBanks, 0, false, true)),
    coordsToIgnoreForOutputs = Set(1),
    dummyData = true, // We can speed up the perf evaluations by leaving out the actual data
  )
  val regParams = new RegFile(nElems = 16, nIOCoords = 1, nDomainCoords = 1, allElemsWillBePoppedBeforeLastOut = false, nameOpt = Some("regParams"))

  // SRAMs
  def hardCodedPartialSums(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] =
    Seq(x.write_req.should_scatter.head -> true.B).toMap[Data, Data] ++
    x.write_req.data_strides.map(_ -> 0.U).toMap ++
    x.write_req.iteration_strides.map(_ -> 1.U).toMap ++
    Seq(x.write_req.should_trail_reads -> false.B, x.read_req.should_trail_writes -> false.B).toMap

  val flex = 4
  val Seq(sramImageValueAndH, sramImageW) = Seq.fill(2)(new SRAM(elemT = elemT, nElems = nSramImageElems, elemsPerRead = maxI * flex, elemsPerWrite = maxI * flex,
    axes = FiberTreeAxis.Compressed +: Seq.fill(nAxes - 1)(FiberTreeAxis.Dense),
    metadatas = CompressedMetadata(nOuter = maxInputChannels * maxWt * maxHt) +: Seq.fill(nAxes - 1)(DenseMetadata),
    name = Some("sramImage")
  ))
  val Seq(sramFilterValueAndH, sramFilterWAndOutChannel) = Seq.fill(2)(new SRAM(elemT = elemT, nElems = nSramFilterElems, elemsPerRead = maxF * flex, elemsPerWrite = maxF * flex,
    axes = FiberTreeAxis.Compressed +: Seq.fill(nAxes - 1)(FiberTreeAxis.Dense),
    metadatas = CompressedMetadata(nOuter = maxOutputChannels * maxInputChannels) +: Seq.fill(nAxes - 1)(DenseMetadata),
    name = Some("sramFilter")
  ))

  val sramParams = new SRAM(elemT = elemT, nElems = nSramParamsElems, elemsPerRead = maxI.max(maxF) * flex, elemsPerWrite = maxI.max(maxF) * flex,
    axes = Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas = Seq.fill(nAxes)(DenseMetadata),
    name = Some("sramParams"))

  val sramResult = new SRAM(elemT = elemT, nElems = nSramResultElems, elemsPerRead = 1, elemsPerWrite = 1, elemsPerRowMultiplier = rowSize,
    axes = Seq.fill(nAxes)(FiberTreeAxis.Dense), metadatas = Seq.fill(nAxes)(DenseMetadata),
    nBanks = nBanks, independentBanks = true,
    hardCodedValues = hardCodedPartialSums,
    writeBankingStrategies = sram_result_write_banking_strategies,
    name = Some("scnn_sramResult"), dummyData = true, // We can speed up the perf evaluations by leaving out the actual data
  )

  // Connect regfiles to spatial arrays
  connectVarToRegFile(mult.Image, regImageValueAndH)
  connectVarToRegFile(mult.ImageHCoord, regImageValueAndH, CoordConn(1))
  connectVarToRegFile(mult.ImageWCoord, regImageW, CoordConn(-1))

  connectVarToRegFile(mult.Filter, regFilterValueAndH)
  connectVarToRegFile(mult.FilterHCoord, regFilterValueAndH, CoordConn(0))
  connectVarToRegFile(mult.FilterWCoord, regFilterWAndOutChannel, CoordConn(-1))
  connectVarToRegFile(mult.FilterOutChannelCoord, regFilterWAndOutChannel, CoordConn(0))

  connectVarToRegFile(mult.PartialSums, regPartialSum)

  connectVarToRegFile(mult.Params, regParams)

  // Connect SRAMs to regfiles
  connectSRAMtoRegFile(sramImageValueAndH, regImageValueAndH)
  connectSRAMtoRegFile(sramImageW, regImageW)

  connectSRAMtoRegFile(sramFilterValueAndH, regFilterValueAndH)
  connectSRAMtoRegFile(sramFilterWAndOutChannel, regFilterWAndOutChannel)

  connectSRAMtoRegFile(sramParams, regParams)

  // Connect regfiles to SRAMS
  connectRegFileToSRAM(regPartialSum, sramResult)

  def sram_result_write_banking_strategies = Seq(WriteBankingStrategy({ x: ChiselSRAMWriteReq[SInt] => Seq.tabulate(nBanks) { bankId =>
    (true.B, { y: rtl.ChiselSRAMWriteReq[SInt] =>
      y.address(1) := bankId.U
      y.spans(1) := 1.U
    })
  }}))
}

class SpArchMerger(isCheap: Boolean, throughput: Int, nLevels: Int, check_result: Boolean) extends Accelerator(withAsserts = false) {
  import chisel3._
  import RfEntryExitOption._
  import FiberTreeAxis._

  val nTotalFibers = 1 << nLevels
  val throughputSqrt = Math.sqrt(throughput).toInt; require(isCheap || throughputSqrt*throughputSqrt == throughput)

  val dummy_output_srams = !check_result
  val outermost_unmerged_sram_read_flex_space = 4

  // Regfiles
  val tensor_rfs = Seq.tabulate(nLevels + 1) { level =>
    // The RF at level-N is the output of merger-N. Level (nLevels) is the output of the SRAM
    val nFibers = 1 << level
    val nCoords = if (isCheap) 3 else 2
    val flex_space = 8
    val isInnermost = level == 0
    val isOutermost = level == nLevels

    new RegFile(
      nElems = nFibers * throughput * flex_space, nIOCoords = nCoords, nDomainCoords = 5,
      nPipelinesOpt = Option.when(isOutermost)(throughput),

      lastInAxisFlags = Seq((nFibers, 0)), lastInAxisFlagsFromOutPorts = false,
      separateLastInAxisFlagsForEachPipeline = isCheap,
      unavailableIfSmallerOpt = Option.when(isCheap)(1, false, Set(0, 2)),

      entryOption = Edge(), exitOption = Edge(),
      subPipelineOpt = Some((nFibers, 0, false, true)),
      groupedInPortStride = if (isCheap || !isOutermost) 1 else throughput,

      coordsToIgnoreForOutputs = Set(0, if (isCheap) 2 else 1),
      coordIsDivisibleByForPipelines = if (isCheap) {
        Seq.empty
      } else {
        Seq.tabulate(throughput)(i => Seq(None, Some(throughput, i)))
      },
      maxOutCoordOpt = Some(1 << 30),

      allElemsWillBePoppedBeforeLastOut = true, getLastInFromInOpCounts = !isOutermost, getTravellingOpCountFromInPorts = isCheap || !isOutermost,
      getTravellingCoordsFromInPorts = Option.when(isCheap)(1).toSet,

      stickyOutPortCoords = Option.when(isCheap)(Set(1), nFibers, 1),
      stickyInPortCoords = Option.when(isCheap)(1).toSet,

      dummyData = isInnermost && dummy_output_srams, nameOpt = Some(s"sparch_tensor_rf_$level"))
  }

  val row_id_rfs = Seq.tabulate(if (isCheap) nLevels + 1 else 0) { level =>
    // The RF at level-N is the output of sorter-N. Level (nLevels) is the output of the SRAM
    val nFibers = 1 << level
    val flex_space = 2
    val isOutermost = level == nLevels
    val isInnermost = level == 0

    new RegFile(
      nElems = nFibers * throughput * flex_space, nIOCoords = 2, nDomainCoords = 5,
      nSubArrays = if (isOutermost) 1 else 2,

      lastInAxisFlags = Seq((nFibers, 0)), lastInAxisFlagsFromOutPorts = false,

      entryOption = Incrementing(skipOccupiedElems = true),
      subPipelineOpt = Some(nFibers, 0, false, true),

      allElemsWillBePoppedBeforeLastOut = true,

      nameOpt = Some(s"sparch_row_id_rf_$level"))
  }

  // Spatial arrays
  val mergers = Seq.tabulate(nLevels) { level =>
    val nFibers = 1 << level
    val isInnermost = level == 0

    MakeSpatialArray(new ParallelSorter(
      throughputPerDimPerChunk = if (isCheap) 1 else throughputSqrt, nChunksPerDim = if (isCheap) 1 else throughputSqrt,
      nTimeMultiplexedInFiberPairs = nFibers, nOuterFibers = if (isCheap) throughput else 1,
      popDuplicates = true, expandOuterCoord = !isInnermost, checkWhichFiberPopped = isCheap,
      stallForInCoords = !isCheap,
      maxElemsPerFiber = Some(tensor_rfs(level).nElems / (if (isCheap) throughput * nFibers else nFibers)),
      isDummy = isInnermost && dummy_output_srams,
      name = s"merger_$level"))
  }

  val row_id_sorters = Seq.tabulate(if (isCheap) nLevels else 0) { level =>
    val nFibers = 1 << level

    MakeSpatialArray(new ParallelSorter(
      throughputPerDimPerChunk = 1, nChunksPerDim = 1, nTimeMultiplexedInFiberPairs = nFibers, nOuterFibers = 1,
      popDuplicates = true, outputWhichFiberPops = true, stallForInCoords = false,
      maxElemsPerFiber = Some(row_id_rfs(level).nElems / nFibers),
      name = s"sparch_row_id_sorter_$level"))
  }

  // SRAMs
  val elemT = SInt(SpArchMerger.bitwidth(isCheap).W)
  val sram_node_size = 32
  import SpArchMerger.{n_sram_elems, n_sram_rows}

  def hardCoded(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] =
    Seq(x.read_req.iteration_strides, x.write_req.iteration_strides).flatMap(_.map(_ -> 1.U)).toMap[Data, Data] ++
    Seq(x.read_req.interleave, x.write_req.interleave).flatMap(y => Seq(y.should_push, y.should_pop)).map(_ -> false.B).toMap[Data,Data] ++
    Seq(x.read_req.should_trail_writes, x.write_req.should_trail_reads).map(_ -> false.B).toMap[Data,Data] ++
    Seq(x.read_req.should_trail_writes_coarse_grained, x.write_req.should_trail_reads_coarse_grained).map(_ -> false.B).toMap[Data,Data] ++
    x.read_req.should_gather.map(_ -> false.B).toMap[Data, Data] ++
    Seq(x.write_req.from_regfile_last_axis_log_size -> ChiselUtil.maxVal(x.write_req.from_regfile_last_axis_log_size)).toMap[Data, Data] ++
    Seq(x.write_req.from_regfile_metadata -> VecInit.fill(x.write_req.from_regfile_metadata.size, x.write_req.from_regfile_metadata.head.size)((new stellar.rtl.ChiselSRAMWriteReq.FromRegfileMetadata).Lit(_.valid -> false.B))).toMap[Data,Data] ++
    Seq(x.write_req.data_strides.tail, x.read_req.data_strides.tail).flatMap(_.map(_ -> 0.U)) ++
    Seq(x.write_req.data_strides.head, x.read_req.data_strides.head).map(_ -> 1.U) ++
    Seq(x.data_addr -> 0.U, x.metadata_addrs(0)(CompressedMetadata.inner_metadata_buffer_id) -> 0.U)

  def setAllMetadataStridesTo0(metadata_strides: Vec[Vec[Vec[UInt]]], except: Set[(Int, Int, Int)]): SMap[Data, Data] =
    metadata_strides.zipWithIndex.flatMap { case (strides, outerAxis) =>
      strides.zipWithIndex.flatMap { case (stridess, innerAxis) =>
        stridess.zipWithIndex.collect {
          case (stride, metadataBufferId) if !except.contains((outerAxis, innerAxis, metadataBufferId)) => stride -> 0.U
        }
      }
    }.toMap[Data, Data]

  def hardCodedUnmerged(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] = hardCoded(x) ++
    Seq(x.read_req.to_regfile -> true.B, x.read_req.to_regfile_last_axis -> (if (isCheap) 2 else 1).U).toMap[Data, Data] ++
    x.write_req.from_regfile.map(_ -> false.B).toMap[Data, Data] ++
    Seq(x.read_req.metadata_strides(0)(0)(CompressedMetadata.inner_metadata_buffer_id) -> 1.U) ++ {
      if (isCheap) {
        Seq(x.read_req.metadata_strides(1)(1)(CompressedMetadata.inner_metadata_buffer_id) -> throughput.U,
          x.read_req.metadata_strides(2)(1)(CompressedMetadata.outer_metadata_buffer_id) -> 1.U,
          x.read_req.metadata_strides_by_addr(1)(0)(CompressedMetadata.outer_metadata_buffer_id) -> 1.U).toMap[Data,Data] ++
          setAllMetadataStridesTo0(x.read_req.metadata_strides, Set((0,0,CompressedMetadata.inner_metadata_buffer_id), (1,1,CompressedMetadata.inner_metadata_buffer_id), (2,1,CompressedMetadata.outer_metadata_buffer_id))) ++
          setAllMetadataStridesTo0(x.read_req.metadata_strides_by_addr, Set((1,0,CompressedMetadata.outer_metadata_buffer_id)))
      } else {
        Seq(x.read_req.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id) -> 1.U).toMap[Data,Data] ++
          setAllMetadataStridesTo0(x.read_req.metadata_strides, Set((0,0,CompressedMetadata.inner_metadata_buffer_id), (1,0,CompressedMetadata.outer_metadata_buffer_id))) ++
          setAllMetadataStridesTo0(x.read_req.metadata_strides_by_addr, Set.empty).toMap[Data,Data]
      }
    }

  def hardCodedMerged(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] = hardCoded(x) ++
    Seq(x.read_req.to_regfile -> false.B, x.write_req.from_regfile_last_axis -> (if (isCheap) 1 else 0).U).toMap[Data, Data] ++
    Seq(x.write_req.metadata_strides(0)(0)(LinkedListMetadata.coord_buffer_id) -> 1.U) ++ (if (dummy_output_srams) {
      Seq(x.read_req.axis -> 0.U).toMap[Data, Data] ++ x.read_req.address.map(_ -> 0.U).toMap[Data, Data] ++
      setAllMetadataStridesTo0(x.read_req.metadata_strides, Set.empty) ++
      setAllMetadataStridesTo0(x.read_req.metadata_strides_by_addr, Set.empty) ++
      (if (isCheap) {
        Seq(x.write_req.metadata_strides(1)(1)(LinkedListMetadata.coord_buffer_id) -> 1.U).toMap[Data,Data] ++
          setAllMetadataStridesTo0(x.write_req.metadata_strides, Set((0,0,LinkedListMetadata.coord_buffer_id), (1,1,LinkedListMetadata.coord_buffer_id), (1,0,LinkedListMetadata.head_ptr_buffer_id))) ++
          setAllMetadataStridesTo0(x.write_req.metadata_strides_by_addr, Set((1,0,LinkedListMetadata.head_ptr_buffer_id)))
      } else {
        setAllMetadataStridesTo0(x.write_req.metadata_strides, Set((0,0,LinkedListMetadata.coord_buffer_id))) ++
          setAllMetadataStridesTo0(x.write_req.metadata_strides_by_addr, Set.empty)
      })
    } else Seq.empty)

  def hardCodedRowIds(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] = hardCoded(x) ++
    Seq(x.read_req.to_regfile -> true.B, x.read_req.to_regfile_last_axis -> 1.U).toMap[Data, Data] ++
    x.write_req.from_regfile.map(_ -> false.B).toMap[Data, Data] ++
    Seq(x.read_req.metadata_strides(1)(0)(CompressedMetadata.outer_metadata_buffer_id) -> 1.U,
      x.read_req.metadata_strides(0)(0)(CompressedMetadata.inner_metadata_buffer_id) -> 1.U) ++
    setAllMetadataStridesTo0(x.read_req.metadata_strides, Set((1,0,CompressedMetadata.outer_metadata_buffer_id), (0,0,CompressedMetadata.inner_metadata_buffer_id))) ++
    setAllMetadataStridesTo0(x.read_req.metadata_strides_by_addr, Set.empty)

  val unmerged_sram: SRAM = new SRAM(elemT = elemT, nElems = n_sram_elems,
    elemsPerRead = (if (isCheap) 1 else throughput) * outermost_unmerged_sram_read_flex_space, elemsPerWrite = throughput,

    axes = Seq(Compressed) ++ Option.when(isCheap)(Compressed).toSeq :+ Dense,
    metadatas = Seq(CompressedMetadata(nOuter = n_sram_rows, nInnerOpt = CompressedMetadata.DontExpandInner())) ++
      Option.when(isCheap)(CompressedMetadata(nOuter = (nTotalFibers * 2) max throughput, nInnerOpt = CompressedMetadata.InnerBuffer(Some(n_sram_rows)))).toSeq :+
      DenseMetadata,
    branchSizes = Seq.fill(if (isCheap) 2 else 1)(nTotalFibers),
    nBanks = if (isCheap) throughput else 1,
    maxElemsInRf = Some(tensor_rfs.last.nElems / (if (isCheap) throughput * nTotalFibers else nTotalFibers)),
    multipleNElemsLookupPorts = true,
    elemsPerRowMultiplier = if (isCheap) 1 else 8,
    hardCodedValues = hardCodedUnmerged,
    readBankingStrategies = unmerged_read_banking_strategies,

    name = Some("sparch_unmerged_sram"))

  val merged_sram: SRAM = new SRAM(elemT = elemT, nElems = n_sram_elems,
    elemsPerRead = throughput, elemsPerWrite = if (isCheap) 1 else throughput,
    axes = Seq(LinkedList) ++ Option.when(isCheap)(LinkedList).toSeq,
    metadatas = Seq(LinkedListMetadata(nHeads = n_sram_rows, nNodes = n_sram_elems/sram_node_size, nodeSize = sram_node_size, nCoordsOpt = CompressedMetadata.DontExpandInner())) ++
      Option.when(isCheap)(LinkedListMetadata(nHeads = 1.max(throughput), nNodes = (n_sram_rows+sram_node_size-1)/sram_node_size, nodeSize = sram_node_size,
        nCoordsOpt = if (dummy_output_srams) CompressedMetadata.DontExpandInner() else CompressedMetadata.InnerBuffer(None))).toSeq,
    nBanks = if (isCheap) throughput else 1,
    hardCodedValues = hardCodedMerged, dummyData = dummy_output_srams, dummyReadStages = dummy_output_srams,
    name = Some("sparch_merged_sram"))

  val row_id_sram_opt: Option[SRAM] = Option.when(isCheap)(new SRAM(elemT = elemT, nElems = n_sram_rows,
    elemsPerRead = 1, elemsPerWrite = throughput,
    axes = Seq(Compressed, Dense, Dense),
    metadatas = Seq(CompressedMetadata(nOuter = nTotalFibers * 2, nInnerOpt = CompressedMetadata.DontExpandInner()), DenseMetadata, DenseMetadata),
    branchSizes = Seq(nTotalFibers),
    maxElemsInRf = Some(row_id_rfs.last.nElems / nTotalFibers), multipleNElemsLookupPorts = true,
    hardCodedValues = hardCodedRowIds,
    name = Some("row_id_sram")))

  if (dummy_output_srams)
    merged_sram.postProcess += { mod =>
      mod.io.read_reqs.foreach(_.valid := false.B)
    }

  // Connect regfiles to spatial arrays
  for (level <- 0 until nLevels) {
    val merger = mergers(level)
    val output_level = level
    val input_level = level + 1

    connectVarToRegFile(merger.InnerCoord, tensor_rfs(input_level), CoordConn(-1))
    connectVarToRegFile(merger.SortedInnerCoord, tensor_rfs(output_level), maxOutPorts = Option.unless(isCheap)(throughput, Some(1)))
    merger.ElemsPerFiberOpt.foreach(ElemsPerFiber => connectVarToRegFile(ElemsPerFiber, tensor_rfs(output_level), NElemLookupConn))

    if (isCheap) {
      val row_id_sorter = row_id_sorters(level)

      connectVarToRegFile(row_id_sorter.InnerCoord, row_id_rfs(input_level), CoordConn(-1), popBits = Some(0), maxRfOut = Some(SpArchMerger.removePoppedDenom))
      connectVarToRegFile(row_id_sorter.SortedInnerCoord, row_id_rfs(output_level), maxOutPorts = Some(1, None))
      row_id_sorter.ElemsPerFiberOpt.foreach(ElemsPerFiber => connectVarToRegFile(ElemsPerFiber, row_id_rfs(output_level), NElemLookupConn))

      connectVarToRegFile(merger.OuterCoordIn, row_id_rfs(output_level), CoordConn(-1), popBits = Some(1))
    }
  }

  // Connect SRAMs to regfiles
  connectSRAMtoRegFile(unmerged_sram, tensor_rfs.last)
  row_id_sram_opt.foreach(connectSRAMtoRegFile(_, row_id_rfs.last))
  connectRegFileToSRAM(tensor_rfs.head, merged_sram)
  row_id_rfs.headOption.foreach(connectRegFileToSRAM(_, merged_sram, stageId = 1))

  def unmerged_read_banking_strategies = Seq(ReadBankingStrategy({ x: ChiselSRAMReadReq => Seq.tabulate(unmerged_sram.nBanks) { bankId =>
    (true.B, { y: rtl.ChiselSRAMReadReq =>
      when (x.to_regfile) {
        // x.metadata_addrs(1)(CompressedMetadata.inner_metadata_buffer_id) := bankId.U
        x.address(1) := bankId.U
        y.metadata_strides(1)(1)(CompressedMetadata.inner_metadata_buffer_id) := unmerged_sram.nBanks.U
      }
    })
  }}))
}

object SpArchMerger {
  val n_sram_elems = 512*1024
  val n_sram_rows = 256*1024

  def bitwidth(isCheap: Boolean): Int = if (isCheap) {
    val for_elems = chisel3.util.log2Ceil(n_sram_elems+1) + 1 // Add one bit because it's signed
    val for_rows = chisel3.util.log2Ceil(n_sram_rows+1) + 3
    for_elems.max(for_rows)
  } else {
    chisel3.util.log2Ceil(n_sram_elems+1) + 1 // Add one bit because it's signed
  }

  val poppedAIndex = 1 << (bitwidth(isCheap=true) - 3)
  val poppedBIndex = 1 << (bitwidth(isCheap=true) - 2)
  val removePoppedDenom = poppedAIndex min poppedBIndex
}

class DenseMatmulAccelerator(size: Int, hasAccumulator: Boolean, inputT: chisel3.SInt, outputT: chisel3.SInt, sramAaxes: Int, sramBaxes: Int, sramCaxes: Int, sramAelems: Int, sramBelems: Int, sramCelems: Int) extends Accelerator(withAsserts = size <= 4) {
  import chisel3._
  import chisel3.util.log2Ceil

  val matmulArrayBitwidth = log2Ceil((BigInt(1) << (inputT.getWidth-1))*(BigInt(1) << (inputT.getWidth-1))*size)
  val matmul = MakeSpatialArray(new DenseMatmul(size = size, dataflow = "ws", unbounded = true, dataWidthBitsOpt = Some(matmulArrayBitwidth)))

  val regA = new RegFile(nElems = (1 to size).sum, nIOCoords=2, nDomainCoords=3, nameOpt = Some("regA"), automaticallyOptimize = true)
  val regB = new RegFile(nElems = (1 to size).sum, nIOCoords = 2, nDomainCoords = 3, nameOpt = Some("regB"), checkOpCountForOuts = false, automaticallyOptimize = true)
  val regC = new RegFile(nElems = (1 to size).sum, nIOCoords=2, nDomainCoords=3, nameOpt = Some("regC"), checkOpCountForOuts = false, automaticallyOptimize = true)

  def hardCoded(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] =
    x.read_req.iteration_strides.map(_ -> 1.U).toMap[Data, Data] ++
    x.write_req.iteration_strides.map(_ -> 1.U).toMap[Data, Data] ++
    x.read_req.should_gather.map(_ -> false.B).toMap ++
    Seq(x.read_req.should_trail_writes, x.write_req.should_trail_reads).map(_ -> false.B).toMap[Data,Data] ++
    Seq(x.read_req.should_trail_writes_coarse_grained, x.write_req.should_trail_reads_coarse_grained).map(_ -> false.B).toMap[Data,Data] ++
    Seq(x.write_req.from_regfile_last_axis_log_size -> ChiselUtil.maxVal(x.write_req.from_regfile_last_axis_log_size)).toMap[Data, Data] ++
    Seq(x.read_req.axis -> 0.U, x.write_req.axis -> 0.U).toMap ++
    Seq(x.read_req.data_strides.head -> 1.U, x.write_req.data_strides.head -> 1.U).toMap

  def hardCodedAB(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] = hardCoded(x) ++
    x.write_req.from_regfile.map(_ -> false.B).toMap ++ Seq(x.read_req.to_regfile -> true.B).toMap ++
    x.read_req.data_strides.lift(4).map(_ -> size.U).toSeq.toMap ++ x.write_req.data_strides.drop(4).map(_ -> size.U).toMap ++
    Seq(x.write_req.interleave, x.read_req.interleave).flatMap(y => Seq(y.should_pop -> false.B, y.should_push -> false.B)).toMap ++
    x.write_req.spans.drop(4).map(_ -> 1.U)

  def hardCodedB(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] = hardCodedAB(x) ++
    Seq(x.read_req.to_regfile_last_axis -> 1.U)

  def hardCodedC(x: ChiselSRAMPipelineData[SInt]): scala.collection.Map[Data, Data] = hardCoded(x) ++
    Seq(x.read_req.to_regfile_last_axis -> 1.U, x.write_req.from_regfile_last_axis -> 1.U)

  val sramA = new SRAM(elemT=inputT, nElems=sramAelems/2, elemsPerRead=size, elemsPerWrite=size, axes=Seq.fill(sramAaxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(sramAaxes)(DenseMetadata), hardCodedValues = hardCodedAB, stridesDivisibleBy = (1 until sramAaxes).map(_ -> size).toMap)
  val sramB = new SRAM(elemT=inputT, nElems=sramBelems/2, elemsPerRead=size, elemsPerWrite=size, axes=Seq.fill(sramBaxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(sramBaxes)(DenseMetadata), hardCodedValues = hardCodedB, stridesDivisibleBy = (1 until sramBaxes).map(_ -> size).toMap)
  val sramC = new SRAM(elemT=outputT, nElems=sramCelems/2, elemsPerRead=size, elemsPerWrite=size, axes=Seq.fill(sramCaxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(sramCaxes)(DenseMetadata), hardCodedValues = hardCodedC, stridesDivisibleBy = (1 until sramCaxes).map(_ -> size).toMap)

  val sramA2 = new SRAM(elemT=inputT, nElems=sramAelems/2, elemsPerRead=size, elemsPerWrite=size, axes=Seq.fill(sramAaxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(sramAaxes)(DenseMetadata), hardCodedValues = hardCodedAB, stridesDivisibleBy = (1 until sramAaxes).map(_ -> size).toMap)
  val sramB2 = new SRAM(elemT=inputT, nElems=sramBelems/2, elemsPerRead=size, elemsPerWrite=size, axes=Seq.fill(sramBaxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(sramBaxes)(DenseMetadata), hardCodedValues = hardCodedB, stridesDivisibleBy = (1 until sramBaxes).map(_ -> size).toMap)
  val sramC2 = new SRAM(elemT=outputT, nElems=sramCelems/2, elemsPerRead=size, elemsPerWrite=size, axes=Seq.fill(sramCaxes)(FiberTreeAxis.Dense), metadatas=Seq.fill(sramCaxes)(DenseMetadata), hardCodedValues = hardCodedC, stridesDivisibleBy = (1 until sramCaxes).map(_ -> size).toMap)

  Seq(sramA, sramA2, sramB, sramB2).foreach(_.postProcess += { x =>
    x.io.read_reqs.foreach(_.bits.spans.head := size.U)
  })
  Seq(sramB, sramB2).foreach(_.postProcess += { x =>
    x.io.read_reqs.foreach(_.bits.spans(1) := size.U)
  })

  connectVarToRegFile(matmul.A, regA)
  connectVarToRegFile(matmul.B, regB)
  connectVarToRegFile(matmul.C, regC)

  for (sram <- Seq(sramA, sramA2)) connectSRAMtoRegFile(sram, regA)
  for (sram <- Seq(sramB, sramB2)) connectSRAMtoRegFile(sram, regB)

  if (hasAccumulator) {
    val matadder = MakeSpatialArray(new MatrixAdder(size, unbounded = true, dataWidthBitsOpt = Some(outputT.getWidth), alwaysStart = true))

    val regD = new RegFile(nElems = size, nIOCoords=2, nDomainCoords=2, nameOpt = Some("regD"),
      // entryOption = Edge(), exitOption = Edge(),
      // constantCoordsForInputs = Seq.tabulate(size)(inPortId => Seq(None, Some(inPortId))),
      // maxOutCoordOpt = Some(size), coordsToIgnoreForOutputs = Set(0,1), allElemsWillBePoppedBeforeLastOut = true,
      /*lockstepIns = true, lockstepOuts = (size, 1),*/
      automaticallyOptimize = true,
    )
    val regE = new RegFile(nElems = size, nIOCoords=2, nDomainCoords=2, nameOpt = Some("regE"),
      // entryOption = Edge(), exitOption = Edge(),
      // constantCoordsForInputs = Seq.tabulate(size)(inPortId => Seq(None, Some(inPortId))),
      // maxOutCoordOpt = Some(size), coordsToIgnoreForOutputs = Set(0, 1), lockstepIns = true, lockstepOuts = (size, 1),
      // allElemsWillBePoppedBeforeLastOut = true,
      checkOpCountForOuts = false,
      automaticallyOptimize = true,
    )

    connectVarToRegFile(matadder.C, regC)
    connectVarToRegFile(matadder.D, regD)
    connectVarToRegFile(matadder.E, regE)

    for (sram <- Seq(sramC, sramC2)) {
      connectSRAMtoRegFile(sram, regD)
      connectRegFileToSRAM(regE, sram)
    }
  } else {
    for (sram <- Seq(sramC, sramC2))
      connectRegFileToSRAM(regC, sram)
  }
}
