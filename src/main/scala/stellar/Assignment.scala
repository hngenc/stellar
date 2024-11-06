package stellar

import Util.SMap

case class Assignment(dst: Indexed, src: Expr)(implicit blockOpt: Option[Block]) {
  override def toString: String = s"$dst = $src"

  assert(dst.dim == 0 && src.dim == 0, "Only scalar values can be assigned")

  blockOpt.foreach(_.registerAssignment(this))

  private def safeIndexed(indices: Seq[Index], indexed: Indexed): Unit = {
    // This function checks whether your indexed values are of the right format
    indexed.indices.zipWithIndex.foreach { case (ind, i) =>
      assert(!ind.isInstanceOf[IndexRange], "Index ranges are not permitted when calculating iteration vectors")
      assert(ind.indicesInExpr.size <= 1, s"At most one index permitted in range for assignments:\n\tAssignment: $this\n\tind = $ind")
      assert(ind.indicesInExpr.isEmpty || ind.indicesInExpr.head == indices(i), s"Indices are used in wrong order: $this")

      ind match {
        case _: Index | Add(_: Index, Const(_)) | Add(Const(_), _: Index) =>
        case _ if ind.indicesInExpr.isEmpty =>
        case _ => throw new Exception("Index expressions must be simple adds (at least for now)")
      }
    }
  }

  private def safeDst(dstIndexed: Indexed, indices: Iterable[Index]): Unit = {
    dstIndexed.indices.zip(indices).foreach { case (dInd, ind) =>
      assert(dInd == ind || dInd.indicesInExpr.isEmpty, "Destinations can't do any fancy math on the iterators")
    }
  }

  def connectionVectors(indices: Seq[Index], boundsMap: scala.collection.Map[Index, (Int, Int)]): Iterable[ConnectionVector] = {
    connectionVectorsWithSrc(indices, boundsMap).map(_._1)
  }

  def fixedVals(boundsMap: SMap[Index, (Int, Int)]): Iterable[Option[Int]] = {
    val indices = dst.variable match {
      case _: Intermediate => dst.indices

      case _: Output =>
        src match {
          case Indexed(_: Intermediate, inds) => inds
          case _ => throw new Exception("output must be assigned to just a single intermediate indexed value (for now)")
        }

      case _ => Seq()
    }

    indices.map {
      // These are all the iterator values which are harcoded into this set of indices

      case dind if dind.indicesInExpr.isEmpty =>
        val replaced = Passes(Passes.replaceBounds(dind, boundsMap))
        replaced match {
          case Const(n) => Some(n)
          case _ => throw new Exception("Too complicated of an index expression for us to correctly fix")
        }
      case _ => None
    }
  }

  def excludedVals(precedingAssignments: Iterable[Assignment], boundsMap: SMap[Index, (Int, Int)]): Iterable[Iterable[Option[Int]]] = {
    // These are all the iterator values for which this assignment should NOT be executed

    precedingAssignments
      .filter(_.dst.variable == dst.variable)
      .map(_.fixedVals(boundsMap))
  }

  def connectionVectorsWithSrc(indices: Seq[Index], boundsMap: SMap[Index, (Int, Int)]): Iterable[(ConnectionVector, Indexed)] = {
    val srcIndexeds = Passes.extractIndexed(src).filter(_.variable.isInstanceOf[Intermediate])

    val connVectorDst = dst.variable match {
      case _: Intermediate => Some(dst)
      case _: Output =>
        assert(srcIndexeds.size <= 1, "Currently, we only handle output statements which are tied to a single intermediate variable")
        srcIndexeds.headOption
      case _ => throw new Exception("An assignment can't write to an input variable")
    }

    val connVectorSrcs = dst.variable match {
      case _: Intermediate => srcIndexeds
      case _: Output => dst.indices.flatMap(Passes.extractIndexed).filter(_.variable.isInstanceOf[Intermediate])
      case _ => throw new Exception("An assignment can't write to an input variable")
    }

    connVectorDst.toSeq.flatMap { dstIndexed =>
      val dstVar = dstIndexed.variable match {
        case d: Intermediate => d
        case _ => throw new Exception("Fixed-offset connection vectors only exist between intermediate variables")
      }

      connVectorSrcs.foreach(sind => safeIndexed(indices.toSeq, sind))
      safeDst(dstIndexed, indices)

      connVectorSrcs.collect { case srcIndexed @ Indexed(_: Intermediate, srcInds) =>
        val srcIndsReplaced = srcInds.map(sind => Passes.replaceExprs(sind, indices.zip(dstIndexed.indices).toMap))
        val conns = (dstIndexed.indices zip srcIndsReplaced).map { case (dstInd, srcInd) =>

          val diff = Passes(Passes.replaceBounds(dstInd - srcInd, boundsMap))

          diff match {
            case Const(n: Int) => n case _ => throw new Exception(s"Too complicated of an index diff ($diff) for us to currently handle in $this")
          }
        }

        (ConnectionVector(srcIndexed = srcIndexed, dstVar = dstVar, diff = conns, dstFixedVals = dstIndexed.fixedVals(boundsMap)), srcIndexed)
      }
    }
  }

  def ioConns: Iterable[(Indexed, Indexed)] = {
    // Output format: (dst, src), where dst and/or src is an Input or Output type
    val srcIndexeds = Passes.extractIndexed(src)

    dst match {
      case Indexed(_: Intermediate, _) =>
        srcIndexeds.filter(!_.variable.isInstanceOf[Intermediate]).map(sind => (dst, sind))
      case _ => srcIndexeds.map(sind => (dst, sind))
    }
  }
}
