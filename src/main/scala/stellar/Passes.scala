package stellar

import Util.{SMap, without}

object Passes {
  def constMult(expr: Expr): Expr = expr match {
    case m: Multiply => {
      val consts = m.ops.collect { case c: Const => c.const }
      val constProd = consts.product

      val withoutConsts = m.ops.filter(!_.isInstanceOf[Const]).map(constMult).toList

      (constProd, withoutConsts) match {
        case (0, _) => Const(0)
        case (c, Nil) => Const(c)
        case (1, e :: Nil) => e
        case (1, _) => Multiply(withoutConsts:_*)
        case (c, _) => Multiply(Const(c) +: withoutConsts:_*)
      }
    }

    case a: Add => Add(a.ops.map(constMult):_*)
    case Divide(numer, denom) => Divide(constMult(numer), constMult(denom))
    case Modulo(numer, denom) => Modulo(constMult(numer), constMult(denom))

    case Equality(left, right) => Equality(constMult(left), constMult(right))
    case LessThan(left, right) => LessThan(constMult(left), constMult(right))
    case GreaterThan(left, right) => GreaterThan(constMult(left), constMult(right))
    case LessThanOrEq(left, right) => LessThanOrEq(constMult(left), constMult(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(constMult(left), constMult(right))
    case Not(e) => Not(constMult(e))

    case And(left, right) => And(constMult(left).asInstanceOf[BoolExpr], constMult(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(constMult(left).asInstanceOf[BoolExpr], constMult(right).asInstanceOf[BoolExpr])

    case Select(cond, iftrue, iffalse) => Select(constMult(cond).asInstanceOf[BoolExpr], constMult(iftrue), constMult(iffalse))

    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, constMult(indexExpr), deps.map(constMult), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(constMult))

    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(constMult)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(constMult)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(constMult)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(constMult)), axisId)

    case IndexRange(start, end) => IndexRange(
      start.map(s => constMult(s)),
      end.map(e => constMult(e)),
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(constMult))

    case _ => expr
  }

  def constAdd(expr: Expr): Expr = expr match {
    case a: Add => {
      val consts = a.ops.collect { case c: Const => c.const }
      val constSum = consts.sum

      val withoutConsts = a.ops.filter(!_.isInstanceOf[Const]).map(constAdd).toList

      (constSum, withoutConsts) match {
        case (c, Nil) => Const(c)
        case (0, e :: Nil) => e
        case (0, _) => Add(withoutConsts:_*)
        case (c, _) => Add(withoutConsts :+ Const(c):_*)
      }
    }

    case m: Multiply => Multiply(m.ops.map(constAdd):_*)
    case Divide(numer, denom) => Divide(constAdd(numer), constAdd(denom))
    case Modulo(numer, denom) => Modulo(constAdd(numer), constAdd(denom))

    case Equality(left, right) => Equality(constAdd(left), constAdd(right))
    case LessThan(left, right) => LessThan(constAdd(left), constAdd(right))
    case GreaterThan(left, right) => GreaterThan(constAdd(left), constAdd(right))
    case LessThanOrEq(left, right) => LessThanOrEq(constAdd(left), constAdd(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(constAdd(left), constAdd(right))
    case Not(e) => Not(constAdd(e))

    case And(left, right) => And(constAdd(left).asInstanceOf[BoolExpr], constAdd(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(constAdd(left).asInstanceOf[BoolExpr], constAdd(right).asInstanceOf[BoolExpr])

    case Select(cond, iftrue, iffalse) => Select(constAdd(cond).asInstanceOf[BoolExpr], constAdd(iftrue), constAdd(iffalse))

    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, constAdd(indexExpr), deps.map(constAdd), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(constAdd))

    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(constAdd)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(constAdd)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(constAdd)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(constAdd)), axisId)

    case IndexRange(start, end) => IndexRange(
      start.map(s => constAdd(s)),
      end.map(e => constAdd(e)),
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(constAdd))

    case _ => expr
  }

  def constModulo(expr: Expr): Expr = expr match {
    case Modulo(Const(0), _) => Const(0)
    case Modulo(_, Const(1) | True) => Const(0)
    case Modulo(False, _) => False
    case Modulo(True, Const(c)) => ConstBool(c > 1)
    case Modulo(Const(c1), Const(c2)) => Const(c1 % c2)
    case Modulo(numer, denom) => Modulo(constModulo(numer), constModulo(denom))

    case a: Add => Add(a.ops.map(constModulo):_*)
    case m: Multiply => Multiply(m.ops.map(constModulo):_*)
    case Divide(numer, denom) => Divide(constModulo(numer), constModulo(denom))

    case Equality(left, right) => Equality(constModulo(left), constModulo(right))
    case LessThan(left, right) => LessThan(constModulo(left), constModulo(right))
    case GreaterThan(left, right) => GreaterThan(constModulo(left), constModulo(right))
    case LessThanOrEq(left, right) => LessThanOrEq(constModulo(left), constModulo(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(constModulo(left), constModulo(right))
    case Not(e) => Not(constModulo(e))

    case And(left, right) => And(constModulo(left).asInstanceOf[BoolExpr], constModulo(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(constModulo(left).asInstanceOf[BoolExpr], constModulo(right).asInstanceOf[BoolExpr])

    case Select(cond, iftrue, iffalse) => Select(constModulo(cond).asInstanceOf[BoolExpr], constModulo(iftrue), constModulo(iffalse))

    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, constModulo(indexExpr), deps.map(constModulo), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(constModulo))

    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(constModulo)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(constModulo)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(constModulo)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(constModulo)), axisId)

    case IndexRange(start, end) => IndexRange(
      start.map(s => constModulo(s)),
      end.map(e => constModulo(e)),
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(constModulo))

    case _ => expr
  }

  def flattenAdds(expr: Expr): Expr = expr match {
    case a: Add => {
      val innerAdds = a.ops.collect { case ia: Add => ia }.flatMap(_.ops).map(flattenAdds)
      val notAdds = a.ops.filter(!_.isInstanceOf[Add]).map(flattenAdds)

      Add(innerAdds ++ notAdds:_*)
    }

    case m: Multiply => Multiply(m.ops.map(flattenAdds):_*)
    case Divide(numer, denom) => Divide(flattenAdds(numer), flattenAdds(denom))
    case Modulo(numer, denom) => Modulo(flattenAdds(numer), flattenAdds(denom))

    case Equality(left, right) => Equality(flattenAdds(left), flattenAdds(right))
    case LessThan(left, right) => LessThan(flattenAdds(left), flattenAdds(right))
    case GreaterThan(left, right) => GreaterThan(flattenAdds(left), flattenAdds(right))
    case LessThanOrEq(left, right) => LessThanOrEq(flattenAdds(left), flattenAdds(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(flattenAdds(left), flattenAdds(right))
    case Not(e) => Not(flattenAdds(e))

    case And(left, right) => And(flattenAdds(left).asInstanceOf[BoolExpr], flattenAdds(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(flattenAdds(left).asInstanceOf[BoolExpr], flattenAdds(right).asInstanceOf[BoolExpr])

    case Select(cond, iftrue, iffalse) => Select(flattenAdds(cond).asInstanceOf[BoolExpr], flattenAdds(iftrue), flattenAdds(iffalse))

    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, flattenAdds(indexExpr), deps.map(flattenAdds), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(flattenAdds))

    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(flattenAdds)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(flattenAdds)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(flattenAdds)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(flattenAdds)), axisId)

    case IndexRange(start, end) => IndexRange(
      start.map(s => flattenAdds(s)),
      end.map(e => flattenAdds(e)),
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(flattenAdds))

    case _ => expr
  }

  def addTermsWithConstantCoefficients(expr: Expr): Expr = expr match {
    case a: Add => {
      val terms = a.ops.collect {
        case Multiply(Const(_), x) => x
        case x => x
      }

      val summedTerms = terms.map { t =>
        val coeffs = a.ops.collect {
          case Multiply(Const(n), `t`) => n
          case `t` => 1
        }

        coeffs.sum match {
          case 0 => Const(0)
          case 1 => t
          case n => Multiply(Const(n), t)
        }
      }

      Add(summedTerms.map(addTermsWithConstantCoefficients):_*)
    }

    case m: Multiply => Multiply(m.ops.map(addTermsWithConstantCoefficients):_*)
    case Divide(numer, denom) => Divide(addTermsWithConstantCoefficients(numer), addTermsWithConstantCoefficients(denom))
    case Modulo(numer, denom) => Modulo(addTermsWithConstantCoefficients(numer), addTermsWithConstantCoefficients(denom))

    case Equality(left, right) => Equality(addTermsWithConstantCoefficients(left), addTermsWithConstantCoefficients(right))
    case LessThan(left, right) => LessThan(addTermsWithConstantCoefficients(left), addTermsWithConstantCoefficients(right))
    case GreaterThan(left, right) => GreaterThan(addTermsWithConstantCoefficients(left), addTermsWithConstantCoefficients(right))
    case LessThanOrEq(left, right) => LessThanOrEq(addTermsWithConstantCoefficients(left), addTermsWithConstantCoefficients(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(addTermsWithConstantCoefficients(left), addTermsWithConstantCoefficients(right))
    case Not(e) => Not(addTermsWithConstantCoefficients(e))

    case And(left, right) => And(addTermsWithConstantCoefficients(left).asInstanceOf[BoolExpr], addTermsWithConstantCoefficients(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(addTermsWithConstantCoefficients(left).asInstanceOf[BoolExpr], addTermsWithConstantCoefficients(right).asInstanceOf[BoolExpr])

    case Select(cond, iftrue, iffalse) => Select(addTermsWithConstantCoefficients(cond).asInstanceOf[BoolExpr], addTermsWithConstantCoefficients(iftrue), addTermsWithConstantCoefficients(iffalse))

    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, addTermsWithConstantCoefficients(indexExpr), deps.map(addTermsWithConstantCoefficients), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(addTermsWithConstantCoefficients))

    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(addTermsWithConstantCoefficients)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(addTermsWithConstantCoefficients)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(addTermsWithConstantCoefficients)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(addTermsWithConstantCoefficients)), axisId)

    case IndexRange(start, end) => IndexRange(
      start match { case Some(s) => Some(addTermsWithConstantCoefficients(s)) case None => None },
      end match { case Some(e) => Some(addTermsWithConstantCoefficients(e)) case None => None },
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(addTermsWithConstantCoefficients))

    case _ => expr
  }

  def simplifyMultiplies(expr: Expr): Expr = expr match {
    case m: Multiply if m.ops.exists(_.isInstanceOf[Multiply]) => {
      val multsOps = m.ops.collect { case m: Multiply => m.ops }.flatten
      val nonMults = m.ops.filter(!_.isInstanceOf[Multiply])
      Multiply(multsOps ++ nonMults:_*)
    }

    case m: Multiply if m.ops.exists(_.isInstanceOf[Add]) => {
      val add = m.ops.collectFirst { case a: Add => a }.get
      val addIndex = m.ops.indexOf(add)

      val otherMultOps = m.ops.take(addIndex) ++ m.ops.drop(addIndex+1)

      Add(add.ops.map(a => simplifyMultiplies(Multiply(otherMultOps :+ a:_*))):_*)
    }

    case Multiply(sk: IndexSkipFunc, c: Const) => sk.copy(indexExpr = simplifyMultiplies(sk.indexExpr * c))
    case Multiply(c: Const, sk: IndexSkipFunc) => sk.copy(indexExpr = simplifyMultiplies(sk.indexExpr * c))

    case a: Add => Add(a.ops.map(simplifyMultiplies):_*)
    case Divide(numer, denom) => Divide(simplifyMultiplies(numer), simplifyMultiplies(denom))
    case Modulo(numer, denom) => Modulo(simplifyMultiplies(numer), simplifyMultiplies(denom))
    case Equality(left, right) => Equality(simplifyMultiplies(left), simplifyMultiplies(right))
    case LessThan(left, right) => LessThan(simplifyMultiplies(left), simplifyMultiplies(right))
    case GreaterThan(left, right) => GreaterThan(simplifyMultiplies(left), simplifyMultiplies(right))
    case LessThanOrEq(left, right) => LessThanOrEq(simplifyMultiplies(left), simplifyMultiplies(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(simplifyMultiplies(left), simplifyMultiplies(right))
    case Not(e) => Not(simplifyMultiplies(e))
    case And(left, right) => And(simplifyMultiplies(left).asInstanceOf[BoolExpr], simplifyMultiplies(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(simplifyMultiplies(left).asInstanceOf[BoolExpr], simplifyMultiplies(right).asInstanceOf[BoolExpr])
    case Select(cond, iftrue, iffalse) => Select(simplifyMultiplies(cond).asInstanceOf[BoolExpr], simplifyMultiplies(iftrue), simplifyMultiplies(iffalse))
    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, simplifyMultiplies(indexExpr), deps.map(simplifyMultiplies), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(simplifyMultiplies))
    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(simplifyMultiplies)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(simplifyMultiplies)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(simplifyMultiplies)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(simplifyMultiplies)), axisId)

    case IndexRange(start, end) => IndexRange(
      start match { case Some(s) => Some(simplifyMultiplies(s)) case None => None },
      end match { case Some(e) => Some(simplifyMultiplies(e)) case None => None },
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(simplifyMultiplies))

    case _ => expr
  }

  def addSkipFuncs(expr: Expr): Expr = expr match {
    case a: Add => {
      val skipFuncs = a.ops.collect { case sk: IndexSkipFunc => sk }
      val notSkipFuncs = a.ops.filter(!_.isInstanceOf[IndexSkipFunc]).map(addSkipFuncs)

      val skipFuncsToSum = skipFuncs.groupMap(sk => (sk.index, sk.dependencies, sk.isSync))(_.indexExpr)

      val summedSkipFuncs: Seq[IndexSkipFunc] = skipFuncsToSum.toSeq.map { case ((index, deps, isSync), indexExprs) =>
        val summedIndexExprs = if (indexExprs.size > 1) Add(indexExprs:_*) else indexExprs.head
        IndexSkipFunc(index, summedIndexExprs, deps, isSync)
      }

      val addOps = (summedSkipFuncs ++ notSkipFuncs).toList

      addOps match {
        case e :: Nil => e
        case _ => Add(addOps:_*)
      }
    }

    case _ => expr
  }

  def simplifyBools(expr: Expr): Expr = expr match {
    case Equality(x, y) if x == y => True
    case Equality(Const(x), Const(y)) if x != y => False
    case Equality(left, right) => Equality(simplifyBools(left), simplifyBools(right))

    case LessThan(Const(x), Const(y)) if x < y => True
    case LessThan(Const(_), Const(_)) => False
    case LessThan(x, Const(0)) => LessThan(simplifyBools(x), Const(0))
    case LessThan(left, right) => LessThan(simplifyBools(left - right), Const(0))

    case LessThanOrEq(Const(x), Const(y)) if x <= y => True
    case LessThanOrEq(Const(_), Const(_)) => False
    case LessThanOrEq(x, Const(0)) => LessThanOrEq(simplifyBools(x), Const(0))
    case LessThanOrEq(left, right) => LessThanOrEq(simplifyBools(left - right), Const(0))

    case GreaterThan(Const(x), Const(y)) if x > y => True
    case GreaterThan(Const(_), Const(_)) => False
    case GreaterThan(x, Const(0)) => GreaterThan(simplifyBools(x), Const(0))
    case GreaterThan(left, right) => GreaterThan(simplifyBools(left - right), Const(0))

    case GreaterThanOrEq(Const(x), Const(y)) if x >= y => True
    case GreaterThanOrEq(Const(_), Const(_)) => False
    case GreaterThanOrEq(x, Const(0)) => GreaterThanOrEq(simplifyBools(x), Const(0))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(simplifyBools(left - right), Const(0))

    case Not(True) => False
    case Not(False) => True
    case Not(b) => Not(simplifyBools(b))

    case Or(_, True) | Or(True, _) => True
    case Or(left, False) => simplifyBools(left)
    case Or(False, right) => simplifyBools(right)
    case Or(left, right) => Or(simplifyBools(left).asInstanceOf[BoolExpr], simplifyBools(right).asInstanceOf[BoolExpr])

    case And(_, False) | Or(False, _) => False
    case And(left, True) => simplifyBools(left)
    case And(True, right) => simplifyBools(right)
    case And(left, right) => And(simplifyBools(left).asInstanceOf[BoolExpr], simplifyBools(right).asInstanceOf[BoolExpr])

    case a: Add => Add(a.ops.map(simplifyBools):_*)
    case m: Multiply => Multiply(m.ops.map(simplifyBools):_*)
    case Divide(numer, denom) => Divide(simplifyBools(numer), simplifyBools(denom))
    case Modulo(numer, denom) => Modulo(simplifyBools(numer), simplifyBools(denom))
    case Select(cond, iftrue, iffalse) => Select(simplifyBools(cond).asInstanceOf[BoolExpr], simplifyBools(iftrue), simplifyBools(iffalse))
    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, simplifyBools(indexExpr), deps.map(simplifyBools), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(simplifyBools))
    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(simplifyBools)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(simplifyBools)))

    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(simplifyBools)), coordId)

    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(simplifyBools)), axisId)

    case IndexRange(start, end) => IndexRange(
      start match { case Some(s) => Some(simplifyBools(s)) case None => None },
      end match { case Some(e) => Some(simplifyBools(e)) case None => None },
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(simplifyBools))

    case _ => expr
  }

  def simplifySelects(expr: Expr): Expr = expr match {
    case Select(True, iftrue, _) => simplifySelects(iftrue)
    case Select(False, _, iffalse) => simplifySelects(iffalse)
    case Select(_, iftrue, iffalse) if iftrue == iffalse => simplifySelects(iftrue)
    case Select(cond, iftrue, iffalse) => Select(simplifySelects(cond).asInstanceOf[BoolExpr], simplifySelects(iftrue), simplifySelects(iffalse))

    case a: Add => Add(a.ops.map(simplifySelects):_*)
    case m: Multiply => Multiply(m.ops.map(simplifySelects):_*)
    case Divide(numer, denom) => Divide(simplifySelects(numer), simplifySelects(denom))
    case Modulo(numer, denom) => Modulo(simplifySelects(numer), simplifySelects(denom))
    case Equality(left, right) => Equality(simplifySelects(left), simplifySelects(right))
    case LessThan(left, right) => LessThan(simplifySelects(left), simplifySelects(right))
    case GreaterThan(left, right) => GreaterThan(simplifySelects(left), simplifySelects(right))
    case LessThanOrEq(left, right) => LessThanOrEq(simplifySelects(left), simplifySelects(right))
    case GreaterThanOrEq(left, right) => GreaterThanOrEq(simplifySelects(left), simplifySelects(right))
    case Not(e) => Not(simplifySelects(e))
    case And(left, right) => And(simplifySelects(left).asInstanceOf[BoolExpr], simplifySelects(right).asInstanceOf[BoolExpr])
    case Or(left, right) => Or(simplifySelects(left).asInstanceOf[BoolExpr], simplifySelects(right).asInstanceOf[BoolExpr])
    case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, simplifySelects(indexExpr), deps.map(simplifySelects), isSync)
    case Indexed(variable, indices) => Indexed(variable, indices.map(simplifySelects))
    case Found(Indexed(variable, indices)) => Found(Indexed(variable, indices.map(simplifySelects)))
    case Unavailable(Indexed(variable, indices)) => Unavailable(Indexed(variable, indices.map(simplifySelects)))
    case CoordOf(Indexed(variable, indices), coordId) => CoordOf(Indexed(variable, indices.map(simplifySelects)), coordId)
    case AxisSpan(Indexed(variable, indices), axisId) => AxisSpan(Indexed(variable, indices.map(simplifySelects)), axisId)

    case IndexRange(start, end) => IndexRange(
      start match { case Some(s) => Some(simplifySelects(s)) case None => None },
      end match { case Some(e) => Some(simplifySelects(e)) case None => None },
    )

    case custom: Custom => custom.copy(ops = custom.ops.map(simplifySelects))

    case _ => expr
  }

  def apply(expr: Expr): Expr = {
    var result = expr

    var i = 0; val max_passes = 10
    while (i < 10) {
      val new_result = simplifySelects(simplifyBools(addSkipFuncs(simplifyMultiplies(addTermsWithConstantCoefficients(constModulo(constAdd(constMult(flattenAdds(result)))))))))
      if (result == new_result) {
        i = max_passes
      } else {
        i += 1
      }
      result = new_result
    }

    result
  }

  /*
  Now, we get to passes which aren't simply algebraic simplifications
   */

  def extractIndexed(expr: Expr): Seq[Indexed] = expr match {
    case indexed: Indexed => Seq(indexed) ++ indexed.indices.flatMap(extractIndexed)
    case add: Add => add.ops.flatMap(extractIndexed)
    case mult: Multiply => mult.ops.flatMap(extractIndexed)
    case Divide(numer, denom) => extractIndexed(numer) ++ extractIndexed(denom)
    case Modulo(numer, denom) => extractIndexed(numer) ++ extractIndexed(denom)
    case Equality(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case LessThan(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case GreaterThan(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case LessThanOrEq(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case GreaterThanOrEq(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case Not(e) => extractIndexed(e)
    case And(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case Or(left, right) => extractIndexed(left) ++ extractIndexed(right)
    case Select(cond, iftrue, iffalse) => extractIndexed(cond) ++ extractIndexed(iftrue) ++ extractIndexed(iffalse)
    case Found(indexed) => extractIndexed(indexed)
    case Unavailable(indexed) => extractIndexed(indexed)
    case CoordOf(indexed, _) => extractIndexed(indexed)
    case AxisSpan(indexed, _) => extractIndexed(indexed)
    case Custom(ops, _, _) => ops.flatMap(extractIndexed)
    case _ => Seq()
  }

  def extractSkipFuncs(expr: Expr): Seq[IndexSkipFunc] = expr match {
    case skipFunc @ IndexSkipFunc(_, indexExpr, dependencies, _) => Seq(skipFunc) ++ extractSkipFuncs(indexExpr) ++ dependencies.flatMap(extractSkipFuncs)
    case indexed: Indexed => indexed.indices.flatMap(extractSkipFuncs)
    case add: Add => add.ops.flatMap(extractSkipFuncs)
    case mult: Multiply => mult.ops.flatMap(extractSkipFuncs)
    case Divide(numer, denom) => extractSkipFuncs(numer) ++ extractSkipFuncs(denom)
    case Modulo(numer, denom) => extractSkipFuncs(numer) ++ extractSkipFuncs(denom)
    case Equality(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case LessThan(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case GreaterThan(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case LessThanOrEq(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case GreaterThanOrEq(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case Not(e) => extractSkipFuncs(e)
    case And(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case Or(left, right) => extractSkipFuncs(left) ++ extractSkipFuncs(right)
    case Select(cond, iftrue, iffalse) => extractSkipFuncs(cond) ++ extractSkipFuncs(iftrue) ++ extractSkipFuncs(iffalse)
    case Found(indexed) => extractSkipFuncs(indexed)
    case Unavailable(indexed) => extractSkipFuncs(indexed)
    case CoordOf(indexed, _) => extractSkipFuncs(indexed)
    case AxisSpan(indexed, _) => extractSkipFuncs(indexed)
    case Custom(ops, _, _) => ops.flatMap(extractSkipFuncs)
    case _ => Seq()
  }

  def containsSkipFunc(expr: Expr): Boolean = expr match {
    case _: IndexSkipFunc => true
    case indexed: Indexed => indexed.indices.exists(containsSkipFunc)
    case add: Add => add.ops.exists(containsSkipFunc)
    case mult: Multiply => mult.ops.exists(containsSkipFunc)
    case Divide(numer, denom) => containsSkipFunc(numer) || containsSkipFunc(denom)
    case Modulo(numer, denom) => containsSkipFunc(numer) || containsSkipFunc(denom)
    case Equality(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case LessThan(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case GreaterThan(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case LessThanOrEq(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case GreaterThanOrEq(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case Not(e) => containsSkipFunc(e)
    case And(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case Or(left, right) => containsSkipFunc(left) || containsSkipFunc(right)
    case Select(cond, iftrue, iffalse) => containsSkipFunc(cond) || containsSkipFunc(iftrue) || containsSkipFunc(iffalse)
    case Found(indexed) => containsSkipFunc(indexed)
    case Unavailable(indexed) => containsSkipFunc(indexed)
    case CoordOf(indexed, _) => containsSkipFunc(indexed)
    case AxisSpan(indexed, _) => containsSkipFunc(indexed)
    case Custom(ops, _, _) => ops.exists(containsSkipFunc)
    case _ => false
  }

  def containsFound(expr: Expr, inVar: Input): Boolean = {
    def recurse(e: Expr): Boolean = containsFound(e, inVar)
    expr match {
      case Found(Indexed(`inVar`, _)) => true
      case Found(indexed) => recurse(indexed)
      case add: Add => add.ops.exists(recurse)
      case mult: Multiply => mult.ops.exists(recurse)
      case Divide(numer, denom) => recurse(numer) || recurse(denom)
      case Modulo(numer, denom) => recurse(numer) || recurse(denom)
      case Equality(left, right) => recurse(left) || recurse(right)
      case LessThan(left, right) => recurse(left) || recurse(right)
      case GreaterThan(left, right) => recurse(left) || recurse(right)
      case LessThanOrEq(left, right) => recurse(left) || recurse(right)
      case GreaterThanOrEq(left, right) => recurse(left) || recurse(right)
      case Not(e) => recurse(e)
      case And(left, right) => recurse(left) || recurse(right)
      case Or(left, right) => recurse(left) || recurse(right)
      case Select(cond, iftrue, iffalse) => recurse(cond) || recurse(iftrue) || recurse(iffalse)
      case Indexed(_, indices) => indices.exists(recurse)
      case IndexSkipFunc(_, indexExpr, deps, _) => recurse(indexExpr) || deps.exists(recurse)
      case Unavailable(indexed) => recurse(indexed)
      case CoordOf(indexed, _) => recurse(indexed)
      case AxisSpan(indexed, _) => recurse(indexed)
      case Custom(ops, _, _) => ops.exists(recurse)
      case _ => false
    }
  }

  def replaceBounds(expr: Expr, bounds: SMap[Index, (Int, Int)]): Expr = {
    val dict: SMap[Expr, Expr] = bounds.toSeq.flatMap { case (ind, (l, h)) =>
      Seq((ind.lowerBound, Const(l)), (ind.upperBound, Const(h)))
    }.toMap

    replaceExprs(expr, dict)
  }

  def replaceIndex(expr: Expr, values: SMap[Index, Int]): Expr =
    replaceExprs(expr, values.view.mapValues(c => Const(c)).toMap)

  def replaceIndexInIO(expr: Expr, values: SMap[Index, Expr]): Expr = {
    // There are some "asInstanceOf" calls in this function, but they're just here to deal with Scala's annoying
    // typechecker

    val ios = extractIndexed(expr).filter(!_.isIntermediate)

    val values_ = values.toSeq.map(t => (t._1.asInstanceOf[Expr], t._2)).toMap

    val replacements = ios.map { io =>
      (io.asInstanceOf[Expr], replaceExprs(io, values_, terminateAtIntermediate = true))
    }.toMap

    replaceExprs(expr, replacements)
  }

  def replaceExprs(expr: Expr, values: SMap[Expr, Expr], terminateAtIntermediate: Boolean = false): Expr = if (values.isEmpty) expr else {
    def recurse(expr: Expr, values: SMap[Expr, Expr]) = replaceExprs(expr, values, terminateAtIntermediate)

    expr match {
      case _: Expr if values.contains(expr) => values(expr)

      case a: Add => Add(a.ops.map(op => recurse(op, values)):_*)
      case m: Multiply => Multiply(m.ops.map(op => recurse(op, values)):_*)
      case Divide(numer, denom) => Divide(recurse(numer, values), recurse(denom, values))
      case Modulo(numer, denom) => Modulo(recurse(numer, values), recurse(denom, values))
      case Equality(left, right) => Equality(recurse(left, values), recurse(right, values))
      case LessThan(left, right) => LessThan(recurse(left, values), recurse(right, values))
      case GreaterThan(left, right) => GreaterThan(recurse(left, values), recurse(right, values))
      case LessThanOrEq(left, right) => LessThanOrEq(recurse(left, values), recurse(right, values))
      case GreaterThanOrEq(left, right) => GreaterThanOrEq(recurse(left, values), recurse(right, values))
      case Not(e) => Not(recurse(e, values))
      case Or(left, right) => Or(recurse(left, values).asInstanceOf[BoolExpr], recurse(right, values).asInstanceOf[BoolExpr])
      case And(left, right) => And(recurse(left, values).asInstanceOf[BoolExpr], recurse(right, values).asInstanceOf[BoolExpr])
      case Select(cond, iftrue, iffalse) => Select(recurse(cond, values).asInstanceOf[BoolExpr], recurse(iftrue, values), recurse(iffalse, values))

      case Found(indexed) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => Found(indexed)
          case e: Expr if extractIndexed(e).isEmpty => True
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }
      case Unavailable(indexed) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => Unavailable(indexed)
          case e: Expr if extractIndexed(e).isEmpty => False
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }

      case CoordOf(indexed, coordId) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => CoordOf(indexed, coordId)
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }

      case AxisSpan(indexed, axisId) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => AxisSpan(indexed, axisId)
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }

      case Indexed(variable, indices) if variable.isIO || !terminateAtIntermediate =>
        Indexed(variable, indices.map(ind => recurse(ind, values)))

      case IndexRange(start, end) => IndexRange(
        start.map(s => recurse(s, values)),
        end.map(e => recurse(e, values)),
      )

      case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, recurse(indexExpr, values), deps.map(d => recurse(d, values)), isSync)

      case custom: Custom => custom.copy(ops = custom.ops.map(op => recurse(op, values)))

      case _ => expr
    }
  }

  def replaceVariable(expr: Expr, values: SMap[Variable, Variable]): Expr = if (values.isEmpty) expr else {
    def recurse(expr: Expr, values: SMap[Variable, Variable]) = replaceVariable(expr, values)

    expr match {
      case Indexed(variable, indices) if values.contains(variable) =>
        Indexed(values(variable), indices.map(ind => recurse(ind, values)))

      case a: Add => Add(a.ops.map(op => recurse(op, values)):_*)
      case m: Multiply => Multiply(m.ops.map(op => recurse(op, values)):_*)
      case Divide(numer, denom) => Divide(recurse(numer, values), recurse(denom, values))
      case Modulo(numer, denom) => Modulo(recurse(numer, values), recurse(denom, values))
      case Equality(left, right) => Equality(recurse(left, values), recurse(right, values))
      case LessThan(left, right) => LessThan(recurse(left, values), recurse(right, values))
      case GreaterThan(left, right) => GreaterThan(recurse(left, values), recurse(right, values))
      case LessThanOrEq(left, right) => LessThanOrEq(recurse(left, values), recurse(right, values))
      case GreaterThanOrEq(left, right) => GreaterThanOrEq(recurse(left, values), recurse(right, values))
      case Not(e) => Not(recurse(e, values))
      case Or(left, right) => Or(recurse(left, values).asInstanceOf[BoolExpr], recurse(right, values).asInstanceOf[BoolExpr])
      case And(left, right) => And(recurse(left, values).asInstanceOf[BoolExpr], recurse(right, values).asInstanceOf[BoolExpr])
      case Select(cond, iftrue, iffalse) => Select(recurse(cond, values).asInstanceOf[BoolExpr], recurse(iftrue, values), recurse(iffalse, values))

      case Found(indexed) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => Found(indexed)
          case e: Expr if extractIndexed(e).isEmpty => True
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }
      case Unavailable(indexed) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => Unavailable(indexed)
          case e: Expr if extractIndexed(e).isEmpty => False
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }

      case CoordOf(indexed, coordId) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => CoordOf(indexed, coordId)
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }

      case AxisSpan(indexed, axisId) =>
        val replacedIndexed = recurse(indexed, values)
        replacedIndexed match {
          case indexed: Indexed if indexed.isIOInput => AxisSpan(indexed, axisId)
          case e => throw new Exception(s"I'm not sure what to set 'Found' to when the index is replaced with this: $e")
        }

      case IndexRange(start, end) => IndexRange(
        start.map(s => recurse(s, values)),
        end.map(e => recurse(e, values)),
      )

      case IndexSkipFunc(index, indexExpr, deps, isSync) => IndexSkipFunc(index, recurse(indexExpr, values), deps.map(d => recurse(d, values)), isSync)

      case custom: Custom => custom.copy(ops = custom.ops.map(op => recurse(op, values)))

      case _ => expr
    }
  }

  def passThruVariables(assignments: Iterable[Assignment]): Set[Intermediate] = {
    // This function is trying to distinguish "pass-through variables", which simply flow through the spatial array
    // without being modified, from "mutated variables" which are updated as they flow through the array. For example,
    // for matmuls, "a" and "b" is pass-through while "c" is mutated.

    val allIntermediates = assignments.map(_.dst).collect { case Indexed(variable: Intermediate, _) => variable }

    def isPassThru(variable: Intermediate): Boolean = {
      assignments.filter(_.dst.variable == variable)
        .filter(asgn => extractIndexed(asgn.src).exists(_.variable.isInstanceOf[Intermediate]))
        .forall {
          case Assignment(_, Indexed(`variable`, _)) => true
          case _ => false.asInstanceOf[Boolean]
        }
    }

    allIntermediates.filter(isPassThru).toSet
  }

  def canExpandIndicesFromRfInputs(skips: Seq[Skip], assignments: Seq[Assignment]) = {
    // Check if any Skip statements open the possibility of avoiding the need for an explicit coordLookup module

    skips.zipWithIndex.flatMap { case (skip, skipId) =>
      val otherSkips = without(skips, skipId)
      if (otherSkips.exists(_.index == skip.index)) {
        None
      } else {
        val dependencies = skip.cond.indicesInExpr.filter(_ != skip.index)
        if (otherSkips.exists(o => dependencies.contains(o.index))) {
          None
        } else {
          // This skip statement is completely independent, which makes it easier to see if it can be expanded straight
          // from the regfile
          val indicesAreNotTransformedInCond = skip.cond match {
            case Equality(x: Indexed, _: Const /* this const will usually be 0, but it doesn't necessarily have to be */) =>
              x.indices.forall(x => x.isInstanceOf[Const] || x.isInstanceOf[Index])
            case Equality(_: Index, _: Const) => true
            case _ => false // TODO come up with a better pass which can check if indices were transformed in the cond
          }

          if (indicesAreNotTransformedInCond) {
            val indicesAreNotTransformedInIoConns = assignments.flatMap(_.ioConns).flatMap(t => Seq(t._1,t._2)).forall { ioDstOrSrc =>
              ioDstOrSrc.indices.map {
                case add: Add =>
                  constAdd(flattenAdds(add))
                case expr => expr
              }.forall {
                case _: Const | _: Index | _: IndexLowerBound | Add(_: IndexLowerBound, _: Const) | Add(_: IndexUpperBound, _: Const) => true
                case IndexSkipFunc(_, _: Index, dependencies, _) => dependencies.forall {
                  case _: Const | _: Index | _: IndexLowerBound | Add(_: IndexLowerBound, _: Const) | Add(_: IndexUpperBound, _: Const) => true
                }
                case _ => false
              }
            }

            if (indicesAreNotTransformedInIoConns) {
              skip.cond match {
                case Equality(ioInIndexed @ Indexed(inVar: Input, _), _: Const) =>
                  val passThruVars = passThruVariables(assignments)

                  val expandedIndexCarrier = passThruVars.find { interVar =>
                    assignments.filter(_.dst.variable == interVar).exists {
                      case Assignment(dst, `ioInIndexed`) => true
                      case _ => false
                    }
                  }

                  expandedIndexCarrier.flatMap { interVar =>
                    val expandedIndexLocation = ioInIndexed.indices.indexOf(skip.index)
                    Option.when(expandedIndexLocation >= 0)((skip, interVar, ioInIndexed, expandedIndexLocation))
                  }

                case _ => None
              }
            } else
              None
          } else
            None
        }
      }
    }
  }

  def expandCompressedIndices(assignments: Seq[Assignment], indices: Seq[Index]): Seq[Assignment] = {
    val compressed2Expanded = indices.map { compressedInd =>
      val expandedInd = {
        if (compressedInd.isSkipped) {
          val depsSorted = indices.filter(compressedInd.dependencies.contains)
          IndexSkipFunc(compressedInd, compressedInd, depsSorted, compressedInd.isSyncSkipped)
        } else {
          compressedInd
        }
      }

      (compressedInd, expandedInd)
    }.toMap

    assignments.map { case Assignment(dst, src) =>
      val newDst = replaceIndexInIO(dst, compressed2Expanded)
      assert(newDst.isInstanceOf[Indexed])

      Assignment(
        dst = newDst.asInstanceOf[Indexed],
        src = replaceIndexInIO(src, compressed2Expanded)
      )(None)
    }
  }

  def unrolledTiledIndices(assignments: Iterable[Assignment], indices: List[Index]): (Seq[Assignment], Seq[Index]) = {
    // This function is pretty long and involved. You've been warned

    val isUnrolled = {
      def isUnrolledHelper(inds: List[Index]): List[Boolean] = inds match {
        case Nil => Nil
        case i :: is if i.nSubtiles == 0 => true :: isUnrolledHelper(is)
        case i :: j :: is if i(1) == j => true :: isUnrolledHelper(j :: is)
        case _ :: is => false :: isUnrolledHelper(is)
      }

      isUnrolledHelper(indices)
    }

    val indexToUnrollId = isUnrolled.indexOf(false)
    val allIndsUnrolled = indexToUnrollId == -1

    if (allIndsUnrolled) {
      (assignments.toSeq, indices.toSeq)
    } else {
      val indexToUnroll = indices(indexToUnrollId)

      /* There are multiple ways to unroll an index. To illustrate, consider how the following three statements are
         unrolled to the following four statements:

         Original:
         a(i, j, k) = a(i, j-1, k)
         b(i, j, k) = b(i-1, j, k)
         c(i, j, k) = c(i, j, k-1)

         Unrolled:
         a(i, j, ko, ki) = a(i, j-1, ko, ki)
         b(i, j, ko, ki) = b(i-1, j, ko, ki)
         c(i, j, ko, 3)  = c(i, j, ko, 2) + c(i, j, ko-1, 3) + ...
         c(i, j, ko, ki) = c(i, j, ko, ki-1) + ...

         We refer to these as:
         * Passthrough: Where the unrolled index has a diff of -0
         * SubtractedBy1: Where the unrolled index has a diff of -1, and we're NOT unrolling the edge case, and the
              srcIndexed is in the outermost Add or Multiply
         * Edge: Same as SubtractedBy1 but we ARE unrolling (one of the) the edge cases

         Note that IO variables aren't unrolled in the same way, instead, they're just expanded by multiplying out
         the untiled indices.
       */

      def unrollIntermediate(asg: Assignment): Iterable[Assignment] = {
        // This unrolls intra-array assignments

        def passThroughAsgs(asg: Assignment): Iterable[Indexed] = {
          asg.connectionVectorsWithSrc(indices, boundsMap = scala.collection.Map.empty).filter { case (conn, src) =>
            assert(asg.dst.indices(indexToUnrollId) == indexToUnroll, "we're assuming that the dst isn't doing any weird index math when unrolling subtiles")

            // We can't handle cases where the index to unroll is i.upperBound or something like that
            src.indicesInExpr.contains(indexToUnroll) &&
              conn.diff.toSeq(indexToUnrollId) == 0
          }
            .map(_._2)
        }

        def subtractBy1Asgs(asg: Assignment): Iterable[Indexed] = {
          asg.connectionVectorsWithSrc(indices, boundsMap = scala.collection.Map.empty).filter { case (conn, src) =>
            assert(asg.dst.indices(indexToUnrollId) == indexToUnroll, "we're assuming that the dst isn't doing any weird index math when unrolling subtiles")

            val diff = conn.diff.toSeq(indexToUnrollId)

            val srcIsInOutermostOp = asg.src match {
              case `src` => true
              case a: Add => a.ops.contains(src)
              case m: Multiply => m.ops.contains(src)
              case _ => false
            }

            src.indicesInExpr.contains(indexToUnroll) &&
              (diff == 1 && srcIsInOutermostOp)
          }
            .map(_._2)
        }

        val passthrus = passThroughAsgs(asg).toSeq
        val subtractBy1s = subtractBy1Asgs(asg).toSeq

        def helperDst(indexed: Indexed, edge: Int): Indexed = {
          val inds = indexed.indices

          val unrolledIndices = (indexToUnroll +: indexToUnroll.subtiles.toSeq)
            .zipWithIndex.map {
            case (uind, i) if i > indexToUnroll.nSubtiles - edge => uind.upperBound - 1
            case (uind, _) => uind
          }

          indexed.copy(indices=inds.take(indexToUnrollId) ++ unrolledIndices ++ inds.drop(indexToUnrollId+1))
        }

        def helperSrc(expr: Expr, edge: Int, op: Iterable[Expr] => Expr, opLevels: Int): Expr = expr match {
          // Note: "opLevels" is just used to make sure that we don't try to unroll something which is too deeply
          // nested in additions or multiplications

          case indexed @ Indexed(_, inds) if passthrus.contains(indexed) =>
            indexed.copy(indices=inds.take(indexToUnrollId+1) ++ indexToUnroll.subtiles ++ inds.drop(indexToUnrollId+1))

          case indexed @ Indexed(_, inds) if subtractBy1s.contains(indexed) && opLevels <= 1 =>
            val operands = (0 to edge).map { connection =>
              val unrolledIndices = (indexToUnroll +: indexToUnroll.subtiles.toSeq)
                .zipWithIndex.map {
                  case (uind, i) if i > indexToUnroll.nSubtiles - edge => uind.upperBound - 1
                  case (uind, _) => uind
                }
                .zipWithIndex.map {
                  case (uind, i) if i == indexToUnroll.nSubtiles - connection => uind - 1
                  case (uind, _) => uind
                }

              indexed.copy(indices=inds.take(indexToUnrollId) ++ unrolledIndices ++ inds.drop(indexToUnrollId+1))
            }

            if (operands.size > 1)
              op(operands)
            else
              operands.head

          case _: Indexed => throw new Exception("Unrolling something which can't be unrolled")

          case a: Add =>
            val operation = (os: Iterable[Expr]) => Add(os.toSeq:_*)
            flattenAdds(Add(a.ops.map(o => helperSrc(o, edge, operation, opLevels+1)):_*))

          case m: Multiply =>
            val operation = (os: Iterable[Expr]) => Multiply(os.toSeq:_*)
            simplifyMultiplies(Multiply(m.ops.map(o => helperSrc(o, edge, operation, opLevels+1)):_*))

          case _ => expr
        }

        val edges = if (subtractBy1s.isEmpty) 0 else indexToUnroll.nSubtiles

        (0 to edges).reverse.map { edge =>
          val dst = helperDst(asg.dst, edge)
          val src = Passes({
            val unrolledIndices = indices.take(indexToUnrollId+1) ++ indexToUnroll.subtiles ++ indices.drop(indexToUnrollId+1)
            val dstIndexMap: SMap[Expr, Expr] = unrolledIndices.zip(dst.indices).toMap
            val s = helperSrc(asg.src, edge, (_: Any) => throw new Exception("op not specified"), 0)
            replaceExprs(s, dstIndexMap)
          })

          Assignment(dst, src)(None)
        }
      }

      def unrollIO(asg: Assignment): Assignment = {
        val ioIndexReplacement = {
          val ops = (0 to indexToUnroll.nSubtiles).map { i =>
            val bounds: Seq[Expr] = (i+1 to indexToUnroll.nSubtiles).map(j => indexToUnroll(j).upperBound).toSeq

            if (bounds.isEmpty)
              indexToUnroll(i)
            else
              Multiply(indexToUnroll(i) +: bounds:_*)
          }

          Add(ops:_*)
        }

        val ioUpperReplacement = Multiply(indexToUnroll.upperBound +: indexToUnroll.subtiles.map(_.upperBound).toSeq:_*)

        val ioDict = scala.collection.Map((indexToUnroll, ioIndexReplacement), (indexToUnroll.upperBound, ioUpperReplacement))

        val interIndexReplacement = indexToUnroll +: indexToUnroll.subtiles.toSeq
        val interLowerReplacement = indexToUnroll.lowerBound +: indexToUnroll.subtiles.map(_.lowerBound).toSeq

        val indUpperBound = indexToUnroll.upperBound - 1
        val interUpperReplacement = indUpperBound +: indexToUnroll.subtiles.map(_.upperBound - 1).toSeq

        def helper(expr: Expr, isOutput: Boolean): Expr = expr match {
          case indexed @ Indexed(v, inds) if !v.isInstanceOf[Intermediate] =>
            val unrolledIndices = inds.map(ind => replaceExprs(ind, ioDict))
            indexed.copy(indices = unrolledIndices)

          case indexed @ Indexed(_, inds) =>
            val unrolledIndex = inds(indexToUnrollId) match {
              case `indexToUnroll` => interIndexReplacement
              case `indUpperBound` if isOutput => interUpperReplacement
              case indexToUnroll.lowerBound if !isOutput => interLowerReplacement
              case default => throw new Exception(s"sorry, untiling can't handle complex equations: $default")
            }

            indexed.copy(indices = inds.take(indexToUnrollId) ++ unrolledIndex ++ inds.drop(indexToUnrollId+1))

          case a: Add => Add(a.ops.map(o => helper(o, isOutput)):_*)
          case m: Multiply => Add(m.ops.map(o => helper(o, isOutput)):_*)

          case _ => expr
        }

        val isOutput = asg.dst.variable.isInstanceOf[Output]

        val dst = helper(asg.dst, isOutput).asInstanceOf[Indexed]
        val src = helper(asg.src, isOutput)

        Assignment(dst, src)(None)
      }

      // Now, we just identify which assignment needs to be unrolled using which function, and unroll it
      val unrolledAssignments = assignments.flatMap { asg =>
        val isIO = asg.dst.variable.isInstanceOf[Output] ||
          Passes.extractIndexed(asg.src).exists(_.variable.isInstanceOf[Input])

        if (isIO) {
          Seq(unrollIO(asg))
        } else {
          unrollIntermediate(asg)
        }
      }

      unrolledTiledIndices(unrolledAssignments,
        indices.take(indexToUnrollId+1) ++ indexToUnroll.subtiles ++ indices.drop(indexToUnrollId+1))
    }
  }
}
