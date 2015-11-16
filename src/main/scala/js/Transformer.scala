package js

import js.JsToken.Operator._
import js.AstSugar._

/**
 * Created by Kamil on 24.09.2015.
 */
object Transformer {

  import ast._

  def transformLVAL(lval: LVAL): LVAL = lval match {
    case _: VarAccess => lval
    case Access(from, prop) => Access(transform(from), prop)
    case Subscript(arr, idx) => Subscript(transform(arr), transform(idx))
  }

  def transform(node: Expression): Expression = node match {
    case Block(body) => Block(body map transform)
    case Access(from, prop) => Access(transform(from), prop)
    case ArrayLiteral(values) => ArrayLiteral(values map transform)
    case Assignement(lval, rval) => Assignement(transformLVAL(lval), transform(rval))
    case Call(func, params, Seq(), None) => Call(transform(func), params map transform, Seq(), None)
    case Call(func, params, Seq(), Some(block)) =>
      val blockFunc = Function(block.params, if(block.body.size == 1) block.body.head else Block(block.body))
      transform(Call(func, params :+ blockFunc, Seq(), None))
    case Call(func, params, nparams, block) =>
      transform(Call(func, params :+ ObjectLiteral(nparams), Seq(), block))
    case VanillaFor(init, cond, iter, body) => VanillaFor(transform(init), transform(cond), transform(iter), transform(body))
    case Function(params, body: Block) => Function(params, transform(body))
    case Function(params, body) => transform(Function(params, Block(Seq(transform(Return(body))))))
    case FunctionDef(name, params, body: Block) => FunctionDef(name, params, transform(body))
    case FunctionDef(name, params, body) => transform(FunctionDef(name, params, Block(Seq(Return(body)))))
    case If(cond, then, els) => If(transform(cond), transform(then), els map transform)
    case ObjectLiteral(props) => ObjectLiteral(props map (p => p.copy(_2 = transform(p._2))))
    case OperatorAssignement(lval, op, rval) => OperatorAssignement(transformLVAL(lval), op, transform(rval))
    case OperatorExpression(left, op, right) => OperatorExpression(transform(left), op, transform(right))
    case Paren(exp) => Paren(transform(exp))
    case PatternMatch(e, pats) =>
      val ParamName = "_patmat$param"
      val ifs = pats flatMap {
        case MatchCase(pattern, body) =>
          val (pre, check) = patternCheckSterilize(pattern, VarAccess(ParamName))
          val bind = patternBindSterilize(pattern, VarAccess(ParamName))
          pre ++: Seq(
            If(check, Block(bind :+ Return(body)), None)
          )
      }
      val error = Throw(OperatorExpression(StringValue("Pattern match failed on value of"), Plus, VarAccess(ParamName)))
      transform(Call(Function(Seq(VarPattern(ParamName)), Block(ifs :+ error)), Seq(e), Seq(), None))

    case Return(v: If) => v
    case Return(v: ForStatement) => v
    case Return(v: While) => v
    case Return(v: Throw) => v
    case Return(v) => Return(transform(v))
    case Throw(e) => Throw(transform(e))
    case New(c) => New(transform(c))
    case Subscript(arr, idx) => Subscript(transform(arr), transform(idx))
    case UnaryOperatorExpression(op, exp) => UnaryOperatorExpression(op, transform(exp))

    case Var(VarPattern(name), v) => Var(VarPattern(name), v map transform)
    case Var(ConstantPattern(VarAccess(name)), v) => Var(VarPattern(name), v map transform)
    case Var(name, Some(v)) => transform(Block(errOnPatFail(name, v) ++ patternBind(name, v)))

    case While(cond, body) => While(transform(cond), transform(body))

    case Range(start, end, step, inclusive) => Call(VarAccess(if(inclusive) "range" else "rangeExclusive"), Seq(start, end) ++ step, Seq(),  None)

    case ForComprehension(statements, exp) => transform(transformFor(statements, "map", exp))
    case ForEach(statements, exp) => transform(transformFor(statements, "forEach", exp))

    case _: VarAccess => node
    case _: StringValue => node
    case _: NumberValue => node
    case _: BooleanValue => node
    case _: InlineJavascript => node
  }

  private def transformFor(statements: Seq[ForStatement], lastFuncName: String, lastFuncExp: Expression): Expression = statements match {
//    case Generator(bind, col) :+ Filter(filter) :+ rest =>
//      val filtered = Call(Access(col, "filter"), Seq(Function(Seq(bind), filter)), None)
//      rest match {
//        case Seq() =>
//          Call(Access(filtered, lastFuncName), Seq(Function(Seq(bind), lastFuncExp)), None)
//        case _ =>
//          Call(Access(filtered))
//      }
    case Generator(bind, gen) +: rest =>
      val filters = rest takeWhile {
        case _: Filter => true
        case _ => false
      } map { case f: Filter => f }
      val base = filters.foldLeft(gen){ (b, filter) =>
        Call(Access(b, "filter"), Seq(Function(Seq(VarPattern(bind)), filter.condition)), Seq(), None)
      }
      rest.drop(filters.length) match {
        case Seq() => Call(Access(base, lastFuncName), Seq(Function(Seq(VarPattern(bind)), lastFuncExp)), Seq(), None)
        case tail => Call(Access(base, "flatMap"), Seq(Function(Seq(VarPattern(bind)), transformFor(tail, lastFuncName, lastFuncExp))), Seq(), None)
      }
  }

  private def typeof(e: Expression) = Call(VarAccess("typeof"), Seq(e), Seq(), None)

  private def patternCheckSterilize(pattern: Pattern, expr: Expression) = expr match {
    case VarAccess(_) => (None, patternCheck(pattern, expr))
    case _ => (Some(Var(VarPattern("$_temp_patmat"), Some(expr))), patternCheck(pattern, VarAccess("$_temp_patmat")))
  }

  private def patternCheck(pattern: Pattern, expr: Expression): Expression = pattern match {
    case IgnorePattern | VarPattern(_) => BooleanValue(true)
    case ConstantPattern(x) => OperatorExpression(expr, StrictEqual, x)
    case ArrayPattern(elems, None) => allTrue((Seq(
      js$.Array.isArray(expr),
      OperatorExpression(js$(expr).length, StrictEqual, NumberValue(elems.length))
    ) ++ elems.zipWithIndex.map{ case (p, i) =>
      patternCheck(p, Subscript(expr, NumberValue(i)))
    }).filterNot(alwaysTrue))

    case ArrayPattern(elems, Some(tail)) => allTrue((Seq(
      js$.Array.isArray(expr),
      OperatorExpression(js$(expr).length, GreaterEq, NumberValue(elems.length)),
      patternCheck(tail, js$(expr).slice(NumberValue(elems.length), js$(expr).length))
    ) ++ elems.zipWithIndex.map{ case (p, i) =>
      patternCheck(p, Subscript(expr, NumberValue(i)))
    }).filterNot(alwaysTrue))

    case ObjectPattern(props) =>
      val typeCheck = OperatorExpression(typeof(expr), StrictEqual, StringValue("object"))
      val propDefinedChecks = props map { case (prop, _) =>
        OperatorExpression(Access(expr, prop), StrictNotEqual, VarAccess("undefined"))
      }
      val propChecks = props map { case (prop, patt) =>
        patternCheck(patt, Access(expr, prop))
      }
      allTrue((typeCheck +: propDefinedChecks ++: propChecks).filterNot(alwaysTrue))
  }

  def errOnPatFail(pattern: Pattern, expr: Expression): Seq[Expression] = {
    val (prelude, check) = patternCheckSterilize(pattern, expr)
    prelude.toSeq :+ If(UnaryOperatorExpression(LogicalNegate, Paren(check)), Throw(StringValue("Pattern match failed")), None)
  }

  private def patternBindSterilize(pattern: Pattern, expr: Expression): Seq[Expression] = expr match {
    case VarAccess(_) => patternBind(pattern, expr)
    case _ => Var(VarPattern("$_temp_patmat_b"), Some(expr)) +: patternBind(pattern, VarAccess("$_temp_patmat_b"))
  }

  private def patternBind(pattern: Pattern, expr: Expression): Seq[Expression] = pattern match {
    case IgnorePattern => Seq()
    case ConstantPattern(_) => Seq()
    case VarPattern(name) => Seq(Var(VarPattern(name), Some(expr)))

    case ArrayPattern(elems, rest_?) =>
      val elemsbind = elems.zipWithIndex.flatMap { case (p, i) =>
        patternBind(p, Subscript(expr, NumberValue(i)))
      }
      val tailbind = rest_? map { rest =>
        patternBind(rest, js$(expr).slice(NumberValue(elems.length), js$(expr).length))
      }
      elemsbind ++ tailbind.toSeq.flatten

    case ObjectPattern(props) =>
      props flatMap { case (prop, patt) =>
        patternBind(patt, Access(expr, prop))
      }
  }



  private def alwaysTrue(e: Expression): Boolean = e match {
    case BooleanValue(true) => true
    case _ => false
  }

  private def allTrue(es: Seq[Expression]): Expression =
    es.reduceLeft((a, b) => OperatorExpression(a, LogicalAnd, b))

  def main(args: Array[String]) {
    val pattern = ObjectPattern(Seq(
      "foo" -> VarPattern("foo"),
      "bar" -> VarPattern("bar")
    ))
    val (checkpre, check) = patternCheckSterilize(pattern, js$.async(StringValue("http://api.nyan.com/getSuperSecretData")))
    println("check:")
    checkpre.foreach(c => println(Formatter.write(c)))
    println(Formatter.write(check))
    println("bind: ")
    val bind = patternBindSterilize(pattern, js$.async(StringValue("http://api.nyan.com/getSuperSecretData")))
    bind.foreach(b => println(Formatter.write(b)))
  }

}
