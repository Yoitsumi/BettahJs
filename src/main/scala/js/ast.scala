package js

import js.JsToken.{InfixOperator, UnaryOperator, AssignOperator, Operator}

/**
 * Created by Kamil on 24.09.2015.
 */
object ast {

  sealed trait AstNode
  sealed trait Expression extends AstNode
  sealed trait LVAL extends Expression
  case class CallBlock(params: Seq[Pattern], body: Seq[Expression]) extends AstNode
  case class NumberValue(value: Double) extends Expression
  case class StringValue(value: String) extends Expression
  case class BooleanValue(value: Boolean) extends Expression
  case class Block(body: Seq[Expression]) extends Expression
  case class Assignement(name: LVAL, value: Expression) extends Expression
  case class If(condition: Expression, tru: Expression, fals: Option[Expression]) extends Expression
  case class While(condition: Expression, body: Expression) extends Expression
  case class VanillaFor(init: Expression, condition: Expression, iteration: Expression, body: Expression) extends Expression
  case class Var(name: Pattern, value: Option[Expression]) extends Expression
  case class Paren(value: Expression) extends Expression
  case class OperatorExpression(left: Expression, op: InfixOperator, right: Expression) extends Expression
  case class OperatorAssignement(left: LVAL, op: AssignOperator, right: Expression) extends Expression
  case class UnaryOperatorExpression(op: UnaryOperator, expr: Expression) extends Expression
  case class Access(from: Expression, property: String) extends Expression with LVAL
  case class VarAccess(name: String) extends Expression with LVAL
  case class Call(function: Expression, params: Seq[Expression], namedParams: Seq[(String, Expression)], block: Option[CallBlock]) extends Expression
  case class ObjectLiteral(properties: Seq[(String, Expression)]) extends Expression
  case class ArrayLiteral(members: Seq[Expression]) extends Expression
  case class Throw(exception: Expression) extends Expression
  case class New(constructor: Expression) extends Expression

  case class ForComprehension(statements: Seq[ForStatement], yielt: Expression) extends Expression
  case class ForEach(statements: Seq[ForStatement], fun: Expression) extends Expression
  sealed trait ForStatement
  case class Generator(bind: String, collection: Expression) extends ForStatement
  case class Filter(condition: Expression) extends ForStatement

  case class PatternMatch(matched: Expression, patterns: Seq[MatchCase]) extends Expression
  case class MatchCase(pattern: Pattern, body: Expression)

  sealed trait Range extends Expression {
    val start: Expression
    val end: Expression
    val step: Option[Expression]
  }
  object Range {
    def unapply(r: Range) = r match {
      case InclusiveRange(s, e, p) => Some(s, e, p, true)
      case ExclusiveRange(s, e, p) => Some(s, e, p, false)
    }
  }
  case class InclusiveRange(start: Expression, end: Expression, step: Option[Expression]) extends Range
  case class ExclusiveRange(start: Expression, end: Expression, step: Option[Expression]) extends Range


  case class Function(params: Seq[Pattern], body: Expression) extends Expression
  case class FunctionDef(name: String, params: Seq[String], body: Expression) extends Expression
  case class Return(value: Expression) extends Expression
  case class Subscript(array: Expression, index: Expression) extends Expression with LVAL

  case class InlineJavascript(source: String) extends Expression with LVAL



  sealed trait Pattern extends AstNode
  case object IgnorePattern extends Pattern
  case class VarPattern(name: String) extends Pattern
  case class ConstantPattern(value: Expression) extends Pattern
  case class ArrayPattern(elems: Seq[Pattern], rest: Option[Pattern]) extends Pattern
  case class ObjectPattern(props: Seq[(String, Pattern)]) extends Pattern

}
