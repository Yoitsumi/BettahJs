package js

import scala.util.parsing.input.Positional

/**
 * Created by Kamil on 16.10.2015.
 */
sealed trait JsToken extends Positional

object JsToken {

  case class ErrorToken(msg: String) extends JsToken

  case object LParen extends JsToken
  case object RParen extends JsToken
  case object LBracket extends JsToken
  case object RBracket extends JsToken
  case object LBrace extends JsToken
  case object RBrace extends JsToken
  case object LambdaArrow extends JsToken
  case object GeneratorArrow extends JsToken
  case object Semicolon extends JsToken
  case object Colon extends JsToken
  case object Coma extends JsToken
  case object Dot extends JsToken
  case object DotDot extends JsToken
  case object Assign extends JsToken

  case object EOL extends JsToken

  object Keyword {
    case object If extends JsToken
    case object For extends JsToken
    case object While extends JsToken
    case object Function extends JsToken
    case object True extends JsToken
    case object False extends JsToken
    case object Var extends JsToken
    case object Else extends JsToken
    case object Return extends JsToken
    case object Yield extends JsToken
    case object Until extends JsToken
    case object To extends JsToken
    case object By extends JsToken
    case object Throw extends JsToken
    case object New extends JsToken
    case object Match extends JsToken
    case object Case extends JsToken
  }

  sealed abstract class Operator(val repr: String) extends JsToken
  sealed trait InfixOperator extends Operator
  sealed trait AssignOperator extends InfixOperator
  sealed trait UnaryOperator extends Operator
  object Operator {
    case object Plus extends Operator("+") with AssignOperator with UnaryOperator
    case object Minus extends Operator("-") with AssignOperator with UnaryOperator
    case object Multiply extends Operator("*") with AssignOperator
    case object Divide extends Operator("/") with AssignOperator
    case object Modulo extends Operator("%") with AssignOperator
    case object Xor extends Operator("^") with AssignOperator
    case object LogicalAnd extends Operator("&&") with AssignOperator
    case object BitAnd extends Operator("&") with AssignOperator
    case object LogicalOr extends Operator("||") with AssignOperator
    case object BitOr extends Operator("|") with AssignOperator

    case object GreaterEq extends Operator(">=") with InfixOperator
    case object Greater extends Operator(">") with InfixOperator
    case object LesserEq extends Operator("<=") with InfixOperator
    case object Less extends Operator("<") with InfixOperator
    case object StrictEqual extends Operator("===") with InfixOperator
    case object Equal extends Operator("==") with InfixOperator
    case object StrictNotEqual extends Operator("!==") with InfixOperator
    case object NotEqual extends Operator("!=") with InfixOperator

//    case object ArithmeticNegate extends UnaryOperator("-")
    case object BitNegate extends Operator("~") with UnaryOperator
    case object LogicalNegate extends Operator("!") with UnaryOperator



    def unapply(o: Operator) = Some(o.repr)


  }

  case class Identifier(name: String) extends JsToken

  case class NumberToken(str: String, value: Double) extends JsToken

  case class StringToken(str: String) extends JsToken

  case class InlineJSToken(src: String) extends JsToken

}
