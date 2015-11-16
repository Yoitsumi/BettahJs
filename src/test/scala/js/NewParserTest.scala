package js

import js.JsToken.Identifier
import js.JsToken.Operator._
import js.ast._
import org.scalatest.matchers.{BePropertyMatchResult, BePropertyMatcher}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Kamil on 22.10.2015.
 */
class NewParserTest extends FlatSpec with Matchers {

  object Value {
    def unapply(v: Expression) = v match {
      case v: NumberValue => Some(v.value)
      case v: StringValue => Some(v.value)
      case v: BooleanValue => Some(v.value)
      case _ => None
    }
  }
  object -> {
    def unapply[A, B](pair: (A, B)) = Some(pair)
  }

  def $[R](p: NewParser.Parser[R])(src: String) = NewParser.parseAll(p, src)
  def $$[R](p: NewParser.Parser[R])(src: String) = {
    val rg = $(p)(src)
    val re = $(NewParser.expression)(src)
    rg shouldEqual re
    rg
  }
  def $P[R](p: NewParser.Parser[R])(src: String) = {
    val rg = $(p)(src)
    val re = $(NewParser.pattern)(src)
    rg shouldEqual re
    rg
  }
  def aSuccess(f: PartialFunction[Any, Any] = PartialFunction(identity)) = new BePropertyMatcher[NewParser#ParseResult[Any]] {
    override def apply(obj: NewParser#ParseResult[Any]): BePropertyMatchResult = obj match {
      case NewParser.Success(x, _) if f.isDefinedAt(x) => BePropertyMatchResult(true, "success")
      case _ => BePropertyMatchResult(false, "success")
    }
  }
  def aFailure = new BePropertyMatcher[NewParser#ParseResult[Any]] {
    override def apply(obj: NewParser#ParseResult[Any]): BePropertyMatchResult = obj match {
      case NewParser.Failure(_) => new BePropertyMatchResult(true, "failure")
      case _ => new BePropertyMatchResult(false, "failure")
    }
  }

  implicit class StringContextExtensions(val ctxt: StringContext) {
    object vp {
      def apply(x: Any*) = VarPattern(ctxt.s(x: _*))
      def unapply(vp: VarPattern): Boolean = ctxt.parts.mkString == vp.name
    }
    object va {
      def apply(x: Any*) = VarAccess(ctxt.s(x: _*))
      def unapply(va: VarAccess): Boolean = ctxt.parts.mkString == va.name
    }
  }

  val AllowedNames = Seq("foo", "bar20", "$", "a_very_long_snake_case_identifier_with_numb3rs")

  "ident" should "Match correct names" in {
    for (n <- AllowedNames) {
      $(NewParser.ident)(n) shouldBe aSuccess {
        case x if x == n =>
      }
    }
  }
  it should "reject incorrect names" in {
    val names = Seq("50", "if", "name+not")
    for (n <- names) {
      $(NewParser.ident)(n) shouldBe aFailure
    }
  }

  "expSeparator" should "match a semicolon" in {
    $(NewParser.expSeparator)(";") shouldBe aSuccess()
  }
  it should "match a new line character" in {
    $(NewParser.expSeparator)("\n") shouldBe aSuccess()
  }

  "varr" should "match var definitions without value" in {
    $$(NewParser.varr)("var x") shouldBe aSuccess {
      case Var(VarPattern("x"), None) =>
    }
  }
  it should "match var definitions with value assignement" in {
    $$(NewParser.varr)("var x = 10") shouldBe aSuccess {
      case Var(VarPattern("x"), Some(Value(10.0))) =>
    }
  }

  "varAccess" should "match any allowed variable name" in {
    for(name <- AllowedNames) {
      $$(NewParser.varAccess)(name) shouldBe aSuccess {
        case VarAccess(n) if n == name =>
      }
    }
  }

  "lval" should "match single variable names" in {
    for(name <- AllowedNames) {
      $$(NewParser.varAccess)(name) shouldBe aSuccess {
        case VarAccess(n) if n == name =>
      }
    }
  }

  "access" should "match a single dot expression" in {
    $(NewParser.expression)("foo.bar") shouldBe aSuccess {
      case Access(VarAccess("foo"), "bar") =>
    }
  }
  it should "match a chain of dot expressions" in {
    $(NewParser.expression)("foo.bar.baz") shouldBe aSuccess {
      case Access(Access(VarAccess("foo"), "bar"), "baz") =>
    }
  }

  "assignement" should "match a simple variable assign" in {
    $(NewParser.expression)("foo = 20") shouldBe aSuccess {
      case Assignement(VarAccess("foo"), Value(20.0)) =>
    }
  }
  it should "match an assign to an object member" in {
    $(NewParser.expression)("foo.bar = 20") shouldBe aSuccess {
      case Assignement(Access(VarAccess("foo"), "bar"), Value(20.0)) =>
    }
  }
  it should "match an assign to an array member" in {
    $(NewParser.expression)("foo[10] = 20") shouldBe aSuccess {
      case Assignement(Subscript(VarAccess("foo"), Value(10)), Value(20)) =>
    }
  }

  "opAssign" should "match an add-assign" in {
    $(NewParser.expression)("foo += 20") shouldBe aSuccess {
      case OperatorAssignement(VarAccess("foo"), Plus, Value(20.0)) =>
    }
  }

  "call" should "match a call without parameters" in {
    $(NewParser.expression)("foo()") shouldBe aSuccess {
      case Call(VarAccess("foo"), Seq(), Seq(), None) =>
    }
  }
  it should "match a call with a literal parameter" in {
    $(NewParser.expression)("foo(20)") shouldBe aSuccess {
      case Call(VarAccess("foo"), Seq(Value(20.0)), Seq(), None) =>
    }
  }
  it should "match a call with a block without parentheses" in {
    $(NewParser.expression)("foo { 0; }") shouldBe aSuccess {
      case Call(VarAccess("foo"), Seq(), Seq(), Some(CallBlock(Seq(), Seq(
      Value(0)
      )))) =>
    }
  }
  it should "match a call with a block with parameters" in {
    $(NewParser.expression)("foo { x -> x+1 }") shouldBe aSuccess {
      case Call(VarAccess("foo"), Seq(), Seq(), Some(CallBlock(Seq(vp"x"), Seq(
      OperatorExpression(VarAccess("x"), Plus, Value(1))
      )))) =>
    }
  }
  it should "match a call with only named parameters" in {
    $(NewParser.expression)("foo(a: 10, 'b': 20)") shouldBe aSuccess {
      case Call(VarAccess("foo"), Seq(), Seq("a" -> Value(10), "b" -> Value(20)), None) =>
    }
  }

  "opExp" should "match a single addition" in {
    $(NewParser.expression)("2+2") shouldBe aSuccess {
      case OperatorExpression(Value(2.0), Plus, Value(2.0)) =>
    }
  }

  it should "match few operations in succession" in {
    $(NewParser.expression)("2+2-4") shouldBe aSuccess {
      case OperatorExpression(Value(2), Plus, OperatorExpression(Value(2), Minus, Value(4))) =>
    }
  }

  "block" should "match a block with a single expression with a semicolon" in {
    $$(NewParser.block)("{0;}") shouldBe aSuccess {
      case Block(Seq(Value(0))) =>
    }
  }
  it should "match a block with few expressions seperated by semicolons and/or newlines" in {
    $$(NewParser.block)(
      """
        |{
        | 0;
        | 1; 2
        | 3
        |}
      """.stripMargin) shouldBe aSuccess {
      case Block(Seq(
      Value(0), Value(1), Value(2), Value(3)
      )) =>
    }
  }
  it should "match a block with one expression without a newline or a semicolon" in {
    $$(NewParser.block)("{0}") shouldBe aSuccess {
      case Block(Seq(Value(0))) =>
    }
  }

  "vanillaFor" should "match a simple for expression" in {
    $$(NewParser.vanillaFor)("for(var i=0; i<10; i += 1) println(i)") shouldBe aSuccess {
      case VanillaFor(
      Var(VarPattern("i"), Some(Value(0))),
      OperatorExpression(VarAccess("i"), Less, Value(10)),
      OperatorAssignement(VarAccess("i"), Plus, Value(1)),
      Call(VarAccess("println"), Seq(VarAccess("i")), Seq(), None)
      ) =>
    }
  }

  "fohr" should "match a simple for comprehension" in {
    $$(NewParser.fohr)("for(x <- [1, 2, 3]) yield x") shouldBe aSuccess {
      case ForComprehension(Seq(
      Generator("x", ArrayLiteral(Seq(Value(1), Value(2), Value(3))))
      ), VarAccess(x)) =>
    }
  }
  it should "match a simple for comprehension with two generators" in {
    $$(NewParser.fohr)("for(x <- [1 to 10]; y <- [1 to 10]) yield x * y") shouldBe aSuccess {
      case ForComprehension(Seq(
      Generator("x", InclusiveRange(Value(1), Value(10), None)),
      Generator("y", InclusiveRange(Value(1), Value(10), None))
      ), OperatorExpression(VarAccess("x"), Multiply, VarAccess("y"))) =>
    }
  }
  it should "match a simple for comprehension with a filter" in {
    $$(NewParser.fohr)("for(x <- [1 to 100]; if (x % 2) == 1) yield x") shouldBe aSuccess {
      case ForComprehension(Seq(
      Generator("x", InclusiveRange(Value(1), Value(100), None)),
      Filter(OperatorExpression(Paren(OperatorExpression(VarAccess("x"), Modulo, Value(2))), Equal, Value(1)))
      ), VarAccess("x")) =>
    }
  }
  it should "match a simple foreach statement" in {
    $$(NewParser.fohr)("for(x <- [1 to 100]) console.log(x)") shouldBe aSuccess {
      case ForEach(Seq(
      Generator("x", InclusiveRange(Value(1), Value(100), None))
      ), Call(Access(VarAccess("console"), "log"), Seq(VarAccess("x")), Seq(), None)) =>
    }
  }

  "property" should "match a simple key-value pair with quoted key " in {
    $(NewParser.property)(""" "foo": "bar" """) shouldBe aSuccess {
      case "foo" -> Value("bar") =>
    }
  }
  it should "match a simple key-value pair without quotes" in {
    $(NewParser.property)(""" foo: "bar" """) shouldBe aSuccess {
      case "foo" -> Value("bar") =>
    }
  }
  it should "match a pair with complex value" in {
    $(NewParser.property)("foo: 2+2") shouldBe aSuccess {
      case "foo" -> OperatorExpression(Value(2), Plus, Value(2)) =>
    }
  }

  "objectLiteral" should "match an empty object" in {
    $$(NewParser.objectLiteral)("{}") shouldBe aSuccess {
      case ObjectLiteral(Seq()) =>
    }
  }
  it should "match an object with few literal properties" in {
    $$(NewParser.objectLiteral)(
      """
        |{
        | hello: "world",
        | "nyan": 20
        |}
      """.stripMargin) shouldBe aSuccess {
      case ObjectLiteral(Seq(
      "hello" -> Value("world"),
      "nyan" -> Value(20)
      )) =>
    }
  }

  "unOpExp" should "parse a not operator" in {
    $(NewParser.unOpExp)("!a") shouldBe aSuccess {
      case UnaryOperatorExpression(LogicalNegate, VarAccess("a")) =>
    }
  }

  "whil" should "match a simple while loop" in {
    $$(NewParser.whil)("while(i > 0) i -= 1") shouldBe aSuccess {
      case While(
      OperatorExpression(VarAccess("i"), Greater, Value(0)),
      OperatorAssignement(VarAccess("i"), Minus, Value(1))
      ) =>
    }
  }

  "lambda" should "match a lambda without parameters" in {
    $$(NewParser.lambda)("-> 50") shouldBe aSuccess {
      case Function(Seq(), Value(50)) =>
    }
  }
  it should "match a lambda without any parameters with parentheses" in {
    $$(NewParser.lambda)("() -> 50") shouldBe aSuccess {
      case Function(Seq(), Value(50)) =>
    }
  }
  it should "match a lambda with one parameter without parentheses" in {
    $$(NewParser.lambda)("(x) -> x+1") shouldBe aSuccess {
      case Function(Seq(vp"x"), OperatorExpression(VarAccess("x"), Plus, Value(1))) =>
    }
  }
  it should "match a lambda with multiple parameters" in {
    $$(NewParser.lambda)("(x, y, z) -> x+1") shouldBe aSuccess {
      case Function(Seq(vp"x", vp"y", vp"z"), OperatorExpression(VarAccess("x"), Plus, Value(1))) =>
    }
  }
  it should "match a lambda without parentheses with a block as a body" in {
    $$(NewParser.lambda)("-> { console.log('success') }") shouldBe aSuccess {
      case Function(Seq(), Block(Seq(
        Call(Access(VarAccess("console"), "log"), Seq(Value("success")), Seq(), None)
      ))) =>
    }
  }
  it should "match a lambda with an object pattern as argument" in {
    $$(NewParser.lambda)("({foo: foo, bar: bar}) -> foo + bar") shouldBe aSuccess {
      case Function(Seq(ObjectPattern(Seq(
        "foo" -> vp"foo",
        "bar" -> vp"bar"
      ))), OperatorExpression(VarAccess("foo"), Plus, VarAccess("bar"))) =>
    }
  }

  "paren" should "match an expression enclosed in perentheses" in {
    $$(NewParser.paren)("(2+2)") shouldBe aSuccess {
      case Paren(OperatorExpression(Value(2), Plus, Value(2))) =>
    }
  }

  "iff" should "match a simple if expression" in {
    $$(NewParser.iff)("if(a < 0) a = -a") shouldBe aSuccess {
      case If(
      OperatorExpression(VarAccess("a"), Less, Value(0)),
      Assignement(VarAccess("a"), UnaryOperatorExpression(Minus, VarAccess("a"))),
      None
      ) =>
    }
  }
  it should "match an if expression with else clause" in {
    $$(NewParser.iff)("if(a < 0) -a else a") shouldBe aSuccess {
      case If(
      OperatorExpression(VarAccess("a"), Less, Value(0)),
      UnaryOperatorExpression(Minus, VarAccess("a")),
      Some(VarAccess("a"))
      ) =>
    }
  }

  "arrayLiteral" should "match a simple array literal of literals" in {
    $$(NewParser.arrayLiteral)("""["foobar", true, false, 50]""") shouldBe aSuccess {
      case ArrayLiteral(Seq(
      Value("foobar"), Value(true), Value(false), Value(50)
      )) =>
    }
  }
  it should "match an array literal with complex values" in {
    $$(NewParser.arrayLiteral)("""[2+2, 3*3+2]""") shouldBe aSuccess {
      case ArrayLiteral(Seq(
      OperatorExpression(Value(2), Plus, Value(2)),
      OperatorExpression(Value(3), Multiply, OperatorExpression(Value(3), Plus, Value(2)))
      )) =>
    }
  }

  "range" should "match an exclusive range" in {
    $$(NewParser.range)("[0 until 5]") shouldBe aSuccess {
      case ExclusiveRange(Value(0), Value(5), None) =>
    }
  }
  it should "match an inclusive range" in {
    $$(NewParser.range)("[0 to 5]") shouldBe aSuccess {
      case InclusiveRange(Value(0), Value(5), None) =>
    }
  }
  it should "match an inclusive range with a step" in {
    $$(NewParser.range)("[0 to 6 by 2]") shouldBe aSuccess {
      case InclusiveRange(Value(0), Value(6), Some(Value(2))) =>
    }
  }

  "functionDef" should "match a vanilla function def without parameters" in {
    $$(NewParser.functionDef)(
      """
        |function f() {
        | println("nyan")
        |}
      """.stripMargin) shouldBe aSuccess {
      case FunctionDef("f", Seq(), Block(Seq(
      Call(VarAccess("println"), Seq(Value("nyan")), Seq(), None)
      ))) =>
    }
  }
  it should "match a vanilla function def without multiple parameters" in {
    $$(NewParser.functionDef)(
      """
        |function f(x, y) {
        | println(x + y)
        |}
      """.stripMargin) shouldBe aSuccess {
      case FunctionDef("f", Seq("x", "y"), Block(Seq(
      Call(VarAccess("println"), Seq(OperatorExpression(VarAccess("x"), Plus, VarAccess("y"))), Seq(), None)
      ))) =>
    }
  }
  it should "match a one-expression function def" in {
    $$(NewParser.functionDef)("function f(x, y) = x + y") shouldBe aSuccess {
      case FunctionDef("f", Seq("x", "y"), OperatorExpression(VarAccess("x"), Plus, VarAccess("y"))) =>
    }
  }
  it should "match a one-expression def without parentheses" in {
    $$(NewParser.functionDef)("function f = x+=1") shouldBe aSuccess {
      case FunctionDef("f", Seq(), OperatorAssignement(VarAccess("x"), Plus, Value(1))) =>
    }
  }
  it should "match a vanilla def without parentheses" in {
    $$(NewParser.functionDef)("function f { return 10 }") shouldBe aSuccess {
      case FunctionDef("f", Seq(), Block(Seq(Return(Value(10))))) =>
    }
  }

  "expression" should "match a double-quoted string literal" in {
    $(NewParser.expression)("\"nyan\"") shouldBe aSuccess {
      case Value("nyan") =>
    }
  }
  it should "match a single-quoted string literal" in {
    $(NewParser.expression)("'nyan'") shouldBe aSuccess {
      case Value("nyan") =>
    }
  }

  //  "singleStringLiteral" should "match a single-quoted string literal" in {
  //    $(NewParser.singleStringLiteral _)("'nyan'") shouldBe aSuccess {
  //      case "'nyan'" =>
  //    }
  //  }

  "return" should "match a complex return statement" in {
    $$(NewParser.ret)("return 2+2") shouldBe aSuccess {
      case Return(OperatorExpression(Value(2), Plus, Value(2))) =>
    }
  }

  "subscript" should "match a literal index into array" in {
    $(NewParser.expression)("arr[20]") shouldBe aSuccess {
      case Subscript(VarAccess("arr"), Value(20)) =>
    }
  }
  it should "match a complex index into array" in {
    $(NewParser.expression)("arr[2+2]") shouldBe aSuccess {
      case Subscript(VarAccess("arr"), OperatorExpression(Value(2), Plus, Value(2))) =>
    }
  }
  it should "match an index into array, that is a member of object" in {
    $(NewParser.expression)("foo.bar[20]") shouldBe aSuccess {
      case Subscript(Access(VarAccess("foo"), "bar"), Value(20)) =>
    }
  }

  "lval" should "match an inline javascript in an assignement" in {
    $(NewParser.expression)("`nyan()` = 10") shouldBe aSuccess {
      case Assignement(InlineJavascript("nyan()"), Value(10)) =>
    }
  }

  "throw" should "match a simple throw statement" in {
    $$(NewParser.throvv)("throw 'foobar'") shouldBe aSuccess {
      case Throw(Value("foobar")) =>
    }
  }

  "nevv" should "match a simple constructor call with some parameters" in {
    $$(NewParser.nevv)("new Foobar(0, 1)") shouldBe aSuccess {
      case New(Call(VarAccess("Foobar"), Seq(Value(0), Value(1)), Seq(), None)) =>
    }
  }

  "patternMatch" should "match a simple pattern match expression with multiple cases" in {
    $(NewParser.expression)("foo match { case 0 -> 20 case _ -> 10 }") shouldBe aSuccess {
      case PatternMatch(VarAccess("foo"), Seq(
        MatchCase(ConstantPattern(Value(0)), Value(20)),
        MatchCase(IgnorePattern, Value(10))
      )) =>
    }
  }



  "pattern" should "match an ignore pattern" in {
    $(NewParser.pattern)("_") shouldBe aSuccess {
      case IgnorePattern =>
    }
  }
  it should "match a single var pattern" in {
    $(NewParser.pattern)("nyan") shouldBe aSuccess {
      case VarPattern("nyan") =>
    }
  }
  it should "match a single number constant pattern" in {
    $(NewParser.pattern)("20") shouldBe aSuccess {
      case ConstantPattern(Value(20)) =>
    }
  }
  it should "match a single boolean constant pattern" in {
    $(NewParser.pattern)("true") shouldBe aSuccess {
      case ConstantPattern(Value(true)) =>
    }
    $(NewParser.pattern)("false") shouldBe aSuccess {
      case ConstantPattern(Value(false)) =>
    }
  }
  it should "match a single string constant pattern" in {
    $(NewParser.pattern)("'foobar'") shouldBe aSuccess {
      case ConstantPattern(Value("foobar")) =>
    }
  }

  "arrayPattern" should "match a simple tuple pattern" in {
    $P(NewParser.arrayPattern)("[_, _]") shouldBe aSuccess {
      case ArrayPattern(Seq(IgnorePattern, IgnorePattern), None) =>
    }
  }

  it should "match a simple list deconstruction" in {
    $P(NewParser.arrayPattern)("[head : tail]") shouldBe aSuccess {
      case ArrayPattern(Seq(VarPattern("head")), Some(VarPattern("tail"))) =>
    }
  }

  "objectPattern" should "match a simple object pattern" in {
    $P(NewParser.objectPattern)(""" { foo: _, 'bar': _ } """) shouldBe aSuccess {
      case ObjectPattern(Seq("foo" -> IgnorePattern, "bar" -> IgnorePattern)) =>
    }
  }


}
