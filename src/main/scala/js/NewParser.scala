package js

import java.io

import ast._
import com.sun.org.apache.xml.internal.resolver.tools.ResolvingParser
import js.JsToken._

import scala.annotation.tailrec
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.{PagedSeqReader, Reader}

/**
 * Created by Kamil on 22.10.2015.
 */
class NewParser {

  type ?=>[A, B] = PartialFunction[A, B]

//  type Parser[R] =  Input => ParseResult[R]
  type Input = Stream[JsToken]
//  type ParseResult[R] = Either[String, (R, Input)]
  sealed trait ParseResult[+R] {
    def forEach(f: ((R, Input)) => Unit): Unit
    def map[S](f: ((R, Input)) => S): ParseResult[S]
    def flatMap[S](f: ((R, Input)) => ParseResult[S]): ParseResult[S]
    def filter(f: ((R, Input)) => Boolean): ParseResult[R]

//    def forEach(f: R => Unit): Unit = this.forEach({ case (r: R, _: Input) => f(r) })
//    def map[S](f: R => S): ParseResult[S] = map{ case (r: R, _: Input) => f(r)}
//    def flatMap[S](f: R => ParseResult[S]): ParseResult[S] = flatMap{ case (r: R, _: Input) => f(r) }
//    def filter[S](f: R => Boolean): ParseResult[R] = filter{ case (r: R, _: Input) => f(r)}
  }
  case class Success[+R](value: R, tail: Input) extends ParseResult[R] {
    override def forEach(f: ((R, Input)) => Unit): Unit = f(value, tail)
    override def flatMap[S](f: ((R, Input)) => ParseResult[S]): ParseResult[S] = f(value, tail)
    override def filter(f: ((R, Input)) => Boolean): ParseResult[R] =
      if(f(value, tail)) this
      else Failure("Filter failed")
    override def map[S](f: ((R, Input)) => S): ParseResult[S] = Success(f(value, tail), tail)
  }
  case class Failure(message: String) extends ParseResult[Nothing] {
    override def forEach(f: ((Nothing, Input)) => Unit): Unit = ()
    override def flatMap[S](f: ((Nothing, Input)) => ParseResult[S]): ParseResult[S] = this
    override def filter(f: ((Nothing, Input)) => Boolean): ParseResult[Nothing] = this
    override def map[S](f: ((Nothing, Input)) => S): ParseResult[S] = this
  }


  trait Parser[+R] extends (Input => ParseResult[R]) { self =>
    def map[S](f: R => S): Parser[S] = Parser { in => self(in).map{ case (r, _) => f(r) } }
    def flatMap[S](f: R => Parser[S]): Parser[S] = Parser { in =>
      self(in).flatMap{ case (r, i) => f(r)(i) }
    }
    def filter(f: R => Boolean): Parser[R] = Parser { in =>
      val res = self(in)
      res.flatMap {
        case (r, _) if f(r) => res
        case _ => Failure("Parser filter failed")
      }
    }
  }
  object Parser {
    def apply[R](f: Input => ParseResult[R], skipWhitespace: Boolean = false): Parser[R] = new Parser[R] {
      override def apply(in: Input): ParseResult[R] =
        f(if(skipWhitespace) in.dropWhile(_ == EOL) else in)
    }
  }

  def choice[R](f: Input ?=> Parser[R]) = Parser(in => Success(f(in), in)).flatMap(identity)

  def success[R](r: =>R): Parser[R] = Parser(in => Success(r, in))
  def failure[R](r: =>String): Parser[R] = Parser(_ => Failure(r))
  def consume(r: Input ?=> Input): Parser[Unit] = Parser(r andThen (in => Success((), in)))
  def consumeSeq(s: JsToken*): Parser[Unit] = s match {
    case Seq() => success(())
    case head +: tail => Parser {
      case ihead ~:: itail if head == ihead => consumeSeq(tail: _*)(itail)
      case ihead ~:: _ => Failure(s"$head expected, but $ihead found.")
      case Stream() => Failure(s"$head expected, but eof found.")
    }
  }

  object ~:: {
    def unapply(s: Stream[JsToken]): Option[(JsToken, Stream[JsToken])] = s match {
      case Stream() => None
      case EOL #:: rest => unapply(rest)
      case x #:: rest => Some((x, rest))
    }
  }
  def result[R](r: =>ParseResult[R]): Parser[R] = Parser(_ => r)

  private val lexer = new Lexer

  private def createInputStream(sc: Reader[JsToken]): Stream[JsToken] =
    Stream.iterate(sc)(_.rest).takeWhile(!_.atEnd).map(_.first)

  def parse[R](p: Parser[R], src: String): ParseResult[R] = {
    val stream = createInputStream(new lexer.Scanner(src))
    p(stream)
  }

  def parse[R](p: Parser[R], src: io.Reader): ParseResult[R] = {
    val stream = createInputStream(new lexer.Scanner(new PagedSeqReader(PagedSeq.fromReader(src))))
    p(stream)
  }

  def parseAll[R](p: Parser[R], src: String): ParseResult[R] = {
    for {
      (ret, tail) <- parse(p, src)
      _ <- eof(tail)
    } yield ret
  }

  def parseAll[R](p: Parser[R], src: io.Reader): ParseResult[R] = {
    for {
      (ret, tail) <- parse(p, src)
      _ <- eof(tail)
    } yield ret
  }

  private def unexpectedToken(expected: String, found: JsToken) =
    Failure(s"Expected $expected, but $found found!")

  object Eof {
    def unapply(in: Input): Boolean = in.dropWhile{ case EOL => true; case _ => false }.isEmpty
  }

  def program: Parser[Seq[Expression]] = Parser { in =>
    def recur(in: Input, acc: Seq[Expression]): ParseResult[Seq[Expression]] = in match {
      case Eof() => Success(acc, Stream())
      case Semicolon ~:: rest => recur(rest, acc)
      case _ =>
        expression(in) match {
          case Failure(msg) => Failure(msg)
          case Success(exp, Eof() ~:: tail) => Success(acc :+ exp, tail)
          case Success(exp, StatementSeparator() #:: tail) => recur(tail, acc :+ exp)
          case Success(exp, t #:: tail) => Failure(s"Statement separator expected, but $t found")
        }
    }
    recur(in, Seq())
  }


  def eof = Parser[Unit]({
    case Stream.Empty => Success((), Stream.Empty)
    case t #:: _ => unexpectedToken("end of file", t)
  }, skipWhitespace = true )

  def skipAfterMatchingParen(in: Input): Input = {
    val parens = in.scanLeft(1)((p, t) => t match {
      case LParen => p + 1
      case RParen => p - 1
      case _ => p
    })
    val prefix = parens.takeWhile(_ > 0)
    val toDrop = prefix.length
    in.drop(toDrop)
  }

  def expression = for {
    seed <- choice[Expression] {
      case in
        if in.dropWhile { case EOL | Identifier(_) | Coma | LParen | RParen => true; case _ => false }.
          headOption.contains(LambdaArrow) => lambda
      case LParen ~:: rest
        if (skipAfterMatchingParen(rest) match { case LambdaArrow ~:: _ => true; case _ => false }) => lambda
      case (_: Identifier) ~:: _ => varAccess
      case Keyword.Var ~:: _ => varr
      case Keyword.If ~:: _ => iff
      case Keyword.While ~:: _ => whil
      case Keyword.For ~:: LParen ~:: body
        if body.dropWhile{ case EOL | Identifier(_) => true; case _ => false }.
           headOption.contains(GeneratorArrow) => fohr
      case Keyword.For ~:: _ => vanillaFor
      case Keyword.Return ~:: _ => ret
      case Keyword.Throw ~:: _ => throvv
      case Keyword.New ~:: _ => nevv
      case Keyword.Function ~:: Identifier(_) ~:: _ => functionDef
      case (_: UnaryOperator) ~:: _ => unOpExp
      case LBrace ~:: body
        if body.dropWhile{ case EOL | Identifier(_) | StringToken(_) => true; case _ => false }.
           headOption.forall{ case Colon | RBrace => true; case _ => false } => objectLiteral
      case LBrace ~:: _ => block
      case LBracket ~:: body
        if body.takeWhile{ case LBracket | Coma | RBracket => false; case _ => true }.
           exists{ case Keyword.To | Keyword.Until => true; case _ => false} => range
      case LBracket ~:: _ => arrayLiteral
      case LParen ~:: _ => paren
      case NumberToken (_, n) ~:: rest => result(Success(NumberValue(n), rest))
      case StringToken (str) ~:: rest => result(Success(StringValue(str), rest))
      case Keyword.True ~:: rest => result(Success(BooleanValue(true), rest))
      case Keyword.False ~:: rest => result(Success(BooleanValue(false), rest))
      case InlineJSToken(js) ~:: rest => result(Success(InlineJavascript(js), rest))

      case in => failure(s"Expected start of expression, but ${in.take(3).toList mkString " "} found at ${in.head.pos}")
    }
    tail <- expressionTail(seed)
  } yield tail

  def expressionTail(prefix: Expression): Parser[Expression] = choice {
    case in if lvalExprs.isDefinedAt(prefix, in) => lvalExprs(prefix, in)
    case (op: InfixOperator) ~:: _ => opExp(prefix) flatMap expressionTail
    case Dot ~:: _ => access(prefix) flatMap expressionTail
    case LParen #:: _ => call(prefix) flatMap expressionTail
    case LBrace #:: _ => call(prefix) flatMap expressionTail
    case LBracket #:: _ => subscript(prefix) flatMap expressionTail
    case Keyword.Match ~:: _ => patternMatch(prefix) flatMap expressionTail
    case _ => success(prefix)
  }

  def lvalExprs: ((Expression, Input) ?=> Parser[Expression]) = {
    case (l: LVAL, Assign ~:: _) => assignement(l)
    case (l: LVAL, (op: AssignOperator) ~:: Assign ~:: _ ) => opAssign(l)
  }

  def ident = Parser[String] {
    case Identifier(i) ~:: rest => Success(i, rest)
    case t ~:: _ => unexpectedToken("identifier", t)
  }

  def expSeparator = Parser[Unit]({
    case (EOL | Semicolon) #:: rest => Success((), rest)
    case t #:: _ => unexpectedToken("semicolon or end of line", t)
  }, skipWhitespace = false)

//  def varr: Parser[Var] = Parser[Var] {
//    case Keyword.Var ~:: Identifier(id) ~:: Assign ~:: rhs =>
//      expression(rhs).map { case (exp, _) => Var(id, Some(exp)) }
//    case Keyword.Var ~:: Identifier(id) ~:: rest => Success(Var(id, None), rest)
//  }

  def varr: Parser[Var] = for {
    _ <- consumeSeq(Keyword.Var)
    pat <- pattern
    exp <- Parser {
      case Assign ~:: rest => expression(rest) map { case (e, _) => Some(e) }
      case in => Success(None, in)
    }
  } yield Var(pat, exp)

  def varAccess = for(id <- ident) yield VarAccess(id)

  def access(prefix: Expression) = Parser[Access] {
    case Dot #:: Identifier(id) #:: rest => Success(Access(prefix, id), rest)
  }

  def subscript(prefix: Expression): Parser[Subscript] = for {
    _ <- consumeSeq(LBracket)
    idx <- expression
    _ <- consumeSeq(RBracket)
  } yield Subscript(prefix, idx)

  def assignement(prefix: LVAL): Parser[Assignement] = for {
    _ <- consumeSeq(Assign)
    rval <- expression
  } yield Assignement(prefix, rval)

  def opAssign(prefix: LVAL) = Parser[OperatorAssignement] {
    case (op: AssignOperator) ~:: Assign #:: rest =>
      expression(rest) map { case (e, _) => OperatorAssignement(prefix, op, e) }
  }

  def call(prefix: Expression) = for {
    (params, nparams) <- Parser {
      case LParen #:: rest => callParams(rest)
      case in => Success((Seq(), Seq()), in)
    }
    block <- choice {
      case LBrace #:: _ => callBlock map (Some(_))
      case _ => success(None)
    }
  } yield Call(prefix, params, nparams, block)

  def callParams: Parser[(Seq[Expression], Seq[(String, Expression)])] = Parser { in =>

    def recur(in: Input, acc: Seq[Expression]): ParseResult[(Seq[Expression], Seq[(String, Expression)])] = in match {
      case RParen ~:: rest => Success((acc, Seq()), rest)
      case (Identifier(_) | StringToken(_)) ~:: Colon ~:: _ =>
        namedCallParams(in) map { case (np, _) => (acc, np) }
      case _ => expression(in) flatMap {
        case (exp, Coma ~:: tail) => recur(tail, acc :+ exp)
        case (exp, RParen ~:: tail) => Success((acc :+ exp, Seq()), tail)
      }
    }
    recur(in, Seq())
  }

  def namedCallParams: Parser[Seq[(String, Expression)]] = Parser { in =>
    def recur(in: Input, acc: Seq[(String, Expression)]): ParseResult[Seq[(String, Expression)]] = in match {
      case RParen ~:: rest => Success(acc, rest)
      case (Identifier(_) | StringToken(_)) ~:: Colon ~:: _ => property(in) flatMap {
        case (prop, RParen ~:: tail) => Success(acc :+ prop, tail)
        case (prop, Coma ~:: tail) => recur(tail, acc :+ prop)
      }
    }
    recur(in, Seq())
  }

  // TODO pattern matching in call block
  def callBlock: Parser[CallBlock] = for {
    _ <- consumeSeq(LBrace)
    params <- choice {
      case s if s.dropWhile{ case Identifier(_) | Coma | EOL => true; case _ => false }.headOption.contains(LambdaArrow) =>
        funcDefParams(LambdaArrow)
      case _ => success(Seq())
    }
    body <- blockBody
  } yield CallBlock(params map VarPattern, body)

  def funcDefParams(EndToken: JsToken): Parser[Seq[String]] = Parser { in =>
    def recur(in: Input, acc: Seq[String]): ParseResult[Seq[String]] = in match {
      case EndToken ~:: tail => Success(acc, tail)
      case Identifier(p) ~:: EndToken ~:: tail => Success(acc :+ p, tail)
      case Identifier(p) ~:: Coma ~:: tail => recur(tail, acc :+ p)
    }
    recur(in, Seq())
  }

  def opExp(prefix: Expression) = for {
    op <- Parser { case (op: InfixOperator) #:: rest => Success(op, rest) }
    right <- expression
  } yield OperatorExpression(prefix, op, right)

//  def statement: Parser[Option[Expression]] = choice[Option[Expression]] {
//    case LBrace #:: _ => block.map(Some(_))
//    case (Semicolon | EOL) #:: _ => success(None)
//    case _ => for(e <- expression; _ <- expSeparator) yield Some(e)
//  }

  object StatementSeparator {
    def unapply(t: JsToken): Boolean = (t == EOL) || (t == Semicolon)
  }

  def block: Parser[Block] = Parser[Block] {
    case LBrace ~:: body =>
      blockBody(body).map{ case (exps, _) => Block(exps) }
  }
  def blockBody: Parser[Seq[Expression]] = Parser { in =>
    @tailrec
    def recur(in: Input, acc: Seq[Expression]): ParseResult[Seq[Expression]] = in match {
      case RBrace ~:: rest => Success(acc, rest)
      case Semicolon ~:: rest => recur(rest, acc)
      case _ =>
        expression(in) match {
          case Failure(msg) => Failure(msg)
          case Success(exp, RBrace ~:: tail) => Success(acc :+ exp, tail)
          case Success(exp, StatementSeparator() #:: tail) => recur(tail, acc :+ exp)
        }
    }
    recur(in, Seq())
  }

  def vanillaFor: Parser[VanillaFor] = for {
    _ <- consumeSeq(Keyword.For, LParen)
    init <- expression; _ <- consumeSeq(Semicolon)
    cond <- expression; _ <- consumeSeq(Semicolon)
    iter <- expression; _ <- consumeSeq(RParen)
    body <- expression
  } yield VanillaFor(init, cond, iter, body)

  def fohr: Parser[Expression] = for {
    _ <- consumeSeq(Keyword.For, LParen)
    stmnts <- forStatements
    comprehension <- Parser {
      case Keyword.Yield ~:: rest => Success(true, rest)
      case in => Success(false, in)
    }
    body <- expression
  } yield if(comprehension) ForComprehension(stmnts, body)
          else              ForEach(stmnts, body)

  def forStatements: Parser[Seq[ForStatement]] = Parser { in =>
    @tailrec
    def recur(in: Input, acc: Seq[ForStatement]): ParseResult[Seq[ForStatement]] = in match {
      case RParen ~:: rest => Success(acc, rest)
      case Semicolon ~:: rest => recur(rest, acc)
      case _ =>
        forStatement(in) match {
          case Failure(msg) => Failure(msg)
          case Success(exp, RParen ~:: tail) => Success(acc :+ exp, tail)
          case Success(exp, StatementSeparator() #:: tail) => recur(tail, acc :+ exp)
        }
    }
    recur(in, Seq())
  }

  def forStatement: Parser[ForStatement] = Parser {
    case Keyword.If ~:: rest =>
      expression(rest) map { case (e, _) => Filter(e) }
    case Identifier(n) ~:: GeneratorArrow ~:: rest =>
      expression(rest) map { case (e, _) => Generator(n, e) }
  }

  def property: Parser[(String, Expression)] = Parser {
    case StringToken(prop) ~:: Colon ~:: rest =>
      expression(rest) map { case (expr, _) => prop -> expr }
    case Identifier(prop) ~:: Colon ~:: rest =>
      expression(rest) map { case (expr, _) => prop -> expr }
  }

  def objectLiteral = for {
    _ <- consumeSeq(LBrace)
    props <- properties(RBrace)
  } yield ObjectLiteral(props)

  def properties(EndToken: JsToken) = Parser[Seq[(String, Expression)]] { in =>
    def recur(in: Input, acc: Seq[(String, Expression)]): ParseResult[Seq[(String, Expression)]] = in match {
      case EndToken ~:: tail => Success(acc, tail)
      case _ => property(in) flatMap {
        case (prop, EndToken ~:: tail) => Success(acc :+ prop, tail)
        case (prop, Coma ~:: tail) => recur(tail, acc :+ prop)
      }
    }
    recur(in, Seq())
  }

  def unOpExp: Parser[UnaryOperatorExpression] = Parser {
    case (op: UnaryOperator) ~:: rest =>
      expression(rest) map { case (e, _) => UnaryOperatorExpression(op, e) }
  }

  def whil: Parser[While] = for {
    _ <- consumeSeq(Keyword.While, LParen)
    cond <- expression
    _ <- consumeSeq(RParen)
    body <- expression
  } yield While(cond, body)

  def iff: Parser[If] = for {
    _ <- consumeSeq(Keyword.If, LParen)
    cond <- expression
    _ <- consumeSeq(RParen)
    body <- expression
    els <- Parser {
      case Keyword.Else ~:: rest =>
        expression(rest) map { case (e, _) => Some(e) }
      case in => Success(None, in)
    }
  } yield If(cond, body, els)

  def lambda: Parser[Function] = for {
    params <- lambdaParams
    expr <- expression
  } yield Function(params, expr)

  def lambdaParams: Parser[Seq[Pattern]] = Parser {
    case LambdaArrow ~:: rest => Success(Seq(), rest)
    case Identifier(p) ~:: LambdaArrow ~:: rest => Success(Seq(VarPattern(p)), rest)
    case LParen ~:: rest =>
      def recur(in: Input, acc: Seq[Pattern]): ParseResult[Seq[Pattern]] = in match {
        case RParen ~:: tail => Success(acc, tail)
        case _ => pattern(in) flatMap {
          case (p, RParen ~:: rest) => Success(acc :+ p, rest)
          case (p, Coma ~:: rest) => recur(rest, acc :+ p)
        }
      }
      recur(rest, Seq()) flatMap {
        case (params, LambdaArrow ~:: tail) => Success(params, tail)
      }
  }

  def paren: Parser[Paren] = for {
    _ <- consumeSeq(LParen)
    e <- expression
    _ <- consumeSeq(RParen)
  } yield Paren(e)

  def arrayLiteral = for {
    _ <- consumeSeq(LBracket)
    elems <- arrayLiteralBody
  } yield ArrayLiteral(elems)

  def arrayLiteralBody: Parser[Seq[Expression]] = Parser { in =>
    def recur(in: Input, acc: Seq[Expression]): ParseResult[Seq[Expression]] = in match {
      case RBracket ~:: tail => Success(acc, tail)
      case _ => expression(in) flatMap {
        case (expr, RBracket ~:: tail) => Success(acc :+ expr, tail)
        case (expr, Coma ~:: tail) => recur(tail, acc :+ expr)
      }
    }
    recur(in, Seq())
  }

  def range: Parser[Range] = for {
    _ <- consumeSeq(LBracket)
    from <- expression
    inclusive <- Parser {
      case Keyword.To ~:: rest => Success(true, rest)
      case Keyword.Until ~:: rest => Success(false, rest)
    }
    to <- expression
    step <- Parser {
      case Keyword.By ~:: rest =>
        expression(rest) map { case (e, _) => Some(e) }
      case rest => Success(None, rest)
    }
    _ <- consumeSeq(RBracket)
  } yield if(inclusive) InclusiveRange(from, to, step)
          else          ExclusiveRange(from, to, step)

  def functionDef: Parser[FunctionDef] = Parser {
    case Keyword.Function ~:: Identifier(name) ~:: rest => rest match {
      case LParen ~:: tail => funcDefParams(RParen)(tail) flatMap {
        case (params, blck @ (LBrace ~:: _)) =>
          block(blck) map { case (b, _) => FunctionDef(name, params, b) }
        case (params, Assign ~:: body) =>
          expression(body) map { case (e, _) => FunctionDef(name, params, e) }
      }
      case Assign ~:: tail =>
        expression(tail) map { case (e, _) => FunctionDef(name, Seq(), e)}
      case LBrace ~:: _ =>
        block(rest) map { case (b, _) => FunctionDef(name, Seq(), b) }
    }
  }

  def ret: Parser[Return] = for {
    _ <- consumeSeq(Keyword.Return)
    e <- expression
  } yield Return(e)

  def throvv: Parser[Throw] = for {
    _ <- consumeSeq(Keyword.Throw)
    e <- expression
  } yield Throw(e)

  def nevv: Parser[New] = for {
    _ <- consumeSeq(Keyword.New)
    c <- expression
  } yield New(c)


  def pattern: Parser[Pattern] = choice {
    case Identifier("_") ~:: rest => result(Success(IgnorePattern, rest))
    case Identifier(id) ~:: rest
      if id.head.isUpper => result(Success(ConstantPattern(VarAccess(id)), rest))
    case Identifier(id) ~:: rest => result(Success(VarPattern(id), rest))
    case NumberToken(_, n) ~:: rest => result(Success(ConstantPattern(NumberValue(n)), rest))
    case Keyword.True ~:: rest => result(Success(ConstantPattern(BooleanValue(true)), rest))
    case Keyword.False ~:: rest => result(Success(ConstantPattern(BooleanValue(false)), rest))
    case StringToken(str) ~:: rest => result(Success(ConstantPattern(StringValue(str)), rest))
    case LBracket ~:: _ => arrayPattern
    case LBrace ~:: _ => objectPattern
  }
  def patternLookahead(in: Input): Boolean = in match {
    case Identifier(_) ~:: _ => true
    case LBracket ~:: _ => true

    case _ => false
  }

  def arrayPattern: Parser[ArrayPattern] = for {
    _ <- consumeSeq(LBracket)
    (elems, tail) <- arrayPatternBody
  } yield ArrayPattern(elems, tail)
  def arrayPatternBody: Parser[(Seq[Pattern], Option[Pattern])] = Parser { in =>
    def recur(in: Input, acc: Seq[Pattern]): ParseResult[(Seq[Pattern], Option[Pattern])] = in match {
      case RBracket ~:: rest => Success((acc, None), rest)
      case _ => pattern(in) flatMap {
        case (p, RBracket ~:: rest) => Success((acc :+ p, None), rest)
        case (p, Coma ~:: rest) => recur(rest, acc :+ p)
        case (p, Colon ~:: rest) => pattern(rest) flatMap {
          case (t, RBracket ~:: tail) => Success((acc :+ p, Some(t)), tail)
          case (_, tok ~:: _) => Failure(s"RBracket expected, but $tok found.")
        }
      }
    }
    recur(in, Seq())
  }

  def objectPattern: Parser[ObjectPattern] = for {
    _ <- consumeSeq(LBrace)
    props <- objectPatternBody
  } yield ObjectPattern(props)
  def objectPatternBody: Parser[Seq[(String, Pattern)]] = Parser { in =>
    def recur(in: Input, acc: Seq[(String, Pattern)]): ParseResult[Seq[(String, Pattern)]] = in match {
      case RBrace ~:: rest => Success(acc, rest)
      case IdentifierOrString(prop) ~:: Colon ~:: rest => pattern(rest) flatMap {
        case (p, RBrace ~:: tail) => Success(acc :+ (prop -> p), tail)
        case (p, Coma ~:: tail) => recur(tail, acc :+ (prop -> p))
      }
    }
    recur(in, Seq())
  }

  object IdentifierOrString {
    def unapply(t: JsToken): Option[String] = t match {
      case Identifier(id) => Some(id)
      case StringToken(id) => Some(id)
      case _ => None
    }
  }

  def patternMatch(prefix: Expression): Parser[PatternMatch] = Parser { in =>

    def recur(in: Input, acc: Seq[MatchCase]): ParseResult[Seq[MatchCase]] = in match {
      case RBrace ~:: tail => Success(acc, tail)
      case Keyword.Case ~:: tail => matchCase(in) flatMap {
        case (m, RBrace ~:: rest) => Success(acc :+ m, rest)
        case (m, rest) => recur(rest, acc :+ m)
      }
    }

    in match {
      case Keyword.Match ~:: LBrace ~:: body =>
        recur(body, Seq()) map { case (s, _) => PatternMatch(prefix, s) }
    }
  }

  def matchCase: Parser[MatchCase] = for {
    _ <- consumeSeq(Keyword.Case)
    p <- pattern
    _ <- consumeSeq(LambdaArrow)
    e <- expression
  } yield MatchCase(p, e)

}

object NewParser extends NewParser {

  def main(args: Array[String]) {

  }

}
