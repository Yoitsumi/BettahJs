package js

import js.JsToken.Keyword._
import js.JsToken.Operator._

import scala.collection.immutable.PagedSeq
import scala.language.implicitConversions
import scala.util.parsing.combinator.lexical.{Scanners, Lexical}
import scala.util.parsing.input.{PagedSeqReader, StreamReader}

/**
 * Created by Kamil on 16.10.2015.
 */
class Lexer extends Scanners {

  type Token = JsToken

  import JsToken._

  implicit def string2Token(str: String): Parser[String] = acceptSeq(str) ^^ (_.mkString)

  override def errorToken(msg: String): JsToken = ErrorToken(msg)

  override def token: Parser[JsToken] =
    positioned(
    ( identifier ||| keyword )
    | eol
    | inlineJs
    | number
    | string
    | "->" ^^^ LambdaArrow
    | "<-" ^^^ GeneratorArrow
    | operator
    | '(' ^^^ LParen
    | ')' ^^^ RParen
    | '[' ^^^ LBracket
    | ']' ^^^ RBracket
    | '{' ^^^ LBrace
    | '}' ^^^ RBrace
    | ',' ^^^ Coma
    | ".." ^^^ DotDot
    | '.' ^^^ Dot
    | ';' ^^^ Semicolon
    | ':' ^^^ Colon
    | '=' ^^^ Assign)


  def identifier: Parser[Identifier] =
    identifierStart ~ identifierPart.* ^^ {
      case s ~ r => Identifier(s +: r.mkString)
    }

  def identifierStart = elem("a letter", _.isLetter) | '_' | '$'
  def identifierPart = identifierStart | digit

  def keyword =
    ( "if" ^^^ If
    | "function" ^^^ Function
    | "while" ^^^ While
    | "for" ^^^ For
    | "true" ^^^ True
    | "false" ^^^ False
    | "var" ^^^ Var
    | "else" ^^^ Else
    | "yield" ^^^ Yield
    | "to" ^^^ To
    | "until" ^^^ Until
    | "by" ^^^ By
    | "return" ^^^ Return
    | "throw" ^^^ Throw
    | "new" ^^^ New
    | "case" ^^^ Case
    | "match" ^^^ Match )

  def number =
    digit.+ ~ ('.' ~> digit.+).? ^^ {
      case a ~ b =>
        val str = a.mkString ++ "." ++ (b getOrElse Seq()).mkString
        NumberToken(str, str.toDouble)

    }

  def digit: Parser[Elem] = elem("digit", _.isDigit)

  def string =
    ( '\'' ~> singleStringChar.* <~ '\'' |
      '"' ~> doubleStringChar.* <~ '"' ) ^^ (x => StringToken(x.mkString))
  def stringEscape: Parser[String] = "\\\"" | "\\n" | "\\'" | "\\t"
  def singleStringChar = elem("Single quoted string character", _ != '\'') ^^ (_.toString) | stringEscape
  def doubleStringChar = elem("Double quoted string character", _ != '\"') ^^ (_.toString) | stringEscape

  def eol =
    ( actualEol
    | lineComment
    | blockComment ) ^^^ EOL

  def actualEol =
    ( "\r\n"
    | "\n"
    | "\r" )

  def lineComment = "//" ~ elem("not end of line", c => c != '\n' && c != '\r').* ~ actualEol

  def blockComment = "/*" ~ not("*/").* ~ "*/"

  override def whitespace: Parser[Any] = (elem(' ') | elem('\t') ).*

  def operator =
    ( "+" ^^^ Plus
    | "-" ^^^ Minus
    | "*" ^^^ Multiply
    | "/" ^^^ Divide
    | "%" ^^^ Modulo
    | "^" ^^^ Xor
    | "&&" ^^^ LogicalAnd
    | "&" ^^^ BitAnd
    | "||" ^^^ LogicalOr
    | "|" ^^^ BitOr
    | ">=" ^^^ GreaterEq
    | ">" ^^^ Greater
    | "<=" ^^^ LesserEq
    | "<" ^^^ Less
    | "===" ^^^ StrictEqual
    | "==" ^^^ Equal
    | "!==" ^^^ StrictNotEqual
    | "!=" ^^^ NotEqual
    | "~" ^^^ BitNegate
    | "!" ^^^ LogicalNegate )


  def inlineJs =
    inlineJsStart ~> inlineJsBodyChar.+ <~ inlineJsEnd ^^ (x => InlineJSToken(x.mkString))
  def inlineJsStart = '`'
  def inlineJsEnd = '`'
  def inlineJsBodyChar = elem("not ` (backtick)", _ != '`')
}

object Lexer {
  def main(args: Array[String]) {
    val l = new Lexer
    printAll(new l.Scanner(new PagedSeqReader(PagedSeq.fromFile("test.js"))))

    def printAll(s: l.Scanner): Unit =
      if(!s.atEnd) {
        println(s.first)
        printAll(s.rest)
      }
  }
}
