package org.ucombinator.lambdajs.parsing

import util.parsing.combinator.RegexParsers
import util.matching.Regex
import util.parsing.combinator.token.Tokens
import util.parsing.combinator.lexical.Lexical

/**
 * @author ilya
 */

class LambdaJSLexer extends Lexical with RegexParsers {

  import LambdaJSTokens._

  override type Elem = Char
  type Token = LJToken

  def whitespace = rep(whitespaceChar)

  override def skipWhitespace = true

  override protected val whiteSpace = new Regex("([\\r\\n\\t ]*([;][^\\r\\n]*[\\r\\n]?)?)+")

  def string: Parser[String] = (
    "\"\"" ^^^ ""
      | regex(new Regex("\"([^\"\\\\]|\\\\.|\\\\\\\\|)*\"")) ^^ (s => s.substring(1, s.length() - 1))
    )

  def float: Parser[TFloat] = (regex(new Regex("-?[0-9]+[\\.][0-9]+(e[0-9]+)?")) ^^ {
    case s => TFloat(java.lang.Double.parseDouble(s))
  })

  def integer: Parser[TInt] = regex(new Regex("-?[0-9]+")) ^^ {
    case s => throw new Exception("Integers are not used in LambdaJS!")
  }

  def number: Parser[LJToken] = (float | integer)


  def identOrOperator: Parser[LJToken] = regex(new Regex("([^.#; \\t\\r\n()',`\"][^; \\t\\r\\n()',`\"]*|[.][^; \\t\\r\\n()',`\"]+)")) ^^
    (str => if (operators.contains(str)) TOp(str)
    else {
      TIdent(str)
    })

  def token: Parser[LJToken] = (
    "#t" ^^^ TTrue
      | "#f" ^^^ TFalse
      | "null" ^^^ TNull
      | "undefined" ^^^ TUndef
      | "(" ^^^ LPar
      | ")" ^^^ RPar
      | number
      | string ^^ TString
      | "+nan.0" ^^^ TNan
      | "+inf.0" ^^^ TInfP
      | "-inf.0" ^^^ TInfM
      | "lambda" ^^^ TLambda
      | "get-field" ^^^ TIndex
      | "delete-field" ^^^ TDel
      | "update-field" ^^^ TUpdate
      | "alloc" ^^^ TRef
      | "deref" ^^^ TDeref
      | "while" ^^^ TWhile
      | "label" ^^^ TLabel
      | "break" ^^^ TBreak
      | "set!" ^^^ TAsgn
      | "begin" ^^^ TSeq
      | "object" ^^^ TRec
      | "throw" ^^^ TThrow
      | "if" ^^^ TIf
      | "let" ^^^ TLet
      | "try-catch" ^^^ TTryCatch
      | "try-finally" ^^^ TTryFinally
      | identOrOperator)


  def parseAll(input: String): List[LJToken] = {
    val result = parse(phrase(rep(token)), input)
    if (result.successful) {
      result.get
    } else {
      throw new Exception("Parsing failed at position " + result.next.pos + ";\n character: '" + result.next.first + "';\n at end: " + result.next.atEnd)
    }
  }

  def parseAllIn(filename: String): List[LJToken] = {
    val input = scala.io.Source.fromFile(filename).mkString("")
    parseAll(input)
  }

}

object LambdaJSTokens extends Tokens {

  val operators: Set[String] = Set(
    "+", "string-+", "%", "-", "*", "/", "===", "==", "<", "string-<", "\\|", "eval-semantic-bomb",
    "&", "|", "^", "~", "<<", ">>", ">>>",
    "to-integer", "to-uint-32", "to-int-32",
    "=",
    "typeof", "surface-typeof",
    "prim->number", "prim->string", "prim->bool",
    "has-own-prop?",
    "print-string",
    "str-contains", "str-startswith", "str-length", "str-split-regexp", "str-split-strexp",
    "regexp-quote", "regexp-match",
    "obj-iterate-has-next?", "obj-iterate-next", "obj-iterate-key",
    "obj-delete", "obj-can-delete?",
    "math-sin", "math-cos", "math-log", "math-exp", "math-abs", "math-pow",
    "prim?"
  )

  abstract sealed class LJToken(val chars: String) extends Token
  case object TInfP extends LJToken("+inf.0")
  case object TInfM extends LJToken("-inf.0")
  case object TNan extends LJToken("+nan.0")

  case object TTrue extends LJToken("true")
  case object TFalse extends LJToken("false")
  case object TUndef extends LJToken("undefined")
  case object TNull extends LJToken("null")
  case object LPar extends LJToken("(")
  case object RPar extends LJToken(")")
  case class TString(s: String) extends LJToken(s)
  case class TIdent(id: String) extends LJToken(id)
  case class TOp(op: String) extends LJToken(op)
  case class TFloat(f: Double) extends LJToken(f.toString)
  case class TInt(n: Int) extends LJToken(n.toString)
  case object TLambda extends LJToken("lambda")
  case object TLet extends LJToken("let")
  case object TDeref extends LJToken("deref")
  case object TRef extends LJToken("alloc")
  case object TSeq extends LJToken("begin")
  case object TRec extends LJToken("object")
  case object TLabel extends LJToken("label")
  case object TAsgn extends LJToken("set!")
  case object TIndex extends LJToken("get-field")
  case object TDel extends LJToken("delete-field")
  case object TUpdate extends LJToken("update-field")
  case object TWhile extends LJToken("while")
  case object TBreak extends LJToken("break")
  case object TTryCatch extends LJToken("try-catch")
  case object TTryFinally extends LJToken("try-finally")
  case object TIf extends LJToken("if")
  case object TThrow extends LJToken("throw")

}
