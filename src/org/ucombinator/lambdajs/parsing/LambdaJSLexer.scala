package org.ucombinator.lambdajs.parsing

import util.parsing.combinator.RegexParsers
import util.matching.Regex

/**
 * @author ilya
 */

class LambdaJSLexer extends RegexParsers {

  import LambdaJSTokens._

  override def skipWhitespace = true

  override protected val whiteSpace = new Regex("([\\r\\n\\t ]*([;][^\\r\\n]*[\\r\\n]?)?)+")

  def string: Parser[String] = (
    "\"\"" ^^^ ""
      | regex(new Regex("\"([^\"\\\\]|\\\\.|\\\\\\\\|)*\"")) ^^ (s => s.substring(1, s.length() - 1))
    )

  def float: Parser[TFloat] = (regex(new Regex("-?[0-9]+[\\.][0-9]+")) ^^ {
    case s => TFloat(java.lang.Float.parseFloat(s))
  })

  def integer: Parser[TInt] = regex(new Regex("-?[0-9]+")) ^^ {
    case s => TInt(Integer.parseInt(s))
  }

  def number: Parser[LJToken] = (float | integer)


  def identOrOperator: Parser[LJToken] = regex(new Regex("([^.#; \\t\\r\n()',`\"][^; \\t\\r\\n()',`\"]*|[.][^; \\t\\r\\n()',`\"]+)")) ^^
    (str => if (operators.contains(str)) TOp(str) else TIdent(str))

  def token: Parser[LJToken] = (
    "#t" ^^^ TTrue
      | "#f" ^^^ TFalse
      | "null" ^^^ TNull
      | "undefined" ^^^ TUndef
      | "(" ^^^ LPar
      | ")" ^^^ RPar
      | number
      | string ^^ TString
      | "lambda" ^^^ TLambda
      | "get-field" ^^^ TIndex
      | "delete-field" ^^^ TDel
      | "update-field" ^^^ TUpdate
      | "alloc" ^^^ TRef
      | "deref" ^^^ TDeref
      | "while" ^^^ TWhile
      | "break" ^^^ TBreak
      | "throw" ^^^ TThrow
      | "if" ^^^ TIf
      | "let" ^^^ TLet
      | "try" ^^^ TTry
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

object LambdaJSTokens {

  val operators: Set[String] = Set(
    "+", "string-+", "%", "-", "*", "/", "===", "==", "<", "string-<",
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

  abstract sealed class LJToken

  case object TTrue extends LJToken

  case object TFalse extends LJToken

  case object TUndef extends LJToken

  case object TNull extends LJToken

  case object LPar extends LJToken

  case object RPar extends LJToken

  case class TString(s: String) extends LJToken

  case class TIdent(id: String) extends LJToken

  case class TOp(op: String) extends LJToken

  case class TFloat(f: Float) extends LJToken

  case class TInt(n: Int) extends LJToken

  case object TLambda extends LJToken

  case object TLet extends LJToken

  case object TDeref extends LJToken

  case object TRef extends LJToken

  case object TSet extends LJToken

  case object TIndex extends LJToken

  case object TDel extends LJToken

  case object TUpdate extends LJToken

  case object TWhile extends LJToken

  case object TBreak extends LJToken

  case object TTry extends LJToken

  case object TIf extends LJToken

  case object TThrow extends LJToken

}
