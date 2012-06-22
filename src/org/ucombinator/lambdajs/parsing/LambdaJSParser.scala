package org.ucombinator.lambdajs.parsing

import org.ucombinator.lambdajs.syntax.LJSyntax._
import org.ucombinator.lambdajs.parsing.LambdaJSTokens._
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}

/**
 * @author ilya
 */

class LambdaJSParser(val bound: Map[String, Var]) extends Parsers {

  import LambdaJSParser._

  // Default constructor
  type Elem = LJToken

  def this() = this(Map.empty)

  def bind(s: String, v: Var) = new LambdaJSParser(bound + ((s, v)))

  def isBound(s: String): Boolean = bound.keySet.contains(s)

  def boundIdentifier: Parser[String] = acceptIf {
    case TIdent(s) => isBound(s)
    case _ => false
  }(s => "Unbound identifier " + s) ^^ {
    case TIdent(s) => s
  }

  def exp: Parser[Exp] = (
    boundIdentifier ^^ (s => Var(s, newStamp()))
    )
}

object LambdaJSParser {
  private var maxSerialNumber = 1

  def newStamp(): Int = {
    maxSerialNumber += 1
    maxSerialNumber
  }

}
