package org.ucombinator.lambdajs.parsing

import org.ucombinator.lambdajs.syntax.LJSyntax._
import org.ucombinator.lambdajs.parsing.LambdaJSTokens._
import util.matching.Regex
import util.parsing.combinator.{Parsers, RegexParsers}
import util.parsing.input.Reader

/**
 * @author ilya
 */

class LambdaJSParser extends Parsers {

  import LambdaJSParser._

  // Default constructor
  type Elem = LJToken

  //  def bind(s: String, v: Var) = new LambdaJSParser(bound + ((s, v)))
  //
  //  def bindMany(pairs: Set[(String, Var)]) = new LambdaJSParser(bound ++ pairs)

  def isBound(s: String, map: Map[String, Var]): Boolean = map.keySet.contains(s)

  def boundIdentifier(bv: Map[String, Var]): Parser[String] = acceptIf {
    case TIdent(s) => isBound(s, bv)
    case _ => false
  }(s => "Unbound identifier " + s) ^^ {
    case TIdent(s) => s
  }

  def globIdent: Parser[String] = acceptIf(_.isInstanceOf[TGlobalIdent])(_ => "Global identifier expected") ^^ {
    case TGlobalIdent(s) => s
  }

  def op: Parser[String] = acceptIf(_.isInstanceOf[TOp])(_ => "Operation expected") ^^ {
    case TOp(s) => s
  }

  def string: Parser[String] = acceptIf(_.isInstanceOf[TString])(_ => "Operation expected") ^^ {
    case TString(s) => s
  }

  def float: Parser[Float] = acceptIf(_.isInstanceOf[TFloat])(_ => "Operation expected") ^^ {
    case TFloat(f) => f
  }

  def int: Parser[Int] = acceptIf(_.isInstanceOf[TInt])(_ => "Operation expected") ^^ {
    case TInt(i) => i
  }

  def param: Parser[LJIdent] = acceptIf {
    case TGlobalIdent(_) | TIdent(_) => true
    case _ => false
  }(_ => "Not an identifier") ^^ (x => x.asInstanceOf[LJIdent])

  def params: Parser[(List[Var], Set[(String, Var)])] =
    LPar ~> rep(param) <~ RPar ^^ (ids => {
      val vars = ids.map {
        case TIdent(s) => Var(s, newStamp())
        case TGlobalIdent(s) => Var(s, 0)
      }
      val newBound = vars.filter(_.stamp > 0).map(v => (v.name, v)).toSet
      (vars, newBound)
    })

  def entry(bv: Map[String, Var]): Parser[(String, Exp)] = LPar ~> (string ~ exp(bv)) <~ RPar ^^ {
    case s ~ e => (s, e)
  }

  def rec(bv: Map[String, Var]): Parser[List[(String, Exp)]] = rep(entry(bv))


  /**
   * Lambda JS expression parser
   *
   * @param bv A map for variables bound in a context
   * @return context-sensitive parser for expressions
   */
  def exp(bv: Map[String, Var]): Parser[Exp] = (
    string ^^ EString
      | int ^^ EInt
      | float ^^ EFloat
      | TTrue ^^^ EBool(true)
      | TFalse ^^^ EBool(false)
      | TUndef ^^^ EUndef
      | TNull ^^^ ENull
      | (LPar ~ TRec) ~> rec(bv) <~ RPar ^^ (es => Record(es, newStamp()))

      | (LPar ~ TLambda) ~> (params >> {
      case (vars, newBound) => exp(bv ++ newBound) ^^ (e => (vars, e))
    }) <~ RPar ^^ {
      case (params, body) => Fun(params, body, newStamp())
    }

      | globIdent ^^ (s => Var(s, 0))
      | boundIdentifier(bv) ^^ (s => bv(s))
    )


  def parseAll(input: Input): Exp = {
    val result = exp(Map.empty).apply(input)
    if (result.successful) {
      result.get
    } else {
      throw new Exception("Parsing failed at position " + result.next.pos + ";\n character: '" + result.next.first + "';\n at end: " + result.next.atEnd)
    }
  }

}

object LambdaJSParser {
  private var maxSerialNumber = 1

  def newStamp(): Int = {
    maxSerialNumber += 1
    maxSerialNumber
  }

}
