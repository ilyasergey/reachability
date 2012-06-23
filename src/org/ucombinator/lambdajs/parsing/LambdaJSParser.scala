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
  }(s => {
    val msg = "Unbound identifier " + s
    msg
  }) ^^ {
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

  def wrapped[T](p: Parser[T]): Parser[T] = LPar ~> p <~ RPar

  def binder: Parser[LJIdent] = acceptIf {
    case TGlobalIdent(_) | TIdent(_) => true
    case _ => false
  }(_ => "Not an identifier") ^^ (x => x.asInstanceOf[LJIdent])

  def params: Parser[(List[Var], Set[(String, Var)])] =
    wrapped(rep(binder)) ^^ (ids => {
      val vars = ids.map {
        case TIdent(s) => Var(s, newStamp())
        case TGlobalIdent(s) => Var(s, 0)
      }
      val newBound = vars.filter(_.stamp > 0).map(v => (v.name, v)).toSet
      (vars, newBound)
    })

  def entry(bv: Map[String, Var]): Parser[(String, Exp)] = wrapped(string ~ exp(bv)) ^^ {
    case s ~ e => (s, e)
  }

  def rec(bv: Map[String, Var]): Parser[List[(String, Exp)]] = rep(entry(bv))

  def lambda(bv: Map[String, Var]): Parser[(List[Var], Exp)] = params >> {
    case (vars, newBound) => exp(bv ++ newBound) ^^ (e => (vars, e))
  }

  def binding(bv: Map[String, Var]): Parser[(LJIdent, Exp)] =
    wrapped(binder ~ exp(bv)) ^^ {
      case b ~ e => (b, e)
    }

  def bindings(bv: Map[String, Var]): Parser[(List[(Var, Exp)], Set[(String, Var)])] =
    wrapped(rep(binding(bv))) ^^ {
      case bs => {
        val pairs = bs.map {
          case (TIdent(s), e) => (Var(s, newStamp()), e)
          case (TGlobalIdent(s), e) => (Var(s, 0), e)
        }
        val newBound = pairs.map(_._1).filter(_.stamp > 0).map(v => (v.name, v)).toSet
        (pairs, newBound)
      }
    }

  def let(bv: Map[String, Var]): Parser[(List[(Var, Exp)], Exp)] = bindings(bv) >> {
    case (pairs, newBound) => exp(bv ++ newBound) ^^ (e => (pairs, e))
  }

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
      | (LPar ~ TLambda) ~> lambda(bv) <~ RPar ^^ (pb => Fun(pb._1, pb._2, newStamp()))
      | (LPar ~ TLet) ~> let(bv) <~ RPar ^^ (be => {
          val (x, e) :: t = be._1.reverse
          val body = be._2
          t.foldLeft(Let(x, e, body, newStamp())) {
            case (result, (x1, e1)) => Let(x1, e1, result, newStamp())
          }
        })
      | (LPar ~ TIndex) ~> (exp(bv) ~ exp(bv)) <~ RPar ^^ (p => Lookup(p._1, p._2, newStamp()))
      | (LPar ~ TUpdate) ~> (exp(bv) ~ exp(bv) ~ exp(bv)) <~ RPar ^^ {case x ~ y ~ z => Update(x, y, z, newStamp())}
      | (LPar ~ TDel) ~> (exp(bv) ~ exp(bv)) <~ RPar ^^ {case x ~ y => Del(x, y, newStamp())}
      | (LPar ~ TSeq) ~> (exp(bv) ~ exp(bv)) <~ RPar ^^ {case x ~ y => Seq(x, y, newStamp())}
      | (LPar ~ TIf) ~> (exp(bv) ~ exp(bv) ~ exp(bv)) <~ RPar ^^ {case x ~ y~z => If(x, y, z, newStamp())}


      | globIdent ^^ (s => Var(s, 0))
      | boundIdentifier(bv) ^^ (s => bv(s))
      | wrapped(exp(bv) ~ rep(exp(bv))) ^^ {case e ~ es => App(e, es, newStamp())}
    )


  def parseAll(text: String): Exp = {
    val lexer = new LambdaJSLexer
    val input = new lexer.Scanner(text)
    val result = exp(Map.empty).apply(input)
    if (result.successful) {
      result.get
    } else {
      throw new Exception("Parsing failed at position " + result.next.pos + ";\n character: '" + result.next.first + "';\n at end: " + result.next.atEnd)
    }
  }

  def parseAllIn(filename: String): Exp = {
    val input = scala.io.Source.fromFile(filename).mkString("")
    parseAll(input)
  }

}

object LambdaJSParser {
  private var maxSerialNumber = 1

  def newStamp(): Int = {
    maxSerialNumber += 1
    maxSerialNumber
  }

}
