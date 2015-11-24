/*
 * Copyright (c) 2015,
 * Ilya Sergey, Christopher Earl, Matthew Might and David Van Horn
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *  Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 *  Neither the name of the project "Reachability" nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ucombinator.lambdajs.parsing

import org.ucombinator.lambdajs.parsing.LambdaJSTokens._
import util.parsing.combinator.Parsers
import org.ucombinator.lambdajs.syntax.LJSyntax._

/**
 * @author ilya
 */

class LambdaJSSlowParser extends Parsers {

  import LambdaJSSlowParser._

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

  def ident: Parser[String] = acceptIf(_.isInstanceOf[TIdent])(s => "Not an identifier " + s) ^^ {
    case i: TIdent => i.id
  }

  def op: Parser[String] = acceptIf(_.isInstanceOf[TOp])(_ => "Operation expected") ^^ {
    case TOp(s) => s
  }

  def string: Parser[String] = acceptIf(_.isInstanceOf[TString])(_ => "Operation expected") ^^ {
    case TString(s) => s
  }

  def float: Parser[Double] = acceptIf(_.isInstanceOf[TFloat])(_ => "Operation expected") ^^ {
    case TFloat(f) => f
  }

  def int: Parser[Int] = acceptIf(_.isInstanceOf[TInt])(_ => "Operation expected") ^^ {
    case TInt(i) => i
  }

  def wrapped[T](p: Parser[T]): Parser[T] = LPar ~> p <~ RPar

  def binder: Parser[TIdent] = acceptIf (_.isInstanceOf[TIdent])(_ => "Not an identifier") ^^ (x => x.asInstanceOf[TIdent])

  def params: Parser[(List[Var], Set[(String, Var)])] =
    wrapped(rep(binder)) ^^ (ids => {
      val vars = ids.map {
        case TIdent(s) => Var(s, newStamp())
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

  def binding(bv: Map[String, Var]): Parser[(TIdent, Exp)] =
    wrapped(binder ~ exp(bv)) ^^ {
      case b ~ e => (b, e)
    }

  def bindings(bv: Map[String, Var]): Parser[(List[(Var, Exp)], Set[(String, Var)])] =
    wrapped(rep(binding(bv))) ^^ {
      case bs => {
        val pairs = bs.map {
          // Let does not ensure uniqueness of identifiers
          case (TIdent(s), e) => (Var(s, 0), e)
        }
        // val newBound = pairs.map(_._1).filter(_.stamp > 0).map(v => (v.name, v)).toSet
        (pairs, Set())
      }
    }

  def let(bv: Map[String, Var]): Parser[(List[(Var, Exp)], Exp)] = bindings(bv) >> {
    case (pairs, newBound) => exp(bv/* ++ newBound*/) ^^ (e => (pairs, e))
  }

  def realExp(bv: Map[String, Var]): Parser[Exp] = (
      TRec ~> rec(bv) ^^ (es => Record(es, newStamp()))
    | TLambda ~> lambda(bv) ^^ (pb => Fun(pb._1, pb._2, newStamp()))
    | TLet ~> let(bv) ^^ (be => be._1 match {
        case Nil => be._2
        case _ => {
          val (x, e) :: t = be._1.reverse
          val body = be._2
          t.foldLeft(Let(x, e, body, newStamp())) {
            case (result, (x1, e1)) => Let(x1, e1, result, newStamp())

          }
        }})
    | TIndex ~> (exp(bv) ~ exp(bv)) ^^ (p => Lookup(p._1, p._2, newStamp()))
    | TUpdate ~> (exp(bv) ~ exp(bv) ~ exp(bv)) ^^ {case x ~ y ~ z => Update(x, y, z, newStamp())}
    | TDel ~> (exp(bv) ~ exp(bv)) ^^ {case x ~ y => Del(x, y, newStamp())}
    | TSeq ~> (exp(bv) ~ exp(bv)) ^^ {case x ~ y => Seq(x, y, newStamp())}
    | TIf ~> (exp(bv) ~ exp(bv) ~ exp(bv)) ^^ {case x ~ y ~ z => If(x, y, z, newStamp())}
    | TLabel ~> (ident ~ exp(bv)) ^^ {case l ~ e => Labelled(l, e, newStamp())}
    | TBreak ~> (ident ~ exp(bv)) ^^ {case l ~ e => Break(l, e, newStamp())}
    | TWhile ~> (exp(bv) ~ exp(bv)) ^^ {case c ~ b => While(c, b, newStamp())}
    | TAsgn ~> (exp(bv) ~ exp(bv)) ^^ {case e1 ~ e2 => Asgn(e1, e2, newStamp())}
    | TRef ~> exp(bv) ^^ (e => Ref(e, newStamp()))
    | TDeref ~> exp(bv) ^^ (e => Deref(e, newStamp()))
    | TThrow ~> exp(bv) ^^ (e => Throw(e, newStamp()))
    | op ~ rep(exp(bv)) ^^ {case o ~ args => OpApp(Op(o, 0), args, newStamp())}
    // todo implement try-catch-finally
    | exp(bv) ~ rep(exp(bv)) ^^ {case e ~ es => App(e, es, newStamp())}
  )

  /**
   * Lambda JS expression parser
   *
   * @param bv A map for variables bound in a context
   * @return context-sensitive parser for expressions
   */
  def exp(bv: Map[String, Var]): Parser[Exp] = (
        wrapped(realExp(bv))
      | string ^^ EString
//      | int ^^ EInt
      | float ^^ EFloat
      | TTrue ^^^ EBool(true)
      | TFalse ^^^ EBool(false)
      | TUndef ^^^ EUndef
      | TNull ^^^ ENull
      | boundIdentifier(bv) ^^ (s => bv(s))
      | ident ^^ (s => Var(s, 0))
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

object LambdaJSSlowParser {
  private var maxSerialNumber = 1

  def newStamp(): Int = {
    maxSerialNumber += 1
    maxSerialNumber
  }

}
