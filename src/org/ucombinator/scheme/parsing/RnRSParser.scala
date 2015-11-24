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

package org.ucombinator.scheme.parsing

import org.ucombinator.scheme.syntax._

/**
An RnRSParser parses macro-expanded programs into an AST.
 */

class RnRSParser {

  import CommonSSymbols._;


  var markPrimitivesSafe = false;

  var expandQuotes = false;

  def parseExp(sexp: SExp): Exp = {
    // println("parsing exp: " + sexp) // DEBUG
    sexp match {

      // Literals:
      case _: SInt => SelfLit(sexp)
      case t: SText => SelfLit(t)
      case b: SBoolean => SelfLit(b)
      case c: SChar => SelfLit(c)

      // Special primitives:
      case STypeP :+: (ty) :+: SNil() => {
        TypePredicate(parseType(ty))
      }

      // Regular primitives:
      case p: SName if RnRSPrimitives.isPrimitive(p) => {
        Prim(p.string, markPrimitivesSafe)
      }

      case n: SName => Ref(n)

      case SQuote :+: sexp :+: SNil() => {
        if (expandQuotes)
          QuoteLit(sexp).expansion
        else
          QuoteLit(sexp)
      }

      // Quasi-quote templates:
      case SQuasiquote :+: qqexp :+: SNil() => {
        // println("processing quasiquote: " + qqexp) // DEBUG
        parseQuasiquote(1, qqexp)
      }

      // Functions:
      case SLambda :+: formals :+: body =>
        Lambda(parseFormals(formals), parseBody(body))

      // Side effects and sequencing:
      case SSetBang :+: (name: SName) :+: value :+: SNil() =>
        SetVar(name, parseExp(value))
      case SBegin :+: body =>
        Begin(parseBody(body))

      // Conditionals:
      case SIf :+: condition :+: ifTrue :+: ifFalse :+: SNil() =>
        If(parseExp(condition), parseExp(ifTrue), parseExp(ifFalse))
      case SIf :+: condition :+: ifTrue :+: SNil() =>
        If(parseExp(condition), parseExp(ifTrue), Unspecified())
      case SCond :+: clauses =>
        Cond(clauses.toList map parseCondClause)
      case SOr :+: exps =>
        Or(exps.toList map parseExp)
      case SAnd :+: exps =>
        And(exps.toList map parseExp)

      // Binding forms:
      case SLet :+: bindings :+: body =>
        Let(parseBindings(bindings), parseBody(body))
      case SLetStar :+: bindings :+: body =>
        LetStar(parseBindings(bindings), parseBody(body))
      case SLetRec :+: bindings :+: body =>
        LetRec(parseBindings(bindings), parseBody(body))

      // Structures:
      case SMakeStruct :+: ty :+: values =>
        MakeStruct(parseType(ty), values.toList map parseExp)
      case SStructGet :+: base :+: (field: SName) :+: ty :+: SNil() =>
        StructGet(parseExp(base), field, parseType(ty))

      // Applications:
      case fun :+: args =>
        App(parseExp(fun), parseArguments(args))
    }
  }

  def parseType(sx: SExp): Type = {
    sx match {
      case (name: SName) => NamedType(name)
    }
  }

  def parseQuasiquote(depth: Int, qqexp: SExp): Exp = {
    qqexp match {
      case SList(SUnquote, sexp) =>
        if (depth == 1) {
          parseExp(sexp)
        } else {
          ListExp(QuoteLit(SUnquote), parseQuasiquote(depth - 1, sexp))
        }

      case SList(SQuasiquote, sexp) =>
        ListExp(QuoteLit(SQuasiquote), parseQuasiquote(depth + 1, sexp))

      case hd@SList(SUnquoteSplicing, sexp) :+: tl => {
        if (depth == 1) {
          new App(Ref(SAppend), parseExp(sexp), parseQuasiquote(depth, tl))
        } else {
          ConsExp(ListExp(QuoteLit(SUnquoteSplicing), parseQuasiquote(depth - 1, sexp)), parseQuasiquote(depth, tl))
        }
      }

      case hd :+: tl =>
        ConsExp(parseQuasiquote(depth, hd), parseQuasiquote(depth, tl))

      case sexp =>
        QuoteLit(sexp)
    }
  }

  def parseDefExps(sxl: List[SExp]): (List[Def], List[Exp]) = sxl match {
    case Nil => (Nil, Nil)
    case hd :: tl => {
      val (defs, exps) = parseDefExps(tl)
      hd match {
        case SDefine :+: _ => (parseDef(hd) :: defs, exps)
        case _ => (defs, parseExp(hd) :: exps)
      }
    }
  }

  def parseBody(sexps: SExp): Body = {
    val sxl = sexps.toList
    val (defs, exps) = parseDefExps(sxl)
    Body(defs, exps)
  }

  def parseFormals(sxl: SExp): Formals = {
    if (sxl.isList) {
      val names = sxl.toList
      Formals(names map ((n: SExp) => PosFormal(n.asInstanceOf[SName])), None)
    } else if (sxl.isPair) {
      val (names, rest) = sxl.toDottedList
      if (names isEmpty)
        Formals(List(), Some(rest.asInstanceOf[SName]))
      else
        Formals(names map ((n: SExp) => PosFormal(n.asInstanceOf[SName])), Some(rest.asInstanceOf[SName]))
    } else if (sxl.isName) {
      Formals(List(), Some(sxl.asInstanceOf[SName]))
    } else {
      throw new Exception("Unhandled case for formals")
    }
  }

  def parseArguments(sxl: SExp): Arguments = {
    new Arguments(sxl.toList map (x => PosArgument(parseExp(x))))
  }

  def parseBinding(binding: SExp) = {
    binding match {
      case SList(name: SName, value: SExp) => Binding(name, parseExp(value))
    }
  }

  def parseBindings(bindings: SExp) = {
    val binds = bindings.toList
    Bindings(binds map parseBinding)
  }

  def parseDef(s: String): Def = {
    parseDef(SExp.parse(s))
  }

  def parseDef(sexp: SExp): Def = {
    sexp match {
      case SDefine :+: (name: SName) :+: value :+: SNil() =>
        VarDef(name, parseExp(value))
      case SDefine :+: ((name: SName) :+: formals) :+: body =>
        FunctionDef(name, parseFormals(formals), parseBody(body))
      case _ =>
        ImplicitDef(parseExp(sexp))
    }
  }

  def parseDec(sexp: SExp): Dec = {
    sexp match {
      case SDefineStruct :+: (name: SName) :+: fields :+: SNil() =>
        TypeDec(name, StrictStruct(fields.toList.asInstanceOf[List[SName]]))
    }
  }

  def parseCondClause(sexp: SExp): CondClause = {
    sexp match {
      case test :+: SNil() =>
        SelfCondClause(parseExp(test))

      case SElse :+: exps =>
        ElseCondClause(exps.toList map parseExp)

      case test :+: SRightArrow :+: proc :+: SNil() =>
        ProcCondClause(parseExp(test), parseExp(proc))

      case test :+: exps =>
        TestCondClause(parseExp(test), exps.toList map parseExp)
    }
  }

  def parseProgram(decs: List[Dec], defs: List[Def], exps: List[Exp])(sxl: List[SExp]): Program = {
    sxl match {
      case (d@(SDefine :+: _)) :: tl =>
        parseProgram(decs, parseDef(d) :: ((exps map (_.toDef)) ++ defs), Nil)(tl)

      case (d@(SDefineStruct :+: _)) :: tl =>
        parseProgram(parseDec(d) :: decs, defs, exps)(tl)

      case e :: tl =>
        parseProgram(decs, defs, parseExp(e) :: exps)(tl)

      case Nil =>
        Program(decs reverse, defs reverse, Sequence(exps reverse))
    }
  }

  def parseProgram(sxl: List[SExp]): Program = {
    //Program(List(),sxl map parseDef, Unspecified())
    parseProgram(List(), List(), List())(sxl)
  }

}


object RnRSParser {

  def apply(sexps: List[SExp]): Program = {
    val p = new RnRSParser
    p.parseProgram(sexps)
  }

}


/* RnRS primitives */
object RnRSPrimitives {

  // Safe primitives:
  def safe = List("*", "-", "+", "/",
    "quotient", "gcd", "modulo", "log",
    "ceiling",
    "<", "=", ">", "<=", ">=",
    "odd?", "even?", "char?", "symbol?", "list?", "null?", "integer?", "number?", "boolean?", "procedure?", "string?",
    // pair?
    "char-alphabetic?", "char-numeric?", "string<?",
    "eq?", "equal?", "eqv?", "char=?",
    "string-ref", "string-length", "string-append", "number->string", "list->string", "symbol->string",
    "string->symbol",
    "char->integer",
    "not",
    "length",
    "cons","car","cdr", "pair?",
    "newline", "display",
    "random",
    "apply",
    "error")

  // Unsafe primitives:
  def unsafe = List("#p:+/int", "#p:+/float", "#p:+/double")

  val list = safe ++ unsafe

  val prims: scala.collection.immutable.Set[String] =
    scala.collection.immutable.HashSet() ++ list


  def isPrimitive(exp: Exp): Boolean = {
    exp match {
      case Ref(name) => prims contains name.toString
    }
  }

  def isPrimitive(name: SName): Boolean = {
    prims contains name.toString
  }

}


