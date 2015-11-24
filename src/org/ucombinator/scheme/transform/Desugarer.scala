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

package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._

class Desugarer(default: Boolean) extends ProgramTransformer {

  var convertBodyToLetRec = default
  var convertLetRecToLetsAndSets = default
  var convertLetToLetStar = default
  var convertLetStarToLets = default
  var convertLetToRedEx = default
  var convertCaseToCond = default
  var convertCondToIf = default
  var convertAndToIf = default
  var convertOrToIf = default
  var convertQuasiquoteToCons = default

  var convertToUnsafePrimops = default


  def apply(prog: Program): Program = {
    prog match {
      case Program(decs, defs, init) => {
        Program(decs, defs map this.apply, this.apply(init))
      }
    }
  }

  def apply(dec: Dec): Dec = {
    dec
  }

  def apply(d: Def): Def = {
    d match {
      case VarDef(name, exp) =>
        VarDef(name, this(exp))
      case ImplicitDef(exp) =>
        ImplicitDef(this(exp))
      case FunctionDef(name, formals, body) =>
        FunctionDef(name, formals, this(body))
    }
  }

  def apply(exp: Exp): Exp = {
    exp match {
      case (_: Ref) => exp
      case (_: Lit) => exp
      case (_: Unspecified) => exp
      case (typ: TypePredicate) => exp
      case Prim(op, isSafe) if isSafe && convertToUnsafePrimops => {
        throw new Exception()
      }
      case (_: Prim) => exp

      case Lambda(formals, body) => {
        val newFormals = formals
        val newBody = this(body)
        Lambda(formals, body)
      }

      case (letrec: LetRec) if convertLetRecToLetsAndSets =>
        this(letrec.toLetsAndSets)
      case LetRec(bindings, body) => {
        val newBindings = bindings map {
          case Binding(name, exp) => Binding(name, this(exp))
        }
        val newBody = this(body)
        LetRec(newBindings, newBody)
      }

      case Let(binds@Bindings(List()), body) => {
        this(Let(binds, this(body)))
      }
      case (let: Let) if convertLetToRedEx => {
        this(let.toRedEx)
      }
      case (let: Let) if convertLetToLetStar => {
        this(let.toLetStar)
      }
      case Let(bindings, body) => {
        val newBindings = bindings map {
          case Binding(name, exp) => Binding(name, this(exp))
        }
        val newBody = this(body)
        Let(newBindings, newBody)
      }
      case (lets: LetStar) if convertLetStarToLets => {
        this(lets.toLets)
      }
      case LetStar(bindings, body) => {
        val newBindings = bindings map {
          case Binding(name, exp) => Binding(name, this(exp))
        }
        val newBody = this(body)
        LetStar(newBindings, newBody)
      }

      case (cond: Cond) if convertCondToIf =>
        this(cond.toIf)

      case (and: And) if convertAndToIf =>
        this(and.toIf)
      case And(exps) =>
        And(exps map this.apply)

      case (or: Or) if convertOrToIf =>
        this(or.toIf)
      case Or(exps) =>
        Or(exps map this.apply)

      case If(cond, ifTrue, ifFalse) =>
        If(this(cond), this(ifTrue), this(ifFalse))

      case Begin(body) =>
        Begin(this(body))
      case SetVar(name, value) =>
        SetVar(name, this(value))

      case MakeStruct(ty, values) =>
        MakeStruct(ty, values map this.apply)
      case StructGet(base, field, ty) =>
        StructGet(this(base), field, ty)

      case App(f, arguments) =>
        App(this(f), arguments map this.apply)

      case _ => throw new Exception("Unhandled expression: " + exp)
    }
  }

  def apply(body: Body): Body = {
    body match {
      case _ if convertBodyToLetRec =>
        ExpBody(body.toLetRec)
      case Body(defs, exps) =>
        Body(defs map this.apply, exps map this.apply)
    }
  }
}
