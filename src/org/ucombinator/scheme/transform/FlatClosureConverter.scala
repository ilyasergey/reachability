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
import scala.collection.immutable.{Map => ImmMap}

class FlatClosureConverter extends ProgramTransformer {

  private var prog: Program = null

  // BUG: Keyword must be unused elsewhere.
  val currentClosureKeyword = SKeyword.from("clo")
  val environmentName = SName.gensym("env")


  def apply(prog: Program): Program = {
    this.prog = prog
    prog match {
      case Program(decs, defs, init) => {
        val newDecs = decs
        val newDefs = defs map this.apply
        Program(newDecs, newDefs, this.apply(init))
      }
    }
  }

  private def apply(exp: Exp): Exp = {
    exp match {

      // WARNING: WATCH CASE ORDERING AND ATOMICS!

      case Ref(name) => {
        if (prog.valueOfGlobal(name).isLambda) {
          Closure(Ref(name), ClosureStruct(List()), List())
        } else {
          exp
        }
      }

      case Begin(Body(List(), cmds)) =>
        Begin(Body(List(), cmds map this.apply))

      case lam@Lambda(formals, body) => {
        val convertedBody = this.apply(body)
        val frees = ((lam.free -- prog.globals).toList) sort ((a: SName, b: SName) => (a <= b))
        val t = ClosureStruct(frees)
        val values = frees map (name => Ref(name))
        // val newEnv = MakeStruct(t,values)

        val env = Ref(environmentName)
        val newFormals = KeywordFormal(currentClosureKeyword, environmentName) :: formals
        val rebinds = ImmMap() ++ (frees map (name => (name, StructGet(env, name, t))))
        val newBody = convertedBody substitute rebinds
        Closure(Lambda(newFormals, newBody), t, values)
      }

      case Let(bindings, body) =>
        Let(bindings map (_.map(this.apply)), this.apply(body))


      case SetCell(exp, value) =>
        SetCell(this(exp), this.apply(value))

      case SetVar(name, value) =>
        SetVar(name, this.apply(value))

      case (_: Unspecified | _: CPS | _: Lit) => exp

      case _: Prim => Closure(exp, ClosureStruct(List()), List())

      case MakeStruct(ty, values) =>
        MakeStruct(ty, values map this.apply)
      case StructGet(base, field, ty) =>
        StructGet(this(base), field, ty)

      case MakeCell(value) => MakeCell(this.apply(value))

      case CellGet(cell) => CellGet(this.apply(cell))

      case App(f: Prim, arguments) =>
        App(f, arguments map this.apply)

      case App(f@Ref(name), args) if prog.globals contains name => {
        App(f, KeywordArgument(currentClosureKeyword, Unspecified()) :: (args map this.apply))
      }

      case App(f, args) => {
        Call(this.apply(f), currentClosureKeyword, args map this.apply)
      }
    }
  }

  private def apply(body: Body): Body = {
    body match {
      case Body(List(), exps) => Body(List(), exps map this.apply)
    }
  }

  private def apply(d: Def): Def = {
    // Assumes global.

    d match {
      case VarDef(name, lam@Lambda(formals, body)) => {
        val clo = this.apply(lam)
        clo match {
          case Closure(newLam, ClosureStruct(List()), List()) => VarDef(name, newLam)
        }
      }
      case VarDef(name, exp) => {
        VarDef(name, this.apply(exp))
      }
      case _ => throw new Exception()
    }
  }
}
