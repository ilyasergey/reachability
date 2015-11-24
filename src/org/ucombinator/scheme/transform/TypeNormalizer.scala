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

/**
Lifts and names complex types.
 */

class TypeNormalizer extends ProgramTransformer {

  private var prog: Program = null

  private var addedDecs: List[Dec] = null

  private var typeTable: scala.collection.mutable.Map[Type, SName] = null

  def apply(prog: Program): Program = {
    this.prog = prog
    this.addedDecs = Nil
    this.typeTable = scala.collection.mutable.HashMap[Type, SName]()

    prog match {
      case Program(decs, defs, init) => {
        val newDecs = decs map normalize
        val newDefs = defs map normalize
        val newInit = normalize(init)
        Program(newDecs ++ addedDecs, newDefs, newInit)
      }
    }
  }

  private def addType(name: SName, ty: Type) {
    typeTable(ty) = name
  }

  private def nameFor(ty: Type): Type = {
    ty match {
      case (_: NamedType) => ty
      case _ => {}
    }

    (typeTable get ty) match {
      case Some(name) => NamedType(name)
      case None => {
        val newName = SName.gensym("type")
        typeTable(ty) = newName
        addedDecs = TypeDec(newName, ty) :: addedDecs
        NamedType(newName)
      }
    }
  }

  private def normalize(d: Dec): Dec = {
    d match {
      case TypeDec(name, ty) => {
        if (typeTable contains ty) {
          typeTable(NamedType(name)) = typeTable(ty)
          EmptyDec()
        } else {
          addType(name, ty)
          d
        }
      }
    }
  }

  private def normalize(d: Def): Def = {
    d match {
      case VarDef(name, value) => {
        VarDef(name, normalize(value))
      }
      case ImplicitDef(value) => {
        ImplicitDef(normalize(value))
      }
      case FunctionDef(name, formals, body) => {
        FunctionDef(name, formals, normalize(body))
      }
    }
  }


  private def normalize(exp: Exp): Exp = {
    // println("type normalizing: " + exp) ;

    exp match {
      case (_: Unspecified) => exp
      case (_: Ref) => exp
      case (_: Lit) => exp

      case TypePredicate(ty: Type) => {
        TypePredicate(nameFor(ty))
      }
      case (_: Prim) => {
        exp
      }

      case Lambda(formals, body) => {
        Lambda(normalize(formals), normalize(body))
      }
      case Closure(lam, ty, values) => {
        Closure(normalize(lam), nameFor(ty), values map normalize)
      }

      case Let(bindings, body) => {
        Let(bindings map {
          case Binding(name, value) => Binding(name, normalize(value))
        },
          normalize(body))
      }

      case Sequence(first, second) => {
        Sequence(normalize(first), normalize(second))
      }

      case SetVar(name, value) => {
        SetVar(name, normalize(value))
      }


      case MakeStruct(ty, values) => {
        MakeStruct(nameFor(ty), values map normalize)
      }
      case StructGet(base, field, ty) => {
        StructGet(normalize(base), field, nameFor(ty))
      }

      case App(f, arguments) => {
        App(normalize(f), normalize(arguments))
      }

      case Call(f, key, arguments) => {
        Call(normalize(f), key, normalize(arguments))
      }
    }
  }

  private def normalize(formals: Formals): Formals = {
    // TODO: If formal parameters get types, change this code.
    formals
  }

  private def normalize(arguments: Arguments): Arguments = {
    arguments map normalize
  }

  private def normalize(body: Body): Body = {
    body match {
      case Body(defs, exps) => {
        Body(defs map normalize, exps map normalize)
      }
    }
  }
}
