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

import scala.collection.immutable.{SortedMap, TreeMap}

import org.ucombinator.scheme.syntax._

/**
Enures that no two points bind the same name.

 Also ensures that every term has a unique label.
 */
class Alphatizer extends ProgramTransformer {
  private type RenameEnv = SortedMap[SName, SName]

  private val bound = scala.collection.mutable.HashSet[SName]()

  private def renameIfSeen(name: SName, env: RenameEnv): RenameEnv = {
    if (bound contains name) {
      // Bound elsewhere.
      val newName = SName.gensym(name)
      bound += newName
      return env + ((name, newName))
    } else {
      // Not yet bound elsewhere.
      bound += name
      return env + ((name, name))
    }
  }

  private def renameIfSeen(names: List[SName], env: RenameEnv): RenameEnv = {
    names match {
      case hd :: tl => renameIfSeen(tl, renameIfSeen(hd, env))
      case Nil => env
    }
  }

  def apply(prog: Program): Program = {
    prog match {
      case Program(decs, defs, init) => {
        val initEnv: RenameEnv = TreeMap()
        val names = defs map (_.name)
        val newEnv = renameIfSeen(names, initEnv)
        Program(decs, defs map (alphatize(_)(newEnv)), alphatize(init)(newEnv))
      }
    }
  }

  private def alphatize(d: Def)(env: RenameEnv): Def = d match {
    case VarDef(lhs, rhs) => {
      VarDef(env(lhs), alphatize(rhs)(env))
    }
    case ImplicitDef(exp) => {
      ImplicitDef(alphatize(exp)(env))
    }
    case FunctionDef(name, formals, body) => {
      val (newFormals, newEnv) = alphatize(formals)(env)
      FunctionDef(env(name), newFormals, alphatize(body)(newEnv))
    }
  }

  private def alphatize(exp: Exp)(env: RenameEnv): Exp = exp match {
    case Ref(name) => (env get name) match {
      case Some(newName) => Ref(newName)
      case None => Ref(name)
    }

    case SelfLit(lit) => SelfLit(lit)
    case QuoteLit(lit) => QuoteLit(lit)
    case Unspecified() => Unspecified()
    case TypePredicate(ty) => TypePredicate(ty)
    // BUG: Need to rebuild this term anyway:
    case (p: Prim) => exp // BUG

    case Lambda(formals, body) => {
      val (newFormals, newEnv) = alphatize(formals)(env)
      Lambda(newFormals, alphatize(body)(newEnv))
    }

    case LetRec(bindings, body) => {
      val names = bindings.names
      val newEnv = renameIfSeen(names, env)
      val newNames = bindings.names map (newEnv(_))
      val newValues = bindings.values map (alphatize(_)(newEnv))
      val newBindings = Bindings(newNames, newValues)
      val newBody = alphatize(body)(newEnv)
      LetRec(newBindings, newBody)
    }

    case LetStar(bindings, body) => {
      alphatizeLetStarBindings(bindings.bindings)(body)(env)
    }

    case Let(bindings, body) => {
      val newValues = bindings.values map (alphatize(_)(env))
      val names = bindings.names
      val newEnv = renameIfSeen(names, env)
      val newNames = bindings.names map (newEnv(_))
      val newBindings = Bindings(newNames, newValues)
      val newBody = alphatize(body)(newEnv)
      Let(newBindings, newBody)
    }

    case SetVar(name, value) =>
      SetVar(env(name), alphatize(value)(env))

    case MakeStruct(ty, values) =>
      MakeStruct(ty, values map (alphatize(_)(env)))
    case StructGet(base, field, ty) =>
      StructGet(alphatize(base)(env), field, ty)

    /*
    case MakeCell(value) => MakeCell(alphatize (value) (env))
    case CellGet(cell) => CellGet(alphatize (cell) (env))
    case SetCell(cell,value) => SetCell(alphatize (cell) (env), alphatize (value) (env))
    */

    case Begin(body) => Begin(alphatize(body)(env))

    case If(cond, ifTrue, ifFalse) => {
      If(alphatize(cond)(env),
        alphatize(ifTrue)(env),
        alphatize(ifFalse)(env))
    }

    case Or(exps) => {
      Or(exps map (alphatize(_)(env)))
    }

    case And(exps) => {
      And(exps map (alphatize(_)(env)))
    }

    case Cond(clauses) => {
      Cond(clauses map (alphatize(_)(env)))
    }

    case App(f, args) => {
      App(alphatize(f)(env), alphatize(args)(env))
    }
  }

  private def alphatize(body: Body)(env: RenameEnv): Body = body match {
    case Body(defs, exps) => {
      val names = defs map (_.name)
      val newEnv = renameIfSeen(names, env)
      Body(defs map (alphatize(_)(newEnv)), exps map (alphatize(_)(newEnv)))
    }
  }

  private def alphatize(clause: CondClause)(env: RenameEnv): CondClause = clause match {
    case TestCondClause(test, exps) => TestCondClause(alphatize(test)(env), exps map (alphatize(_)(env)))
    case SelfCondClause(test) => SelfCondClause(alphatize(test)(env))
    case ProcCondClause(test, proc) => ProcCondClause(alphatize(test)(env), alphatize(proc)(env))
    case ElseCondClause(exps) => ElseCondClause(exps map (alphatize(_)(env)))
  }

  private def alphatize(formals: Formals)(env: RenameEnv): (Formals, RenameEnv) = formals match {
    case Formals(lst, None) => {
      val (newList, newEnv) = alphatizeFormals(lst)(env)
      (Formals(newList, None), newEnv)
    }
    case Formals(List(), Some(name)) => {
      val newEnv = renameIfSeen(name, env)
      (Formals(List(), Some(newEnv(name))), newEnv)
    }
  }


  private def alphatizeFormals(formals: List[Formal])(env: RenameEnv): (List[Formal], RenameEnv) = formals match {
    case PosFormal(name) :: tl => {
      val newEnv = renameIfSeen(name, env)
      val (rest, restEnv) = alphatizeFormals(tl)(newEnv)
      (PosFormal(newEnv(name)) :: rest, restEnv)
    }
    case KeywordFormal(keyword, name) :: tl => {
      val newEnv = renameIfSeen(name, env)
      val (rest, restEnv) = alphatizeFormals(tl)(newEnv)
      (KeywordFormal(keyword, newEnv(name)) :: rest, restEnv)
    }
    case Nil => {
      (List(), env)
    }
    case _ => throw new Exception("Unhandled formals: " + formals)
  }

  private def alphatize(args: Arguments)(env: RenameEnv): Arguments = {
    args match {
      case Arguments(exps, rest) =>
        Arguments(exps map (alphatize(_)(env)), rest map (alphatize(_)(env)))
    }
  }

  private def alphatize(arg: Argument)(env: RenameEnv): Argument = {
    arg match {
      case PosArgument(exp) => PosArgument(alphatize(exp)(env))
      case KeywordArgument(keyword, exp) => KeywordArgument(keyword, alphatize(exp)(env))
    }
  }

  private def alphatizeLetStarBindings(bindings: List[Binding])(body: Body)(env: RenameEnv): Exp = bindings match {
    case Binding(name, value) :: Nil => {
      val newEnv = renameIfSeen(name, env)
      Let(Bindings(List(Binding(newEnv(name), alphatize(value)(env)))),
        alphatize(body)(newEnv))
    }

    case Binding(name, value) :: tl => {
      val newEnv = renameIfSeen(name, env)
      Let(Bindings(List(Binding(newEnv(name), alphatize(value)(env)))),
        Body(List(), List(alphatizeLetStarBindings(tl)(body)(newEnv))))
    }

    case Nil => {
      LetStar(Bindings(List()), alphatize(body)(env))
    }
  }
}
