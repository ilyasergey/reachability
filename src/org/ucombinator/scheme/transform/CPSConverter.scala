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

class CPSConverter extends ProgramTransformer {

  var atomicsCanMutate = false
  var atomicsCanPerformIO = false
  var atomicsCanAllocate = true

  def isAtomic(exp: Exp) = exp.mustReturnOrFail &&
    (atomicsCanMutate || !exp.mayMutate) &&
    (atomicsCanPerformIO || !exp.mayPerformIO) &&
    (atomicsCanAllocate || !exp.mayAllocate)

  def apply(program: Program): Program = {
    program match {

      case Program(decs, List(ImplicitDef(exp)), Unspecified()) =>
        Program(decs, List(), convert(exp))

      case Program(decs, defs, init) => {
        convertTops(defs) {
          case (newDefs, sets) => {
            Program(decs, newDefs, convert(Sequence(sets, init)))
          }
        }
      }

      //case _ => throw new Exception()
    }
  }

  private def convertTops[A](defs: List[Def])(k: (List[Def], List[Exp]) => A): A = {
    defs match {
      case Nil => k(Nil, Nil)
      case d :: rest => {
        convertTops(rest) {
          case (restDefs, restExps) => {
            val lhs = d.name
            val rhs = d.value
            if (isAtomic(rhs)) {
              k(VarDef(lhs, convertAtom(rhs)) :: restDefs, restExps)
            } else {
              k(VarDef(lhs, Unspecified()) :: restDefs, SetVar(lhs, rhs) :: restExps)
            }
          }
        }
      }
    }
  }


  private def convert(exp: Exp): Exp = {
    val answer = SName.from("answer")
    convertQExp(exp)(KLambda(Formals(List(PosFormal(answer)), None), ExpBody(Ref(answer))))
  }

  private def convertAtom(exp: Exp): Exp = {
    exp match {
      case _: Ref => exp
      case _: Lit => exp
      case _: Unspecified => exp
      case prim: Prim => CPS(prim)

      case CellGet(cell) =>
        CellGet(convertAtom(cell))

      case MakeCell(value) =>
        MakeCell(convertAtom(value))

      case MakeStruct(ty, values) =>
        MakeStruct(ty, values map convertAtom)
      case StructGet(base, field, ty) =>
        StructGet(convertAtom(base), field, ty)

      case Lambda(formals, body) => {
        val cc = SName.gensym("cc")
        ULambda(KeywordFormal(SKeyword.from("cc"), cc) :: formals,
          convertQBody(body)(Ref(cc)))
      }

      case App(f: Prim, arguments) => {
        App(f, arguments map convertAtom)
      }

      // TODO: [ilya] check correctness of this
      case a@And(_) => convertAtom(a.toIf)
      case o@Or(_) => convertAtom(o.toIf)
      case cond: Cond => convertAtom(cond.toIf)

      case If(cond, ifTrue, ifFalse) => convertAtom(App(cond, Arguments(List(PosArgument(ifTrue), PosArgument(ifFalse)), None)))
    }
  }

  def convertExp(exp: Exp)(k: Exp => Exp): Exp = {
    exp match {

      case _ if isAtomic(exp) => k(convertAtom(exp))

      case Let1(name, value, body) if isAtomic(value) =>
        Let(Bindings(List(Binding(name, convertAtom(value)))),
          convertBody(body)(k))

      case Let1(name, value, body) =>
        convertQExp(value)(KLambda(new Formals(List(PosFormal(name))),
          convertBody(body)(k)))

      case App(Prim(ioOp@("display" | "newline"), false), arguments) =>
        convertArguments(arguments)(arguments =>
          Sequence(App(Prim(ioOp, false), arguments),
            k(Unspecified())))

      case _ => {
        convertQExp(exp)(cont(k))
      }
    }
  }

  private def convertExps(exps: List[Exp])(k: List[Exp] => Exp): Exp = {
    exps match {
      case Nil => k(Nil)
      case hd :: tl =>
        convertExp(hd)(hd =>
          convertExps(tl)(tl =>
            k(hd :: tl)))
    }
  }

  private def convertQExp(exp: Exp)(q: Exp): Exp = {
    exp match {
      case _ if isAtomic(exp) => new App(q, convertAtom(exp))

      case Let1(name, value, body) if isAtomic(value) =>
        Let(Bindings(List(Binding(name, convertAtom(value)))),
          convertQBody(body)(q))

      case Let1(name, value, body) =>
        convertQExp(value)(KLambda(new Formals(List(PosFormal(name))),
          convertQBody(body)(q)))

      case let: Let =>
        convertQExp(let.toLetStar)(q)

      case let: LetStar =>
        convertQExp(let.toLets)(q)

      case If(condition, ifTrue, ifFalse) => {
        let(q)(q =>
          convertExp(condition)(condition =>
            If(condition,
              convertQExp(ifTrue)(q),
              convertQExp(ifFalse)(q))))
      }

      case Sequence(App(op@Prim(_, false), arguments), next) =>
        convertArguments(arguments)(arguments =>
          Sequence.strict(App(op, arguments),
            convertQExp(next)(q)))

      case Sequence(SetVar(name, value), next) =>
        convertExp(value)(value =>
          Sequence.strict(SetVar(name, value),
            convertQExp(next)(q)))

      case Sequence(SetCell(cell, value), next) =>
        convertExp(cell)(cell =>
          convertExp(value)(value =>
            Sequence.strict(SetCell(cell, value),
              convertQExp(next)(q))))

      case Sequence(first, next) =>
        convertExp(first)(first =>
          Sequence.strict(first,
            convertQExp(next)(q)))

      case SetVar(name, value) =>
        convertExp(value)(value =>
          Sequence.strict(SetVar(name, value),
            new App(q, Unspecified())))

      case SetCell(cell, value) =>
        convertExp(cell)(cell =>
          convertExp(value)(value =>
            Sequence.strict(SetCell(cell, value),
              new App(q, Unspecified()))))

      case Begin(ExpBody(exp)) =>
        convertQExp(exp)(q)

      case Begin(Body(List(), List())) =>
        convertQExp(Unspecified())(q)


      case App(Prim(ioOp@("display" | "newline"), false), arguments) =>
        convertArguments(arguments)(arguments =>
          Sequence(App(Prim(ioOp, false), arguments),
            new App(q, Unspecified())))


      case App(f, args) =>
        convertExp(f)(f =>
          convertArguments(args)(args =>
            App(f, KeywordArgument(SKeyword.from("cc"), q) :: args)))
    }
  }

  private def cont(k: Exp => Exp): Exp = {
    val rv = SName.gensym("rv")
    KLambda(new Formals(List(PosFormal(rv))),
      ExpBody(k(Ref(rv))))
  }

  private def let(exp: Exp)(k: Exp => Exp): Exp = {
    exp match {
      case _: Ref => k(exp)
      case _: Lit => k(exp)
      // case _ : Void => k(exp)
      case _: Unspecified => k(exp)
      case _: Prim => k(exp)
      case _: CPS => k(exp)

      case lam: Lambda => {
        val tmp = SName.gensym("$tmp")
        new App(KLambda(new Formals(List(PosFormal(tmp))), ExpBody(k(Ref(tmp)))),
          exp)
      }
    }
  }

  private def convertArguments(arguments: Arguments)(k: Arguments => Exp): Exp = {
    arguments match {
      case Arguments(List(), None) =>
        k(Arguments(List(), None))
      case Arguments(List(), Some(rest)) =>
        convertExp(rest)(rest =>
          k(Arguments(List(), Some(rest))))
      case Arguments(hd :: tl, rest) =>
        convertArgument(hd)(hd =>
          convertArguments(Arguments(tl, rest))(argsTl =>
            k(hd :: argsTl)))
    }
  }

  private def convertArgument(argument: Argument)(k: Argument => Exp): Exp = {
    argument match {
      case PosArgument(exp) =>
        convertExp(exp)(exp =>
          k(PosArgument(exp)))
      case KeywordArgument(keyword, exp) =>
        convertExp(exp)(exp =>
          k(KeywordArgument(keyword, exp)))
    }
  }

  private def convertBody(body: Body)(k: Exp => Exp): Body = {
    body match {
      case Body(List(), exps) => ExpBody(convertExp(Sequence(exps))(k))
      case ExpBody(exp) => ExpBody(convertExp(exp)(k))
    }
  }

  private def convertQBody(body: Body)(q: Exp): Body = {
    body match {
      case Body(List(), exps) => ExpBody(convertQExp(Sequence(exps))(q))
      case ExpBody(exp) => ExpBody(convertQExp(exp)(q))
    }
  }
}

object CPSConverter {

  def apply(exp: Exp): Exp = {
    val t = new CPSConverter
    t.convert(exp)
  }

}

