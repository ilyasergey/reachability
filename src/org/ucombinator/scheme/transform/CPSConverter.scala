/*
 * CRAPL 2012.
 * U Combinator, University of Utah
 * DistriNet, KU Leuven
 *
 * THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
 * APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
 * HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT
 * WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
 * DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
 * CORRECTION.
 *
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
 * WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR
 * CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
 * INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT
 * NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR
 * LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM
 * TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER
 * PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * If you have questions or concerns about the CRAPL, or you need more
 * information about this license, please contact:
 *
 *    Matthew Might
 *    http://matt.might.net/
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

