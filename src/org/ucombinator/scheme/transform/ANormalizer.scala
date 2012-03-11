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

// TODO: ANormalize letrec into letrec.
object ANormalizer {
  def apply(p: Program): Exp = {
    val t = new ANormalizer
    val e = p.toExp
    // println ("e: " + e) // DEBUG
    t.normalize(e)
  }

  def size(e: Exp): Int = e match {
    case Ref(name) => 1
    case Prim(name, safe) => 1
    case Unspecified() => 1
    case Lit(sexp) => 1
    case App(fun, args) => {
      size(fun) + args.args.foldLeft(0)((acc, arg) => acc + size(arg.exp))
    }
    case Lambda(formals, body) => {
      1 + body.exps.foldLeft(0)((acc, e) => acc + size(e))
    }
    case If(condition, ifTrue, ifFalse) => {
      size(condition) + size(ifTrue) + size(ifFalse)
    }
    case SetVar(name, value) => 1 + size(value)
    case Values(args) => {
      args.args.foldLeft(0)((acc, arg) => acc + size(arg.exp))
    }
    case LetValues(formals, values, body) => {
      val n1 = values.foldLeft(0)((acc, e) => acc + size(e))
      val n2 = body.exps.foldLeft(0)((acc, e) => acc + size(e))
      n1 + n2
    }
    case Begin(body) => {
      body.exps.foldLeft(0)((acc, e) => acc + size(e))
    }
    case And(exps) => {
      1 + exps.foldLeft(0)((acc, e) => acc + size(e))
    }
    case Or(exps) => {
      1 + exps.foldLeft(0)((acc, e) => acc + size(e))
    }
    case Let(bindings, body) => {
      val n1 = bindings.bindings.foldLeft(0)((acc, b) => acc + size(b.value))
      val n2 = body.exps.foldLeft(0)((acc, e) => acc + size(e))
      n1 + n2
    }
    case e => {
      throw new Exception("Expression supported for size computing:\n" + e.toString)
    }
  }
}


class ANormalizer extends ProgramTransformer {

  var convertBodyToLetRec = true
  var convertLetRecToLetsAndSets = true

  var atomicsCanMutate = false
  var atomicsCanPerformIO = false
  var atomicsCanAllocate = false

  def isAtomic(exp: Exp) =
    exp.isLambda ||
      (exp.mustReturnOrFail &&
        (atomicsCanMutate || !exp.mayMutate) &&
        (atomicsCanPerformIO || !exp.mayPerformIO) &&
        (atomicsCanAllocate || !exp.mayAllocate))

  def apply(prog: Program): Program = {
    prog match {
      case Program(decs, defs, init) => {
        normalizeTops(defs) {
          case (newDefs, List()) => {
            Program(decs, newDefs, normalize(init))
          }
          case (newDefs, newExps) => {
            Program(decs, newDefs, normalize(Sequence(newExps, init)))
          }
        }
      }
      //case _ => throw new Exception("Unmatched program: " + prog)
    }
  }

  private def normalizeTops[A](defs: List[Def])(k: (List[Def], List[Exp]) => A): A = {
    defs match {
      case Nil => k(Nil, Nil)
      case ImplicitDef(exp) :: rest => {
        normalizeTops(rest) {
          case (restDefs, restExps) =>
            k(restDefs, exp :: restExps)
        }
      }
      case d :: rest => {
        normalizeTops(rest) {
          case (restDefs, restExps) => {
            val lhs = d.name
            val rhs = d.value
            if (isAtomic(rhs)) {
              k(VarDef(lhs, normalizeAtom(rhs)) :: restDefs, restExps)
            } else {
              k(VarDef(lhs, Unspecified()) :: restDefs, SetVar(lhs, rhs) :: restExps)
            }
          }
        }
      }
    }
  }

  def normalize(exp: Exp): Exp = normalizeExp(exp)(e => e)

  def normalize(body: Body): Body = {
    if (convertBodyToLetRec) {
      ExpBody(normalize(body.toLetRec))
    } else {
      body match {
        case Body(defs, exps) => Body(defs map normalize, exps map normalize)
      }
    }
  }

  def normalize(defn: Def): Def = defn match {
    case ImplicitDef(exp) => ImplicitDef(normalize(exp))
    case VarDef(name, exp) => VarDef(name, normalize(exp))
    case FunctionDef(name, formals, body) => FunctionDef(name, formals, normalize(body))
  }

  def normalizeAtom(exp: Exp): Exp = {
    exp match {
      case Ref(name) => exp
      case Lit(sexp) => exp
      case Unspecified() => exp
      case (_: Prim) => exp

      case Lambda(formals, body) =>
        Lambda(formals, normalize(body))
      case App(f, args) =>
        App(normalizeAtom(f), args map normalizeAtom)

      case StructGet(base, field, ty) =>
        StructGet(normalizeAtom(base), field, ty)
      case MakeStruct(ty, values) =>
        MakeStruct(ty, values map normalizeAtom)

      // TODO: [ilya] check correctness of this
      case And(exprs) =>
        App(Prim("and", true), Arguments(exprs.map(e => PosArgument(normalizeAtom(e))), None))
      case Or(exprs) =>
        App(Prim("or", true), Arguments(exprs.map(e => PosArgument(normalizeAtom(e))), None))
      case If(cond, ifTrue, ifFalse) =>
        (If(normalizeAtom(cond), normalizeAtom(ifTrue), normalizeAtom(ifFalse)))
      case cond: Cond =>
        normalizeAtom(cond.toIf)

    }
  }

  def normalizeExp(exp: Exp)(k: Exp => Exp): Exp = {
    // System.err.println("Normalizing: " + exp) // DEBUG
    exp match {
      case _ if isAtomic(exp) => k(normalizeAtom(exp))

      case Lambda(formals, body) =>
        Exp.let(Lambda(formals, normalize(body)))(k)
      case Closure(lam, ty, values) => {
        val newLam = if (lam.isLambda) {
          normalizeAtom(lam)
        } else {
          normalize(lam)
        }
        normalizeNames(values)(values =>
          Exp.let(Closure(newLam, ty, values))(k))
      }
      case MakeStruct(ty, values) => {
        normalizeNames(values)(values =>
          Exp.let(MakeStruct(ty, values))(k))
      }

      case If(cond, ifTrue, ifFalse) =>
        normalizeName(cond)(cond =>
          k(If(cond, normalize(ifTrue), normalize(ifFalse))))
      case cond: Cond =>
        normalizeExp(cond.toIf)(k)
      case or: Or =>
        normalizeExp(or.toIf)(k)
      case and: And =>
        normalizeExp(and.toIf)(k)

      case Let(Bindings(List()), body) =>
        Begin(normalizeBody(body)(k))
      //case Let(Bindings(List(Binding(name,value))),body) =>
      case Let1(name, value, body) =>
        normalizeExp(value)(value =>
          Let(Bindings(List(Binding(name, value))), normalizeBody(body)(k)))
      // FIXME: Assumes alphatization; check/fix if not true:
      case let@Let(Bindings(hd :: tl), body) =>
        normalizeExp(let.toLetStar)(k)

      case lets@LetStar(_, _) =>
        normalizeExp(lets.toLets)(k)

      case SetVar(name, value) =>
        normalizeLinearName(value)(value =>
          Sequence(SetVar(name, value),
            k(Unspecified())))

      case Begin(ExpBody(exp)) =>
        normalizeExp(exp)(k)
      case Begin(Body(List(), hd :: tl)) =>
        Sequence(normalize(hd),
          normalizeExp(Begin(Body(List(), tl)))(k))
      case Begin(body) if !convertBodyToLetRec =>
        Begin(normalizeBody(body)(k))
      case Begin(body) if convertBodyToLetRec =>
        normalizeExp(body.toLetRec)(k)

      case letrec@LetRec(_, _) if convertLetRecToLetsAndSets =>
        normalizeExp(letrec.toLetsAndSets)(k)

      case ListExp(Nil) => QuoteLit(SNil())
      case ListExp(hd :: tl) =>
        normalizeExp(ConsExp(hd, ListExp(tl)))(k)


      case App(fun, args) =>
        normalizeLinearName(fun)(fun =>
          normalizeArguments(args)(args =>
            k(App(fun, args))))

      case Call(clo, key, args) =>
        normalizeLinearName(clo)(clo =>
          normalizeArguments(args)(args =>
            k(Call(clo, key, args))))

      case QuoteLit(e) => k(QuoteLit(e))

      case _ => throw new Exception("Unhandled expression during A-Normalization: " + exp)
    }
  }

  def normalizeName(exp: Exp)(k: Exp => Exp): Exp =
    normalizeExp(exp)(exp =>
      if (isAtomic(exp) && exp.isDuplicable)
        k(normalize(exp))
      else
        Exp.let(normalize(exp))(k))

  def normalizeNames(exps: List[Exp])(k: List[Exp] => Exp): Exp =
    if (exps.isEmpty)
      k(Nil)
    else
      normalizeName(exps.head)(e =>
        normalizeNames(exps.tail)(es =>
          k(e :: es)))


  def normalizeLinearName(exp: Exp)(k: Exp => Exp): Exp = {
    normalizeExp(exp)(exp =>
      if (isAtomic(exp))
        k(normalize(exp))
      else
        Exp.let(normalize(exp))(k))
  }


  def normalizeLinearNames(exps: List[Exp])(k: List[Exp] => Exp): Exp =
    if (exps.isEmpty)
      k(Nil)
    else
      normalizeLinearName(exps.head)(e =>
        normalizeLinearNames(exps.tail)(es =>
          k(e :: es)))


  def normalizeBody(body: Body)(k: Exp => Exp): Body = body match {
    case Body(defs, exps) => {
      val normDefs = defs map normalize
      exps reverse match {
        case List(exp) => Body(normDefs, List(normalizeExp(exp)(k)))
        case last :: front => {
          val inner = front.foldLeft(normalizeExp(last)(k))((x, y) => Sequence(normalize(y), x))
          Body(normDefs, List(inner))
          //          Body(normDefs, (((normalizeExp(last)(k)) :: (front map normalize)).reverse))
        }
      }
    }
  }

  def normalizeArgs(argList: List[Argument])(k: List[Argument] => Exp): Exp = argList match {
    case Nil => k(Nil)
    case PosArgument(exp) :: tl =>
      normalizeLinearName(exp)(exp =>
        normalizeArgs(tl)(tl =>
          k(PosArgument(exp) :: tl)))
    case KeywordArgument(kw, exp) :: tl =>
      normalizeLinearName(exp)(exp =>
        normalizeArgs(tl)(tl =>
          k(KeywordArgument(kw, exp) :: tl)))
  }

  def normalizeArguments(args: Arguments)(k: Arguments => Exp): Exp = args match {
    case Arguments(argList, Some(exp)) =>
      normalizeLinearName(exp)(exp =>
        normalizeArgs(argList)(argList =>
          k(Arguments(argList, Some(exp)))))
    case Arguments(argList, None) =>
      normalizeArgs(argList)(argList =>
        k(Arguments(argList, None)))
  }

}