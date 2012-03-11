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
