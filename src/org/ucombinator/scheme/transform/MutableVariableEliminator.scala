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

class MutableVariableEliminator extends ProgramTransformer {

  private var prog: Program = null

  def apply(prog: Program): Program = {
    this.prog = prog

    prog match {
      case Program(decs, defs, init) => {
        val a = new Alphatizer()
        a.apply(Program(decs, defs.map(this.apply), this(init)))
      }
    }
  }

  private def apply(d: Def): Def = {
    d match {

      case VarDef(name, value) if (prog.mutables contains name) && value.isLambda =>
        VarDef(name, MakeCell(this(value)))

      case VarDef(name, value) =>
        VarDef(name, this(value))

      case ImplicitDef(value) =>
        ImplicitDef(this(value))

      case FunctionDef(name, formals, body) if prog.mutables contains name =>
        VarDef(d.name, MakeCell(this(d.value)))

      case FunctionDef(name, formals, body) =>
        FunctionDef(name, formals, this(body))
    }
  }

  private def rebind(bound: List[SName], body: Body): Body = {
    bound match {
      case hd :: tl if prog.mutables contains hd => {
        rebind(tl, ExpBody(new Let(hd, MakeCell(Ref(hd)), body)))
      }

      case _ :: tl =>
        rebind(tl, body)

      case Nil =>
        body
    }
  }

  private def apply(exp: Exp): Exp = {
    exp match {
      case _: Lit => exp
      case _: Unspecified => exp
      case _: Prim => exp

      case Ref(name) if prog.mutables contains name => {
        if ((prog.globals contains name) && !prog.valueOfGlobal(name).isLambda)
          exp
        else
          CellGet(exp)
      }
      case Ref(_) => exp

      case Lambda(formals, body) => {
        Lambda(formals, rebind(formals.bound.toList, this(body)))
      }

      case Let(bindings, body) => {
        Let(bindings map {
          case Binding(name, value) if prog.mutables contains name =>
            Binding(name, MakeCell(this(value)))
          case b => b
        }, this(body))
      }

      case SetVar(name, value) if prog.globals contains name => {
        SetVar(name, this(value))
      }

      case SetVar(name, value) => {
        SetCell(Ref(name), this(value))
      }

      case MakeStruct(ty, values) => {
        MakeStruct(ty, values map this.apply)
      }
      case StructGet(base, field, ty) => {
        StructGet(this(base), field, ty)
      }

      case Begin(body) => {
        Begin(this(body))
      }

      case App(f, args) => App(this(f), args map this.apply)
      case Call(f, key, args) => Call(this(f), key, args map this.apply)

      case _ => throw new Exception("Unhandled exp in MVE: " + exp)
    }
  }

  private def apply(body: Body): Body = {
    body match {
      case Body(defs, exps) => {
        Body(defs map this.apply, exps map this.apply)
      }
    }
  }

}
