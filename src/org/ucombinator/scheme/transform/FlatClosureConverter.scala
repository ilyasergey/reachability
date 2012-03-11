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
