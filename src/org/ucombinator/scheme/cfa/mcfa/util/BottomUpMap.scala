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

package org.ucombinator.scheme.cfa.mcfa.util

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.cfa.mcfa.D
import collection.immutable.{TreeMap, SortedMap}


class BottomUpMap {

  var applyDec: Dec => Dec = (dec => dec);
  var applyType: Type => Type = (ty => ty);

  var applyDef: Def => Def = (d => d);

  var applyExp: Exp => Exp = (exp => exp);
  var applyBody: Body => Body = (body => body);
  var applyFormals: Formals => Formals = (formals => formals);
  var applyFormal: Formal => Formal = (formal => formal);
  var applyArguments: Arguments => Arguments = (arguments => arguments);
  var applyArgument: Argument => Argument = (argument => argument);

  var applyBindings: Bindings => Bindings = (bindings => bindings);
  var applyBinding: Binding => Binding = (binding => binding);

  var applyCondClause: CondClause => CondClause = (clause => clause);

  def apply(prog: Program): Program = {
    prog match {
      case Program(decs, defs, init) => {
        Program(decs map this.apply, defs map this.apply, this(init))
      }
    }
  }

  def apply(d: Dec): Dec = {
    d match {
      case TypeDec(name, ty) =>
        applyDec(TypeDec(name, this(ty)))
    }
  }

  def apply(d: Def): Def = {
    d match {
      case VarDef(name, exp) =>
        applyDef(VarDef(name, this(exp)))
      case ImplicitDef(exp) =>
        applyDef(ImplicitDef(this(exp)))
      case FunctionDef(name, formals, exp) =>
        applyDef(FunctionDef(name, this(formals), this(exp)))
    }
  }

  def apply(ty: Type): Type = {
    applyType(ty)
  }

  def apply(exp: Exp): Exp = {
    val newExp =
      exp match {
        case (_: Unspecified) => exp
        case (_: Ref) => exp
        case (_: Lit) => exp
        case (_: Prim) => exp

        case Lambda(formals, body) =>
          Lambda(formals, this(body))

        case Let(bindings, body) =>
          Let(this(bindings), this(body))
        case LetStar(bindings, body) =>
          LetStar(this(bindings), this(body))
        case LetRec(bindings, body) =>
          LetRec(this(bindings), this(body))

        case If(cond, ifTrue, ifFalse) =>
          If(this(cond), this(ifTrue), this(ifFalse))
        case And(exps) =>
          And(exps map this.apply)
        case Or(exps) =>
          Or(exps map this.apply)
        case Cond(clauses) =>
          Cond(clauses map this.apply)


        case SetVar(name, value) =>
          SetVar(name, this(value))
        case Begin(body) =>
          Begin(this(body))

        case App(f, arguments) =>
          App(this(f), this(arguments))
        case Call(f, key, arguments) =>
          Call(this(f), key, this(arguments))
      }
    applyExp(newExp)
  }

  def apply(formals: Formals): Formals = {
    formals match {
      case Formals(forms, rest) => {
        applyFormals(Formals(forms map this.apply, rest))
      }
    }
  }

  def apply(formal: Formal): Formal = {
    formal match {
      case PosFormal(name) =>
        applyFormal(PosFormal(name))
      case KeywordFormal(kw, name) =>
        applyFormal(KeywordFormal(kw, name))
    }
  }

  def apply(argument: Argument): Argument = {
    argument match {
      case PosArgument(exp) =>
        applyArgument(PosArgument(this(exp)))
      case KeywordArgument(kw, exp) =>
        applyArgument(KeywordArgument(kw, this(exp)))
    }
  }

  def apply(arguments: Arguments): Arguments = {
    arguments match {
      case Arguments(args, rest) =>
        applyArguments(Arguments(args map this.apply, rest map this.apply))
    }
  }

  def apply(body: Body): Body = {
    body match {
      case Body(defs, exps) =>
        applyBody(Body(defs map this.apply, exps map this.apply))
    }
  }

  def apply(bindings: Bindings): Bindings = {
    bindings match {
      case Bindings(binds) =>
        applyBindings(Bindings(binds map this.apply))
    }
  }

  def apply(binding: Binding): Binding = {
    binding match {
      case Binding(name, value) =>
        applyBinding(Binding(name, this(value)))
    }
  }

  def apply(clause: CondClause): CondClause = {
    val newClause =
      clause match {
        case SelfCondClause(test) => SelfCondClause(this(test))
        case TestCondClause(test, exps) => TestCondClause(this(test), exps map this.apply)
        case ProcCondClause(test, proc) => ProcCondClause(this(test), this(proc))
        case ElseCondClause(exps) => ElseCondClause(exps map this.apply)
      }
    applyCondClause(newClause)
  }

}


case class Parameters(val keywords: SortedMap[SKeyword, D], val positionals: List[D], val rest: Option[D]) {

  def fits(formals: Formals): Boolean = {
    formals match {
      case Formals(list, None) =>
        (positionals.length == formals.positionals.length) &&
          (keywords.keySet == formals.keywordSet)
      case Formals(list, Some(rest)) =>
        (formals.positionals.length <= positionals.length) &&
          (keywords.keySet == formals.keywordSet)
    }
  }

  def this() = this(TreeMap(), List(), None)

  def this(rest: Option[D]) = this(TreeMap(), List(), rest)

  def apply(keyword: SKeyword): D = keywords(keyword)

  def apply(position: Int): D = positionals(position)

  def update(keyword: SKeyword, d: D): Parameters =
    new Parameters(keywords + ((keyword, d)), positionals, rest)

  def ::(d: D): Parameters =
    new Parameters(keywords, d :: positionals, rest)
}

