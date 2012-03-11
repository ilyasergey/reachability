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
