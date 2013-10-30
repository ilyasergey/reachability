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

package org.ucombinator.scheme.syntax

import scala.collection.immutable.{Set => ImmSet, Map => ImmMap}


/* Abstract syntax. */


/**
Help methods for terms.
 */
object Term {

  private var maxLabel: Int = 0

  /**
  Allocates a fresh label for a term.
   */
  def allocateLabel(): Int = {
    maxLabel += 1
    maxLabel
  }
}


object Exp {

  /**
  Creates a new let expression only if duplicating the argument could alter the meaning of the program.
   */
  def let(exp: Exp)(k: Exp => Exp) = {
    exp match {
      case _: SelfLit => k(exp)
      case _: Ref => k(exp)
      case _: Unspecified => k(exp)
      case _ => {
        val tmp = SName.gensym("$tmp")
        new Let(tmp, exp, k(Ref(tmp)))
      }
    }
  }
}


/*

Proposal:

isPredicate: mustReturn && !mayAllocate && !mayPerformIO && mayReturnFalse

mayMutate
mayPerformIO
mayAllocate
mayFail
mustReturnOrFail
mustReturn
mustReturnUnspecified
mustTakeConstantTime -- violated by compound expressions

invocationMayMutate
invocationMayPerformIO
invocationMayAllocate
invocationMustReturnOrFail
invocationMustReturnUnspecified
invocationMustTakeConstantTime


*/


sealed abstract class Exp extends Ordered[Exp] {
  lazy val label = Term.allocateLabel()

  def isUnspecified: Boolean = false

  def substitute(map: ImmMap[SName, Exp]): Exp;

  /*
  def expMap (f : Exp => Exp) : Exp ;

  def buExpMap (f : Exp => Exp) : Exp = {
    f(expMap(exp => exp.buExpMap(f)))
  }
*/

  /**
  If true, execution of this expression will not mutate any data structures or variables, or perform I/O.

   Execution may result in undefined behavior.

   For example, trying to add 3 to "foo" without safety checks results in  undefined behavior.
   */
  lazy val isPure: Boolean = !mayMutate && !mayPerformIO && !mayAllocate

  /**
  If true, there is no risk of code explosion from duplicating this expression.
   */
  def isDuplicable: Boolean;

  /**
  If true, execution of this expression will always eventually return to its evaluation context.

   Exceptions, continuations and infinite loops can violate this condition.
   */
  def mustReturnOrFail: Boolean;

  def mustReturnUnspecified: Boolean;

  def mayMutate: Boolean;

  def mayAllocate: Boolean;

  def mayPerformIO: Boolean;

  def isLambda: Boolean = false

  def free: ImmSet[SName];

  def keywords: ImmSet[SKeyword];

  def variables: ImmSet[SName];

  def mutables: ImmSet[SName];

  def compare(that: Exp) = this.label compare that.label

  def toDef = ImplicitDef(this)
}


abstract class Def {
  lazy val label = Term.allocateLabel()

  def substitute(map: ImmMap[SName, Exp]): Def;

  def name: SName;

  def value: Exp;

  def keywords: ImmSet[SKeyword];

  def variables: ImmSet[SName];

  def mutables: ImmSet[SName];
}


abstract class Dec {
  lazy val label = Term.allocateLabel()

  def name: SName;
}

case class EmptyDec() extends Dec {
  // TODO: Pick a reasonable name.
  lazy val name: SName = SName.from("__deadname")
}

case class TypeDec(val name: SName, val ty: Type) extends Dec {
}


/*
 Types:

 String (32-bit unicode)

 Raw(Int[bits]|UInt[bits]|NativeInt|NativeUInt|Float[bits|IEE754]|NativeFloat)

 HomoStruct/StrictStruct: FieldSet -> Value, Value^FieldSet
 HeteroStruct/RawStruct: Raw Typed Fields

 FlexiStruct/DynamicStruct: Hash-backed dynamic struct (JavaScript)

 Array: Value^n
 Vector: Read-only Array

*/


abstract class Type {
  lazy val label = Term.allocateLabel()

  def isNamed: Boolean
}

case class NamedType(name: SName) extends Type {
  override def hashCode = name.hashCode()

  override def equals(that: Any) = that match {
    case NamedType(thatName: SName) => this.name equals thatName
    case _ => false
  }

  def isNamed = true
}

case class StrictStruct(val fields: List[SName]) extends Type {
  override def hashCode = fields.foldRight(1)((n: SName, h: Int) => 2 * h + n.hashCode())

  override def equals(that: Any) = that match {
    case StrictStruct(thoseFields) => this.fields.equalsWith(thoseFields)(_ equals _)
    case _ => false
  }

  def isNamed = false
}

case class ClosureStruct(val fields: List[SName]) extends Type {
  override def hashCode = fields.foldRight(1)((n: SName, h: Int) => 2 * h + n.hashCode())

  override def equals(that: Any) = that match {
    case ClosureStruct(thoseFields) => this.fields.equalsWith(thoseFields)(_ equals _)
    case _ => false
  }

  def isNamed = false
}


case class Program(val decs: List[Dec], val defs: List[Def], val init: Exp) {
  /*

  Proposal:

  Syntactic analysis:

  has-escaping-continuations : Boolean

  variables : Set[SName]

  global
  escaping (into continuation)
  escaping (into user procedure)
  # uses
  unused
  mutable
  first-order

  maxArguments : Int
  maxFormals : Int
  keywords

  argumentsPassed : List[(Int,List[SKeyword],Boolean)]
  formalsAccepted : List[(Int,List[SKeyword],Boolean)]

  types

   */

  lazy val globals: ImmSet[SName] = {
    ImmSet() ++ (defs map (_.name))
  }

  private lazy val typeTable: scala.collection.mutable.Map[SName, Type] = {
    val tbl = scala.collection.mutable.HashMap[SName, Type]()
    for (d <- decs) {
      d match {
        case TypeDec(name, ty) => {
          tbl(name) = ty
        }
      }
    }
    tbl
  }

  def typeOf(name: SName): Type = {
    // TODO: Guard against infinite recursion.
    typeTable(name) match {
      case NamedType(newName) => typeOf(newName)
      case groundType => groundType
    }
  }

  def typeOf(ty: Type): Type = ty match {
    case NamedType(name) => typeOf(name)
    case _ => ty
  }

  def valueOfGlobal(name: SName): Exp = {
    for (d <- defs) {
      if (d.name == name)
        return d.value
    }
    return Unspecified()
  }

  lazy val label = Term.allocateLabel()

  override def toString = {
    "; declarations:\n" + (decs mkString "\n\n") +
      "\n\n; definitions:\n" + (defs mkString "\n\n") +
      "\n\n; initialization:\n" + init + "\n"
  }

  def toExp: Exp = {
    /* TODO: Eliminate this procedure entirely. */

    if (!decs.isEmpty)
      throw new Exception("Can't convert program to expression if declarations exist")
    defs match {
      case List() =>
        Begin(Body(List(), List(init)))
      case List(ImplicitDef(exp)) if init.isInstanceOf[Void] =>
        exp
      case _ => {
        val bindings = Bindings(defs map (d => Binding(d.name, d.value)))
        LetRec(bindings, Body(List(), List(init)))
      }
    }
  }


  lazy val keywords: ImmSet[SKeyword] =
    init.keywords ++ (defs flatMap (_.keywords))
  lazy val variables: ImmSet[SName] =
    init.variables ++ (defs flatMap (_.variables))
  lazy val mutables: ImmSet[SName] =
    init.mutables ++ (defs flatMap (_.mutables))
}


case class Formals(formals: List[Formal], val rest: Option[SName]) {
  lazy val label = Term.allocateLabel()

  def this() = this(List(), None)

  def this(formals: List[Formal]) = this(formals, None)

  def this(rest: SName) = this(List(), Some(rest))

  def this(formals: List[Formal], rest: SName) = this(formals, Some(rest))

  override def toString = (formals, rest) match {
    case (List(), Some(rest)) => rest.toString
    case (_, Some(rest)) => "(" + (formals mkString " ") + " . " + rest + ")"
    case (_, None) => "(" + (formals mkString " ") + ")"
  }

  def ::(formal: Formal): Formals = Formals(formal :: formals, rest)

  lazy val bound: ImmSet[SName] = rest match {
    case Some(rest) => ImmSet(rest) ++ (formals map (_.name)) ++ (keywords map (_.name))
    case None => ImmSet() ++ (formals map (_.name)) ++ (keywords map (_.name))
  }

  private val (_positionals, _keywords) = formals partition (_.isInstanceOf[PosFormal])

  val positionals = _positionals.asInstanceOf[List[PosFormal]]
  val keywords = _keywords.asInstanceOf[List[KeywordFormal]]
  lazy val keywordSet: ImmSet[SKeyword] = ImmSet() ++ (keywords map (_.keyword))

  lazy val variables: ImmSet[SName] = bound
  // In the future, formals might have optional values:
  lazy val mutables: ImmSet[SName] = ImmSet()
}

abstract class Formal {
  val name: SName;
  lazy val label = Term.allocateLabel()
}

case class PosFormal(val name: SName) extends Formal {
  override def toString = name.toString
}

case class KeywordFormal(val keyword: SKeyword, val name: SName) extends Formal {
  override def toString = keyword + " " + name.toString
}

case object NumTopExp extends Lit(null){
  def isDuplicable = false

  def mayAllocate = false
}


case class PairExp(e1: Exp, e2: Exp) extends Lit(null){
  def isDuplicable = true

  def mayAllocate = false
}



abstract class Argument {
  lazy val label = Term.allocateLabel()

  def exp: Exp;

  def substitute(map: ImmMap[SName, Exp]): Argument;

  def map(f: Exp => Exp): Argument;

  def free: ImmSet[SName];

  def keywords: ImmSet[SKeyword];

  def variables: ImmSet[SName] = exp.variables

  def mutables: ImmSet[SName] = exp.mutables
}

case class PosArgument(val exp: Exp) extends Argument {
  override def toString = exp.toString

  def substitute(map: ImmMap[SName, Exp]): Argument = PosArgument(exp substitute map)

  def map(f: Exp => Exp): PosArgument = PosArgument(f(exp))

  lazy val free: ImmSet[SName] = exp.free
  lazy val keywords = exp.keywords
}

case class KeywordArgument(val keyword: SKeyword, val exp: Exp) extends Argument {
  override def toString = keyword + " " + exp.toString

  def substitute(map: ImmMap[SName, Exp]): Argument = KeywordArgument(keyword, exp substitute map)

  def map(f: Exp => Exp): KeywordArgument = KeywordArgument(keyword, f(exp))

  lazy val free: ImmSet[SName] = exp.free
  lazy val keywords: ImmSet[SKeyword] = ImmSet(keyword) ++ exp.keywords
}

case class Arguments(val args: List[Argument], val rest: Option[Exp]) {

  def this() = this(List(), None)

  def this(args: List[Argument]) = this(args, None)

  lazy val label = Term.allocateLabel()

  def substitute(map: ImmMap[SName, Exp]): Arguments =
    Arguments(args map (_.substitute(map)), rest map (_.substitute(map)))

  def map(f: Exp => Exp): Arguments =
    Arguments(args map (_.map(f)), rest map f)

  def forall(p: Exp => Boolean): Boolean = {
    val allArgs = args forall {
      case PosArgument(e) => p(e)
      case KeywordArgument(kw, e) => p(e)
    }
    if (!allArgs)
      return false

    rest match {
      case Some(e) => return p(e)
      case _ => return true
    }
  }

  def exists(p: Exp => Boolean): Boolean = {
    val anyArg = args exists {
      case PosArgument(e) => p(e)
      case KeywordArgument(kw, e) => p(e)
    }
    if (anyArg)
      return true

    rest match {
      case Some(e) => return p(e)
      case _ => return false
    }
  }


  override def toString = {
    (positionals, keywords, rest) match {
      case (poss, keys, None) =>
        (positionals mkString " ") + " " + (keywords mkString " ")
      case (List(), List(), rest) =>
        " . " + rest.get
    }
  }

  def ::(argument: Argument): Arguments =
    Arguments(argument :: args, rest)

  private lazy val (_positionals, _keywords) = args partition (_.isInstanceOf[PosArgument])

  lazy val positionals: List[PosArgument] = _positionals.asInstanceOf[List[PosArgument]]
  lazy val keywords: List[KeywordArgument] = _keywords.asInstanceOf[List[KeywordArgument]]

  /**
  The set of keywords used at this particular point.
   */
  lazy val keywordSet: ImmSet[SKeyword] = ImmSet() ++ (keywords map (_.keyword))

  lazy val free: ImmSet[SName] = ImmSet() ++ (args flatMap (_.free))

  /**
  All the keywords used anywhere in these arguments.
   */
  lazy val keywordsUsed: ImmSet[SKeyword] =
    if (rest.isEmpty) {
      ImmSet() ++ (args flatMap (_.keywords))
    } else {
      rest.get.keywords ++ (args flatMap (_.keywords))
    }
  lazy val variables: ImmSet[SName] = ImmSet() ++ (args flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (args flatMap (_.mutables))
}


case class Body(val defs: List[Def], val exps: List[Exp]) {
  lazy val label = Term.allocateLabel()

  override def toString = {
    (defs mkString " ") + " " +
      (exps mkString " ")
  }

  // TODO: Insert heuristics here.
  lazy val mustReturnOrFail: Boolean = false
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = true
  lazy val mayAllocate: Boolean = true
  lazy val mayPerformIO: Boolean = true

  def substitute(map: ImmMap[SName, Exp]): Body = {
    val bound: ImmSet[SName] = ImmSet() ++ (defs map (_.name))
    val newMap = map -- bound
    Body(defs map (_.substitute(newMap)), exps map (_.substitute(newMap)))
  }

  def toLetRec: Exp = {
    val bindings = Bindings(defs map (d => Binding(d.name, d.value)))
    LetRec(bindings, Body(List(), exps))
  }

  lazy val free: ImmSet[SName] = {
    val bound = defs map (_.name)
    val free: ImmSet[SName] = ImmSet() ++ (defs flatMap (_.value.free))
    (free ++ (exps flatMap (_.free))) -- bound
  }

  lazy val keywords: ImmSet[SKeyword] =
    ImmSet() ++ (defs flatMap (_.keywords)) ++ (exps flatMap (_.keywords))
  lazy val variables: ImmSet[SName] =
    ImmSet() ++ (defs flatMap (_.variables)) ++ (exps flatMap (_.variables))
  lazy val mutables: ImmSet[SName] =
    ImmSet() ++ (defs flatMap (_.mutables)) ++ (exps flatMap (_.mutables))
}


object ExpBody {
  def apply(exp: Exp): Body = {
    exp match {
      case Begin(body@Body(List(), List(_))) => body
      case _ => Body(List(), List(exp))
    }
  }

  def unapply(body: Body): Option[Exp] = body match {
    case Body(List(), List(exp)) => Some(exp)
    case _ => None
  }
}


case class ImplicitDef(val value: Exp) extends Def {
  override def toString = value.toString

  def substitute(map: ImmMap[SName, Exp]): Def = {
    ImplicitDef(value substitute map)
  }

  lazy val name = SName.gensym("_")

  lazy val keywords: ImmSet[SKeyword] = value.keywords
  lazy val variables: ImmSet[SName] = ImmSet(name) ++ value.variables
  lazy val mutables: ImmSet[SName] = value.mutables
}

case class VarDef(val name: SName, val value: Exp) extends Def {
  override def toString = "(define " + name + " " + value.toString + ")"

  def substitute(map: ImmMap[SName, Exp]): Def = {
    VarDef(name, value substitute (map - name))
  }

  lazy val keywords: ImmSet[SKeyword] = value.keywords
  lazy val variables: ImmSet[SName] = value.variables + name
  lazy val mutables: ImmSet[SName] = value.mutables
}

case class FunctionDef(val name: SName, val formals: Formals, val body: Body) extends Def {
  override def toString = "(define " + name + " (lambda " + formals + " " + body.toString + "))"

  def value = Lambda(formals, body)

  def substitute(map: ImmMap[SName, Exp]): Def = {
    FunctionDef(name, formals, body substitute (map - name -- formals.bound))
  }

  lazy val keywords: ImmSet[SKeyword] = formals.keywordSet ++ body.keywords
  lazy val variables: ImmSet[SName] = (formals.variables ++ body.variables) + name
  lazy val mutables: ImmSet[SName] = value.mutables
}


/* Core expressions. */
case class Ref(val name: SName) extends Exp {
  override def toString = name.toString

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    map getOrElse(name, this)
  }

  def isDuplicable = true

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = false
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = false
  lazy val mayPerformIO: Boolean = false


  lazy val free: ImmSet[SName] = ImmSet(name)
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet(name)
  lazy val mutables: ImmSet[SName] = ImmSet()
}

/*
 Proposed prim policy:


 <name> - high-level safe primitives; may throw exceptions or (error) gracefully
 #p:<name> - low-level unsafe "return or fail" primitives--correspond to roughly one instruction

 */

case class Prim(val name: String, val safe: Boolean) extends Exp {
  override def toString = name

  def substitute(map: ImmMap[SName, Exp]): Exp = this

  def isDuplicable = true

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = false
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = false
  lazy val mayPerformIO: Boolean = false

  lazy val free: ImmSet[SName] = ImmSet()
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet()
  lazy val mutables: ImmSet[SName] = ImmSet()

  // Procedural predicates:
  def invocationMustReturnUnspecified = (name, safe) match {
    case ("display" | "newline", _) => true
    case ("type?", _) => false
  }

  def invocationMustReturnOrFail = (name, safe) match {
    case ("display" | "newline", _) => !safe
    case ("+" | "-" | "*" | "/", _) => !safe
    case ("make-cell" | "cell-get" | "set-cell!", _) => !safe
    case ("type?", _) => true

    // A stub so far
    case (_, _) => !safe
  }

  def invocationMayPerformIO = name match {
    case "display" | "newline" => true
    case "+" | "-" | "*" | "/" => false
    case "make-cell" | "cell-get" | "set-cell!" => false
    case "type?" => false

    // A stub so far
    case _ => false
  }

  def invocationMayMutate = name match {
    case "display" | "newline" => false
    case "+" | "-" | "*" | "/" => false
    case "type?" => false
    case "make-cell" | "cell-get" => false
    case "set-cell!" => true

    // A stub so far
    case _ => false
  }

  def invocationMayAllocate = name match {
    case "display" | "newline" => false
    case "+" | "-" | "*" | "/" => false
    case "type?" => false
    case "set-cell!" | "cell-get" => false
    case "make-cell" => true

    // A stub so far
    case _ => false
  }
}

case class TypePredicate(val ty: Type) extends Prim("type?", true) {
}


case class CPS(p: Prim) extends Exp {
  override def toString = "(cps " + p + ")"

  def substitute(map: ImmMap[SName, Exp]): Exp = this

  def isDuplicable = true

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = false
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = false
  lazy val mayPerformIO: Boolean = false

  lazy val free: ImmSet[SName] = ImmSet()
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet()
  lazy val mutables: ImmSet[SName] = ImmSet()
}

case class Unspecified() extends Exp {
  override def toString = "'(unspecified)"

  override def isUnspecified = true

  def substitute(map: ImmMap[SName, Exp]): Exp = this

  def isDuplicable = true

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = true
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = false
  lazy val mayPerformIO: Boolean = false

  lazy val free: ImmSet[SName] = ImmSet()
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet()
  lazy val mutables: ImmSet[SName] = ImmSet()
}

case object BadExp extends Exp {
  override def toString = "'(bad-expr)"

  override def isUnspecified = true

  def substitute(map: ImmMap[SName, Exp]): Exp = this

  def isDuplicable = true

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = true
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = false
  lazy val mayPerformIO: Boolean = false

  lazy val free: ImmSet[SName] = ImmSet()
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet()
  lazy val mutables: ImmSet[SName] = ImmSet()
}



abstract case class Lit(val sexp: SExp) extends Exp {

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = false
  lazy val mayMutate: Boolean = false
  lazy val mayPerformIO: Boolean = false

  def substitute(map: ImmMap[SName, Exp]): Exp = this

  lazy val free: ImmSet[SName] = ImmSet()
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet()
  lazy val mutables: ImmSet[SName] = ImmSet()
}

case class SelfLit(val value: SExp) extends Lit(value) {
  override def toString = sexp.toString

  def isDuplicable = true

  lazy val mayAllocate: Boolean = false
}

case class QuoteLit(val value: SExp) extends Lit(value) {
  override def toString = "(quote " + value.toString + ")"

  def isDuplicable = value match {
    case _: SSymbol => true
    case _: SInt => true
    case SNil => true
    case _ => false
  }

  lazy val mayAllocate: Boolean = value match {
    case _: SSymbol => false
    case _: SInt => false
    case SNil => false
    case _ => true
  }

  def expansion: Exp = {
    value match {
      case hd :+: tl => ConsExp(QuoteLit(hd).expansion, QuoteLit(tl).expansion)
      case _ => this
    }
  }
}


case class App(val fun: Exp, val args: Arguments) extends Exp {
  override def toString = "(" + fun + " " + args + ")"

  def this(fun: Exp, args: Exp*) =
    this(fun, new Arguments(args.toList map (PosArgument(_))))

  def substitute(map: ImmMap[SName, Exp]) = App(fun substitute map, args substitute map)

  def isDuplicable = false

  lazy val mustReturnOrFail = {
    fun match {
      case p: Prim =>
        p.invocationMustReturnOrFail && (args forall (_.mustReturnOrFail))
      case _ => false
    }
  }
  lazy val mustReturnUnspecified = {
    fun match {
      case p: Prim => p.invocationMustReturnUnspecified
      case _ => false
    }
  }
  lazy val mayMutate: Boolean = {
    fun match {
      case p: Prim =>
        p.invocationMayMutate || (args exists (_.mayMutate))
      case _ => true
    }
  }
  lazy val mayAllocate: Boolean = {
    fun match {
      case p: Prim =>
        p.invocationMayAllocate || (args exists (_.mayAllocate))
      case _ => true
    }
  }
  lazy val mayPerformIO: Boolean = {
    fun match {
      case p: Prim =>
        p.invocationMayPerformIO || (args exists (_.mayPerformIO))
      case _ => true
    }
  }


  lazy val free = fun.free ++ args.free
  lazy val keywords: ImmSet[SKeyword] = fun.keywords ++ args.keywordsUsed
  lazy val variables: ImmSet[SName] = fun.variables ++ args.variables
  lazy val mutables: ImmSet[SName] = fun.mutables ++ args.mutables
}


case class Lambda(val formals: Formals, val body: Body) extends Exp {
  override def toString = "(lambda " + formals + " " + body + ")"

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    Lambda(formals, body substitute (map -- formals.bound))
  }

  def isDuplicable = false

  lazy val mustReturnOrFail = true // -- the expression, not the function itself.
  lazy val mustReturnUnspecified = false
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = true
  lazy val mayPerformIO: Boolean = false

  override def isLambda = true

  lazy val free = body.free -- formals.bound
  lazy val keywords: ImmSet[SKeyword] = formals.keywordSet ++ body.keywords
  lazy val variables: ImmSet[SName] = formals.variables ++ body.variables
  lazy val mutables: ImmSet[SName] = formals.mutables ++ body.mutables
}

case class ULambda(fs: Formals, b: Body) extends Lambda(fs, b)

case class KLambda(fs: Formals, b: Body) extends Lambda(fs, b)


case class If(val condition: Exp, ifTrue: Exp, ifFalse: Exp) extends Exp {
  override def toString = "(if " + condition + " " + ifTrue + " " + ifFalse + ")"

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    If(condition substitute map,
      ifTrue substitute map,
      ifFalse substitute map)
  }

  def isDuplicable = false

  lazy val mustReturnOrFail = condition.mustReturnOrFail && ifTrue.mustReturnOrFail && ifFalse.mustReturnOrFail
  lazy val mustReturnUnspecified = ifTrue.mustReturnUnspecified && ifFalse.mustReturnUnspecified
  lazy val mayMutate: Boolean = condition.mayMutate || ifTrue.mayMutate || ifFalse.mayMutate
  lazy val mayAllocate: Boolean = condition.mayAllocate || ifTrue.mayAllocate || ifFalse.mayAllocate
  lazy val mayPerformIO: Boolean = condition.mayPerformIO || ifTrue.mayPerformIO || ifFalse.mayPerformIO

  lazy val free = condition.free ++ ifTrue.free ++ ifFalse.free
  lazy val keywords: ImmSet[SKeyword] = condition.keywords ++ ifTrue.keywords ++ ifFalse.keywords
  lazy val variables: ImmSet[SName] = condition.variables ++ ifTrue.variables ++ ifFalse.variables
  lazy val mutables: ImmSet[SName] = condition.mutables ++ ifTrue.mutables ++ ifFalse.mutables
}


case class SetVar(val name: SName, val value: Exp) extends Exp {
  override def toString = "(set! " + name + " " + value + ")"

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    if (map contains name)
      map(name) match {
        // BUGGY: Should have a second substitute method instead.
        //case Ref(newName) => SetVar(newName, value substitute map)
        case _ => throw new Exception("Cannot substitute a mutable name for a non-variable expression: " +
          name + " v. " + map(name))
      }
    else
      SetVar(name, value substitute map)
  }

  def isDuplicable = false

  lazy val mustReturnOrFail = value.mustReturnOrFail
  lazy val mustReturnUnspecified = true
  lazy val mayMutate: Boolean = true
  lazy val mayAllocate: Boolean = value.mayAllocate
  lazy val mayPerformIO: Boolean = value.mayPerformIO

  lazy val free = value.free + name
  lazy val keywords: ImmSet[SKeyword] = value.keywords
  lazy val variables: ImmSet[SName] = value.variables + name
  lazy val mutables: ImmSet[SName] = value.mutables + name
}

case class Values(val args: Arguments) extends Exp {
  override def toString = "(values " + args + ")"

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    Values(args substitute map)
  }

  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = args forall (_.mustReturnOrFail)
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = args exists (_.mayMutate)
  lazy val mayAllocate: Boolean = args exists (_.mayAllocate)
  lazy val mayPerformIO: Boolean = args exists (_.mayPerformIO)

  lazy val free = args.free
  lazy val keywords: ImmSet[SKeyword] = args.keywordsUsed
  lazy val variables: ImmSet[SName] = args.variables
  lazy val mutables: ImmSet[SName] = args.mutables
}

case class LetValues(formals: List[Formals], values: List[Exp], body: Body) extends Exp {
  def substitute(map: ImmMap[SName, Exp]): Exp = {
    throw new Exception("TODO: Code incomplete!")
  }

  def isDuplicable = false

  lazy val mustReturnOrFail = (values forall (_.mustReturnOrFail)) && body.mustReturnOrFail
  lazy val mustReturnUnspecified = body.mustReturnUnspecified
  lazy val mayMutate: Boolean = body.mayMutate || (values exists (_.mayMutate))
  lazy val mayAllocate: Boolean = body.mayAllocate || (values exists (_.mayAllocate))
  lazy val mayPerformIO: Boolean = body.mayPerformIO || (values exists (_.mayPerformIO))

  lazy val free = ImmSet() ++ (values flatMap (_.free)) ++ (body.free -- (formals flatMap (_.bound)))
  lazy val keywords: ImmSet[SKeyword] =
    body.keywords ++ (formals flatMap (_.keywordSet)) ++ (values flatMap (_.keywords))
  lazy val variables: ImmSet[SName] =
    body.variables ++ (formals flatMap (_.variables)) ++ (values flatMap (_.variables))
  lazy val mutables: ImmSet[SName] =
    body.mutables ++ (formals flatMap (_.mutables)) ++ (values flatMap (_.mutables))
}

case class Begin(body: Body) extends Exp {
  override def toString = "(begin " + body + ")"

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    Begin(body substitute map)
  }

  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = body.mustReturnOrFail
  lazy val mustReturnUnspecified: Boolean = body.mustReturnUnspecified
  lazy val mayMutate: Boolean = body.mayMutate
  lazy val mayAllocate: Boolean = body.mayAllocate
  lazy val mayPerformIO: Boolean = body.mayPerformIO

  lazy val free = body.free
  lazy val keywords: ImmSet[SKeyword] = body.keywords
  lazy val variables: ImmSet[SName] = body.variables
  lazy val mutables: ImmSet[SName] = body.mutables
}


case class And(val exps: List[Exp]) extends Exp {

  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = exps forall (_.mustReturnOrFail)
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = exps exists (_.mayMutate)
  lazy val mayAllocate: Boolean = exps exists (_.mayAllocate)
  lazy val mayPerformIO: Boolean = exps exists (_.mayPerformIO)

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    And(exps map (_.substitute(map)))
  }

  lazy val free = ImmSet() ++ (exps flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = ImmSet() ++ (exps flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.mutables))

  lazy val toIf: Exp = exps match {
    case List() => SelfLit(SBoolean(true))
    case List(exp) => exp
    case hd :: tl => If(hd, And(tl).toIf, SelfLit(SBoolean(false)))
  }
}


case class Or(val exps: List[Exp]) extends Exp {

  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = exps forall (_.mustReturnOrFail)
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = exps exists (_.mayMutate)
  lazy val mayAllocate: Boolean = exps exists (_.mayAllocate)
  lazy val mayPerformIO: Boolean = exps exists (_.mayPerformIO)

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    Or(exps map (_.substitute(map)))
  }

  lazy val free = ImmSet() ++ (exps flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = ImmSet() ++ (exps flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.mutables))

  lazy val toIf: Exp = exps match {
    case List() => SelfLit(SBoolean(false))
    case List(exp) => exp
    case hd :: tl =>
      Exp.let(hd)(hd =>
        If(hd, hd, Or(tl).toIf))
  }
}


abstract class CondClause {
  def substitute(map: ImmMap[SName, Exp]): CondClause;

  def free: ImmSet[SName];

  def keywords: ImmSet[SKeyword];

  def variables: ImmSet[SName];

  def mutables: ImmSet[SName];
}


case class SelfCondClause(val test: Exp) extends CondClause {
  def substitute(map: ImmMap[SName, Exp]): CondClause = {
    SelfCondClause(test substitute map)
  }

  lazy val free = test.free
  lazy val keywords: ImmSet[SKeyword] = test.keywords
  lazy val variables: ImmSet[SName] = test.variables
  lazy val mutables: ImmSet[SName] = test.mutables
}

case class TestCondClause(val test: Exp, val exps: List[Exp]) extends CondClause {
  def substitute(map: ImmMap[SName, Exp]): CondClause = {
    TestCondClause(test substitute map, exps map (_.substitute(map)))
  }

  lazy val free = test.free ++ (exps flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = test.keywords ++ (exps flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = test.variables ++ (exps flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = test.mutables ++ (exps flatMap (_.mutables))
}


case class ProcCondClause(val test: Exp, val proc: Exp) extends CondClause {
  def substitute(map: ImmMap[SName, Exp]): CondClause = {
    ProcCondClause(test substitute map, proc substitute map)
  }

  lazy val free = test.free ++ proc.free
  lazy val keywords: ImmSet[SKeyword] = test.keywords ++ proc.keywords
  lazy val variables: ImmSet[SName] = test.variables ++ proc.variables
  lazy val mutables: ImmSet[SName] = test.mutables ++ proc.mutables
}

case class ElseCondClause(val exps: List[Exp]) extends CondClause {
  def substitute(map: ImmMap[SName, Exp]): CondClause = {
    ElseCondClause(exps map (_.substitute(map)))
  }

  lazy val free = ImmSet() ++ (exps flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = ImmSet() ++ (exps flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.mutables))
}

case class Cond(clauses: List[CondClause]) extends Exp {

  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = false
  lazy val mustReturnUnspecified: Boolean = clauses.isEmpty
  lazy val mayMutate: Boolean = true
  lazy val mayAllocate: Boolean = true
  lazy val mayPerformIO: Boolean = true

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    Cond(clauses map (_.substitute(map)))
  }

  def toIf(clauses: List[CondClause]): Exp = {
    clauses match {
      case SelfCondClause(test) :: tl =>
        Exp.let(test)(v => If(v, v, toIf(tl)))
      case TestCondClause(test, exps) :: tl => If(test, Begin(Body(List(), exps)), toIf(tl))
      case List(ElseCondClause(exps)) => Begin(Body(List(), exps))
      case Nil => Unspecified()
    }
  }

  lazy val toIf: Exp = toIf(clauses)

  lazy val free = ImmSet() ++ (clauses flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = ImmSet() ++ (clauses flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = ImmSet() ++ (clauses flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (clauses flatMap (_.mutables))
}


case class Binding(val name: SName, val value: Exp) {
  lazy val keywords: ImmSet[SKeyword] = value.keywords
  lazy val variables: ImmSet[SName] = value.variables + name
  lazy val mutables: ImmSet[SName] = value.mutables

  def map(f: Exp => Exp): Binding = {
    Binding(name, f(value))
  }

  override def toString = "(" + name + " " + value + ")"
}

case class Bindings(val bindings: List[Binding]) {

  def this(renameEnv: ImmMap[SName, Exp]) =
    this(renameEnv.toList map {
      case (name, exp) => Binding(name, exp)
    })

  lazy val keywords: ImmSet[SKeyword] = ImmSet() ++ (bindings flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = ImmSet() ++ (bindings flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (bindings flatMap (_.mutables))

  override def toString = bindings mkString " "

  lazy val names = bindings map (_.name)
  lazy val values = bindings map (_.value)

  def map(f: Binding => Binding): Bindings = {
    Bindings(bindings map f)
  }
}

object Bindings {
  def apply(names: List[SName], values: List[Exp]): Bindings = {
    val binds = for ((n, v) <- names zip values) yield {
      Binding(n, v)
    }
    Bindings(binds)
  }
}


abstract case class LetForm(val bindings: Bindings, val body: Body) extends Exp {
  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = false
  lazy val mustReturnUnspecified: Boolean = body.mustReturnUnspecified
  lazy val mayMutate: Boolean = true
  lazy val mayAllocate: Boolean = true
  lazy val mayPerformIO: Boolean = true

  protected def toString(head: String) =
    "(" + head + " (" + bindings + ") " + body + ")"

  lazy val keywords: ImmSet[SKeyword] = body.keywords ++ bindings.keywords
  lazy val variables: ImmSet[SName] = body.variables ++ bindings.variables
  lazy val mutables: ImmSet[SName] = body.mutables ++ bindings.mutables
}

case class Let(_bindings: Bindings, _body: Body) extends LetForm(_bindings, _body) {
  def this(renameEnv: ImmMap[SName, Exp], body: Body) =
    this(new Bindings(renameEnv), body)

  def this(name: SName, value: Exp, exp: Exp) =
    this(Bindings(List(Binding(name, value))), Body(List(), List(exp)))

  def this(name: SName, value: Exp, body: Body) =
    this(Bindings(List(Binding(name, value))), body)

  override def toString = toString("let")

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    val bound = bindings.names
    val newBindings =
      bindings map {
        case Binding(name, value) => Binding(name, value substitute map)
      }
    Let(newBindings, body substitute (map -- bound))
  }

  lazy val free = ImmSet() ++ (bindings.values flatMap (_.free)) ++ (body.free -- (bindings.names))

  def toRedEx = {
    App(Lambda(Formals(bindings.names map {
      name => PosFormal(name)
    }, None), body),
      Arguments(bindings.values map {
        value => PosArgument(value)
      }, None))
  }

  def toLetStar = {
    val free = (bindings.values) flatMap (_.free)
    val rebinding = bindings.names exists (name => free contains name)
    if (rebinding)
      throw new Exception("Program not alphatised!")
    LetStar(bindings, body)
  }
}

case class LetRec(_bindings: Bindings, _body: Body) extends LetForm(_bindings, _body) {
  override def toString = toString("letrec")

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    val bound = ImmSet() ++ bindings.names
    val newMap = map -- bound
    val newBindings =
      bindings map {
        case Binding(name, value) => Binding(name, value substitute newMap)
      }
    Let(newBindings, body substitute newMap)
  }

  lazy val toLetsAndSets: Exp = {
    val newBindings = bindings map (b => Binding(b.name, Unspecified()))
    val sets = bindings.bindings map (b => SetVar(b.name, b.value))
    Let(newBindings, Body(List(), sets ++ List(Begin(body))))
  }


  lazy val free = ImmSet() ++ (bindings.values flatMap (_.free)) ++ body.free -- bindings.names
}

case class LetStar(_bindings: Bindings, _body: Body) extends LetForm(_bindings, _body) {
  override def toString = toString("let*")

  def substitute(map: ImmMap[SName, Exp]): Exp =
    throw new Exception("let* not yet substitutable")

  private def freeIn(bindings: List[Binding], body: Body): ImmSet[SName] = {
    if (bindings isEmpty)
      body.free
    else
      ImmSet() ++ bindings.head.value.free ++ (freeIn(bindings.tail, body) - bindings.head.name)
  }

  lazy val toLets: Exp = bindings match {
    case Bindings(List()) => Begin(body)
    case Bindings(List(binding)) => Let(bindings, body)
    case Bindings(hd :: tl) => Let(Bindings(List(hd)),
      ExpBody(LetStar(Bindings(tl), body).toLets))
  }

  lazy val free = freeIn(bindings.bindings, body)
}


/* Intermediate terms. */


case class MakeStruct(t: Type, exps: List[Exp]) extends Exp {
  def isDuplicable = false

  lazy val mustReturnOrFail = exps forall (_.mustReturnOrFail)
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = exps exists (_.mayMutate)
  lazy val mayAllocate: Boolean = true
  lazy val mayPerformIO: Boolean = exps exists (_.mayAllocate)

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    MakeStruct(t, exps map (_.substitute(map)))
  }

  lazy val free = ImmSet() ++ (exps flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = ImmSet() ++ (exps flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = ImmSet() ++ (exps flatMap (_.mutables))
}


case class StructGet(val base: Exp, val field: SName, val t: Type) extends Exp {

  def isDuplicable = base.isDuplicable

  lazy val mustReturnOrFail: Boolean = base.mustReturnOrFail
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = base.mayMutate
  lazy val mayAllocate: Boolean = base.mayAllocate
  lazy val mayPerformIO: Boolean = base.mayPerformIO

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    StructGet(base substitute map, field, t)
  }

  lazy val free = base.free
  lazy val keywords: ImmSet[SKeyword] = base.keywords
  lazy val variables: ImmSet[SName] = base.variables
  lazy val mutables: ImmSet[SName] = base.mutables
}


case class StructSet(val base: Exp, val field: SName, val t: Type, val value: Exp) extends Exp {

  def isDuplicable = base.isDuplicable && value.isDuplicable

  lazy val mustReturnOrFail: Boolean = base.mustReturnOrFail && value.mustReturnOrFail
  lazy val mustReturnUnspecified: Boolean = true
  lazy val mayMutate: Boolean = base.mayMutate || value.mayMutate
  lazy val mayAllocate: Boolean = base.mayAllocate || value.mayAllocate
  lazy val mayPerformIO: Boolean = base.mayPerformIO || value.mayPerformIO

  def substitute(map: ImmMap[SName, Exp]): Exp = {
    StructSet(base substitute map, field, t, value substitute map)
  }

  lazy val free = base.free ++ value.free
  lazy val keywords: ImmSet[SKeyword] = base.keywords ++ value.keywords
  lazy val variables: ImmSet[SName] = base.variables ++ value.variables
  lazy val mutables: ImmSet[SName] = base.mutables ++ value.mutables
}


/*
 A closure is as a procedure plus a datum (usually an environment).
 */
case class Closure(val lam: Exp, val ty: Type, val fields: List[Exp]) extends Exp {
  def substitute(map: ImmMap[SName, Exp]): Exp = {
    Closure(lam substitute map, ty, fields map (_ substitute map))
  }

  def isDuplicable = lam.isDuplicable && (fields forall (_.isDuplicable))

  lazy val mustReturnOrFail: Boolean = lam.mustReturnOrFail && (fields forall (_.mustReturnOrFail))
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = lam.mayMutate || (fields exists (_.mayMutate))
  lazy val mayAllocate: Boolean = true
  lazy val mayPerformIO: Boolean = lam.mayPerformIO || (fields exists (_.mayPerformIO))

  lazy val free = lam.free ++ (fields flatMap (_.free))
  lazy val keywords: ImmSet[SKeyword] = lam.keywords ++ (fields flatMap (_.keywords))
  lazy val variables: ImmSet[SName] = lam.variables ++ (fields flatMap (_.variables))
  lazy val mutables: ImmSet[SName] = lam.mutables ++ (fields flatMap (_.mutables))
}


case class Call(val fun: Exp, val key: SKeyword, val args: Arguments) extends Exp {
  override def toString = "(call " + key + " " + fun + " " + args + ")"

  def this(fun: Exp, key: SKeyword, args: Exp*) =
    this(fun, key, new Arguments(args.toList map (PosArgument(_))))

  def substitute(map: ImmMap[SName, Exp]) = Call(fun substitute map, key, args substitute map)

  def isDuplicable = false

  lazy val mustReturnOrFail: Boolean = false
  lazy val mustReturnUnspecified: Boolean = false
  lazy val mayMutate: Boolean = fun.mayMutate || (args exists (_.mayMutate))
  lazy val mayAllocate: Boolean = fun.mayAllocate || (args exists (_.mayAllocate))
  lazy val mayPerformIO: Boolean = fun.mayPerformIO || (args exists (_.mayPerformIO))

  lazy val free = fun.free ++ args.free
  lazy val keywords: ImmSet[SKeyword] = (fun.keywords ++ args.keywordsUsed) + key
  lazy val variables: ImmSet[SName] = fun.variables ++ args.variables
  lazy val mutables: ImmSet[SName] = fun.mutables ++ args.mutables
}


/* Synthetic constructs */

object Sequence {

  /*
  def apply(first: Exp, second: Exp): Exp =
    second match {
      case Unspecified() => first
      case Begin(Body(List(), seconds)) => Sequence(first, seconds)
      case _ => Begin(Body(List(), List(first, second)))
    }

  */


  // Matt: I changed this to direct let bindings:
  def apply(first : Exp, second : Exp) : Exp =
    Let1(SName.gensym("$_"), first, ExpBody(second))

  def apply(first: Exp, seconds: List[Exp]): Exp =
    first match {
      case Begin(Body(List(), firsts)) =>
        Begin(Body(List(), firsts ++ seconds))
      case _ =>
        Begin(Body(List(), first :: seconds))
    }

  def apply(firsts: List[Exp], second: Exp): Exp =
    second match {
      case Begin(Body(List(), seconds)) =>
        Begin(Body(List(), firsts ++ seconds))
      case _ =>
        Begin(Body(List(), firsts ++ List(second)))
    }

  def apply(exps: List[Exp]): Exp = {
    exps match {
      case List(exp) => exp
      case _ => Begin(Body(List(), exps))
    }
  }

  def unapply(exp: Exp): Option[(Exp, Exp)] = exp match {
    case Begin(Body(List(), List(first, second))) => Some((first, second))
    case Begin(Body(List(), first :: rest)) => Some((first, Begin(Body(List(), rest))))
    case _ => None
  }

  def strict(first: Exp, second: Exp): Exp =
    Begin(Body(List(), List(first, second)))
}


object ListExp {

  import CommonSSymbols._

  def apply(exps: Exp*): Exp = {
    apply(exps.toList)
  }

  def apply(exps: List[Exp]): Exp = {
    exps match {
      case hd :: tl => new App(Prim("cons", true), hd, apply(tl))
      case Nil => QuoteLit(SNil)
    }
  }

  def unapply(exp: Exp): Option[List[Exp]] = {
    exp match {
      case App(Ref(SListSym), Arguments(args, None)) => Some(args map (_.exp))
      case _ => None
    }
  }
}


object ConsExp {
  def apply(car: Exp, cdr: Exp): Exp = {
    new App(Prim("cons", true), car, cdr)
  }
}


object MakeCell {
  def apply(value: Exp): Exp = {
    new App(Prim("make-cell", false), value)
  }

  def unapply(exp: Exp): Option[Exp] = {
    exp match {
      case App(Prim("make-cell", false), Arguments(List(PosArgument(value)), None)) =>
        Some(value)
      case _ => None
    }
  }
}

object SetCell {
  def apply(cell: Exp, value: Exp): Exp = {
    new App(Prim("set-cell!", false), cell, value)
  }

  def unapply(exp: Exp): Option[(Exp, Exp)] = {
    exp match {
      case App(Prim("set-cell!", false), Arguments(List(PosArgument(cell), PosArgument(value)), None)) => {
        Some(cell, value)
      }
      case _ => {
        None
      }
    }
  }
}


object CellGet {
  def apply(cell: Exp): Exp = {
    new App(Prim("cell-get", false), cell)
  }

  def unapply(exp: Exp): Option[Exp] = {
    exp match {
      case App(Prim("cell-get", false), Arguments(List(PosArgument(cell)), None)) =>
        Some(cell)
      case _ => None
    }
  }
}

object Let1 {

  def apply(name : SName, value : Exp, body : Body) : Exp = {
   Let(Bindings(List(Binding(name,value))), body)
  }

  def unapply(exp: Exp): Option[(SName, Exp, Body)] = {
    exp match {
      case Let(Bindings(List(Binding(name, exp))), body) =>
        Some(name, exp, body)
      case LetStar(Bindings(List(Binding(name, exp))), body) =>
        Some(name, exp, body)
      case _ => None
    }
  }
}





