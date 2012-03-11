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

package org.ucombinator.scheme.cfa.mcfa

import org.ucombinator.scheme.syntax._
import util.Parameters

class KCFA_CPS(prog: Program, bEnv0: BEnv, t0: Time, store0: Store, val botD: D) extends AbstractLinkedCFA_CPS {

  var k: Int = 0

  val (bEnv1, store1) = {
    prog match {
      case Program(decs, defs, init) => {
        var newBEnv = bEnv0
        var newStore = store0

        for (d <- defs) {
          newStore = newStore +(GlobalAddr(d.name), atomEval(bEnv0, store0)(d.value))
        }

        (newBEnv, newStore)
      }
    }
  }

  val exp = prog.init


  def atomEval(bEnv: BEnv, store: Store)(exp: Exp): D = exp match {
    case (prim: Prim) => botD + PrimValue(prim)
    case Lit(SBoolean(value)) => botD + BooleanValue(value)
    case (_: SelfLit) => botD
    case Unspecified() => botD
    case Ref(name) if prog.globals contains name =>
      store(GlobalAddr(name))
    case Ref(name) => {
      val addr = bEnv(name)
      (store get addr) match {
        case Some(d) => d
        case None => {
          throw new Exception("could not find address: " + addr + " in " + store)
        }
      }
    }
    case (lam: Lambda) => {
      botD + Clo(lam, bEnv | (lam.free -- prog.globals))
    }
    case StructGet(base, field, ty) => {
      val locs = atomEval(bEnv, store)(base)
      var ans = botD
      val values = for (l <- locs.toList if l.isObjectLocation) yield {
        ans = ans join store(FieldAddr(l.asInstanceOf[ObjectLocation], field))
      }
      ans
    }
    case App(TypePredicate(ty), _) => botD + BooleanValue(true) + BooleanValue(false)
    case App(p: Prim, args) => applyAtomicPrimitive(p, evalArgs(args, bEnv, store))

    // TODO: [ilya] check correctness of this
    case And(exprs) => botD
    case Or(exprs) =>botD
    case If(cond, ifTrue, ifFalse) => botD

  }

  def inject(exp: Exp): State = {
    State(CFlat(exp, bEnv1, t0), StoreSharp(store1))
  }

  lazy val initialState = inject(exp)

  def tick(call: Exp, t: Time): Time = t.succ(k, call.label)

  def evalArgs(args: Arguments, bEnv: BEnv, store: Store): Parameters = {
    args match {
      case Arguments(arglist, Some(rest)) => evalArgs(new Parameters(Some(atomEval(bEnv, store)(rest))))(arglist, bEnv, store)
      case Arguments(arglist, None) => evalArgs(new Parameters(None))(arglist, bEnv, store)
    }
  }

  private def evalArgs(parameters: Parameters)(arglist: List[Argument], bEnv: BEnv, store: Store): Parameters = {
    arglist match {
      case Nil => parameters
      case hd :: tl => {
        val p = evalArgs(parameters)(tl, bEnv, store)
        hd match {
          case PosArgument(exp) => (atomEval(bEnv, store)(exp)) :: p
          case KeywordArgument(kw, exp) => p(kw) = atomEval(bEnv, store)(exp)
        }
      }
    }
  }

  private def car(cell: D, store: Store): D = {
    val cars = for (loc <- cell.toList if loc.isObjectLocation) yield {
      val aloc = loc.asInstanceOf[ObjectLocation]
      val carAddr = FieldAddr(aloc, CommonSSymbols.SCar)
      (store get carAddr) match {
        case Some(d) => d
        case None => botD
      }
    }
    cars.foldLeft(botD)((d1, d2) => d1 join d2)
  }

  private def cdr(cell: D, store: Store): D = {
    val cdrs = for (loc <- cell.toList if loc.isObjectLocation) yield {
      val aloc = loc.asInstanceOf[ObjectLocation]
      val cdrAddr = FieldAddr(aloc, CommonSSymbols.SCdr)
      (store get cdrAddr) match {
        case Some(d) => d
        case None => botD
      }
    }
    cdrs.foldLeft(botD)((d1, d2) => d1 join d2)
  }

  private def nth(base: D, n: Int, store: Store): D = {
    // OPTIMIZE: Cache this function.
    if (n == 0)
      base
    else
      cdr(nth(base, n - 1, store), store)
  }

  private def paramnth(params: Parameters, n: Int, store: Store): D = {
    if (n < params.positionals.length)
      params(n)
    else params.rest match {
      case Some(base) => car(nth(base, n - params.positionals.length, store), store)
      case None => botD
    }
  }

  private def listOf(ds: List[D], newTime: Time, n: Int, store: Store, tail: D): (D, Store) = {
    ds match {
      case Nil => (tail, store)
      case hd :: tl => {
        val (cdr, newStore) = listOf(tl, newTime, n + 1, store, tail)
        val loc = StructLocation(newTime, SName.from("cons"), n)
        val carAddr = FieldAddr(loc, CommonSSymbols.SCar)
        val cdrAddr = FieldAddr(loc, CommonSSymbols.SCdr)

        val newNewStore = newStore +(carAddr, hd) +(cdrAddr, cdr)
        (botD + loc, newNewStore)
      }
    }
  }


  private def remainder(params: Parameters, i: Int, newTime: Time, store: Store): (D, Store) = {
    if (i < params.positionals.length) {
      val tail = params.rest match {
        case Some(rest) => rest
        case None => botD + NilValue()
      }
      listOf(params.positionals.drop(i), newTime, 0, store, tail)
    } else {
      (nth(params.rest.get, i - params.positionals.length, store), store)
    }
  }


  private def applyProcedure(params: Parameters, store: Store, newTime: Time)(proc: Value): List[State] = {
    proc match {

      case Clo(lam@Lambda(Formals(List(), Some(name)), ExpBody(call)), bEnv2) if !(call.free contains name) => {
        List(State(CFlat(call, bEnv2, newTime), StoreSharp(store)))
      }

      case Clo(lam@Lambda(formals, ExpBody(call)), bEnv2) if params fits formals => {

        dumpln(params.toString)

        var newStore = store
        var newBEnv = bEnv2

        var i = 0
        // Bind positional arguments:
        for (PosFormal(name) <- formals.positionals) {
          // TODO: Un-hard-code the MapBind; factor into alloc
          if (call.free contains name) {
            val d = paramnth(params, i, newStore)
            dumpln("binding " + name + " to " + d)
            dumpln("positionals(" + i + ") = " + params.positionals(i))
            newBEnv = (newBEnv(name) = MapBind(name, newTime))
            newStore +=(newBEnv(name), d)
          }
          i += 1
        }

        // If formals.rest exists, bind it to with formal ++
        if (formals.rest.isDefined) {
          // Stuff the rest into a list.
          // val remainder = params.positionals.drop(i)
          val (listD, newStore_) = remainder(params, i, newTime, newStore)
          //val (listD,newStore_) = listOf(remainder, newTime, 0, newStore, tail)
          newBEnv = (newBEnv(formals.rest.get) = MapBind(formals.rest.get, newTime))
          newStore = newStore_ +(newBEnv(formals.rest.get), listD)
        }

        // Bind keyword arguments:
        for (KeywordFormal(keyword, name) <- formals.keywords) {
          if (call.free contains name) {
            newBEnv = (newBEnv(name) = MapBind(name, newTime))
            newStore +=(newBEnv(name), params(keyword))
          }
        }


        List(State(CFlat(call, newBEnv, newTime), StoreSharp(newStore)))
      }

      // Anything that returns a Boolean:
      case PrimValue(Prim("not" | "equal?" | "eqv?" | "eq?" | "odd?" | "even?" | "char?" | "char=?" | "char-alphabetic?" |
                          "char-numeric?" | "string<?" | "boolean?" | "procedure?" | "string?" |
                          "symbol?" | "pair?" | "list?" | "null?" | "integer?" | "number?" | "<" |
                          "=" | ">" | "<=" | ">=", _)) => {
        val conts = params(SKeyword.from("cc"))
        val primParams = (botD + BooleanValue(true) + BooleanValue(false)) :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure(primParams, store, newTime)(cont)) yield {
          succ
        }
      }


      case PrimValue(prim) if !prim.invocationMayPerformIO && !prim.invocationMayMutate && !prim.invocationMustReturnOrFail => {
        // TODO: Remove "cc" param
        val conts = params(SKeyword.from("cc"))
        val primParams = applyAtomicPrimitive(prim, params) :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure(primParams, store, newTime)(cont)) yield {
          succ
        }
      }


      case _ => throw new Exception("Unhandled proc: " + proc)

    }
  }

  def isAtomic(exp: Exp) = {
    !exp.mayPerformIO && !exp.mayMutate && exp.mustReturnOrFail
  }

  def isPureIO(exp: Exp) = {
    !exp.mayMutate && exp.mustReturnOrFail
  }

  def next(state: State): List[State] = {
    state match {

      case State(CFlat(exp@Let1(name, value, ExpBody(call)), bEnv, t), StoreSharp(store)) => {
        value match {
          case MakeStruct(ty@NamedType(typeName), values) => {
            prog.typeOf(ty) match {
              case StrictStruct(fieldNames) => {
                val newTime = tick(exp, t)
                val loc = StructLocation(newTime, typeName, 0)
                val fieldAddrs = fieldNames map (FieldAddr(loc, _))
                var newStore = store
                val fieldValues = values map (atomEval(bEnv, store)(_))
                for ((a, d) <- fieldAddrs zip fieldValues) {
                  newStore = (newStore +(a, d))
                }
                val binding = MapBind(name, newTime)
                newStore = (newStore +(binding, botD + loc))
                val newBEnv = (bEnv(name) = binding)
                List(State(CFlat(call, newBEnv, newTime), StoreSharp(newStore)))
              }
            }
          }

          // It must be atomic:
          case _ if isAtomic(value) => {
            val d = atomEval(bEnv, store)(value)
            val newTime = tick(exp, t)
            val binding = MapBind(name, newTime)
            val newBEnv = (bEnv(name) = binding)
            val newStore = (store +(binding, d))
            List(State(CFlat(call, newBEnv, newTime), StoreSharp(newStore)))
          }


        }
      }


      case State(CFlat(exp@App(f, args), bEnv, t), StoreSharp(store)) => {
        val procs = atomEval(bEnv, store)(f)
        val params = evalArgs(args, bEnv, store)
        val newTime = tick(exp, t)
        for (procValue <- procs.toList if procValue.isProcedure;
             succ <- applyProcedure(params, store, newTime)(procValue)) yield {
          succ
        }
      }

      case State(CFlat(exp@If(condition, ifTrue, ifFalse), bEnv, t), StoreSharp(store)) => {
        for (call <- List(ifTrue, ifFalse)) yield {
          val newTime = tick(exp, t)
          State(CFlat(call, bEnv, newTime), StoreSharp(store))
        }
      }

      case State(CFlat(exp@Sequence(SetVar(name, value), call), bEnv, t), StoreSharp(store)) => {
        val d = atomEval(bEnv, store)(value)

        val addr = if (prog.globals contains name) {
          GlobalAddr(name)
        } else {
          bEnv(name)
        }
        val newStore = (store(addr) = d)

        List(State(CFlat(call, bEnv, t), StoreSharp(newStore)))
      }

      case State(CFlat(exp@Sequence(first, second), bEnv, t), StoreSharp(store)) if isAtomic(first) => {
        List(State(CFlat(second, bEnv, t), StoreSharp(store)))
      }

      case State(CFlat(exp@Sequence(first, second), bEnv, t), StoreSharp(store)) if isPureIO(first) => {
        List(State(CFlat(second, bEnv, t), StoreSharp(store)))
      }

      // Non-call expression: Halt!
      case State(CFlat(ae@(_: Ref | _: Lit | _: Unspecified), bEnv, t), StoreSharp(store)) => {
        System.err.println("Halting state; final value: " + atomEval(bEnv, store)(ae)) // DEBUG
        List()
      }

      case _ => throw new Exception("unhandled state: " + state)
    }
  }

}

/*
TODO:
class XCFA_CPS(prog : Program, botBEnv : BEnv, botStore : Store, t0 : Time, val botD : D) {

  var k : Int = 0

  val (bEnv1,store1) = {
    prog match {
      case Program(decs,defs,init) => {
        var newBEnv = bEnv0
        var newStore = store0

        for (d <- defs) {
          newBEnv = newBEnv + d.name
          newStore = newStore + (GlobalAddr(d.name), atomEval (bEnv0,store0) (d.value))
        }

        (newBEnv,newStore)
      }
    }
  }

  def inject (exp : Exp) : State = {
    State(CFlat(exp,bEnv1,t0),StoreSharp(store1))
  }

  lazy val initialState = inject(exp)

  def next (state : State) : List[State] = {
    throw new Exception("next not implemented")
  }

}
*/

