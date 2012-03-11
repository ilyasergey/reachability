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

class MCFA_CPS(exp: Exp, bEnv0: BEnv, t0: Time, store0: Store, val botD: D) extends SmallStepAbstractInterpretation {

  var k: Int = 1
  var m: Int = 1

  var flatPolicy: String = "k"

  val bEnv1 = bEnv0
  val store1 = throw new Exception()
  //RnRSPrimitives.list.foldRight (store0) ((name,store) => store(bEnv1(SName.from(name))) = botD + PrimValue(Prim(name.string)))

  def atomEval(bEnv: BEnv, store: Store)(exp: Exp): D = exp match {
    case Lit(SBoolean(value)) => botD + BooleanValue(value)
    case (_: SelfLit) => botD
    case Unspecified() => botD
    case Ref(name) => {
      val addr = bEnv(name)
      (store get addr) match {
        case Some(d) => d
        case None => {
          throw new Exception("could not find address: " + addr)
        }
      }
    }
    //case Void() => botD
    case lam: Lambda => {
      botD + Clo(lam, bEnv)
    }
  }

  def inject(exp: Exp): State = {
    State(CFlat(exp, bEnv0, t0), StoreSharp(store1))
  }

  lazy val initialState = inject(exp)


  def tick(call: Exp, t: Time): Time = t.succ(k, call.label)

  def allocateBEnv(exp: Exp, current: BEnv, lam: Lambda, captured: BEnv, nextTime: Time): BEnv = {
    flatPolicy match {
      case "m" =>
        if (lam.isInstanceOf[ULambda])
          current.asInstanceOf[FlatBEnv].succ(m, exp.label)
        else
          captured
      case "k" =>
        FlatBEnv(nextTime.asInstanceOf[KTime].last)
    }
  }


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

  /*
  private def listOf (ds : List[D], newTime : Time,n : Int, store : Store) : (D,Store) = {
    ds match {
      case Nil => (botD + NilValue(),store)
        case hd :: tl => {
          val (cdr,newStore) = listOf(tl, newTime, n + 1, store)
          val loc = ConsLocation(newTime,n)
          val carAddr = FieldAddr(loc,CommonSSymbols.SCar)
          val cdrAddr = FieldAddr(loc,CommonSSymbols.SCdr)

          val newNewStore = newStore + (carAddr,hd) + (cdrAddr,cdr)
          (botD + loc, newNewStore)
        }
    }
  }
  */

  private def applyProcedure(allocBEnv: (Lambda, BEnv) => BEnv)(params: Parameters, store: Store, newTime: Time)(proc: Value): List[State] = {
    proc match {

      case Clo(lam@Lambda(Formals(List(), Some(name)), ExpBody(call)), bEnv2) if !(call.free contains name) => {
        val newBEnv = allocBEnv(lam, bEnv2) // allocateBEnv(exp,bEnv,lam,bEnv2,newTime)
        var newStore = store
        if (newBEnv != bEnv2)
          for (x <- lam.free) {
            newStore +=(newBEnv(x), store(bEnv2(x)))
          }
        List(State(CFlat(call, newBEnv, newTime), StoreSharp(newStore)))
      }


      /*
      case Clo(lam @ Lambda(formals,ExpBody(call)),bEnv2) if params fits formals => {

        val newBEnv = allocBEnv(lam,bEnv2) // allocateBEnv(exp,bEnv,lam,bEnv2,newTime)

        var newStore = store

        // Bind positional arguments:
        for ((PosFormal(name),d) <- formals.positionals zip params.positionals)  {
          newStore += (newBEnv(name), d)
        }

        // Bind keyword arguments:
        for (KeywordFormal(keyword,name) <- formals.keywords) {
          newStore += (newBEnv(name), params(keyword))
        }

        // Copy free variables from the old environment to the new:
        // println("[[" +lam+ "]].free = " + lam.free) // DEBUG
        if (newBEnv != bEnv2)
          for (x <- lam.free) {
            //println("Copying free variable: " + newBEnv(x) + " from " + bEnv2(x)) // DEBUG
            newStore += (newBEnv(x),store(bEnv2(x)))
          }

        if (formals.positionals.length < params.positionals.length) {
          // Stuff the rest into a list.
          val remainder = params.positionals.drop(formals.positionals.length)
          val (listD,newStore_) = listOf(remainder, newTime, 0, newStore)
          // newBEnv = (newBEnv(formals.rest) = MapBind(formals.rest,newTime))
          newStore = newStore_ + (newBEnv(formals.rest.get), listD)
        }

        List(State(CFlat(call,newBEnv,newTime),StoreSharp(newStore)))
      }
      */


      /*
      case PrimValue("*"|"+"|"-"|"/"|"quotient"|"gcd"|"modulo") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("ceiling"|"log"|"length"|"char->integer") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("string-append"|"number->string"|"string-length"|"string-ref"|"list->string"|"symbol->string") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("string->symbol") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("display"|"newline") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("random") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = botD :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue("cons") => {
        if (params.positionals.length != 2)
          List()
        else {
          val conts = params(SKeyword.from("cc"))
          val loc = ConsLocation(newTime,0)
          val primParams = (botD + loc) :: (new Parameters())
          val carD = params(0)
          var cdrD = params(1)
          val carAddr = FieldAddr(loc,SName.from("car"))
          val cdrAddr = FieldAddr(loc,SName.from("cdr"))
          var newStore = store
          newStore = (newStore + (carAddr,carD))
          newStore = (newStore + (cdrAddr,cdrD))
          for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
            succ
          }
        }
      }

      // Anything that returns a Boolean:
      case PrimValue("not"|"equal?"|"eqv?"|"eq?"|"odd?"|"even?"|"char?"|"char=?"|"char-alphabetic?"|"char-numeric?"|"string<?"|"boolean?"|
                     "procedure?"|"string?"|
                     "symbol?"|"pair?"|"list?"|"null?"|"integer?"|"number?"|"<"|"="|">"|"<="|">=") => {
        val conts = params(SKeyword.from("cc"))
        val primParams = (botD + BooleanValue(true) + BooleanValue(false)) :: (new Parameters())
        for (cont <- conts.toList; succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
          succ
        }
      }

      case PrimValue(field @ ("car"|"cdr")) => {
        val conts = params(SKeyword.from("cc"))
        if (params.positionals.length == 1) {
          val cellLocs = params(0)
          val statess : List[List[State]] =
            for (cellLoc <- cellLocs.toList if cellLoc.isObjectLocation) yield {
              val loc = cellLoc.asInstanceOf[ObjectLocation]
              val fieldValue = store.getOrElse(FieldAddr(loc,SName.from("cons")),botD)
              val primParams = fieldValue :: (new Parameters())
              val states : List[State] =
                for (cont <- conts.toList;
                     succ <- applyProcedure (allocBEnv) (primParams,store,newTime) (cont)) yield {
                       succ
                     }
              states
            }
          val states : List[State] = statess.flatMap[State](states => states)
          states
        } else {
          List()
        }
      }

      case PrimValue("error") => List()
      */

      case _ => throw new Exception("Unhandled proc: " + proc)

    }
  }


  def next(state: State): List[State] = {
    state match {

      case State(CFlat(exp@App(f, args), bEnv, t), StoreSharp(store)) => {
        val procs = atomEval(bEnv, store)(f)
        val params = evalArgs(args, bEnv, store)
        val newTime = tick(exp, t)
        def allocBEnv(lam: Lambda, bEnv2: BEnv) = allocateBEnv(exp, bEnv, lam, bEnv2, newTime)
        for (procValue <- procs.toList if procValue.isProcedure;
             succ <- applyProcedure(allocBEnv)(params, store, newTime)(procValue)) yield {
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

        val newStore = (store(bEnv(name)) = d)

        List(State(CFlat(call, bEnv, t), StoreSharp(newStore)))
      }

      // Non-call expression: Halt!
      case State(CFlat(ae@(_: Ref | _: Lit | _: Unspecified), bEnv, t), StoreSharp(store)) => {
        System.out.println("Halting state; final value: " + atomEval(bEnv, store)(ae)) // DEBUG
        List()
      }

      case _ => throw new Exception("unhandled state: " + state)
    }
  }

}
