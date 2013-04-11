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

package org.ucombinator.scheme.cfa.kcfa

import org.ucombinator.scheme.syntax._
import org.ucombinator.util.FancyOutput
import org.ucombinator.cfa.AnalysisRunner
import org.ucombinator.scheme.cfa.cesk.CESKMachinery

/**
 * @author ilya
 */

trait PointerCESKMachinery extends CESKMachinery with FancyOutput {
  self: AnalysisRunner with KCFAGarbageCollector =>

  type Kont = KStore

  type Addr = (Var, List[Exp])

  /********************************************************************
   * Continuation sotre
   ********************************************************************/
  type KAddr = (Either[Var, Exp], List[AKont])

  type KStore = KAddr :-> Set[AKont]

  /********************************************************************
   * Continuations with pointers
   ********************************************************************/
  abstract sealed class AKont

  object MT extends AKont

  case class Pointed(frame: Frame, kptr: KAddr) extends AKont


  /********************************************************************
   * Utility functions
   ********************************************************************/
  def alloc(v: Var, c: Conf): Addr = c match {
    case (PState(e, _, _, kptr), _) =>
      if (isDummy) {
        (SName("SingleAddr", 0), Nil)
      } else k match {
        case 0 => (v, Nil)
        case 1 => (v, List(e))
        case _ => throw new PointerCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => {
      throw new PointerCESKException("Illegal allocation configuartion:\n" + c.toString)
    }
  }

  def initState(e: Exp): Conf = {
    val a0: KAddr = (Left(SName.gensym("mt")), Nil)
    val newKStore: KStore = Map.empty + ((a0, Set(MT)))
    (PState(e, Map.empty, Map.empty, a0), newKStore)
  }

  /**
   * Allocate a new continuation address basing on the control state and store
   *
   * @param c control state
   * @param kstore kontinuation store
   * @return new continuation address
   */
  def kalloc(c: ControlState, kstore: AKont): KAddr = c match {
    // todo: Fix
    case PState(Let(Bindings(List(Binding(name, _))), _), rho, s, kptr) => k match {
      case 0 | 1 => (Left(name), Nil)
      //      case 1 => (Left(name), List(kont))
      case _ => throw new PointerCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
    }

    case PState(e, rho, s, kptr) => k match {
      case 0 | 1 => (Right(e), Nil)
      //      case 1 => (Right(e), List(kont))
      case _ => throw new PointerCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
    }
    case _ => throw new PointerCESKException("Wrong state to allocate a continuation address:\n" + c.toString)
  }

  def lookupKStore(kstore: KStore, a: KAddr): Set[AKont] = kstore.get(a) match {
    case Some(x) => x
    case None => throw new PointerCESKException("No associated continuations found for an address " +
      a + "\nin store\n" + kstore.toString)
  }

  def updateKStore(kstore: KStore, pair: (KAddr, AKont)) = {
    val (a, k) = pair
    val oldKonts = kstore.getOrElse(a, Set())
    kstore + ((a, oldKonts + k))
  }

  class PointerCESKException(s: String) extends CESKException(s)

  /********************************************************************
   * Main non-deterministic abstract step function
   ********************************************************************/
  def mnext: Conf => Set[Conf] = {
    // Application of lambda or reference
    case c@(PState(App(f@(Lambda(_, _) | Ref(_)), args), rho, s, a), kstore) =>

      atomicEval(f, rho, s).flatMap[Conf, Set[Conf]]{
        // f refers to a closure
        case Clo(lam@Lambda(Formals(params, _), body), rho1) => {
          val paramNames = params.map(_.name)
          val ai = paramNames.map(alloc(_, c)) // allocate addresses
          val rho2 = updateEnv(rho1, paramNames.zip(ai)) // update function env

          val arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s)) // map atomic arguments to values
          val s1 = updateStore(s, ai.zip(arg_vals))

          // In A-normal form only one expression in body
          val e = getLambdaBodyInANF(lam)
          mkSet(PState(e, rho2, s1, a), kstore)
        }
        // f refers to a stored primitive
        case p@PrimLit(prim, _) => mnext((PState(App(embedValueToExp(p), args), rho, s, a), kstore))
      }

    /**
     * Special hacky case because of (set!) desugaring
     */
    case c@(PState(l@Let(_, _), rho, s, kaddr), kstore)
      if (decomposeLetInANF(l)._2.isUnspecified) => {
      val (v, _, e) = decomposeLetInANF(l)
      val a = alloc(v, c)
      val rho1 = updateEnv(rho, List((v, a)))
      Set((PState(e, rho1, s, kaddr), kstore))
    }


    case (c@PState(l@Let(_, _), rho, s, a), kstore) => {
      val (v, call, e) = decomposeLetInANF(l)
      val frameToAdd = LetFrame(v, e, rho)
      for {
        k <- lookupKStore(kstore, a)
        b = kalloc(c, k)
        kstore1 = updateKStore(kstore, (b, Pointed(frameToAdd, a)))
      } yield (PState(call, rho, s, b), kstore1)
    }

    // return state
    case c@(PState(ae, rho, s, kaddr), kstore)
      if isAtomic(ae) => for {
      k <- lookupKStore(kstore, kaddr)
      value <- atomicEval(ae, rho, s)
    } yield (returnValue(k, value, kstore, c, s))


    /******************************************************
     * Conditional operator
     ******************************************************/
    case (c@PState(b@If(cond, tBranch, eBranch), rho, s, a), kstore) => {
      val frameToAdd = IfFrame(tBranch, eBranch, rho)
      for {
        k <- lookupKStore(kstore, a)
        b = kalloc(c, k)
        kstore1 = updateKStore(kstore, (b, Pointed(frameToAdd, a)))
      } yield (PState(cond, rho, s, b), kstore1)
    }

    /******************************************************
     * Set!
     ******************************************************/
    case c@(PState(SetVar(v, ae), rho, s, kaddr), kstore)
      // Only atomic values are assigned
      if (isAtomic(ae)) => {
      val addr = lookupEnv(rho, v)
      val eval = atomicEval(ae, rho, s)
      val s1 = updateStore(s, List((addr, eval)))
      for {
        k <- lookupKStore(kstore, kaddr)
      } yield {
        returnValue(k, UnspecifiedVal, kstore, c, s1)
      }
    }

    /******************************************************
     * Primitive applications
     ******************************************************/
    // only atomic values or variable are supported in primops
    case c@(PState(app@App(p@Prim(primName, _), args), rho, s, a), kstore) => {
      // map atomic arguments to values (sets)
      val arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s))
      for {
        results <- arg_vals.size match {
          case 0 => Set(evalPrimApp(primName, List()))
          case 1 => for {a <- arg_vals.head} yield evalPrimApp(primName, List(a))
          case 2 => for {
            a <- arg_vals.head
            b <- arg_vals.tail.head
          } yield evalPrimApp(primName, List(a, b))
          case n => {
            throw new PointerCESKException("Primitive functions of arity " + n + " are not supported:\n" + app.toString)
          }
        }
        result <- results
        state = analyseResult(result, rho, s, app, a)
      } yield (state, kstore)
    }

    /******************************************************
     * Final state
     ******************************************************/
    // Ok, folks, that's it!
    case (PFinal(_), _) => Set()

    case c => {
      throw new PointerCESKException("Wrong state: " + c.toString)
    }
  }

  /**
   * Value return
   */
  def returnValue(k: AKont,
                  value: Val,
                  kstore: KStore,
                  c: Conf,
                  s: Store): Conf = {
    k match {

      // return to final state
      case MT => (PFinal(value), kstore)

      // return from let-statement
      case Pointed(LetFrame(v, e, rho1), b) => {
        val a = alloc(v, c)
        val rho2 = updateEnv(rho1, List((v, a)))
        val s1 = updateStore(s, List((a, Set(value))))
        (PState(e, rho2, s1, b), kstore)
      }

      // return from if-statement
      case Pointed(IfFrame(tb, eb, rho1), b) => value match {
        case BoolLit(false) => (PState(eb, rho1, s, b), kstore)
        case _ => (PState(tb, rho1, s, b), kstore)
      }
    }
  }


  /**
   * Kleene iteration of a work set of states
   */
  private def iterateKCFA(workSet: Set[Conf], edges: Set[Edge], accumStates: Set[Conf]): (Set[(Conf, Conf)], Set[Conf]) = {

     val newConfsEdges: Set[(Conf, Edge)] = workSet.map((c: Conf) => {
      val next: Set[Conf] = mnext(c)
      val cleanNext = if (shouldGC) {
        next.map {
          case (c1@PState(_, _, _, kaddr), kont) => {
            (gc(c1, kont), gcKStore(kaddr, kont))
          }
          case q => q
        }
      } else {
        next
      }
      cleanNext.map(x => (x, (c, x)))
    }).flatten

    val (newStates, newEdges) = newConfsEdges.unzip

    println(progressPrefix + " " + accumStates.size + " states computed so far.")

    val collectedEdges : Set[Edge] = edges ++ newEdges

    if (newStates.subsetOf(accumStates)) {
      (collectedEdges, accumStates)
    } else if (interrupt && accumStates.size > interruptAfter) {
      (collectedEdges, accumStates)
    } else {
      iterateKCFA(newStates, collectedEdges, accumStates ++ newStates)
    }
  }

  type Edge = (Conf, Conf)

  def evaluateKCFA(e: Exp): (Set[Edge], Set[Conf]) = {
    val initialStates = Set(initState(e))
    iterateKCFA(initialStates, Set(), initialStates)
  }

}
