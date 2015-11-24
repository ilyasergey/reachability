/*
 * Copyright (c) 2015,
 * Ilya Sergey, Christopher Earl, Matthew Might and David Van Horn
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *  Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 *  Neither the name of the project "Reachability" nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

  /** ******************************************************************
    * Continuation sotre
    * *******************************************************************/
  type KAddr = (Either[Var, Exp], List[AKont])

  type KStore = KAddr :-> Set[AKont]

  /** ******************************************************************
    * Continuations with pointers
    * *******************************************************************/
  abstract sealed class AKont

  object MT extends AKont

  case class Pointed(frame: Frame, kptr: KAddr) extends AKont


  /** ******************************************************************
    * Utility functions
    * *******************************************************************/
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

  /** ******************************************************************
    * Main non-deterministic abstract step function
    * *******************************************************************/
  def mnext: Conf => Set[Conf] = {
    // Application of lambda or reference
    case c@(PState(App(f@(Lambda(_, _) | Ref(_)), args), rho, s, a), kstore) =>

      atomicEval(f, rho, s).flatMap[Conf, Set[Conf]] {
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
        case p@PrimLit(prim, _) => embedValueToExp(p) match {
          case BadExp => Set.empty
          case exp => mnext((PState(App(exp, args), rho, s, a), kstore))
        }
        case _ => Set.empty
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


    /** ****************************************************
      * Conditional operator
      * *****************************************************/
    case (c@PState(b@If(cond, tBranch, eBranch), rho, s, a), kstore) => {
      val frameToAdd = IfFrame(tBranch, eBranch, rho)
      for {
        k <- lookupKStore(kstore, a)
        b = kalloc(c, k)
        kstore1 = updateKStore(kstore, (b, Pointed(frameToAdd, a)))
      } yield (PState(cond, rho, s, b), kstore1)
    }

    /** ****************************************************
      * Set!
      * *****************************************************/
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

    /** ****************************************************
      * Primitive applications
      * *****************************************************/
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

    /** ****************************************************
      * Final state
      * *****************************************************/
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

    val collectedEdges: Set[Edge] = edges ++ newEdges

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
