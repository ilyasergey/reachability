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

package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.cfa.cesk._
import org.ucombinator.util.DataUtil._

/**
 * @author ilya
 */

trait StackCESKMachinery extends CESKMachinery {

  type Kont = List[Frame]

  type Addr = (Var, List[Exp])

  // No continuation pointers
  type KAddr = Unit

  /********************************************************************
   * Utility functions
   ********************************************************************/
  def initState(e: Exp): Conf = (PState(e, Map.empty, Map.empty, ()), Nil)

  /********************************************************************
   * Main non-deterministic abstract step function
   ********************************************************************/
  def mnext: Conf => Set[Conf] = {

    /********************************************
     * Core transitions
     *********************************************/

    // Application of lambda or reference
    case c@(PState(App(f@(Lambda(_, _) | Ref(_)), args), rho, s, kptr), k) =>
      for {
        Clo(lam@Lambda(Formals(params, _), body), rho1) <- atomicEval(f, rho, s)

        // process lambda parameters
        paramNames = params.map(_.name)
        ai = paramNames.map(alloc(_, c)) // allocate addresses
        rho2 = updateEnv(rho1, paramNames.zip(ai)) // update function env

        arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s)) // map atomic arguments to values
        s1 = updateStore(s, ai.zip(arg_vals))

        // In A-normal form only one expression in body
        e = getLambdaBodyInANF(lam)
      } yield (PState(e, rho2, s1, kptr), k)


    //    case c@(PState(l@Let(_, _), rho, s, kptr), k)
    //      if (decomposeLetInANF(l)._2.isUnspecified) => {
    //      val (v, _, e) = decomposeLetInANF(l)
    //      val a = alloc(v, c)
    //      val rho1 = updateEnv(rho, List((v, a)))
    //      Set((PState(e, rho1, s, kptr), k))
    //    }


    case (PState(l@Let(_, _), rho, s, kptr), k) => {
      val (v, call, e) = decomposeLetInANF(l)
      Set((PState(call, rho, s, kptr), LetFrame(v, e, rho) :: k))
    }

    case c@(PState(ae, rho, s, kptr), LetFrame(v, e, rho1) :: k)
      if isAtomic(ae) => {
      val a = alloc(v, c)
      val rho2 = updateEnv(rho1, List((v, a)))
      val s1 = updateStore(s, List((a, atomicEval(ae, rho, s))))
      Set((PState(e, rho2, s1, kptr), k))
    }

    /******************************************************
     * Conditional operator
     ******************************************************/
    case c@(PState(b@If(cond, tBranch, eBranch), rho, s, kptr), k) => {
      Set((PState(cond, rho, s, kptr), IfFrame(tBranch, eBranch, rho) :: k))
    }
    case c@(PState(ae, rho, s, kptr), IfFrame(tBranch, eBranch, rho1) :: k)
      if isAtomic(ae) => {
      val boolValues = getBooleanValues(ae, rho, s)
      boolValues.map {
        b => if (b) {
          (PState(tBranch, rho1, s, kptr), k)
        } else {
          (PState(eBranch, rho1, s, kptr), k)
        }
      }
    }

    /******************************************************
     * Set!
     ******************************************************/
    // It is always the case that the stack is not empty
    case c@(PState(SetVar(v, ae), rho, s, kptr), LetFrame(_, e, rho1) :: k)
      // Only atomic values are assigned
      if (isAtomic(ae)) => {
      val addr = lookupEnv(rho, v)
      val s1 = updateStore(s, List((addr, atomicEval(ae, rho, s))))
      Set((PState(e, rho1, s1, kptr), k))
    }


    /******************************************************
     * Primitive applications
     ******************************************************/
    // only atomic values or variable are supported in primops
    case c@(PState(app@App(p@Prim(primName, _), args), rho, s, kptr), k) => {
      // map atomic arguments to values (sets)
      val arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s))
      val setOfLists = toSetOfLists(arg_vals)
      for {
        arg_vector <- setOfLists
        results = evalPrimApp(primName, arg_vector)
        result <- results
        state = analyseResult(result, rho, s, app, kptr)
      } yield (state, k)
    }


    /******************************************************
     * Final state
     ******************************************************/
    case c@(PState(ae, rho, s, kptr), Nil)
      if isAtomic(ae) => for {
    /**
     * (REMARK)
     * [Non-sense values in a final state]
     * Consider them as a lost of precision
     */
      v <- atomicEval(ae, rho, s) //.filter(isValidValueToReturn(_))
    } yield (PFinal(v), Nil)

    // Ok, folks, that's it!
    case (PFinal(_), Nil) => Set()

    case (ErrorState(_, _), _) => Set()

    case c => {
      val c1 = c // for convenient debug
      throw new StackCESKException("Wrong state: " + c1.toString)
    }

  }

  class StackCESKException(s: String) extends CESKException(s)


}