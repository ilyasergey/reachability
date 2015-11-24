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

package org.ucombinator.scheme.cfa.cesk

import scala.collection.immutable._

import org.ucombinator.scheme.syntax._
import org.ucombinator.dsg._


/**
 * @author ilya
 */

trait StateSpace extends PrimOperators {

  // Partial map
  type :->[T, S] = Map[T, S]

  /**Abstract addresses
   * Should be provided in a particular implementation
   */
  type Addr

  /**
   * Continuation addr
   */
  type KAddr

  /**
   * Variables are just SNames from standard Abstract syntax
   */
  type Var = SName

  /**
   * Abstract type to carry continuations
   */
  type Kont

  /**
   * Standard abstract state-space, following the paper text
   */
  type Env = Var :-> Addr
  type Store = Addr :-> Set[Val]


  /********************************************************************
   * Frames
   ********************************************************************/
  abstract sealed class Frame

  case class LetFrame(v: Var, e: Exp, rho: Env) extends Frame

  case class SeqFrame(defs: List[Def], next: List[Exp], rho: Env, label: Begin) extends Frame

  case class IfFrame(thenE: Exp, elseE: Exp, rho: Env) extends Frame

  /********************************************************************
   * Abstract values
   ********************************************************************/
  abstract sealed class Val

  case class Clo(lam: Lambda, rho: Env) extends Val

  case object UnspecifiedVal extends Val

  case object BadVal extends Val

  case class QuotedLit(v: SExp) extends Val

  case class StringLit(s: String) extends Val

  case class PairLit(left: Val, right: Val) extends Val

  case class BoolLit(b: Boolean) extends Val

  case class PrimLit(name: String, safe: Boolean) extends Val

  abstract class AbstractNumLit extends Val

  case class NumLit(n: Long) extends AbstractNumLit

  case object NumTop extends AbstractNumLit

  def mkNumLit(n: Long): AbstractNumLit = {
//    if (n > 2) {
//      NumTop
//    } else if (n < -2) {
//      NumTop
//    } else {
//      NumLit(n)
//    }
    NumTop
  }

  //todo: add more: literals etc

  /********************************************************************
   * Configurations are split into control states
   * and continuation frames
   ********************************************************************/
  abstract sealed class ControlState

  // Partial CES[K] state
  case class PState(e: Exp, rho: Env, s: Store, kptr: KAddr) extends ControlState

  // final state
  case class PFinal(v: Val) extends ControlState

  case class ErrorState(e: Exp, msg: String) extends ControlState

  type Conf = (ControlState, Kont)

  /**
   * Inject an expression into a program
   * @param e initial expression
   */
  def initState(e: Exp): Conf


  /******************************************************
   * Utility functions
   ******************************************************/

  def kindOf(a: StackAction[_]): StackActionKind.Value = a match {
    case Eps => StackActionKind.Eps
    case Pop(_) => StackActionKind.Pop
    case Push(_) => StackActionKind.Push
    case Switch(_, _, _) => throw new SemanticException("No switch edges allowed here.")
  }

  def lookupStore(s: Store, a: Addr): Set[Val] = s.get(a) match {
    case Some(x) => x
    case None => Set.empty
      //throw new SemanticException("No associated values found for an address " + a.toString + "\nin store\n" + s.toString)
  }

  def lookupEnv(rho: Env, v: Var): Addr = rho.get(v) match {
    case Some(x) => x
    case None => throw new SemanticException("No associated address found for a name " +
      v + "\nin environment\n" + rho.toString)
  }

  def updateEnv(rho: Env, pairs: List[(Var, Addr)]) =
    pairs.foldLeft(rho)((accum, pair) => accum + pair)


  def updateStore(s: Store, pairs: List[(Addr, Set[Val])]) =
    pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: Set[Val] = accum.getOrElse(a, Set())
      val newVals: Set[Val] = oldVals ++ vs.filter(v => v != UnspecifiedVal)
      accum + ((a, newVals))
    })

  /**
   * Abstract
   */
  def alloc(v: Var, c: Conf): Addr

  def k: Int

  def isDummy: Boolean

  /**
   * Store merging machinery
   * For single-store passing optimization
   */
  def mergeTwoStores[K, V](s1: K :-> Set[V], s2: K :-> Set[V]): K :-> Set[V] = {
    s2.foldLeft(s1)((resultStore: K :-> Set[V], keyValue: (K, Set[V])) => {
      // these are from s2
      val (k, vs) = keyValue
      // these are from s1
      val newValues = s1.getOrElse(k, Set())
      resultStore + ((k, vs ++ newValues))
    })
  }

  def mergeStores[K, V](initial: K :-> Set[V], newStores: List[K :-> Set[V]]): K :-> Set[V] = {
    newStores.foldLeft(initial)((result, current) => mergeTwoStores(result, current))
  }


}

class SemanticException(s: String) extends Exception(s)

