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

package org.ucombinator.scheme.cfa.cesk

import scala.collection.immutable._

import org.ucombinator.scheme.syntax._
import math.BigInt._


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

  case class BoolLit(b: Boolean) extends Val

  case class QuotedLit(v: SExp) extends Val

  case class StringLit(s: String) extends Val

  case class PairLit(left: Val, right: Val) extends Val

  abstract class AbstractNumLit extends Val

  case class NumLit(n: Long) extends AbstractNumLit

  case object NumTop extends AbstractNumLit

  def mkNumLit(n: Long): AbstractNumLit = {
    if (n > 2) {
      NumTop
    } else if (n < -2) {
      NumTop
    } else {
      NumLit(n)
    }
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

  /********************************************************************
   * Stack-action markers
   ********************************************************************/
  abstract sealed class StackAction

  // Stack unchanged
  case object Eps extends StackAction

  // Push a frame
  case class Push(frame: Frame) extends StackAction

  // Pop a frame
  case class Pop(frame: Frame) extends StackAction

  /**
   * Inject an expression into a program
   * @param e initial expression
   */
  def initState(e: Exp): Conf


  /******************************************************
   * Utility functions
   ******************************************************/

  object StackActionKind extends Enumeration {
    type StackActionKind = Value
    val Eps, Pop, Push = Value
  }

  import StackActionKind._


  def kindOf(a: StackAction): StackActionKind = a match {
    case Eps => StackActionKind.Eps
    case Pop(_) => StackActionKind.Pop
    case Push(_) => StackActionKind.Push
  }

  def lookupStore(s: Store, a: Addr): Set[Val] = s.get(a) match {
    case Some(x) => x
    case None => throw new SemanticException("No associated values found for an address " +
      a.toString + "\nin store\n" + s.toString)
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

