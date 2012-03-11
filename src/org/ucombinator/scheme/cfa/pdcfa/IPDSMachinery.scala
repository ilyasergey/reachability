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

package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.scheme.cfa.cesk._

/**
 * Implementation of Introspective pushdown system
 * based on a provided CESK machinery
 *
 * @author ilya
 */


trait IPDSMachinery extends StateSpace with PDCFAGarbageCollector {
  self: StackCESKMachinery =>

  type Q = ControlState

  /**
   * Main iteration function of IPDS
   *
   * @param q source control state
   * @param k a [shallow] continuation to make a next step passed instead of a full stack
   * @param frames a set of possible frames in the stack at this state (for Garbage Collection)
   * @return a set of paired control states and stack actions
   */
  def stepIPDS(q: Q, k: Kont, frames: Kont): Set[(StackAction, Q)] = {
    val newQ: Q = (if (shouldGC) gc(q, frames) else q)
    for {
      (q1, k_new) <- mnext(newQ, k)
      // TODO: implement GC using passed frames
      g = decideStackAction(k, k_new)
    } yield (g, q1)
  }

  def decideStackAction(k1: Kont, k2: Kont): StackAction = (k1, k2) match {
    case (x, y) if x == y => Eps
    case (h :: t1, t2) if t1 == t2 => Pop(h)
    case (t1, h :: t2) if t1 == t2 => Push(h)
    case _ => throw new IPDSException("Continuation par is malfomed:\n" +
      "k1: " + k1.toString + "\n" +
      "k2: " + k2.toString)
  }


}


class IPDSException(s: String) extends PDCFAException(s)
