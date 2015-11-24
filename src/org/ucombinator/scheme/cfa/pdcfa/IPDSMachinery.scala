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

import org.ucombinator.scheme.cfa.cesk._
import org.ucombinator.dsg._

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
   * @param framesForGC a set of possible frames in the stack at this state (for Garbage Collection)
   * @return a set of paired control states and stack actions
   */
  def stepIPDS(q: Q, k: List[Frame], framesForGC: List[Frame]): Set[(StackAction[Frame], Q)] = {
    val newQ: Q = (if (shouldGC) gc(q, framesForGC) else q)
    for {
      (q1, k_new) <- mnext(newQ, k)
      g = decideStackAction(k, k_new)
    } yield (g, q1)
  }

  def decideStackAction(k1: List[Frame], k2: List[Frame]): StackAction[Frame] = (k1, k2) match {
    case (x, y) if x == y => Eps
    case (h :: t1, t2) if t1 == t2 => Pop(h)
    case (t1, h :: t2) if t1 == t2 => Push(h)
    case _ => throw new IPDSException("Continuation par is malfomed:\n" +
      "k1: " + k1.toString + "\n" +
      "k2: " + k2.toString)
  }


}


class IPDSException(s: String) extends PDCFAException(s)
