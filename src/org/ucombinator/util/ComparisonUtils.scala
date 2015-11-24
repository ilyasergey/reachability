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

package org.ucombinator.util

/* Totally ordered comparison utilities. */
object ComparisonUtils {

  def compare2[A <: Ordered[A], B <: Ordered[B]](a1: A, b1: B)(a2: A, b2: B): Int = {
    val cmpA = a1 compare a2
    if (cmpA != 0)
      cmpA
    else
      b1 compare b2
  }

  def compare3[A <: Ordered[A], B <: Ordered[B], C <: Ordered[C]](a1: A, b1: B, c1: C)(a2: A, b2: B, c2: C): Int = {
    val cmpA = a1 compare a2
    if (cmpA != 0)
      cmpA
    else {
      val cmpB = b1 compare b2
      if (cmpB != 0)
        cmpB
      else
        c1 compare c2
    }
  }


  def compareLists[A <% Ordered[A]](l1: List[A], l2: List[A]): Int = (l1, l2) match {
    case (hdA :: tlA, hdB :: tlB) => {
      val cmpHD = hdA compare hdB
      if (cmpHD != 0)
        cmpHD
      else
        compareLists(tlA, tlB)
    }
    case (List(), List()) => 0
    case (hd :: tl, List()) => 1
    case (List(), hd :: tl) => -1
  }
}
