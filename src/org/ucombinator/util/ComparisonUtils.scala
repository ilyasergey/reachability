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
