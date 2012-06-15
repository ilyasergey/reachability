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

package org.ucombinator.scheme.cfa.gc

import org.ucombinator.scheme.cfa.cesk._
import org.ucombinator.gc.GCInterface

/**
 * @author ilya
 */

trait SchemeGarbageCollector extends StateSpace with GCInterface {

  def gc(c: ControlState, frames: Kont): ControlState = c match {
    case ErrorState(_, _) | PFinal(_) => c
    case PState(e, rho, s, kptr) => {
      val alive = reachable(c, frames)
      val cleanStore = s.filter {
        case (a, _) => alive.contains(a)
      }
      PState(e, rho, cleanStore, kptr)
    }
  }

  def reachable(c: ControlState, frames: Kont): Set[Addr] = {
    val rootAddresses: Set[Addr] = rootAddr(c, frames)
    c match {
      case ErrorState(_, _) | PFinal(_) => Set.empty
      case PState(_, _, s, kptr) => {
        val result: Set[Addr] = collectAdjacent(rootAddresses, s)
        result

        if (printGCDebug) {
          val original = s.keys.toSet
          val delta = original -- result
          if (!delta.isEmpty) {
            println("Original store size: " + original.size + "")
            println("Result size: " + result.size + "")
            println("Store delta (size " + delta.size + "):")
            println(delta)
            println()
          }
        }

        result
      }
    }
  }

  /**
   * Get addresses from a stack
   *
   * @param f a frame with environment
   * @return
   */
  def fetchAddressesFromFrame(f: Frame) = f match {
    case LetFrame(_, _, rho) => rho.values
    case IfFrame(_, _, rho) => rho.values
    case SeqFrame(_, _, rho, _) => rho.values
    case _ => Set()
  }

  def collectAdjacent(previousAddrs: Set[Addr], store: Store): Set[Addr] = {

    val filteredStore = store.filter {
      // only addresses in previousAddrs
      case (a, vals) => previousAddrs.contains(a)
    }

    val relevantValues = filteredStore.flatMap {
      // flatten values
      case (a, vals) => vals
    }

    val relevantClosures = relevantValues.filter {
      // take closures only
      case Clo(lam, rho) => true
      case _ => false
    }.toSet


    val relevantEnvs = relevantClosures.map {
      (v: Val) => v match {
        // take environments
        case Clo(_, rho) => rho
      }
    }
    val newAddresses: Set[Addr] = relevantEnvs.flatMap {
      // take ranges of all environments
      // from closures in store
      rho => rho.values
    }

    if (newAddresses subsetOf previousAddrs) {
      previousAddrs
    } else {
      collectAdjacent(newAddresses ++ previousAddrs, store)
    }
  }

}

