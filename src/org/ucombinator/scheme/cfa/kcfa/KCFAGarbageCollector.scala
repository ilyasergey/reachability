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

import org.ucombinator.scheme.cfa.gc.GarbageCollector

/**
 * @author ilya
 */

trait KCFAGarbageCollector extends GarbageCollector {
  self: PointerCESKMachinery =>

  def rootAddr(c: ControlState, kstore: Kont): Set[Addr] = {
    c match {
      case ErrorState(_, _) | PFinal(_) => Set.empty
      case PState(e, rho, s, kaddr) => {
        val envAddr: Set[Addr] = rho.values.toSet

        val initalkAddr = Set(kaddr)
        val reachableKPtrs: Set[KAddr] = iterateKStore(initalkAddr, kstore)
        val reachableAKonts: Set[AKont] = reachableKPtrs.flatMap(ka => lookupKStore(kstore, ka))

        val reachableFrames: Set[Frame] = reachableAKonts.filter(x => x != MT).map {
          case Pointed(frame, _) => frame
        }

        val stackAddr: Set[Addr] = reachableFrames.flatMap(fetchAddressesFromFrame(_))

        envAddr ++ stackAddr

      }
    }
  }


  /**
   * Garbage collect KStore as well for consistency
   */
  def gcKStore(kaddr: KAddr, kstore: Kont): Kont = {
    val initalkAddr = Set(kaddr)
    val reachableKPtrs: Set[KAddr] = iterateKStore(initalkAddr, kstore)
    kstore.filter {
      case (ka, _) => reachableKPtrs.contains(ka)
    }
  }

  /**
   * get all reachable continuations addresses
   */
  def iterateKStore(workset: Set[KAddr], kstore: KStore): Set[KAddr] = {

    //    val akonts: Set[AKont] = kstore.values.flatten.toSet

    // seems to be incorrect ...

    val akonts: Set[AKont] = workset.flatMap {
      ka => lookupKStore(kstore, ka)
    }

    val newKPtrs: Set[KAddr] = akonts.filter {
      case MT => false
      case _ => true
    }.map {
      case Pointed(frame, kptr) => kptr
    }

    val newWorkSet = workset ++ newKPtrs
    if (newWorkSet == workset) {
      //      println(newWorkSet.size)
      //      println(kstore.keys.size)
      //      println("-----")
      newWorkSet
    } else {
      iterateKStore(newWorkSet, kstore)
    }
  }

}
