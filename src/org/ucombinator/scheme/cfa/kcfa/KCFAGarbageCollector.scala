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

import org.ucombinator.scheme.cfa.gc.SchemeGarbageCollector

/**
 * @author ilya
 */

trait KCFAGarbageCollector extends SchemeGarbageCollector {
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
