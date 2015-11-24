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

package org.ucombinator.lambdajs.cfa

import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait StoreInterface {
  self: LJSyntax =>

  import LJSyntax._

  type ControlState

  type :->[A, B] = Map[A, B]

  type Store = Addr :-> Set[Value]

  // analysis functions

  def alloc(s: ControlState): Addr

  def alloc(s: ControlState, x: Var): Addr

  // utility

  def putMany[A, B](map: A :-> Set[B], keyVals: List[(A, Set[B])]): A :-> Set[B] =
    keyVals.foldLeft(map) {
      case (result, (k, vs)) => put(result, k, vs)
    }

  def put[A, B](map: A :-> Set[B], key: A, newVals: Set[B]): A :-> Set[B] = {
    val oldVals = map.getOrElse(key, Set())
    val values = oldVals ++ newVals
    map + ((key, values))
  }

  def get[A, B](map: A :-> Set[B], key: A): Set[B] = map.getOrElse(key, Set())

}
