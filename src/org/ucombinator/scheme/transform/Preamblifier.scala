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

package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.parsing.RnRSParser

class Preamblifier extends ProgramTransformer {
  def apply(prog: Program): Program = {
    // Adds the standard preamble.:

    // cons, car, cdr, pair?

    prog match {
      case Program(decs, defs, init) => {
        val p = new RnRSParser

        var newDecs = decs
        var newDefs = defs
        var newInit = init

        newDecs = TypeDec(SName.from("pair"), StrictStruct(List(SName.from("car"), SName.from("cdr")))) :: newDecs

        newDefs = p.parseDef("(define (car p) (struct-get p car pair))") :: newDefs

        newDefs = p.parseDef("(define (cdr p) (struct-get p cdr pair))") :: newDefs

        newDefs = p.parseDef("(define (cons a b) (make-struct pair a b))") :: newDefs

        newDefs = p.parseDef("(define (pair? p) ((type? pair) p))") :: newDefs

        Program(newDecs, newDefs, newInit)
      }
    }
  }
}
