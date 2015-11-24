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

package org.ucombinator.scheme.scalatest

/**
 * @author ilya
 */

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.ucombinator.scheme.parsing.SExpParser

class SExpParsingTest extends FunSpec with ShouldMatchers {

  val p = new SExpParser

  describe("An S-Expression parser") {

    it("should parse simple comments") {
      p.parse(";; Math routines \n (foo)").toString should equal("(foo)")
    }


    it("should parse simple numbers") {
      p.parse("3").toString should equal("3")
    }

    it("should parse simple simple s-expressions") {
      p.parse("()").toString should equal("()")
      p.parse("(3)").toString should equal("(3)")
      p.parse("foo").toString should equal("foo")
      p.parse("( foo bar\n\n\t baz)").toString should equal("(foo bar baz)")
    }

    it("should parse more comments") {
      p.parse("(foo ;bar\n\n baz)").toString should equal("(foo baz)")
    }

    it("should parse spaces") {
      p.parse("(foo )").toString should equal("(foo)")
    }

    it("should other stuff") {
      (p.parseAll(";; Math routines \n ; test \n (foo) (bar) ; baz\n").toString should
        equal("List((foo), (bar))"))
    }

  }

}
