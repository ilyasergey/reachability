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
