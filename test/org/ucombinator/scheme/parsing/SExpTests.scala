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

package org.ucombinator.scheme.parsing

import org.ucombinator.scheme.syntax._


object SExpTests {

  def assert (test : => Boolean) {
    if (!test) {
      throw new Exception("Test failed!")
    }
  }

  def main (args : Array[String]) {

    import CommonSSymbols._ ;

    val p = new SExpParser

    if (args.length > 0) {

      val filename = args(0)

      // println("Testing s-expression parser on " + filename)

      val lines = scala.io.Source.fromFile(filename).mkString("")

      println(lines)

      val sexps = p.parseAll(lines)

      println(sexps)
    }


    assert (p.parse(";; Math routines \n (foo)").toString
            ==
            "(foo)") ;

    assert (p.parse("3").toString
            ==
            "3") ;

    assert (p.parse("()").toString
            ==
            "()") ;

    assert (p.parse("foo").toString
            ==
            "foo") ;

    assert (p.parse("(3)").toString
            ==
            "(3)") ;

    assert (p.parse("( foo bar\n\n\t baz)").toString
            ==
            "(foo bar baz)") ;

    assert (p.parse("foo ;bar").toString
            ==
            "foo") ;

    assert (p.parse("(foo ;bar\n\n baz)").toString
            ==
            "(foo baz)") ;

    assert (p.parse("(foo )").toString
            ==
            "(foo)") ;

    assert (p.parse("(lambda)") match {
      case SList(SLet) => false
      case SList(SLambda) => true
    })


    //println("parseAll test:")
    //println(p.parseAll(";; Math routines \n (foo) (bar)").toString)

    assert (p.parseAll(";; Math routines \n ; test \n (foo) (bar) ; baz\n").toString
            ==
            "List((foo), (bar))") ;

  }

}
