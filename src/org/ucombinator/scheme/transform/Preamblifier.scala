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
