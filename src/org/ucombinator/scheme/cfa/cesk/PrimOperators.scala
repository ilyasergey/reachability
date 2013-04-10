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

package org.ucombinator.scheme.cfa.cesk

import org.ucombinator.scheme.syntax._

/**
 * @author ilya
 */

trait PrimOperators {
  self: StateSpace =>

  /**
   * Main dispatch method
   * @param primName name of the primitive
   * @param args list of arguments
   * @return set of abstract values
   */
  def evalPrimApp(primName: String, args: List[Val]): Set[Val] = {
    (primName, args) match {

      // No result for unspecified value
      case (name, UnspecifiedVal :: _) => Set()

      /**
       * General
       */
      case ("eq?", v1 :: v2 :: Nil) => Set(BoolLit(v1 eq v2))
      case ("equal?", v1 :: v2 :: Nil) => Set(BoolLit(v1 == v2))
      case ("char?", v1 :: Nil) => Set(BoolLit(true), BoolLit(false))
      case ("symbol?", v1 :: Nil) => Set(BoolLit(true), BoolLit(false))
      case ("null?", v1 :: Nil) => Set(BoolLit(true), BoolLit(false))

      /**
       * Comparisons
       */
      case (">", NumLit(x) :: NumLit(y) :: Nil) => mkSet(BoolLit(x > y))
      case (">=", NumLit(x) :: NumLit(y) :: Nil) => mkSet(BoolLit(x >= y))
      case ("<", NumLit(x) :: NumLit(y) :: Nil) => mkSet(BoolLit(x < y))
      case ("<=", NumLit(x) :: NumLit(y) :: Nil) => mkSet(BoolLit(x <= y))

      case (">" | "<" | ">=" | "<=" | "=" | "!=", NumTop :: (NumLit(_) | NumTop) :: Nil) =>
        Set(BoolLit(true), BoolLit(false))
      case (">" | "<" | ">=" | "<=" | "=" | "!=", NumLit(_) :: NumTop :: Nil) =>
        Set(BoolLit(true), BoolLit(false))

      case ("=", NumLit(x) :: NumLit(y) :: Nil) => mkSet(BoolLit(x == y))
      case ("=", x :: y :: Nil) => mkSet(BoolLit(x == y))


      /**
       * Boolean operations
       */
      case ("not", BoolLit(b) :: Nil) => mkSet(BoolLit(!b))
      case ("or", BoolLit(b) :: values)
        if values.forall {
          case BoolLit(_) => true;
          case _ => false
        } => {
        val result = values.foldLeft(b) {
          case (b1, BoolLit(v)) => b1 || v
        }
        mkSet(BoolLit(result))
      }

      case ("and", BoolLit(b) :: values)
        if values.forall {
          case BoolLit(_) => true;
          case _ => false
        } => {
        val result = values.foldLeft(b) {
          case (b1, BoolLit(v)) => b1 && v
        }
        mkSet(BoolLit(result))
      }

      /**
       * Arithmetic
       */
      case ("+", NumLit(x) :: NumLit(y) :: Nil) => mkSet(mkNumLit(x + y))
      case ("-", NumLit(x) :: NumLit(y) :: Nil) => mkSet(mkNumLit(x - y))
      case ("*", NumLit(x) :: NumLit(y) :: Nil) => mkSet(mkNumLit(x * y))
      case ("/", NumLit(x) :: NumLit(0) :: Nil) => mkSet(NumTop)
      case ("/", NumLit(x) :: NumLit(y) :: Nil) => mkSet(mkNumLit(x / y))

      case ("odd?", NumLit(n) :: Nil) => mkSet(BoolLit(n % 2 == 0))
      case ("even?", NumLit(n) :: Nil) => mkSet(BoolLit(n % 2 == 1))
      case ("even?" | "odd?", NumTop :: Nil) => Set(BoolLit(true), BoolLit(false))


      case ("random", _) => mkSet(NumTop)

      case ("/" | "*" | "+" | "-" | "quotient"
            | "gcd" | "modulo" | "log", NumLit(_) :: NumTop :: Nil) => mkSet(NumTop)
      case ("/" | "*" | "+" | "-" | "quotient"
            | "gcd" | "modulo" | "log", NumTop :: (NumLit(_) | NumTop) :: Nil) => mkSet(NumTop)

      case ("log", NumTop :: Nil) => mkSet(NumTop)
      case ("log", NumLit(n) :: Nil) => mkSet(mkNumLit(math.log(n.toDouble).toLong))

      case ("ceiling", NumTop :: Nil) => mkSet(NumTop)
      case ("ceiling", NumLit(n) :: Nil) => mkSet(mkNumLit(math.ceil(n.toDouble).toLong))

      /**
       * Input, output and errors
       */
      case ("display", _) => mkSet(UnspecifiedVal)
      case ("newline", _) => mkSet(UnspecifiedVal)
      case ("error", _) => mkSet(UnspecifiedVal)

      /**
       * Lists and pairs
       */
      case ("cons", v1 :: v2 :: Nil) => mkSet(PairLit(v1, v2))
      case ("car", PairLit(v1, v2) :: Nil) => mkSet(v1)
      case ("cdr", PairLit(v1, v2) :: Nil) => mkSet(v2)

      case ("pair?", PairLit(v1, v2) :: Nil) => mkSet(BoolLit(true))
      case ("pair?", v :: Nil) => Set(BoolLit(true), BoolLit(false))

      case ("integer?", (_ : AbstractNumLit) :: Nil) => Set(BoolLit(true))
      case ("integer?", _ :: Nil) => Set(BoolLit(false))

      case ("boolean?", (_ : BoolLit) :: Nil) => Set(BoolLit(true))
      case ("boolean?", _ :: Nil) => Set(BoolLit(false))

      case ("car", QuotedLit(x :+: y) :: Nil) => mkSet(QuotedLit(x))
      case ("cdr", QuotedLit(x :+: y) :: Nil) => mkSet(QuotedLit(y))

      case ("car", v :: Nil) => mkSet(BadVal(v, "car"))
      case ("cdr", v :: Nil) => mkSet(BadVal(v, "cdr"))

      /**
       * Terra Incognita
       */
      case _ => {
        throw new PrimAppException("Primitive operation not yet implemented or number of parameters is wrong.\n"
          + "Operation: " + primName + "\n"
          + "Arguments: " + args.toString)
      }
    }
  }

  def mkSet[T](t: T): Set[T] = Set(t)

}

class PrimAppException(s: String) extends SemanticException(s)
