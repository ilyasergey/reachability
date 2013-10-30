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
      case ("char=?", _ :: _ :: Nil) => Set(BoolLit(true), BoolLit(false))
      case ("eqv?", _ :: _ :: Nil) => Set(BoolLit(true), BoolLit(false))

      case ("char?", v1 :: Nil) => Set(BoolLit(true), BoolLit(false))
      case ("symbol?", v1 :: Nil) => Set(BoolLit(true), BoolLit(false))
      case ("null?", NilVal :: Nil) => {
        Set(BoolLit(true))
      }
      case ("null?", BadVal :: Nil) => {
        Set(BoolLit(true), BoolLit(false))
      }
      case ("null?", v :: Nil) => {
        Set(BoolLit(false))
      }
      case ("nil", Nil) => Set(NilVal)

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
      case ("=", v1 :: v2 :: Nil) => Set(BoolLit(v1 == v2))

      /**
       * Boolean operations
       */
      case ("not", BoolLit(b) :: Nil) => mkSet(BoolLit(!b))
      case ("not", v :: Nil) => mkSet(BadVal)

      case ("or", BoolLit(b) :: values)
        if values.forall {
          case BoolLit(_) => true
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
      case ("list", v1 :: Nil) => mkSet(PairLit(v1, NilVal))

      case ("car", PairLit(v1, v2) :: Nil) => mkSet(v1)
      case ("cdr", PairLit(v1, v2) :: Nil) => mkSet(v2)

      // case ("append", v1 :: v2 :: Nil) => mkSet(append(v1, v2))


      case ("pair?", PairLit(v1, v2) :: Nil) => {
        mkSet(BoolLit(true))
      }
      case ("pair?", BadVal :: Nil) => {
        Set(BoolLit(true), BoolLit(false))
      }
      case ("pair?", v :: Nil) => {
        mkSet(BoolLit(false))
      }

      case ("list?", PairLit(v1, v2) :: Nil) => mkSet(BoolLit(true))
      case ("list?", v :: Nil) => Set(BoolLit(true), BoolLit(false))

      case ("integer?" | "number?", (_: AbstractNumLit) :: Nil) => Set(BoolLit(true))
      case ("integer?" | "number?", _ :: Nil) => Set(BoolLit(false))

      case ("procedure?", Clo(_, _) :: Nil) => Set(BoolLit(true))
      case ("procedure?", _ :: Nil) => Set(BoolLit(true), BoolLit(false))

      case ("string?", StringLit(_) :: Nil) => Set(BoolLit(true))
      case ("string?", _ :: Nil) => Set(BoolLit(false))

      case ("boolean?", (_: BoolLit) :: Nil) => Set(BoolLit(true))
      case ("boolean?", _ :: Nil) => Set(BoolLit(false))

      case ("car", QuotedLit(x :+: y) :: Nil) => mkSet(QuotedLit(x))
      case ("cdr", QuotedLit(x :+: y) :: Nil) => mkSet(QuotedLit(y))

      case ("car", v :: Nil) => {
        Set.empty
      }
      case ("cdr", v :: Nil) => {
        Set.empty
      }


      case ("number->string", (n: NumLit) :: Nil) => mkSet(StringLit(n.n.toString))
      case ("number->string", NumTop :: Nil) => mkSet(StringLit("NumTop"))
      case ("number->string", _ :: Nil) => mkSet(StringLit("some-string-from-number"))

      case ("char-alphabetic?", _ :: Nil) => Set(BoolLit(true), BoolLit(false))
      case ("char-numeric?", _ :: Nil) => Set(BoolLit(true), BoolLit(false))

      case ("string-append", _) => mkSet(StringLit("result-of-appending-strings"))
      case ("symbol->string", _ :: Nil) => mkSet(StringLit("some-symbol-as-string"))
      case ("string->symbol", StringLit(s) :: Nil) => mkSet(QuotedLit(SText(s)))
      case ("string->symbol", v :: Nil) => mkSet(BadVal)
      case ("list->string", _ :: Nil) => mkSet(StringLit("some-list-as-string"))
      case ("string-length", _ :: Nil) => mkSet(NumTop)
      case ("string-ref", _ :: _ :: Nil) => mkSet(StringLit("some-char"))
      case ("char->integer", _ :: Nil) => Set(NumTop)


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

/*
  def append(p1: Val, p2: Val): Val = (p1, p2) match {
    case (NilVal, NilVal) => NilVal
    case (QuotedLit(s), NilVal) => QuotedLit(s)
    case (NilVal, QuotedLit(s)) => QuotedLit(s)
    case (QuotedLit(x :+: y), QuotedLit(s)) => PairLit(QuotedLit(x), append(QuotedLit(y), QuotedLit(s)))

    case (PairLit(h1, t1), NilVal) => PairLit(h1, t1)
    case (NilVal, PairLit(h1, t1)) => PairLit(h1, t1)
    case (PairLit(h1, t1), PairLit(h2, t2)) => PairLit(h1, append(t1, PairLit(h2, t2)))
    case _ => BadVal
  }
*/

  def mkSet[T](t: T): Set[T] = Set(t)

  val NilVal: Val = QuotedLit(SNil)

}

class PrimAppException(s: String) extends SemanticException(s)
