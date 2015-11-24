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
      case ("car", PairLit(v1, v2) :: Nil) => mkSet(v1)
      case ("cdr", PairLit(v1, v2) :: Nil) => mkSet(v2)

      case ("pair?", PairLit(v1, v2) :: Nil) => mkSet(BoolLit(true))
      case ("pair?", v :: Nil) => Set(BoolLit(true), BoolLit(false))

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

      case ("car", v :: Nil) => mkSet(BadVal)
      case ("cdr", v :: Nil) => mkSet(BadVal)


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

  def mkSet[T](t: T): Set[T] = Set(t)

}

class PrimAppException(s: String) extends SemanticException(s)
