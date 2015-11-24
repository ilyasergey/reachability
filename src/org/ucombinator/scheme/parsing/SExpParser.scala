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

package org.ucombinator.scheme.parsing

import util.parsing.combinator._
import scala.util.matching.Regex
import org.ucombinator.scheme.syntax._

class SExpParser extends RegexParsers {

  /**

  BUG! try: "\n(a)\n"

   */

  override def skipWhitespace = true

  override protected val whiteSpace = new Regex("([\\r\\n\\t ]*([;][^\\r\\n]*[\\r\\n]?)?)+")

  private def lpar: Parser[String] =
    regex(new Regex("[(]")) ^^ {
      case "(" => "("
    }

  private def rpar: Parser[String] =
    regex(new Regex("[)]")) ^^ {
      case ")" => ")"
    }

  private def integer: Parser[SExp] =
    regex(new Regex("-?[0-9]+")) ^^ {
      case s => SInt(Integer.parseInt(s))
    }

  private def strue: Parser[SExp] =
    ("#t" | "#T") ^^ {
      case _ => SBoolean(true)
    }

  private def sfalse: Parser[SExp] =
    ("#f" | "#F") ^^ {
      case _ => SBoolean(false)
    }

  private def schar: Parser[SExp] =
    ("#\\" ~ regex(new Regex("[^\\r\\n\\t ]"))) ^^ {
      case "#\\" ~ c => SChar(c.charAt(0))
    }

  private def symbol: Parser[SExp] =
    regex(new Regex("([^.#; \\t\\r\n()',`\"][^; \\t\\r\\n()',`\"]*|[.][^; \\t\\r\\n()',`\"]+)")) ^^ {
      case s => SName.from(s)
    }

  private def keyword: Parser[SExp] =
    regex(new Regex("([#][:][^; \\t\\r\\n()',`\"]+)")) ^^ {
      case s => SKeyword.from(s.substring(2))
    }

  private def text: Parser[SExp] =
    "\"\"" ^^ {
      case _ => SText("")
    } |
      regex(new Regex("\"([^\"\\\\]|\\\\.|\\\\\\\\|)*\"")) ^^ {
        case s => SText(s.substring(1, s.length() - 1))
      }

  private def nil: Parser[SExp] =
    (lpar ~ rpar) ^^ {
      case "(" ~ ")" => SNil()
    }

  private def sboolean: Parser[SExp] = strue | sfalse

  private def sxlist: Parser[SExp] =
    (lpar ~ sexplist ~ rpar) ^^ {
      case "(" ~ l ~ ")" => l
    }

  private def special: Parser[SExp] =
    (",@" ~ sexp) ^^ {
      case ",@" ~ sexp => SExp(List(CommonSSymbols.SUnquoteSplicing, sexp))
    } |
      ("'" ~ sexp) ^^ {
        case "'" ~ sexp => SExp(List(CommonSSymbols.SQuote, sexp))
      } |
      ("`" ~ sexp) ^^ {
        case "`" ~ sexp => SExp(List(CommonSSymbols.SQuasiquote, sexp))
      } |
      ("," ~ sexp) ^^ {
        case "," ~ sexp => SExp(List(CommonSSymbols.SUnquote, sexp))
      }


  private def sexp: Parser[SExp] = positioned(nil | sxlist | integer | sboolean | keyword | schar | text | symbol | special)

  private def sexplist: Parser[SExp] =
    rep(sexp) ~ (("." ~ sexp) ?) ^^ {
      case sexps ~ Some("." ~ sexp) => SExp(sexps, sexp)
      case sexps ~ None => SExp(sexps)
    }

  def parse(input: String): SExp = parse(sexp, input).get

  def parseAll(input: String): List[SExp] = {
    val result = parse(phrase(rep(sexp)), input)

    if (result.successful)
      result.get
    else {
      throw new Exception("Parsing failed at position " + result.next.pos + ";\n character: '" + result.next.first + "';\n at end: " + result.next.atEnd)
    }
  }

}
