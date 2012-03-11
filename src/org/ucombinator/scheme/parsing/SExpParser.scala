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
    "#t" ^^ {
      case "#t" => SBoolean(true)
    }

  private def sfalse: Parser[SExp] =
    "#f" ^^ {
      case "#f" => SBoolean(false)
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
