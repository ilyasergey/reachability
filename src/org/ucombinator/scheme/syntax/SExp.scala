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

package org.ucombinator.scheme.syntax

import util.parsing.input.Positional
import org.ucombinator.scheme.parsing.SExpParser

object SExp {

  /**
  Determines the style with which names are printed.
   */
  var shouldNamesBeSymbols = true

  def apply(list: List[SExp]): SExp = list match {
    case hd :: tl => :+:(hd, apply(tl))
    case Nil => SNil
  }

  def apply(list: List[SExp], tombstone: SExp): SExp = list match {
    case hd :: tl => :+:(hd, apply(tl, tombstone))
    case Nil => tombstone
  }

  def parseAllIn(filename: String): List[SExp] = {
    val input = scala.io.Source.fromFile(filename).mkString("")
    parseAll(input)
  }

  def parseAll(input: String): List[SExp] = {
    val p = new SExpParser
    p.parseAll(input)
  }

  def parse(input: String): SExp = {
    val p = new SExpParser
    p.parse(input)
  }

  private var maxSerialNumber = 0

  def allocateSerialNumber(): Long = {
    maxSerialNumber += 1
    maxSerialNumber
  }
}

/* S-Expressions. */



abstract class SExp extends Positional {
  lazy val serialNumber: Long = SExp.allocateSerialNumber()

  def toString: String;

  def toList: List[SExp];

  def toDottedList: (List[SExp], SExp);

  def isKeyword: Boolean;

  def isInteger: Boolean;

  def isList: Boolean;

  def isPair: Boolean;

  def isNull: Boolean;

  def isSymbol: Boolean;

  def isName: Boolean;
}


final case class SInt(val value: BigInt) extends SExp {
  override def toString = value.toString

  def toList = throw new Exception("Cannot convert integer to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


final case class SChar(val value: Char) extends SExp {
  override def toString = "#\\" + value.toString

  def toList = throw new Exception("Cannot convert integer to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isChar = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


final case class SText(val value: String) extends SExp {
  // TODO/FIXME: Escape the string value
  override def toString = "\"" + value + "\""

  def toList = throw new Exception("Cannot convert string to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isString = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


case class SBoolean(val value: Boolean) extends SExp {
  override def toString = (if (value) {
    "#t"
  } else {
    "#f"
  })

  def toList = throw new Exception("Cannot convert Boolean to list.")

  def toDottedList = (List(), this)

  def isFalse = value

  def isKeyword = false

  def isInteger = false

  def isString = false

  def isBoolean = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


case class SKeyword(val string: String) extends SExp with Ordered[SKeyword] {
  override def toString = "#:" + string

  def toList = throw new Exception("Cannot convert keyword to list.")

  def toDottedList = (List(), this)

  def isFalse = false

  def isKeyword = true

  def isInteger = false

  def isString = false

  def isBoolean = false

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false

  def compare(that: SKeyword) = this.string compare that.string
}


abstract case class SSymbol(val string: String) extends SExp {
}


final case class SName(s: String, version: Int) extends SSymbol(s) with Ordered[SName] {
  def compare(that: SName): Int = that match {
    case SName(s2, v2) => {
      val cmpString = s compare s2
      if (cmpString != 0)
        cmpString
      else
        version compare v2
    }
  }

  override def toString =
    if (version == 0) {
      string
    } else {
      if (SExp.shouldNamesBeSymbols)
        s + "$" + version
      else
        "#name[" + string + " " + version + "]"
    }

  def toList = throw new Exception("Cannot convert symbol to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isList = false

  def isPair = false

  def isNull = false

  def isSymbol = true

  def isName = true

  override def hashCode: Int = s.hashCode() * 10 + version

  override def equals(a: Any) = a match {
    case SName(s2, v2) => (s equals s2) && (version == v2)
    case _ => false
  }
}

case object SNil extends SExp {
  override def toString = "()"

  def toList = List()

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isList = true

  def isPair = false

  def isNull = true

  def isName = false

  def isSymbol = false
}

final case class :+:(var car: SExp, var cdr: SExp) extends SExp {
  override def toString = this.toDottedList match {
    case (l, SNil) => "(" + (l mkString " ") + ")"
    case (l, end) => "(" + ((l mkString " ") + " . " + end) + ")"
  }

  def toList = car :: cdr.toList

  def toDottedList: (List[SExp], SExp) = {
    val (lst, end) = cdr.toDottedList
    return (car :: lst, end)
  }

  def isKeyword = false

  def isInteger = false

  def isList = cdr.isList

  def isPair = true

  def isNull = false

  def isName = false

  def isSymbol = false
}


object SList {
  def apply(sx: SExp*): SExp =
    SExp(sx.toList)

  def unapplySeq(sx: SExp): Option[List[SExp]] = {
    if (sx isList)
      Some(sx toList)
    else
      None
  }
}


object SKeyword {

  private val keywordTable = scala.collection.mutable.HashMap[String, SKeyword]()

  def from(string: String): SKeyword = {
    (keywordTable get string) match {
      case Some(kw) => kw
      case None => {
        val kw = SKeyword(string)
        keywordTable(string) = kw
        kw
      }
    }
  }
}


object SName {

  private val nameTable = scala.collection.mutable.HashMap[String, SName]()
  private val maxTable = scala.collection.mutable.HashMap[String, SName]()

  def from(string: String): SName = {
    (nameTable get string) match {
      case Some(name) => name
      case None => {
        val name = SName(string, 0)
        nameTable(string) = name
        name
      }
    }
  }

  def from(symbol: SSymbol): SName = {
    from(symbol.string)
  }

  def gensym(string: String): SName = {
    (maxTable get string) match {
      case Some(SName(_, v)) => {
        val name = SName(string, v + 1)
        maxTable(string) = name
        name
      }
      case None => {
        val name = SName(string, 1)
        maxTable(string) = name
        name
      }
    }
  }

  def gensym(symbol: SSymbol): SName = {
    gensym(symbol.string)
  }
}


object CommonSSymbols {

  val SQuote = SName.from("quote")
  val SQuasiquote = SName.from("quasiquote")
  val SUnquote = SName.from("unquote")
  val SUnquoteSplicing = SName.from("unquote-splicing")

  val SCons = SName.from("cons")
  val SListSym = SName.from("list")
  val SAppend = SName.from("append")
  val SCar = SName.from("car")
  val SCdr = SName.from("cdr")
  val STypeP = SName.from("type?")

  val SDefine = SName.from("define")

  val SDefineStruct = SName.from("define-struct")
  val SMakeStruct = SName.from("make-struct")
  val SStructGet = SName.from("struct-get")
  val SStructSet = SName.from("struct-set!")

  val SLambda = SName.from("lambda")

  val SLet = SName.from("let")
  val SLetStar = SName.from("let*")
  val SLetRec = SName.from("letrec")

  val SSetBang = SName.from("set!")
  val SBegin = SName.from("begin")
  val SVoid = SName.from("void")

  val SIf = SName.from("if")
  val SCond = SName.from("cond")
  val SCase = SName.from("cond")
  val SElse = SName.from("else")
  val SRightArrow = SName.from("=>")
  val SAnd = SName.from("and")
  val SOr = SName.from("or")

  val SValues = SName.from("values")
  val SLetValues = SName.from("let-values")
}