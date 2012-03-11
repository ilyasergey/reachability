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

package org.ucombinator.scheme.cfa.mcfa

import scala.collection.immutable.TreeMap

import org.ucombinator.scheme.transform._
import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.parsing.RnRSParser
import org.ucombinator.util.CFAOptions


object RunOldMCFA {

  def main(args: Array[String]) {

    val opts = CFAOptions.parse(args)

    if (opts.fileName == null) {
      System.err.println("Please specify a filename.")
      return
    }

    val filename = opts.fileName
    System.err.print("Parsing s-expressions...")
    val sexps = SExp.parseAllIn(filename)
    System.err.println("done")

    System.err.print("Bulding AST...")
    val ast = RnRSParser(sexps)
    System.err.println("done")

    System.err.print("A-normalizing...")
    val anastP: Program = (new ANormalizer).apply(ast)
    val anast: Exp = ANormalizer(ast)
    System.err.println("done")

    System.err.print("CPS-converting...")
    val cpastP: Program = (new CPSConverter).apply(anastP)
    val cpast: Exp = CPSConverter(anast)
    System.err.println("done")

    System.out.println("Input program:")
    System.out.println(ast)
    System.out.println("\n")

    System.out.println("ANF program:")
    System.out.println(anastP)
    System.out.println("\n")

    System.out.println("CPS program:")
    System.out.println(cpastP)
    System.out.println("\n")

    System.out.println("Free variables:")
    System.out.println(cpast.free)
    System.out.println("\n")

    opts.analysis match {
      case "full" => {
        val CFA = new KCFA_CPS(cpastP, new MapBEnv(TreeMap()), KTime(List()), new MapStore(), new SortedSetD())
        CFA.printStates = opts.printStates
        CFA.k = opts.k
        CFA.runWithGlobalSharp(filename)
        val sharp = CFA.globalSharp
        val store = sharp.asInstanceOf[StoreSharp].store
        ()
      }
      case "flat" => {
        val CFA = new MCFA_CPS(cpast, new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
        CFA.printStates = opts.printStates
        CFA.k = opts.k
        CFA.m = opts.m
        CFA.flatPolicy = opts.flatPolicy
        CFA.runWithGlobalSharp(filename)
        val sharp = CFA.globalSharp
        val store = sharp.asInstanceOf[StoreSharp].store
        ()
      }
    }

    ()
  }

}
