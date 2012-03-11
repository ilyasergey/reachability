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

package org.ucombinator.scheme.cfa

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.scheme.parsing.RnRSParser
import org.ucombinator.util._
import org.ucombinator.scheme.cfa.kcfa.KCFAAnalysisRunner
import org.ucombinator.scheme.cfa.pdcfa.PDCFAAnalysisRunner
import AnalysisType._

/**
 * @author ilya
 *
 * Standard DSG Runner class
 * Takes a parameter k for polyvariance
 */

object RunCFA {

  val version: String = "20120310"
  val versionString: String = "    Version " + version + "\n"

  val helpMessage = ("  GenericCFAForScheme - a runner for k-CFA and Push-down k-CFA with optional Abstract Garbage Collection \n" +
    versionString +
    """
    Usage (for a prebuilt jar with Scala SDK included):

    java -jar GenericCFAForScheme.jar [--pdcfa | --kcfa] [--k k] [--gc] [--verbose] [--dump-graph] [--dump-statistics] [--simple-graph] [--interrupt-after n] [--help] filePath

    where

    --pdcfa                run Pushdown k-CFA (run by default)
    --kcfa                 run classic k-CFA
    --k k                  "k-degree" of the analysis, by default k = 0, only k = 0,1 are supported so far
    --gc                   switch on abstract Garbage Collector (default = off)
    --dump-graph           dump Transition/Dyck State Graphs into a GraphViz file ./graphs/filename/graph-(analysis-type).gv
    --dump-statisitcs      dump analysis statistics into ./statistics/filename/stat-(analysis-type).txt
    --simple-graph         if the graph is dumped, distinct natural numbers are displayed on its nodes instead of actual configurations
    --interrupt-after n    interrupts the analysis after n states computed (default = off)
    --help                 print this message
    --verbose              print additiona information on the analysis and results
    filePath               path to a Scheme file to be analysed
    """)


  def main(args: Array[String]) {

    val opts = CFAOptions.parse(args)

    if (args.size == 0 || opts.help) {
      println(helpMessage)
      return
    }

    if (opts.fileName == null) {
      println()
      System.err.println("Please, specify a filename to process")
      println()
      println(helpMessage)
      return
    }

    val filename = opts.fileName
    if (opts.verbose) {
      System.err.print("Parsing s-expressions...")
    }
    val sexps = SExp.parseAllIn(filename)
    if (opts.verbose) {
      System.err.println("done")
    }

    if (opts.verbose) {
      System.err.print("Bulding AST...")
    }
    val ast = RnRSParser(sexps)
    if (opts.verbose) {
      System.err.println("done")
    }

    if (opts.verbose) {
      System.err.print("A-normalizing...")
    }
    val anast: Exp = ANormalizer(ast)
    if (opts.verbose) {
      System.err.println("done")
    }

    if (opts.verbose) {
      System.out.println("Input program:")
      System.out.println(ast)
      System.out.println("\n")

      System.out.println("ANF program:")
      System.out.println(anast)
      System.out.println("\n")
    }

    opts.analysisType match {
      case PDCFA => {
        val runner = new PDCFAAnalysisRunner(opts)
        runner.runPDCFA(opts, anast)
      }
      case KCFA => {
        val runner = new KCFAAnalysisRunner(opts)
        runner.runKCFA(opts, anast)
      }
    }

  }

}


