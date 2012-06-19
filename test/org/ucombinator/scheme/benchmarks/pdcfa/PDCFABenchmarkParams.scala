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

package org.ucombinator.scheme.benchmarks.pdcfa

import org.ucombinator.cfa.RunCFA

/**
 * @author ilya
 */

trait PDCFABenchmarkParams {

  val args_0_KCFA_GC = Array("--kcfa", "--k", "0", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_0_KCFA = Array("--kcfa", "--k", "0",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_KCFA_GC = Array("--kcfa", "--k", "1", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_KCFA = Array("--kcfa", "--k", "1",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_0_PDCFA_GC = Array("--pdcfa", "--k", "0", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_0_PDCFA = Array("--pdcfa", "--k", "0",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_PDCFA_GC = Array("--pdcfa", "--k", "1", "--gc",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  val args_1_PDCFA = Array("--pdcfa", "--k", "1",
    "--interrupt-after", "10000", "--simple-graph", "--dump-statistics", "--dump-graph", "--verbose")

  def tryAndRecover(prefixMessage: String, filePath: String, run: () => Unit) {
    println()
    println(prefixMessage + filePath)
    println()

    try {
      run()
    } catch {
      case e: Exception => {
        println(prefixMessage + "failed with the exception:")
        println(e.getMessage)
        System.err.println(e.getStackTraceString)
      }
    }
  }

  def run0CFAs(filePath: String) {

    tryAndRecover("Running 0-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_0_PDCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 0-KCFA-GC for ", filePath,
      (() => RunCFA.main(args_0_KCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 0-PDCFA for ", filePath,
      (() => RunCFA.main(args_0_PDCFA ++ Array(filePath))))

    tryAndRecover("Running 0-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_0_KCFA ++ Array(filePath))))

  }

  def run1CFAs(filePath: String) {

    tryAndRecover("Running 1-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_1_PDCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 1-KCFA-GC for ", filePath,
      (() => RunCFA.main(args_1_KCFA_GC ++ Array(filePath))))

    tryAndRecover("Running 1-PDCFA for ", filePath,
      (() => RunCFA.main(args_1_PDCFA ++ Array(filePath))))

    tryAndRecover("Running 1-PDCFA-GC for ", filePath,
      (() => RunCFA.main(args_1_KCFA ++ Array(filePath))))

  }

  def runForAll(filePath: String) {
    println()
    println("Running benchmars suite for " + filePath)
    println()

    run0CFAs(filePath)
    run1CFAs(filePath)

    println()
    println("Suite is successfully finished for " + filePath)
    println()

  }

}
