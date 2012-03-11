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


/**
 * Benchmark suites
 *
 * @author ilya
 */

// 1
object Eta extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/eta.scm"
    runForAll(path)
  }
}

// 2
object Midtgaard2009 extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/midtgaard-icfp09.scm"
    runForAll(path)
  }
}

// 3
object DoubleLoop extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/double-loop.scm"
    runForAll(path)
  }
}

// 4
object Diamond extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/diamond/diamond.scm"
    runForAll(path)
  }
}

// 5
object KcfaWorstCase extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/kcfa-worst-case.scm"
    runForAll(path)
  }
}

// 6
object KcfaEvenWorse extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/kcfa-even-worse.scm"
    runForAll(path)
  }
}

// 7
object Fermat extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/fermat.scm"
    runForAll(path)
  }
}

// 8
object RSA extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/rsa.scm"
    runForAll(path)
  }
}

// 9
object SatBrute extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/sat-brute.scm"
    runForAll(path)
  }
}

// 10
object RegexDerivative extends PDCFABenchmarkParams {
  def main(args: Array[String]) {
    val path = "benchmarks/gcfa2/regex-derivative.scm"
    runForAll(path)
  }
}

object PDCFABenchmarks
