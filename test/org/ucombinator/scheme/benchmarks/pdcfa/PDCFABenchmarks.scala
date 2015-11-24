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
