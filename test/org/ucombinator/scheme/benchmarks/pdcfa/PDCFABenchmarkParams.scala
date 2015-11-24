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
