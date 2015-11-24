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

package org.ucombinator.lambdajs.simple

import org.ucombinator.util.CFAOptions
import org.ucombinator.lambdajs.cfa.pdcfa.LambdaJSPDCFARunner
import org.ucombinator.lambdajs.syntax.LJSyntax
import org.ucombinator.lambdajs.parsing.LambdaJSParser

/**
 * @author ilya
 */

object TestPDCFAForLambdaJS {

  import LJSyntax._

  val args_1_PDCFA_GC = Array("--pdcfa", "--k", "1", "--interrupt-after", "10000", "--dump-statistics", "--dump-graph", "--verbose", "id-example")

  // Examples
  val app = Fun(List(Var("f", 1), Var("x", 2)), App(Var("f", 1), List(Var("x", 2)), 3), 0)
  val id = Fun(List(Var("y", 4)), Var("y", 4), 5)

  val example1 = Let(Var("z", 6), EFloat(42.0), App(id, List(Var("z", 6)), 8), 7)
  val example2 = Let(Var("z", 6), EFloat(42.0), App(app, List(id, Var("z", 6)), 8), 7)

  val text3 = """
  (let ((g (lambda (f x) (f x)))
        (z (lambda (y) y)))
        (g z 42.0))
  """

  val text4 = """
  (let ((z 42.0))
       ((lambda (f x) (f x)) (lambda (y) y) z))
  """


  def main(args: Array[String]) {
    val opts = CFAOptions.parse(args_1_PDCFA_GC)
    val runner = new LambdaJSPDCFARunner(opts)


    val example3 = parseExample(text3)
    runner.runPDCFA(example3)
  }

  def parseExample(text: String) = {
    val parser = new LambdaJSParser
    val result = parser.parseText(text)
    result
  }

}
