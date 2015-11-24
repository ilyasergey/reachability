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

package org.ucombinator.cfa

import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.scheme.parsing.RnRSParser
import org.ucombinator.util._
import org.ucombinator.scheme.cfa.kcfa.KCFAAnalysisRunner
import org.ucombinator.scheme.cfa.pdcfa.PDCFAAnalysisRunner
import AnalysisType._
import org.ucombinator.lambdajs.cfa.pdcfa.LambdaJSPDCFARunner
import org.ucombinator.lambdajs.parsing.{LambdaJSParser, LambdaJSSlowParser}

/**
 * @author ilya
 */

object RunCFA {

  val version: String = "20120619"
  val versionString: String = "    Version " + version + "\n"

  val helpMessage = " GenericCFA - a runner for k-CFA and Push-down k-CFA with optional Abstract Garbage Collection \n" +
    versionString +
    """
    Usage (for a prebuilt jar with Scala SDK included):

    java -jar GenericCFA.jar [--lang lang][--pdcfa | --kcfa] [--k k] [--gc] [--verbose] [--dump-graph] [--dump-statistics] [--simple-graph] [--interrupt-after n] [--help] filePath

    where

    --lang l               Target language (default = scheme)
                             js        -- LambdaJS
                             scheme    -- Scheme
    --pdcfa                run Pushdown k-CFA (run by default)
    --kcfa                 run classic k-CFA
    --k k                  "k-degree" of the analysis, by default k = 0, only k = 0,1 are supported so far
    --gc                   switch on abstract Garbage Collector (default = off)
    --dump-graph           dump Transition/Dyck State Graphs into a GraphViz file ./graphs/filename/graph-(analysis-type).gv
    --dump-statisitcs      dump analysis statistics into ./statistics/filename/stat-(analysis-type).txt
    --simple-graph         if the graph is dumped, distinct natural numbers are displayed on its nodes instead of actual configurations
    --interrupt-after n    interrupts the analysis after n states computed (default = off)
    --help                 print this message
    --verbose              print additional information on the analysis and results
    filePath               path to a file to be analysed
    """


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

    if (opts.lang == "js") {
      processJS(opts)
    } else {
      processScheme(opts)
    }
  }

  def processJS(opts: CFAOptions) {

    import org.ucombinator.lambdajs.syntax.LJSyntax._

    opts.analysisType match {
      case KCFA => {
        println("Standard k-CFA for LambdaJS is not supported")
        return
      }
      case PDCFA => {
        val runner = new LambdaJSPDCFARunner(opts)

        val filename = opts.fileName
        if (opts.verbose) {
          System.err.println("Parsing LambdaJS program...")
        }

        val parser = new LambdaJSParser

        val firstTime = (new java.util.Date()).getTime
        val program: Exp = parser.parseAllIn(filename)
        val secondTime = (new java.util.Date()).getTime
        val delta = secondTime - firstTime


        System.err.println("Program is parsed in " + delta + " milliseconds")

        if (opts.verbose) {
          System.err.println("Input program:")
          System.out.println(program)
          System.out.println("\n")
        }

        runner.runPDCFA(program)
      }
    }

    // todo implement me
  }


  def processScheme(opts: CFAOptions) {

    import org.ucombinator.scheme.syntax._

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


