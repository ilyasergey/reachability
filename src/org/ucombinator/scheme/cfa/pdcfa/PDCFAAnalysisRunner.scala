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

package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.util._
import org.ucombinator.scheme.syntax.{Exp, SName}
import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.dsg.DSGMachinery
import org.ucombinator.scheme.cfa.SchemeCFARunner
import org.ucombinator.cfa.{DSGAnalysisRunner, CFAStatistics}


/**
 * @author ilya
 */

class PDCFAAnalysisRunner(opts: CFAOptions) extends SchemeCFARunner(opts) with StackCESKMachinery
with PDCFAGarbageCollector with IPDSMachinery with DSGMachinery with DSGAnalysisRunner with FancyOutput {

  type Term = Exp

  type Value = Val

  override type Kont = List[Frame]

  def canHaveSwitchFrames = false

  def isStoreSensitive(s: ControlState) = false

  def step(q: ControlState, k: Kont, frames: Kont, store: SharedStore) = {
    val result = stepIPDS(q, k, frames)
    result.map {
      case (x, y) => (x, y, store)
    }
  }

  def alloc(v: Var, c: Conf): Addr = c match {
    case (PState(e, _, _, _), _) =>
      if (isDummy) {
        (SName("SingleAddr", 0), Nil)
      } else k match {
        case 0 => (v, Nil)
        case 1 => (v, List(e))
        case _ => throw new StackCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => {
      throw new StackCESKException("Illegal allocation configuartion:\n" + c.toString)
    }
  }

  /**
   * Run Pushdown Control Flow Analysis
   * @param opts analysis options
   * @param anast inital expresion in ANF
   */
  def runPDCFA(opts: CFAOptions, anast: Term) {
    val sizeExp = ANormalizer.size(anast)
    val vars = ANormalizer.vars(anast)

    val firstTime = (new java.util.Date()).getTime

    val (resultDSG, _) = evaluateDSG(anast)

    val secondTime = (new java.util.Date()).getTime
    val delta = secondTime - firstTime

    println()
    println("The analysis has taken " + (
      if (delta / 1000 < 1) "less than one second."
      else if (delta / 1000 == 1) "1 second."
      else delta / 1000 + " seconds."))



    if (opts.verbose) {
      println()
      println("Dyck State Graph computed.")
    }

    if (opts.verbose) {
      val res = prettyPrintDSG(resultDSG)
      println()
      if (!opts.simplifyGraph && res.contains("Final")) {
        if (opts.verbose) {
          println("Has final state.\n")
        }
      } else if (!opts.simplifyGraph) {
        println("Warning: no final state!\n")
      }
    }

    println()
    println("Computing statistics")
    println()
    val (_, singletons) = computeSingletons(resultDSG.nodes, anast)
    val interrupted = delta >= 60 * 1000 * 30

    dumpStatistics(opts, CFAStatistics(delta, sizeExp, vars,
      singletons.size, resultDSG.nodes.size, resultDSG.edges.size, interrupted))

    if (opts.dumpGraph) {
      println()
      println("Writing Dyck State Graph")
      println()
      val path = dumpDSGGraph(opts, resultDSG)
      println("Dyck State Graph dumped into " + path)

    }
  }

}
