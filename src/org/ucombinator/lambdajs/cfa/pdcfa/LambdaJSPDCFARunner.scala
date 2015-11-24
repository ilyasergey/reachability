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

package org.ucombinator.lambdajs.cfa.pdcfa

import org.ucombinator.util.CFAOptions
import org.ucombinator.lambdajs.cfa.LambdaJSCFARunner
import org.ucombinator.dsg.DSGMachinery
import org.ucombinator.cfa.{CFAStatistics, DSGAnalysisRunner}
import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

class LambdaJSPDCFARunner(opts: CFAOptions) extends LambdaJSCFARunner(opts) with DSGMachinery
with DSGAnalysisRunner with LambdaJSGarbageCollector {

  import LJSyntax._

  type Addr = (Option[Var], List[Int])

  type Term = Exp
  override type Kont = List[Frame]

  override type SharedStore = Store

  def isStoreSensitive(s: ControlState) = s match {
    case Apply(PR_VAR(_, _)) | Apply(PR_DEREF(_, _)) => true
    case _ => false
  }

  def canHaveSwitchFrames = true

  override def shouldGC = false

  override def printGCDebug = false

  def runPDCFA(program: Term) {
    // todo implement me!
    val sizeExp = 0

    val firstTime = (new java.util.Date()).getTime

    val (resultDSG, resultStore) = evaluateDSG(program)

    val secondTime = (new java.util.Date()).getTime
    val delta = secondTime - firstTime

    println()
    println("The analysis has taken " + (
      if (delta / 1000 < 1) "less than one second."
      else if (delta / 1000 == 1) "1 second."
      else delta / 1000 + " seconds."))



    if (isVerbose) {
      println()
      println("Dyck State Graph computed.")
      println(resultStore)
    }

    if (isVerbose) {
      val res = prettyPrintDSG(resultDSG)
      println()

      for (s <- resultDSG.nodes) {
        s match {
          case PFinal(v) =>
            println("Has final state.\n" + "PFinal(" + v.toString + ")")
            println
          case PError(msg) =>
            println("Has error state.\n" + msg)
            println
          case _ =>
        }
      }
    }

    println()
    println("Computing statistics")
    println()
    // val (allVars, singletons) = computeSingletons(resultDSG.nodes, program)
    val interrupted = interrupt && resultDSG.nodes.size > interruptAfter

    dumpStatistics(opts, CFAStatistics(delta, sizeExp, 0, 0, resultDSG.nodes.size, resultDSG.edges.size, interrupted))

    if (dumpDSG) {
      println()
      println("Writing Dyck State Graph")
      println()
      val path = dumpDSGGraph(opts, resultDSG)
      println("Dyck State Graph dumped into " + path)

    }
  }

  var counter: Int = 0

  def next(): List[Int] = {
    val result = counter
    counter = counter + 1
    List(result % 20)
  }


  def alloc(s: ControlState) = {
    s match {
      case Apply(pr) =>
        k match {
          case 0 => (None, List(pr.hashCode()))
          case 1 => (None, List(pr.hashCode()))
          case _ => throw new Exception("Analysis not implemented for k greater than 1 (" + k + ")")
        }
      case _ => throw new Exception("Allocation in a wrong state: " + s + "")
    }
  }

  def alloc(s: ControlState, x: Var) = {
    s match {
      case Apply(pr) =>
        if (isDummy) {
          (Some(Var("SingleAddr", 0)), Nil)
        } else k match {
          case 0 => (Some(x), Nil)
          case 1 => (Some(x), List(pr.hashCode()))
          case _ => throw new Exception("Analysis not implemented for k greater than 1 (" + k + ")")
        }
      case _ => throw new Exception("Allocation in a wrong state: " + s + "")
    }
  }

}
