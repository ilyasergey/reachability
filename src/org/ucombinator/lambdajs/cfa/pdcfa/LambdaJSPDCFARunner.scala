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

  type Addr = (Option[Var], List[PotentialRedex])

  type Term = Exp

  override type Kont = List[Frame]

  def canHaveSwitchFrames = true

  override def shouldGC = false

  override def printGCDebug = false


  override def simplify = true

  def runPDCFA(program: Term) {
    // todo implement me!
    val sizeExp = 0

    val firstTime = (new java.util.Date()).getTime

    val resultDSG = evaluateDSG(program)

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
    }

    if (isVerbose) {
      val res = prettyPrintDSG(resultDSG)
      println()
      if (!simplify && res.contains("Final")) {
        if (isVerbose) {
          println("Has final state.\n")
        }
      } else if (!simplify) {
        println("Warning: no final state!\n")
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


  def alloc(s: ControlState) = s match {
    case Apply(_, pr) =>
      k match {
        case 0 => (None, List(pr))
        case 1 => (None, List(pr))
        case _ => throw new Exception("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => throw new Exception("Allocation in a wrong state: " + s + "")
  }

  def alloc(s: ControlState, x: Var) = s match {
    case Apply(_, pr) =>
      if (isDummy) {
        (Some(Var("SingleAddr", 0)), Nil)
      } else k match {
        case 0 => (Some(x), Nil)
        case 1 => (Some(x), List(pr))
        case _ => throw new Exception("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => throw new Exception("Allocation in a wrong state: " + s + "")
  }


}
