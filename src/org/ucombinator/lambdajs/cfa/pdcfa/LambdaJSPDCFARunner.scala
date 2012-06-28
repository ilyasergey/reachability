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
    case Apply(PR_VAR(_, _)) | Apply(PR_DEREF(_)) => true
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
          case PError(msg) =>
            println("Has error state.\n" + msg)
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


  def alloc(s: ControlState) = s match {
    case Apply(pr) =>
      k match {
        case 0 => (None, List(pr.hashCode()))
        case 1 => (None, List(pr.hashCode()))
        case _ => throw new Exception("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => throw new Exception("Allocation in a wrong state: " + s + "")
  }

  def alloc(s: ControlState, x: Var) = s match {
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
