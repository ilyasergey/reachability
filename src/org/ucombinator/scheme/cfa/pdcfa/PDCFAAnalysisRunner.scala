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

package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.scheme.cfa.cesk.AnalysisRunner
import tools.nsc.io.Directory
import org.ucombinator.util._
import org.ucombinator.scheme.syntax.{Exp, SName}
import org.ucombinator.scheme.cfa.CFAStatistics
import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.dsg.DSGMachinery


/**
 * @author ilya
 */

class PDCFAAnalysisRunner(opts: CFAOptions) extends AnalysisRunner(opts) with StackCESKMachinery
with PDCFAGarbageCollector with IPDSMachinery with DSGMachinery with FancyOutput {

  type Term = Exp

  override type Kont = List[Frame]

  def canHaveSwitchFrames = false

  def step(q: ControlState, k: Kont, frames: Kont) = stepIPDS(q, k, frames)

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

  def dumpDSGGraph(opts: CFAOptions, resultDSG: DSG): String = {

    import java.io._

    val graphs = new Directory(new File(graphsDirName))
    if (!graphs.exists) {
      graphs.createDirectory(force = true)
      graphs.createFile(failIfExists = false)
    }

    val subfolderPath = graphsDirName + File.separator + StringUtils.trimFileName(opts.fileName)
    val subfolder = new Directory(new File(subfolderPath))
    if (!subfolder.exists) {
      subfolder.createDirectory(force = true)
      subfolder.createFile(failIfExists = false)
    }


    val path = subfolderPath + File.separator + getGraphDumpFileName(opts)
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)
    writer.write(prettyPrintDSG(resultDSG))
    writer.close()
    path
  }


  /**
   * Prints DSG according to the passed parameters
   */
  def prettyPrintDSG(dsg: DSG): String = {

    val edges = dsg.edges
    val states: Set[S] = dsg.nodes.asInstanceOf[Set[S]]

    var stateCounter = 0
    val map: Map[S, Int] = states.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap

    val buffer = new StringBuffer
    buffer.append("digraph BST {\nsize=\"6,4\" \n ")

    var list: List[String] = List()
    for (Edge(s, g, s1) <- edges if s != s1) {
      val buf = new StringBuffer()
      buf.append("\"" + prettyPrintState(s, map) + "\"")
      buf.append(" -> ")
      buf.append("\"" + prettyPrintState(s1, map) + "\"")

      if (!simplify) {
        buf.append(" [label=\"")
        buf.append(g.toString)
        buf.append("\"]")
      }

      buf.append(";\n")
      list = buf.toString :: list
    }

    buffer.append(list.distinct.mkString(""))
    buffer.append("}\n")

    buffer.toString
  }

  /**
   * Run Pushdown Control Flow Analysis
   * @param opts analysis options
   * @param anast inital expresion in ANF
   */
  def runPDCFA(opts: CFAOptions, anast: Term) {
    val sizeExp = ANormalizer.size(anast)

    val firstTime = (new java.util.Date()).getTime

    val resultDSG = evaluateDSG(anast)

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
    val (allVars, singletons) = computeSingletons(resultDSG.nodes, anast)
    val interrupted = opts.interrupt && resultDSG.nodes.size > opts.interruptAfter

    dumpStatistics(opts, CFAStatistics(delta, sizeExp, allVars.size,
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
