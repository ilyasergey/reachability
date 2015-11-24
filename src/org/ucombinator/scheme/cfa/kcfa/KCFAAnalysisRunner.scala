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

package org.ucombinator.scheme.cfa.kcfa

import org.ucombinator.scheme.syntax._
import tools.nsc.io.Directory
import org.ucombinator.util._
import org.ucombinator.scheme.transform.ANormalizer
import org.ucombinator.scheme.cfa.SchemeCFARunner
import org.ucombinator.cfa.CFAStatistics

/**
 * @author ilya
 */

class KCFAAnalysisRunner(opts: CFAOptions) extends SchemeCFARunner(opts) with PointerCESKMachinery with KCFAGarbageCollector
with FancyOutput {

  def runKCFA(opts: CFAOptions, anast: Exp) {
    val sizeExp = ANormalizer.size(anast)


    val firstTime = (new java.util.Date()).getTime

    val (resultEdges, resultConfs): (Set[(Conf, Conf)], Set[Conf]) =
      evaluateKCFA(anast)

    val secondTime = (new java.util.Date()).getTime
    val delta = (secondTime - firstTime)

    println()
    println("The analysis has taken " + (
      if (delta / 1000 < 1) "less than one second."
      else if (delta / 1000 == 1) "1 second."
      else delta / 1000 + " seconds."))



    if (opts.verbose) {
      println()
      println("Finished. Computed states: " + resultConfs.size)
    }

    if (!opts.simplifyGraph &&
      resultConfs.toString().contains("Final")) {
      if (opts.verbose) {
        println("Has final state.\n")
      }
    } else if (!opts.simplifyGraph) {
      println("Warning: no final state!\n")
    }

    println()
    println("Computing statistics")
    println()
    val controlStates: Set[ControlState] = resultConfs.map(_._1)

    var stateCounter = 0
    val map: Map[ControlState, Int] = controlStates.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap.asInstanceOf[Map[ControlState, Int]]


    val intNodes: Set[Int] = map.values.toSet
    val intEdges: Set[(Int, Int)] = resultEdges.flatMap[(Int, Int), Set[(Int, Int)]]{
      case (c, c1) => if (map.isDefinedAt(c._1) && map.isDefinedAt(c1._1)){
        Set((map.apply(c._1), map.apply(c1._1)))
      } else Set.empty
    }


    val (allVars, singletons) = computeSingletons(controlStates, anast)
    val interrupted = opts.interrupt && resultConfs.size > opts.interruptAfter

    dumpStatistics(opts, CFAStatistics(delta, sizeExp, allVars.size,
      singletons.size, intNodes.size, intEdges.size, interrupted))

    if (interrupt) {
      println ("Interrupted after " + resultConfs.size + " states visited")
    }


    if (opts.dumpGraph) {
      println()
      println("Writing State Transition Graph")
      println()
      val path = dumpTransitionGraph(opts, resultConfs, resultEdges)
      println("Transition Graph dumped into " + path)
    }
  }

  /**
   * Prints transition graph according to the passed parameters
   */
  def prettyPrintTransitions(states: Set[Conf], edges: Set[(Conf, Conf)]): String = {

    val controlStates: Set[ControlState] = states.map(x => x._1)
    var stateCounter = 0
    val map: Map[ControlState, Int] = controlStates.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap.asInstanceOf[Map[ControlState, Int]]

    println("Control states: " + controlStates.size)

    val buffer = new StringBuffer
    buffer.append("digraph BST {\nsize=\"6,4\" ; ratio = fill;\n ")

    var list: List[String] = List()
    for (edge <- edges: Set[(Conf, Conf)]) {
      val buf = new StringBuffer()
      val (s, _) = edge._1
      val (s1, _) = edge._2

      buf.append("\"" + prettyPrintState(s, map) + "\"")
      buf.append(" -> ")
      buf.append("\"" + prettyPrintState(s1, map) + "\"")

      buf.append(";\n")
      list = buf.toString :: list
    }

    buffer.append(list.distinct.mkString(""))
    buffer.append("}\n")

    buffer.toString
  }

  def dumpTransitionGraph(opts: CFAOptions, states: Set[Conf], edges: Set[(Conf, Conf)]): String = {
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
    writer.write(prettyPrintTransitions(states, edges))
    writer.close()
    path
  }


}
