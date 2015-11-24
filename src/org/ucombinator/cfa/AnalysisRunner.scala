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

import tools.nsc.io.Directory
import org.ucombinator.util._

/**
 * @author ilya
 */

abstract class AnalysisRunner(opts: CFAOptions) extends FancyOutput {

  type ControlState

  def k = opts.k

  def isDummy = opts.dummy

  def simplify = opts.simplifyGraph

  lazy val isVerbose = opts.verbose

  lazy val progressPrefix = ("[" + StringUtils.trimFileName(opts.fileName) + ", "
    + getAnalysisKind(opts) + "]")

  def shouldGC = opts.gc

  def dumpDSG = opts.dumpGraph

  def printGCDebug = opts.gcDebug

  def interrupt = opts.interrupt

  def interruptAfter = opts.interruptAfter

  /**
   * Pretty-print analysis type
   */
  def getAnalysisKind(opts: CFAOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-CFA"
      case AnalysisType.PDCFA => "-PDCFA"
    }

    val analysis = if (opts.dummy) {
      "dummy"
    } else {
      opts.k
    }
    val withGc = if (opts.gc) "-gc" else ""
    analysis + cfa + withGc
  }

  def dumpStatistics(opts: CFAOptions, stat: CFAStatistics): String = {
    import java.io._

    val CFAStatistics(time, size, vars, singletons, states, edges, interrupted) = stat

    val buffer = new StringBuffer()
    buffer.append("Expressions: " + size + "\n")
    buffer.append("Control states: " + states + "\n")
    buffer.append("Transitions / DSG edges: " + edges + "\n")
    buffer.append("Total amount of variables: " + vars + "\n")
    buffer.append("Singletons: " + singletons + "\n")
    buffer.append("Analysis run for: " + time + " milliseconds\n")
    if (interrupted) {
      buffer.append("Interrupted after " + opts.interruptAfter + " states.")
    }

    if (isVerbose) {
      println(buffer.toString)
    }

    if (opts.dumpStatistics) {
      val statDir = new Directory(new File(statisticsDirName))
      if (!statDir.exists) {
        statDir.createDirectory(force = true)
        statDir.createFile(failIfExists = false)
      }

      val subfolderPath = statisticsDirName + File.separator + StringUtils.trimFileName(opts.fileName)
      val subfolder = new Directory(new File(subfolderPath))
      if (!subfolder.exists) {
        subfolder.createDirectory(force = true)
        subfolder.createFile(failIfExists = false)
      }


      val path = subfolderPath + File.separator + getStatisticsDumpFileName(opts)
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      val writer = new FileWriter(file)

      writer.write(buffer.toString)
      writer.close()

      println("Statistics dumped into: " + path)

      path
    } else ""
  }


}
