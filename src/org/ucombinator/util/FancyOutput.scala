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

package org.ucombinator.util

/**
 * @author ilya
 */

trait FancyOutput {

  val graphsDirName: String = "graphs"

  val statisticsDirName: String = "statistics"

  type ControlState

  def isVerbose: Boolean

  def progressPrefix: String

  def shouldGC: Boolean

  def simplify: Boolean

  def printGCDebug: Boolean

  def interrupt: Boolean

  def dumpDSG: Boolean

  def interruptAfter: Int

  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]): String

  /**
   * Get a fancy name dump files
   */
  def getGraphDumpFileName(opts: CFAOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "graph-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    prefix + arity + cfa + gc + ".gv"
  }

  def getStatisticsDumpFileName(opts: CFAOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "stat-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    prefix + arity + cfa + gc + ".txt"
  }



}
