package org.ucombinator.cfa

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
