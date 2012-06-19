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
