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

/* Imports. */


object AnalysisType extends Enumeration {
  type AnalysisType = Value
  val PDCFA, KCFA = Value
}

import AnalysisType._

class CFAOptions {
  var fileName: String = null;
  var help: Boolean = false;
  var k: Int = 0
  var m: Int = 1
  var printStates = false;
  var flatPolicy = "m"
  var analysis = "flat"
  var analysisType: AnalysisType = PDCFA
  var verbose = false
  var dumpStatistics = false
  var simplifyGraph = false
  var dummy = false
  var gc = false
  var gcDebug = false
  var dumpGraph = false
  var lang = "scheme"

  var interrupt = false
  var interruptAfter = 0
}


object CFAOptions {

  def parse(args: List[String], opts: CFAOptions) {
    args match {
      case List() => {}
      case "--k" :: k :: rest => {
        opts.k = Integer.parseInt(k)
        parse(rest, opts)
      }

      case "--lang" :: lan :: rest => {
        lan match {
          case "js" => opts.lang = "js"
          case _ => opts.lang = "scheme"
        }
        parse(rest, opts)
      }

      case "--help" :: rest => {
        opts.help = true
        parse(rest, opts)
      }

      case "--dummy" :: rest => {
        opts.dummy = true
        parse(rest, opts)
      }

      case "--simple-graph" :: rest => {
        opts.simplifyGraph = true
        parse(rest, opts)
      }

      case "--dump-statistics" :: rest => {
        opts.dumpStatistics = true
        parse(rest, opts)
      }

      case "--kcfa" :: rest => {
        opts.analysisType = KCFA
        parse(rest, opts)
      }

      case "--pdcfa" :: rest => {
        opts.analysisType = PDCFA
        parse(rest, opts)
      }

      case "--gc" :: rest => {
        opts.gc = true
        parse(rest, opts)
      }

      case "--gcDebug" :: rest => {
        opts.gcDebug = true
        parse(rest, opts)
      }

      case "--verbose" :: rest => {
        opts.verbose = true
        parse(rest, opts)
      }

      case "--m" :: m :: rest => {
        opts.m = Integer.parseInt(m)
        parse(rest, opts)
      }

      case "--flat-policy" :: s :: rest => {
        opts.flatPolicy = s
        parse(rest, opts)
      }

      case "--analysis" :: a :: rest => {
        opts.analysis = a
        parse(rest, opts)
      }

      case "--print-states" :: "true" :: rest => {
        opts.printStates = true
        parse(rest, opts)
      }

      case "--print-states" :: "false" :: rest => {
        opts.printStates = false
        parse(rest, opts)
      }

      case "--dump-graph" :: rest => {
        opts.dumpGraph = true
        parse(rest, opts)
      }

      case "--interrupt-after" :: v :: rest => {
        opts.interrupt = true
        opts.interruptAfter = Integer.parseInt(v)
        parse(rest, opts)
      }

      case fileName :: rest => {
        opts.fileName = fileName;
        parse(rest, opts)
      }

    }
  }

  def parse(args: Array[String]): CFAOptions = {
    val opts = new CFAOptions
    parse(args.toList, opts)
    opts
  }

}




