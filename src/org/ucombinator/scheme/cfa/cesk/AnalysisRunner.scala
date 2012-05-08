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

package org.ucombinator.scheme.cfa.cesk

import org.ucombinator.scheme.syntax._
import tools.nsc.io.Directory
import org.ucombinator.util._
import org.ucombinator.scheme.cfa.CFAStatistics

/**
 * @author ilya
 */

abstract class AnalysisRunner(opts: CFAOptions) extends StateSpace {

  def k = opts.k

  def isDummy = opts.dummy

  def simplify = opts.simplifyGraph

  lazy val isVerbose = opts.verbose

  lazy val progressPrefix = ("[" + StringUtils.trimFileName(opts.fileName) + ", "
    + getAnalysisKind(opts) + "]")

  def shouldGC = opts.gc

  def printGCDebug = opts.gcDebug

  def interrupt = opts.interrupt

  def interruptAfter = opts.interruptAfter


  val graphsDirName: String = "graphs"
  val statisticsDirName: String = "statistics"


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


  def computeSingletons(states: Set[ControlState], exp: Exp): (Set[Var], Set[Var]) = {

    val goodStates: Set[ControlState] = states.filter({
      case PState(_, _, _, _) => true
      case _ => false
    })

    val allEnvs: Set[Env] = goodStates.map {
      case PState(_, rho, _, _) => rho
    }

    // all variables
    val allVars: Set[Var] = allEnvs.map(rho => rho.keys).flatten

    val varAddrMap: Var :-> Set[Addr] = Map.empty


    val globalVarAddrMap: Var :-> Set[Addr] =
      allEnvs.foldLeft(varAddrMap) {
        // for all environments
        case (vaMap, env) => {
          val augmentedEnv: Var :-> Set[Addr] =
            env.map(vAddrs => {
              val (v: Var, addr: Addr) = vAddrs
              // already found addresses for variable v
              val foundAddr: Set[Addr] = vaMap.getOrElse(v, Set())
              // addresses in this environments
              val withNewAddr: Set[Addr] = foundAddr ++ Set(addr)
              (v, withNewAddr)
            })

          // Combine old and new maps together
          vaMap ++ augmentedEnv
        }
      }


    // Now stores..
    val allStores: Set[Store] = goodStates.map {
      case PState(_, _, s, _) => s
    }


    // Mapping variables to values
    var varValMap: Var :-> Set[Val] = Map.empty

    for {
      s <- allStores
      (v, ax) <- globalVarAddrMap
    } {
      val newValues: Set[Val] = ax.map(addr => s.getOrElse(addr, Set())).flatten[Val]
      val oldValues: Set[Val] = varValMap.getOrElse(v, Set())
      val newVarValMap: Var :-> Set[Val] = varValMap + ((v, newValues ++ oldValues))
      varValMap = newVarValMap
    }


    val singletonVars: Set[Var] = varValMap.filter {
      case (v, ax) => ax.size == 1
    }.toSet[(Var, Set[Val])].map {
      case (v, ax) => v
    }

    // (vars-total, vars-singletons)
    (allVars, singletonVars)


  }

  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]) = {
    val result: String = if (simplify) {
      map.get(state) match {
        case Some(n) => n.toString
        case None => {
          throw new Exception("Index not found for state " + state.toString)
        }
      }
    } else state match {
      case p@PState(e, rho, st, k) => {
        (e.toString +
          "\\n" + " Env = " + rho.toString +
          "\\n" + "  store hash = " + st.hashCode().toString +
          "\\n" + k.toString)
      }
      case PFinal(v) => "Final(" + v.toString + ")"
      case ErrorState(_, _) => "ErrorState"
    }
    result
  }


}
