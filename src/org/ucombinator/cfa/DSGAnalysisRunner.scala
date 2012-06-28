package org.ucombinator.cfa

import org.ucombinator.dsg.DSGMachinery
import tools.nsc.io.Directory
import org.ucombinator.util.{StringUtils, CFAOptions, FancyOutput}

/**
 * An utility trait to process computed DSG graphs
 *
 * @author ilya
 */

trait DSGAnalysisRunner {
  self: FancyOutput with DSGMachinery =>

  import org.ucombinator.util.StringUtils._

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
        buf.append(truncateIfLong(g.toString, 100))
        buf.append("\"]")
      }

      buf.append(";\n")
      list = buf.toString :: list
    }

    buffer.append(list.distinct.mkString(""))
    buffer.append("}\n")

    buffer.toString
  }




}
