package org.ucombinator.lambdajs.cfa

import org.ucombinator.cfa.AnalysisRunner
import org.ucombinator.util.{FancyOutput, CFAOptions}

/**
 * @author ilya
 */

abstract class LambdaJSCFARunner(opts: CFAOptions) extends AnalysisRunner(opts) with JAM with FancyOutput with StoreInterface {

  import org.ucombinator.util.StringUtils._

  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]): String = {
    val result: String = if (simplify) {
      map.get(state) match {
        case Some(n) => n.toString
        case None => {
          throw new Exception("Index not found for state " + state.toString)
        }
      }
    } else state match {
      case Eval(clo) => "Eval(" + truncateIfLong(clo.toString, 100) + ")" + "\\n"
      case Apply(pr) => "Apply(" + truncateIfLong(pr.toString, 100) + ")" + "\\n"
      case Cont(v) => "Cont(" + truncateIfLong(v.toString, 100) + ")" + "\\n"

      case PFinal(v) => "Final(" + truncateIfLong(v.toString, 200) + ")"
      case PError(msg) => "ErrorState[" + msg + "]"
      case PSwitch(s1, s2, f1, f2) => "SwitchState[" + map.get(state).get + "]"
    }
    result
  }

}
