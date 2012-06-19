package org.ucombinator.lambdajs.cfa

import org.ucombinator.cfa.AnalysisRunner
import org.ucombinator.util.{FancyOutput, CFAOptions}
import org.ucombinator.dsg.DSGMachinery

/**
 * @author ilya
 */

abstract class LambdaJSCFARunner(opts: CFAOptions) extends AnalysisRunner(opts) with JAM with FancyOutput with StoreInterface {

  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]): String = {
    val result: String = if (simplify) {
      map.get(state) match {
        case Some(n) => n.toString
        case None => {
          throw new Exception("Index not found for state " + state.toString)
        }
      }
    } else state match {
      case Eval(st, clo) => (clo.toString + "\\n" + "  store hash = " + st.hashCode().toString)
      case Apply(st, pr) => (pr.toString + "\\n" + "  store hash = " + st.hashCode().toString)
      case Cont(st, v) => (v.toString + "\\n" + "  store hash = " + st.hashCode().toString)

      case PFinal(v, _) => "Final(" + v.toString + ")"
      case PError(msg) => "ErrorState[" + msg + "]"
      case PSwitch(s1, s2, f1, f2) => "SwitchState[" + map.get(state).get + "]"
    }
    result
  }


}
