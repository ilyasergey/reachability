package org.ucombinator.scheme.cfa

import cesk.StateSpace
import org.ucombinator.cfa.AnalysisRunner
import org.ucombinator.scheme.syntax.Exp
import org.ucombinator.util.{FancyOutput, CFAOptions}

/**
 * @author ilya
 */

abstract class SchemeCFARunner(opts: CFAOptions) extends AnalysisRunner(opts) with StateSpace with FancyOutput {

  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]): String = {
    val result: String = if (simplify) {
      map.get(state) match {
        case Some(n) => n.toString
        case None => "..."
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


}
