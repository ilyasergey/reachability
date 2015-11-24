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
