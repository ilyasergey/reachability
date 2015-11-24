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

package org.ucombinator.lambdajs.cfa

import org.ucombinator.dsg._
import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait JAM extends LJFrames with LJSyntax with LJPrimOperators {self: StoreInterface =>

  import LJSyntax._

  /********************************************
   * Partial states
   ********************************************/

  sealed abstract class PState

  case class Eval(clo: Closure) extends PState {
    override def toString = "Eval(" + clo.toString + ")"
  }

  case class Cont(v: Value) extends PState {
    override def toString = "Cont(" + v.toString + ")"
  }

  case class Apply(pr: PotentialRedex) extends PState {
    override def toString = "Apply(" + pr.toString + ")"
  }

  case class PFinal(v: Value) extends PState {
    override def toString = "Final(" + v.toString + ")"
  }

  case class PError(m: String) extends PState

  // An administrative state for immediate pop/push
  case class PSwitch(s1: PState, s2: PState, popped: Frame, pushed: Frame) extends PState {
    override def toString = "SwitchState"
  }

  ///////////////////////////////////////////////////////////////////////

  type Kont = List[Frame]

  type ControlState = PState

  def step(state: ControlState, k: Kont, frames: Kont, store: Store): Set[(StackAction[Frame], ControlState, Store)] = {

    state match {

      // Administrative states
      case p@PError(_) => {
        Set()
      }
      case PFinal(_) => Set()
      case PSwitch(_, _, _, _) => Set()

      // Eval states
      case Eval(clo) => clo match {

        case GroundClo(x@Var(_, _), rho) => withEps(Apply(PR_VAR(x, rho)), store)
        case GroundClo(fun@Fun(_, _, _), rho) => withEps(Cont(FunValue(fun, rho)), store)
        case GroundClo(e, rho) => withEps(Eval(exp2Clo(e, rho)), store)
        case ValueClo(v) => withEps(Cont(v), store)
        case RecordClo((s, c) :: tail) => withPush(Eval(c), RecFrame(List(), s, tail), store)
        case LetClo(x, c, d) => withPush(Eval(c), LetFrame(x, d), store)
        case AppClo(c, ds) => withPush(Eval(c), AppFrame(ds), store)
        case LookupClo(c, d) => withPush(Eval(c), Lookup1Frame(d), store)
        case UpdateClo(c, d, d1) => withPush(Eval(c), Update1Frame(d, d1), store)
        case DelClo(c, d) => withPush(Eval(c), Del1Frame(d), store)
        case AsgnClo(c, d) => withPush(Eval(c), Asgn1Frame(d), store)
        case RefClo(c) => withPush(Eval(c), RefFrame(c), store)
        case DerefClo(c) => withPush(Eval(c), DerefFrame(c), store)
        case IfClo(c, d, d1) => withPush(Eval(c), IfFrame(d, d1), store)
        case SeqClo(c, d) => withPush(Eval(c), SeqFrame(d), store)
        case WhileClo(c, d) => withPush(Eval(c), IfFrame(SeqClo(d, WhileClo(c, d)), ValueClo(UndefValue)), store)
        case LabelledClo(l, c) => withPush(Eval(c), LabFrame(l, c), store)
        case BreakClo(l, c) => withPush(Eval(c), BreakFrame(l, c), store)
        case TryCatchClo(c, x, d) => withPush(Eval(c), TryCatchFrame(x, d), store)
        case TryFinallyClo(c, d) => withPush(Eval(c), TryFinallyFrame(d), store)
        case ThrowClo(c) => withPush(Eval(c), ThrowFrame(c), store)
        case OpClo(op, c :: tail) => withPush(Eval(c), OpFrame(op, List(), tail), store)
        case _ => {
          val msg = "No transition for eval-state\n" + state + "\nand a stack\n" + k
          Set((Eps, error(msg), store))
        }
      }

      case Apply(pr) => pr match {

        case PR_VAR(x, rho) if rho.keySet.contains(x) => {
          for (v <- get(store, rho(x))) yield (Eps, Cont(v), store)
        }
        case PR_LET(x, v, GroundClo(e, rho)) => {
          val a = alloc(state, x)
          withEps(Eval(GroundClo(e, rho + ((x, a)))), put(store, a, Set(v)))
        }
        case PR_APP(FunValue(Fun(xs, e, _), rho), vs) => if (xs.length == vs.length) {
          val as = xs.map(x => alloc(state, x))
          val rho1 = rho ++ xs.zip(as)
          withEps(Eval(GroundClo(e, rho1)), putMany(store, as.zip(vs.map(Set(_)))))
        } else {
          val msg = "Wrong number of arguments in the state\n" + state + "\nand a stack\n" + k
          withEps(error(msg), store)
        }
        case PR_REC_REF(RecValue(entries), sv) => {
          sv match {
            // Fetching any field of a record
            case StringTop => {
              val msg = "Alarm! StringTop is used as a reference for a record in state \n" + state
              System.err.println(msg)
              withEps(error(msg), store)
            }
            // Fetching a particular field
            case s@StringValue(_) => entries.filter {
              // take exact matches and StringTops
              case (si, _) => (si == s) || (si == StringTop)
            } match {
              // something is found
              case l@(h :: _) => l.toSet.map((p: (StringValue, Value)) => (Eps, Cont(p._2), store))
              // nothing is found
              case Nil => {
                withEps(Cont(UndefValue), store)
              }
            }
          }
        }
        case PR_REC_SET(RecValue(entries), sv, rhs, _) => {
          sv match {
            // Fetching any field of a record
            case StringTop => {
              val msg = "Alarm! StringTop is used as a reference for record update in state \n" + state
              System.err.println(msg)
              withEps(error(msg), store)
            }
            // Fetching a particular field
            case s@StringValue(_) => {
              val newEntries = (s, rhs) :: entries.filter {
                case (si, _) => (si != s)
              }
              withEps(Cont(RecValue(newEntries)), store)
            }
          }
        }
        case PR_REC_DEL(RecValue(entries), sv, _) => {
          sv match {
            // Fetching any field of a record
            case StringTop => {
              val msg = "Alarm! StringTop is used as a reference for record delete in state \n" + state
              System.err.println(msg)
              withEps(error(msg), store)
            }
            // Fetching a particular field
            case s@StringValue(_) => {
              val newEntries = entries.filter {
                case (si, _) => (si != s)
              }
              withEps(Cont(RecValue(newEntries)), store)
            }
          }
        }

        case PR_IF(c, tc, ec) => c match {
          case BoolValue(true) => withEps(Eval(tc), store)
          case BoolValue(false) => withEps(Eval(ec), store)
          case _ => Set(
            (Eps, Eval(tc), store),
            (Eps, Eval(ec), store))
        }

        case PR_OP(op, values) => {
          for (result <- delta(op.op, values))
          yield (Eps, Cont(result), store)
        }

        case PR_REF(v, _) => {
          val a = alloc(state)
          withEps(Cont(AddrValue(a)), put(store, a, Set(v)))
        }

        case PR_DEREF(AddrValue(a), _) => {
          if (get(store, a).isEmpty) {
            val msg = "Null pointer " + a
            withEps(error(msg), store)
          } else {
            for (v <- get(store, a))
            yield (Eps, Cont(v), store)
          }
        }
        case PR_DEREF(v, _) => {
          val msg = "Not an address value: " + v + "\nin a state\n" + state
          withEps(error(msg), store)
        }

        case PR_ASGN(AddrValue(a), v, c) => {
          withEps(Cont(v), put(store, a, Set(v)))
        }
        case PR_ASGN(v, _, _) => {
          val msg = "Not an address value: " + v + "\nin a state\n" + state
          withEps(error(msg), store)
        }



        case PR_THROW(v) => k match {
          case Nil => {
            val msg = "Uncaught exception with value " + v.toString
            withEps(error(msg), store)
          }
          case (f@TryCatchFrame(x, GroundClo(e, rho))) :: kk => {
            val a = alloc(state, x)
            withPop(Eval(GroundClo(e, rho + ((x, a)))), f, put(store, a, Set(v)))
          }
          case (f@TryFinallyFrame(c)) :: _ => {
            withPop(Eval(SeqClo(c, ThrowClo(ValueClo(v)))), f, store)
          }
          case (f@LabFrame(l, _)) :: _ => {
            withPop(Apply(PR_THROW(v)), f, store)
          }
          case f :: _ => {
            withPop(Apply(PR_THROW(v)), f, store)
          }
        }

        case PR_BREAK(l, v) => k match {
          case Nil => {
            val msg = "Unhandled break with label " + l + " and value " + v.toString
            withEps(error(msg), store)
          }
          case (f@TryCatchFrame(_, _)) :: kk => {
            withPop(Eval(BreakClo(l, ValueClo(v))), f, store)
          }
          case (f@TryFinallyFrame(c)) :: _ => {
            withPop(Eval(SeqClo(c, BreakClo(l, ValueClo(v)))), f, store)
          }
          case (f@LabFrame(l1, _)) :: _ => {
            if (l1 == l) {
              withPop(Cont(v), f, store)
            } else {
              withPop(Apply(PR_BREAK(l, v)), f, store)
            }
          }
          case f :: _ => {
            withPop(Apply(PR_THROW(v)), f, store)
          }
        }
        case _ => {
          val msg = "No transition for apply-state \n" + state + "\na stack\n" + k + "\nand a store\n" + store
          Set((Eps, error(msg), store))
        }

      }

      case Cont(v) => k match {
        case Nil => withEps(PFinal(v), store)

        case head :: _ => head match {
          case pop@LetFrame(x, c) => withPop(Apply(PR_LET(x, v, c)), pop, store)

          case pop@AppFrame(Nil) => withPop(Apply(PR_APP(v, Nil)), pop, store)
          case pop@AppFrame(c :: cx) => {
            val push = ArgFrame(v, Nil, cx)
            withSwitch(state, Eval(c), pop, push, store)
          }

          case pop@ArgFrame(t, ux, Nil) => withPop(Apply(PR_APP(t, ux ++ List(v))), pop, store)
          case pop@ArgFrame(t, ux, c :: cx) => {
            val push = ArgFrame(t, ux ++ List(v), cx)
            withSwitch(state, Eval(c), pop, push, store)

          }

          case pop@RecFrame(us, sn, Nil) => withPop(Cont(RecValue(us ++ List((sn, v)))), pop, store)
          case pop@RecFrame(us, si, (si1, c) :: cs) => {
            val push = RecFrame(us ++ List((si, v)), si1, cs)
            withSwitch(state, Eval(c), pop, push, store)
          }

          case pop@Lookup1Frame(c) => {
            val push = Lookup2Frame(v, c)
            withSwitch(state, Eval(c), pop, push, store)
          }
          case pop@Lookup2Frame(u, _) if v.isInstanceOf[AbstractStringValue] => {
            withPop(Apply(PR_REC_REF(u, v.asInstanceOf[AbstractStringValue])), pop, store)
          }

          case pop@Update1Frame(c, d) => {
            val push = Update2Frame(v, d)
            withSwitch(state, Eval(c), pop, push, store)
          }
          case pop@Update2Frame(u, c) => {
            val push = Update3Frame(u, v, c)
            withSwitch(state, Eval(c), pop, push, store)
          }
          case pop@Update3Frame(u, t, c) if t.isInstanceOf[AbstractStringValue] => {
            withPop(Apply(PR_REC_SET(u, t.asInstanceOf[AbstractStringValue], v, c)), pop, store)
          }

          case pop@Del1Frame(c) => {
            val push = Del2Frame(v, c)
            withSwitch(state, Eval(c), pop, push, store)
          }
          case pop@Del2Frame(u, c) if v.isInstanceOf[AbstractStringValue] => {
            withPop(Apply(PR_REC_DEL(u, v.asInstanceOf[AbstractStringValue], c)), pop, store)
          }


          case pop@RefFrame(c) => withPop(Apply(PR_REF(v, c)), pop, store)
          case pop@DerefFrame(c) => withPop(Apply(PR_DEREF(v, c)), pop, store)

          case pop@Asgn1Frame(c) => {
            val push = Asgn2Frame(v, c)
            withSwitch(state, Eval(c), pop, push, store)
          }
          case pop@Asgn2Frame(u, c) => withPop(Apply(PR_ASGN(u, v, c)), pop, store)


          case pop@IfFrame(c, d) => withPop(Apply(PR_IF(v, c, d)), pop, store)

          case pop@SeqFrame(c) => withPop(Eval(c), pop, store)

          case pop@ThrowFrame(_) => withPop(Apply(PR_THROW(v)), pop, store)

          case pop@BreakFrame(l, _) => withPop(Apply(PR_BREAK(l, v)), pop, store)

          case pop@OpFrame(op, us, Nil) => withPop(Apply(PR_OP(op, us ++ List(v))), pop, store)
          case pop@OpFrame(op, us, c :: cs) => {
            val push = OpFrame(op, us ++ List(v), cs)
            withSwitch(state, Eval(c), pop, push, store)
          }


          case _ => {
            val msg = "No transition for cont-state \n" + state + "\nand a stack\n" + k
            Set((Eps, error(msg), store))
          }
        }
      }

      case _ => {
        val msg = "No transition for state \n" + state + "\nand a stack\n" + k
        Set((Eps, error(msg), store))
      }
    }
  }

  def error(msg: String): PError = {
    //System.err.println(msg)
    PError(msg)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////

  private def withEps(pst: PState, store: Store): Set[(StackAction[Frame], ControlState, Store)] =
    Set((Eps, pst, store))

  private def withPush(pst: PState, f: Frame, store: Store): Set[(StackAction[Frame], ControlState, Store)] =
    Set((Push(f), pst, store))

  private def withPop(pst: PState, f: Frame, store: Store): Set[(StackAction[Frame], ControlState, Store)] =
    Set((Pop(f), pst, store))

  private def withSwitch(s1: PState, s2: PState, popped: Frame, pushed: Frame, store: Store):
  Set[(StackAction[Frame], ControlState, Store)] = {
    val state = PSwitch(s1, s2, popped, pushed)
    val frame = Switch(popped, s2, pushed)
    Set((frame, state, store))
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////

  def initState(e: Exp): (ControlState, Kont) = (Eval(GroundClo(e, Map.empty)), List())

  def mustHaveOnlyEmptyContinuation(c: ControlState) = c match {
    case PFinal(_) => true
    case _ => false
  }

  def canHaveEmptyContinuation(c: ControlState) = c match {
    case PFinal(_) | PError(_) | Cont(_) => true
    case _ => false
  }

}
