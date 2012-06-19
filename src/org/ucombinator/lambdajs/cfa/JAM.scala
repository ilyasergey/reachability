package org.ucombinator.lambdajs.cfa

import org.ucombinator.dsg._
import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait JAM extends LJFrames with LJSyntax with LJPrimOperators {
  self: StoreInterface =>

  /********************************************
   * Partial states
   ********************************************/


  sealed abstract class PState

  case class Eval(store: Store, clo: Closure) extends PState

  case class Cont(store: Store, v: Value) extends PState

  case class Apply(store: Store, pr: PotentialRedex) extends PState

  case class PFinal(v: Value, store: Store) extends PState

  case class PError(m: String) extends PState

  ///////////////////////////////////////////////////////////////////////

  type Kont = List[Frame]

  type ControlState = PState

  def step(state: ControlState, k: Kont, frames: Kont): Set[(StackAction[Frame], ControlState)] = state match {

    // Error state
    case PError(_) => Set()

    // Eval states
    case Eval(store, clo) => clo match {

      case GroundClo(x@Var(_, _), rho) => withEps(Apply(store, PR_VAR(x, rho)))
      case GroundClo(e, rho) => withEps(Eval(store, exp2Clo(e, rho)))
      case ValueClo(v) => withEps(Cont(store, v))
      case RecordClo((s, c) :: tail) => withPush(Eval(store, c), RecFrame(List(), s, tail))
      case LetClo(x, c, d) => withPush(Eval(store, c), LetFrame(x, d))
      case AppClo(c, ds) => withPush(Eval(store, c), AppFrame(ds))
      case LookupClo(c, d) => withPush(Eval(store, c), Lookup1Frame(d))
      case UpdateClo(c, d, d1) => withPush(Eval(store, c), Update1Frame(d, d1))
      case DelClo(c, d) => withPush(Eval(store, c), Del1Frame(d))
      case AsgnClo(c, d) => withPush(Eval(store, c), Asgn1Frame(d))
      case RefClo(c) => withPush(Eval(store, c), RefFrame)
      case DerefClo(c) => withPush(Eval(store, c), DerefFrame)
      case IfClo(c, d, d1) => withPush(Eval(store, c), IfFrame(d, d1))
      case SeqClo(c, d) => withPush(Eval(store, c), SeqFrame(d))
      case WhileClo(c, d) => withPush(Eval(store, c), IfFrame(SeqClo(d, WhileClo(c, d)), ValueClo(UndefValue)))
      case LabelledClo(l, c) => withPush(Eval(store, c), LabFrame(l))
      case BreakClo(l, c) => withPush(Eval(store, c), BreakFrame(l))
      case TryCatchClo(c, x, d) => withPush(Eval(store, c), TryCatchFrame(x, d))
      case TryFinallyClo(c, d) => withPush(Eval(store, c), TryFinallyFrame(d))
      case ThrowClo(c) => withPush(Eval(store, c), ThrowFrame)
      case OpClo(op, c :: tail) => withPush(Eval(store, c), OpFrame(op, List(), tail))
      case _ => {
        val msg = "No transition for eval-state\n" + state + "\nand a stack\n" + k
        Set((Eps, PError(msg)))
      }
    }

    case Apply(store, pr) => pr match {

      case PR_VAR(x, rho) => {
        for (v <- get(store, rho(x))) yield (Eps, Cont(store, v))
      }
      case PR_LET(x, v, GroundClo(e, rho)) => {
        val a = alloc(state, x)
        withEps(Eval(put(store, a, Set(v)), GroundClo(e, rho + ((x, a)))))
      }
      case PR_APP(FunValue(Fun(xs, e, _), rho), vs) => if (xs.length == vs.length) {
        val as = xs.map(x => alloc(state, x))
        val rho1 = rho ++ xs.zip(as)
        withEps(Eval(putMany(store, as.zip(vs.map(Set(_)))), GroundClo(e, rho1)))
      } else {
        val msg = "Wrong number of arguments in the state\n" + state + "\nand a stack\n" + k
        Set((Eps, PError(msg)))
      }
      case PR_REC_REF(RecValue(entries), sv) => {
        sv match {
          // Fetching any field of a record
          case StringTop => {
            val msg = "Alarm! StringTop is used as a reference for a record in state \n" + state
            System.err.println(msg)
            Set((Eps, PError(msg)))
          }
          // Fetching a particular field
          case s@StringValue(_) => entries.filter {
            // take exact matches and StringTops
            case (si, _) => (si == s) || (si == StringTop)
          } match {
            // something is found
            case l@(h :: _) => l.toSet.map((p: (StringValue, Value)) => (Eps, Cont(store, p._2)))
            // nothing is found
            case Nil => withEps(Cont(store, UndefValue))
          }
        }
      }
      case PR_REC_SET(RecValue(entries), sv, rhs) => {
        sv match {
          // Fetching any field of a record
          case StringTop => {
            val msg = "Alarm! StringTop is used as a reference for record update in state \n" + state
            System.err.println(msg)
            Set((Eps, PError(msg)))
          }
          // Fetching a particular field
          case s@StringValue(_) => {
            val newEntries = (s, rhs) :: entries.filter {
              case (si, _) => (si != s)
            }
            withEps(Cont(store, RecValue(newEntries)))
          }
        }
      }
      case PR_REC_DEL(RecValue(entries), sv) => {
        sv match {
          // Fetching any field of a record
          case StringTop => {
            val msg = "Alarm! StringTop is used as a reference for record delete in state \n" + state
            System.err.println(msg)
            Set((Eps, PError(msg)))
          }
          // Fetching a particular field
          case s@StringValue(_) => {
            val newEntries = entries.filter {
              case (si, _) => (si != s)
            }
            withEps(Cont(store, RecValue(newEntries)))
          }
        }
      }

      case PR_IF(c, tb, eb, rho) => c match {
        case BoolValue(true) => withEps(Eval(store, GroundClo(tb, rho)))
        case BoolValue(false) => withEps(Eval(store, GroundClo(eb, rho)))
        case _ => Set(
          (Eps, Eval(store, GroundClo(tb, rho))),
          (Eps, Eval(store, GroundClo(eb, rho))))
      }

      case PR_OP(op, values) => withEps(Cont(store, delta(op, values)))

      case PR_REF(v) => {
        val a = alloc(state)
        withEps(Cont(put(store, a, Set(v)), AddrValue(a)))
      }

      case PR_DEREF(AddrValue(a)) => {
        if (get(store, a).isEmpty) {
          val msg = "Null pointer " + a
          withEps(PError(msg))
        } else {
          for (v <- get(store, a))
          yield (Eps, Cont(store, v))
        }
      }

      case PR_ASGN(AddrValue(a), v) => withEps(Cont(put(store, a, Set(v)), v))

      case PR_THROW(v) => k match {
        case Nil => {
          val msg = "Uncaught exception with value " + v.toString
          withEps(PError(msg))
        }
        case (f@TryCatchFrame(x, GroundClo(e, rho))) :: kk => {
          val a = alloc(state, x)
          withPop(Eval(put(store, a, Set(v)), GroundClo(e, rho + ((x, a)))), f)
        }
        case (f@TryFinallyFrame(c)) :: _ => {
          withPop(Eval(store, SeqClo(c, ThrowClo(ValueClo(v)))), f)
        }
        case (f@LabFrame(l)) :: _ => {
          withPop(Apply(store, PR_THROW(v)), f)
        }
        case f :: _ => {
          withPop(Apply(store, PR_THROW(v)), f)
        }
      }

      case PR_BREAK(l, v) => k match {
        case Nil => {
          val msg = "Unhandled break with label " + l + " and value " + v.toString
          withEps(PError(msg))
        }
        case (f@TryCatchFrame(_, _)) :: kk => {
          withPop(Eval(store, BreakClo(l, ValueClo(v))), f)
        }
        case (f@TryFinallyFrame(c)) :: _ => {
          withPop(Eval(store, SeqClo(c, BreakClo(l, ValueClo(v)))), f)
        }
        case (f@LabFrame(l1)) :: _ => {
          if (l1 == l) {
            withPop(Cont(store, v), f)
          } else {
            withPop(Apply(store, PR_BREAK(l, v)), f)
          }
        }
        case f :: _ => {
          withPop(Apply(store, PR_THROW(v)), f)
        }
      }
      case _ => {
        val msg = "No transition for apply-state \n" + state + "\nand a stack\n" + k
        Set((Eps, PError(msg)))
      }

    }

    case Cont(store, v) => k match {
      case Nil => withEps(PFinal(v, store))
      case LetFrame(x, c) :: _ => withEps(Apply(store, PR_LET(x, v, c)))


      case _ => {
        val msg = "No transition for cont-state \n" + state + "\nand a stack\n" + k
        Set((Eps, PError(msg)))
      }
    }

    // todo implement the rest
    case _ => throw new Exception("No transition for state \n" + state + "\nand a stack\n" + k)
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////

  private def withEps(pst: PState): Set[(StackAction[Frame], ControlState)] = Set((Eps, pst))

  private def withPush(pst: PState, f: Frame): Set[(StackAction[Frame], ControlState)] = Set((Push(f), pst))

  private def withPop(pst: PState, f: Frame): Set[(StackAction[Frame], ControlState)] = Set((Pop(f), pst))

  ///////////////////////////////////////////////////////////////////////////////////////////////

  def initState(e: Exp): (ControlState, Kont) = (Eval(Map.empty, GroundClo(e, Map.empty)), List())

  def mustHaveOnlyEmptyContinuation(c: ControlState) = c match {
    case PFinal(_, _) => true
    case _ => false
  }

  def canHaveEmptyContinuation(c: ControlState) = c match {
    case PFinal(_, _) | PError(_) | Cont(_, _) => true
    case _ => false
  }

}
