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

package org.ucombinator.scheme.cfa.pdcfa

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.cfa.cesk._

/**
 * @author ilya
 */

trait StackCESKMachinery extends CESKMachinery {

  type Kont = List[Frame]

  type Addr = (Var, List[Exp])

  // No continuation pointers
  type KAddr = Unit

  /********************************************************************
   * Utility functions
   ********************************************************************/
  def initState(e: Exp): Conf = (PState(e, Map.empty, Map.empty, ()), Nil)

  /********************************************************************
   * Main non-deterministic abstract step function
   ********************************************************************/
  def mnext: Conf => Set[Conf] = {

    /********************************************
     * Core transitions
     *********************************************/

    // Application of lambda or reference
    case c@(PState(App(f@(Lambda(_, _) | Ref(_)), args), rho, s, kptr), k) =>
      for {
        Clo(lam@Lambda(Formals(params, _), body), rho1) <- atomicEval(f, rho, s)

        // process lambda parameters
        paramNames = params.map(_.name)
        ai = paramNames.map(alloc(_, c)) // allocate addresses
        rho2 = updateEnv(rho1, paramNames.zip(ai)) // update function env

        arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s)) // map atomic arguments to values
        s1 = updateStore(s, ai.zip(arg_vals))

        // In A-normal form only one expression in body
        e = getLambdaBodyInANF(lam)
      } yield (PState(e, rho2, s1, kptr), k)

    case (PState(l@Let(_, _), rho, s, kptr), k) => {
      val (v, call, e) = decomposeLetInANF(l)
      Set((PState(call, rho, s, kptr), LetFrame(v, e, rho) :: k))
    }

    case c@(PState(ae, rho, s, kptr), LetFrame(v, e, rho1) :: k)
      if isAtomic(ae) => {
      val a = alloc(v, c)
      val rho2 = updateEnv(rho1, List((v, a)))
      val s1 = updateStore(s, List((a, atomicEval(ae, rho, s))))
      Set((PState(e, rho2, s1, kptr), k))
    }

    /**************************************************************
     * Real-world stuff
     *************************************************************/


    /*******************************************************
     * Sequential composition
     * TODO: prohibited!
     ******************************************************/
    /*
        case c@(PState(b@Begin(Body(defs, exprs)), rho, s), k) => {
          exprs.size match {
            case 0 => throw new CESKException("No statements in begin statement")
            case 1 => Set((PState(exprs.head, rho, s), k))
            case _ => {
              Set((PState(exprs.head, rho, s), SeqFrame(defs, exprs.tail, rho, b) :: k))
            }
          }
        }
        case c@(PState(ae, rho, s), SeqFrame(defs, ex, rho1, b) :: k)
          if isAtomic(ae) => {
          // just drop the value
          Set((PState(Begin(Body(defs, ex)), rho1, s), k))
        }
    */

    /******************************************************
     * Conditional operator
     ******************************************************/
    case c@(PState(b@If(cond, tBranch, eBranch), rho, s, kptr), k) => {
      Set((PState(cond, rho, s, kptr), IfFrame(tBranch, eBranch, rho) :: k))
    }
    case c@(PState(ae, rho, s, kptr), IfFrame(tBranch, eBranch, rho1) :: k)
      if isAtomic(ae) => {
      val boolValues = getBooleanValues(ae, rho, s)
      boolValues.map {
        b => if (b) {
          (PState(tBranch, rho1, s, kptr), k)
        } else {
          (PState(eBranch, rho1, s, kptr), k)
        }
      }
    }

    /******************************************************
     * Set!
     ******************************************************/
    case c@(PState(SetVar(v, ae), rho, s, kptr), k)
      // Only atomic values are assigned
      if (isAtomic(ae)) => {
      val addr = lookupEnv(rho, v)
      val s1 = updateStore(s, List((addr, atomicEval(ae, rho, s))))
      Set((PState(Unspecified(), rho, s1, kptr), k))
    }


    /******************************************************
     * Primitive applications
     ******************************************************/
    // only atomic values or variable are supported in primops
    case c@(PState(app@App(p@Prim(primName, _), args), rho, s ,kptr), k) => {
      // map atomic arguments to values (sets)
      val arg_vals = args.args.map(ae => atomicEval(ae.exp, rho, s))
      for {
        results <- arg_vals.size match {
          case 0 => Set(evalPrimApp(primName, List()))
          case 1 => for {a <- arg_vals.head} yield evalPrimApp(primName, List(a))
          case 2 => for {
            a <- arg_vals.head
            b <- arg_vals.tail.head
          } yield evalPrimApp(primName, List(a, b))
          case n => {
            throw new StackCESKException("Primitive functions of arity " + n + " are not supported:\n" + app.toString)
          }
        }
        result <- results
        state = analyseResult(result, rho, s, app, kptr)
      } yield (state, k)
    }


    /******************************************************
     * Final state
     ******************************************************/
    case c@(PState(ae, rho, s, kptr), Nil)
      if isAtomic(ae) => for {
    /**
     * (REMARK)
     * [Non-sense values in a final state]
     * Consider them as a lost of precision
     */
      v <- atomicEval(ae, rho, s) //.filter(isValidValueToReturn(_))
    } yield (PFinal(v), Nil)

    // Ok, folks, that's it!
    case (PFinal(_), Nil) => Set()


    case c => {
      val c1 = c // for convenient debug
      throw new StackCESKException("Wrong state: " + c1.toString)
    }

  }

  class StackCESKException(s: String) extends CESKException(s)


}