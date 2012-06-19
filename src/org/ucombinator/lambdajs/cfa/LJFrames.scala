package org.ucombinator.lambdajs.cfa

import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait LJFrames extends LJSyntax {

  import LJSyntax._

  /**
   * Frames (aka Continuations)
   */

  sealed abstract class Frame

  case class LetFrame(x: Var, body: Closure) extends Frame

  case class AppFrame(args: List[Closure]) extends Frame

  case class ArgFrame(fun: Value, pre: List[Value], post: List[Closure]) extends Frame

  case class RecFrame(pre: List[(StringValue, Value)], s: StringValue, post: List[(StringValue, Closure)]) extends Frame

  case class Lookup1Frame(index: Closure) extends Frame

  case class Lookup2Frame(rec: Value) extends Frame

  case class Update1Frame(index: Closure, rhs: Closure) extends Frame

  case class Update2Frame(rec: Value, rhs: Closure) extends Frame

  case class Update3Frame(rec: Value, index: Value) extends Frame

  case class Del1Frame(index: Closure) extends Frame

  case class Del2Frame(rec: Value) extends Frame

  case object RefFrame extends Frame

  case object DerefFrame extends Frame

  case class Asgn1Frame(rhs: Closure) extends Frame

  case class Asgn2Frame(lhs: Value) extends Frame

  case class IfFrame(tb: Closure, eb: Closure) extends Frame

  case class SeqFrame(snd: Closure) extends Frame

  case object ThrowFrame extends Frame

  case class BreakFrame(l: Label) extends Frame

  case class LabFrame(l: Label) extends Frame

  case class TryCatchFrame(x: Var, d: Closure) extends Frame

  case class TryFinallyFrame(d: Closure) extends Frame

  case class OpFrame(op: String, pre: List[Value], post: List[Closure]) extends Frame

}
