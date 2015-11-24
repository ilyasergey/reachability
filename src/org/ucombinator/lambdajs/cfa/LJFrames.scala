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

  case class Lookup2Frame(rec: Value, c: Closure) extends Frame

  case class Update1Frame(index: Closure, rhs: Closure) extends Frame

  case class Update2Frame(rec: Value, rhs: Closure) extends Frame

  case class Update3Frame(rec: Value, index: Value, c: Closure) extends Frame

  case class Del1Frame(index: Closure) extends Frame

  case class Del2Frame(rec: Value, c: Closure) extends Frame

  case class RefFrame(c: Closure) extends Frame

  case class DerefFrame(c: Closure) extends Frame

  case class Asgn1Frame(rhs: Closure) extends Frame

  case class Asgn2Frame(lhs: Value, c: Closure) extends Frame

  case class IfFrame(tb: Closure, eb: Closure) extends Frame

  case class SeqFrame(snd: Closure) extends Frame

  case class ThrowFrame(c: Closure) extends Frame

  case class BreakFrame(l: Label, c: Closure) extends Frame

  case class LabFrame(l: Label, c: Closure) extends Frame

  case class TryCatchFrame(x: Var, d: Closure) extends Frame

  case class TryFinallyFrame(d: Closure) extends Frame

  case class OpFrame(op: Op, pre: List[Value], post: List[Closure]) extends Frame

}
