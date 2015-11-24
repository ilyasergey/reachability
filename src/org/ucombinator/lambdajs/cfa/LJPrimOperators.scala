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

trait LJPrimOperators extends LJFrames with LJSyntax {
  def delta(op: String, values: List[Value]): Set[Value] = op match {

    case "typeof" => values match {
      case Nil => opUndefined(op, values)
      case UndefValue :: Nil => set(StringValue("undefined"))
      case NullValue :: Nil => set(StringValue("undefined"))
      case AddrValue(_) :: Nil => set(StringValue("location"))
      case RecValue(_) :: Nil => set(StringValue("object"))
      case FunValue(_, _) :: Nil => set(StringValue("lambda"))
      case BoolValue(_) :: Nil => set(StringValue("boolean"))
      case (_: AbstractStringValue) :: Nil => set(StringValue("string"))
      case (_: AbstractNumValue) :: Nil => set(StringValue("number"))
      case _ => opUndefined(op, values)
    }

    case "===" => values match {
      case x :: y :: Nil if x == y => set(BoolValue(true))
      case UndefValue :: y :: Nil => Set(BoolValue(true), BoolValue(false))
      case x :: UndefValue :: Nil => Set(BoolValue(true), BoolValue(false))
      case _ => set(BoolValue(false))
    }

    case "prim->string" => values match {
      case (s@StringValue(_)) :: Nil  => set(s)
      case _ => opUndefined(op, values)
    }

    case o => {
      opUndefined(o, values)
    }
  }

  private def set[T](t: T): Set[T] = Set(t)

  def opUndefined(o: String, values: List[Value]): Set[Value] = {
    System.err.println("Undefined operation " + o + ", arguments: " + values)
    Set(UndefValue)
  }

}
