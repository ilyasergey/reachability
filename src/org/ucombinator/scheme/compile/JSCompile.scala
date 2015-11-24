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

package org.ucombinator.scheme.compile

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.transform._
import org.ucombinator.scheme.parsing.RnRSParser

/**
Emits JavaScript code from CPS.
 */
class CPSEmitJS {

  var prog: Program = null

  private def mangle(string: String): String = {
    var s = "";
    for (c <- string) {
      // println("mangling: " + c)  // DEBUG
      // println("isLoD:    " + Character.isLetterOrDigit(c)) // DEBUG
      s += (c match {
        case _ if Character.isLetterOrDigit(c) => c.toString
        case _ => {
          // println("MANGLING") // DEBUG
          "_" + char2int(c)
        }
      })
    }
    // println("mangle: " + name + " => " + s) // DEBUG
    s
  }

  private def mangle(name: SName): String = mangle(name.toString)

  private def mangle(prefix: String, name: SName): String =
    prefix + mangle(name.toString)

  private def mangle(kw: SKeyword): String = "__kw_" + mangle(kw.string)


  def emitln(s: String) {
    println(s);
  }

  def emitDecs(decs: List[Dec]) = decs match {
    case List() => {}
  }

  def emitDefs(defs: List[Def]) = defs match {
    case List() => {}
  }

  def emitExp(exp: Exp) = exp match {
    case _ => throw new Exception()
  }

  def apply(prog: Program) {

    this.prog = prog

    prog match {
      case Program(decs, defs, exp) => {
        emitDecs(decs);
        emitDefs(defs);
        emitExp(exp);
      }
    }

    throw new Exception()
  }

}


object JSCompile {

  private def write(file: java.io.File, contents: String) {
    val fw = new java.io.FileWriter(file);
    fw.write(contents);
    fw.close()
  }

  private def write(fileName: String, contents: String) {
    write(new java.io.File(fileName), contents)
  }

  def main(args: Array[String]) {

    /*

     Passes:

     + Preamblification
     + Alphatization
     + Desugaring
     + A-normalization
     + CPS conversion
     + Emission

     */

    val filename = args(0)

    var sexps: List[SExp] = Nil

    if (filename == "-") {
      sexps = SExp.parseAll(scala.io.Source.fromInputStream(System.in).getLines mkString "")
    } else {
      sexps = SExp.parseAllIn(filename)
    }

    // println("input:\n" + (sexps mkString "\n"))

    var ast = RnRSParser(sexps)
    write("tmp/original.scm", ast.toString);

    val preamblifier = new Preamblifier
    ast = preamblifier(ast)
    write("tmp/preamblified.scm", ast.toString);

    val desugarer = new Desugarer(false)
    ast = desugarer(ast)
    write("tmp/desugared.scm", ast.toString);

    val alphatizer = new Alphatizer
    ast = alphatizer(ast)
    write("tmp/alphatized.scm", ast.toString);

    val anormalizer = new ANormalizer()
    ast = anormalizer(ast)
    write("tmp/a-normalized.scm", ast.toString);

    val cpsConverter = new CPSConverter()
    ast = cpsConverter(ast)
    write("tmp/cps-converted.scm", ast.toString);

    val emitter = new CPSEmitJS()
    emitter(ast)

  }
}
