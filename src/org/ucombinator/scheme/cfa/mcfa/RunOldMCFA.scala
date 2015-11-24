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

package org.ucombinator.scheme.cfa.mcfa

import scala.collection.immutable.TreeMap

import org.ucombinator.scheme.transform._
import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.parsing.RnRSParser
import org.ucombinator.util.CFAOptions


object RunOldMCFA {

  def main(args: Array[String]) {

    val opts = CFAOptions.parse(args)

    if (opts.fileName == null) {
      System.err.println("Please specify a filename.")
      return
    }

    val filename = opts.fileName
    System.err.print("Parsing s-expressions...")
    val sexps = SExp.parseAllIn(filename)
    System.err.println("done")

    System.err.print("Bulding AST...")
    val ast = RnRSParser(sexps)
    System.err.println("done")

    System.err.print("A-normalizing...")
    val anastP: Program = (new ANormalizer).apply(ast)
    val anast: Exp = ANormalizer(ast)
    System.err.println("done")

    System.err.print("CPS-converting...")
    val cpastP: Program = (new CPSConverter).apply(anastP)
    val cpast: Exp = CPSConverter(anast)
    System.err.println("done")

    System.out.println("Input program:")
    System.out.println(ast)
    System.out.println("\n")

    System.out.println("ANF program:")
    System.out.println(anastP)
    System.out.println("\n")

    System.out.println("CPS program:")
    System.out.println(cpastP)
    System.out.println("\n")

    System.out.println("Free variables:")
    System.out.println(cpast.free)
    System.out.println("\n")

    opts.analysis match {
      case "full" => {
        val CFA = new KCFA_CPS(cpastP, new MapBEnv(TreeMap()), KTime(List()), new MapStore(), new SortedSetD())
        CFA.printStates = opts.printStates
        CFA.k = opts.k
        CFA.runWithGlobalSharp(filename)
        val sharp = CFA.globalSharp
        val store = sharp.asInstanceOf[StoreSharp].store
        ()
      }
      case "flat" => {
        val CFA = new MCFA_CPS(cpast, new FlatBEnv(List()), KTime(List()), new MapStore(), new SortedSetD())
        CFA.printStates = opts.printStates
        CFA.k = opts.k
        CFA.m = opts.m
        CFA.flatPolicy = opts.flatPolicy
        CFA.runWithGlobalSharp(filename)
        val sharp = CFA.globalSharp
        val store = sharp.asInstanceOf[StoreSharp].store
        ()
      }
    }

    ()
  }

}
