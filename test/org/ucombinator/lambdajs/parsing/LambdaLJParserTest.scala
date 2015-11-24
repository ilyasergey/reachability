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

package org.ucombinator.lambdajs.parsing

/**
 * @author ilya
 */

object LambdaLJParserTest {

  def main(args: Array[String]) {

    parseLet()
    parseApp()
    parseOp()
    parseLabel()
    parseIf()
    parseUpdate()
    parseGet()
    parseBegin()
    parseAsgn()
    parseDelete()
    parseRec()
    parseRec2()
    parseLambda()
    parseInt()
    parseString()
    parseUndef()

  }

  def parseAndPrint(text: String) {
    val parser = new LambdaJSParser
    val result = parser.parseText(text)

    println(result)
  }

  def parseLambda() {
    parseAndPrint("(lambda (x y z) x)")
  }

  def parseAsgn() {
    val text = """
    (set!
      $4
      #t)
    """
    parseAndPrint(text)
  }

  def parseOp() {
    val text = """
   (if (=== (deref $3) null)
      #f
      #t)
    """
    parseAndPrint(text)
  }

  def parseInt() {
    parseAndPrint("239")
  }

  def parseString() {
    parseAndPrint("\"This Is Sparta!\"")
  }

  def parseUndef() {
    parseAndPrint("undefined")
  }

  def parseRec() {
    val rec = """
    (lambda (x) (object ("$proto" "$Boolean.prototype")
                        ("$class" "Boolean")
                        ("$value" x)))
    """
    parseAndPrint(rec)
  }

  def parseRec2() {
    val rec = """
    (lambda (x) (object))
    """
    parseAndPrint(rec)
  }

  def parseGet() {
    val rec = """
    (get-field  (object ("$proto" "$Boolean.prototype")
                        ("$class" "Boolean")
                        ("$value" 42))
                "$value")
    """
    parseAndPrint(rec)
  }

  def parseUpdate() {
    val rec = """
    (update-field null
                  "length"
                  1.0)
    """
    parseAndPrint(rec)
  }

  def parseDelete() {
    val rec = """
    (delete-field null
                  "length")
    """
    parseAndPrint(rec)
  }

  def parseApp() {
    val text = """
    ((lambda (f x) (f x)) (lambda (z) z) 42)
    """
    parseAndPrint(text)
  }
  def parseIf() {
    val text = """
    (if #t 42 "$code")
    """
    parseAndPrint(text)
  }

  def parseLabel() {
    val text = """
    (label $return
      (break $return
        undefined))
    """
    parseAndPrint(text)
  }

  def parseBegin() {
    val text = """
    (begin 42 (begin 43 null))
    """
    parseAndPrint(text)
  }

  def parseLet() {
    val let = """
    (let ((x "abc")
          (y 42)
          (z 7.4))
          (lambda (u) z))
    """
    parseAndPrint(let)
  }

}
