package org.ucombinator.lambdajs.parsing

/**
 * @author ilya
 */

object LambdaLJParserTest {

  def main(args: Array[String]) {

    parseLambda()
    parseInt()
    parseString()
    parseUndef()

  }

  def parseAndPrint(text: String) {
    val lexer = new LambdaJSLexer
    val input = new lexer.Scanner(text)

    val parser = new LambdaJSParser
    val result = parser.parseAll(input)

    println(result)
  }

  def parseLambda() {
    parseAndPrint("(lambda (x y z) x)")
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


}
