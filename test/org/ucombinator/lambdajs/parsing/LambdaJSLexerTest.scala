package org.ucombinator.lambdajs.parsing

/**
 * @author ilya
 */

object LambdaJSLexerTest {

  import org.ucombinator.lambdajs.parsing.LambdaJSTokens._

  def main(args: Array[String]) {

    val lexer = new LambdaJSLexer
    if (args.size == 0) {
      return
    }

    val filename = args(0)
    val result = lexer.parseAllIn(filename)

    println(result)
    println()

    println(result.filter(_.isInstanceOf[TOp]))
    println()

    println(result.filter(_.isInstanceOf[TIdent]))
    println()

    println(result.filter(_.isInstanceOf[TString]))
    println()

    println(result.filter(_.isInstanceOf[TFloat]))
    println()
  }
}
