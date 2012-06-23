package org.ucombinator.lambdajs.parsing

/**
 * @author ilya
 */

object LambdaLJParserTest {

  def main(args: Array[String]) {

    parseIf()
    parseBegin()
    parseApp()
    parseDelete()
    parseUpdate()
    parseGet()
    parseLet()
    parseRec()
    parseLambda()
    parseInt()
    parseString()
    parseUndef()

  }

  def parseAndPrint(text: String) {
    val parser = new LambdaJSParser
    val result = parser.parseAll(text)

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

  def parseRec() {
    val rec = """
    (lambda (x) (object ("$proto" "$Boolean.prototype")
                        ("$class" "Boolean")
                        ("$value" x)))
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
