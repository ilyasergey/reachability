package org.ucombinator.lambdajs.cfa

import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait LJPrimOperators extends LJFrames with LJSyntax {

  def delta(op: String, values: List[Value]): Value = null

}
