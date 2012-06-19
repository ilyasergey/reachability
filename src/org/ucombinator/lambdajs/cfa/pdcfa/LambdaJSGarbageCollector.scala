package org.ucombinator.lambdajs.cfa.pdcfa

import org.ucombinator.gc.GCInterface

/**
 * @author ilya
 */

trait LambdaJSGarbageCollector extends GCInterface {

  def rootAddr(c: ControlState, frames: Kont) = {
    throw new Exception("Asbtract GC for LambdaJS CFA2 is not yet implemented")
  }

  def gc(c: ControlState, frames: Kont) = {
    throw new Exception("Asbtract GC for LambdaJS CFA2 is not yet implemented")
  }

  def shouldGC = false

  def printGCDebug = false
}
