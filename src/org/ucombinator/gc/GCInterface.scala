package org.ucombinator.gc

/**
 * @author ilya
 */

trait GCInterface {

  type Addr
  type ControlState
  type Kont

  def rootAddr(c: ControlState, frames: Kont): Set[Addr]

  def gc(c: ControlState, frames: Kont): ControlState

  def shouldGC: Boolean

  def printGCDebug: Boolean

}
