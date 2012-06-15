package org.ucombinator.lambdajs.cfa

import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait StoreInterface {
  self : LJSyntax =>

  type ControlState

  type :->[A, B] = Map[A, B]

  type Store = Addr :-> Set[Value]

  // analysis functions

  def alloc(s: ControlState, x: Var): Addr

  // utility

//  def putMany[A, B](map: A :-> Set[B], keyVals: List[(A, Set[B])]): A :-> Set[B] = {
//    keyVals.foldLeft(map) {case (result, (k, vs)) => }
//  }

  def put[A, B](map: A :-> Set[B], key: A, newVals: Set[B]): A :-> Set[B] = {
    val oldVals = map.getOrElse(key, Set())
    val values = oldVals ++ newVals
    map + ((key, values))
  }

  def get[A, B](map: A :-> Set[B], key: A): Set[B] = map.getOrElse(key, Set())

}
