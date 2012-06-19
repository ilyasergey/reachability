package org.ucombinator.util

/**
 * @author ilya
 */

object DataUtil {

  /**
   * A reminiscence of good old days and the undergrad course on FP :)
   *
   * todo: rewrite in CPS
   *
   * @param list list of sets of values of length N
   * @return set of lists of values, each of size N,  with possible combinations of constituents
   */
  def toSetOfLists[A](list: List[Set[A]]): Set[List[A]] = {
    list match {
      case Nil => Set()
      case hd :: Nil => hd.map(x => List(x))
      case hd :: tail => {
        val rest = toSetOfLists(tail)
        for {
          elem <- hd
          suffix <- rest
        } yield (elem :: suffix)
      }
    }
  }


}
