/*
 * CRAPL 2012.
 * U Combinator, University of Utah
 * DistriNet, KU Leuven
 *
 * THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
 * APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
 * HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT
 * WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
 * DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
 * CORRECTION.
 *
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
 * WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR
 * CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
 * INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT
 * NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR
 * LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM
 * TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER
 * PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * If you have questions or concerns about the CRAPL, or you need more
 * information about this license, please contact:
 *
 *    Matthew Might
 *    http://matt.might.net/
 */

package org.ucombinator.scheme.cfa.mcfa

import scala.collection.immutable.{SortedMap, TreeMap, SortedSet, TreeSet}

import org.ucombinator.scheme.syntax._
import org.ucombinator.util._


trait Stores extends Ordered[Stores] {

  def localCompare(that: Stores): Int;

  def compare(that: Stores): Int = {
    val thisClass = this.getClass()
    val thatClass = that.getClass()
    val cmp = thisClass.getName() compare thatClass.getName()
    if (cmp != 0)
      cmp
    else
      localCompare(that)
  }

}


trait ObjectLocation extends Value {
  def isProcedure = false

  def isObjectLocation = true

  def objectType: SName;
}


/*
case class ConsLocation(time : Time, n : Int) extends ObjectLocation {
  def localCompare (that : Value) : Int = that match {
    case ConsLocation(thatTime,thatN) => {
      val cmp1 = time compare thatTime
      if (cmp1 != 0)
        return cmp1
      n compare thatN
    }
  }

  lazy val next = ConsLocation(time,n+1)

  val objectType = SName.from("cons")

  def toSourceLabel : Int = time match {
    case KTime(hd :: tl) => -hd + -10
    case _ => -1
  }
}
*/


case class StructLocation(time: Time, tag: SName, n: Int) extends ObjectLocation {
  def localCompare(that: Value): Int = that match {
    case StructLocation(thatTime, thatTag, thatN) => {
      val cmp1 = time compare thatTime
      if (cmp1 != 0)
        return cmp1

      val cmp2 = this.tag compare thatTag
      if (cmp2 != 0)
        return cmp2

      this.n compare thatN
    }
  }

  val objectType = tag

  def toSourceLabel: Int = {
    throw new Exception()
  }

}


case class FieldAddr(baseAddr: ObjectLocation, field: SName) extends Stores {
  def localCompare(that: Stores): Int = {
    that match {
      case FieldAddr(ba2, field2) =>
        ComparisonUtils.compare2(baseAddr.asInstanceOf[Value], field)(ba2.asInstanceOf[Value], field2)
    }
  }
}


trait BEnv extends Ordered[BEnv] {
  def apply(name: SName): Stores;

  override def equals(that: Any) = that match {
    case thatBEnv: BEnv => (this compare thatBEnv) == 0
    case _ => false
  }

  def update(name: SName, addr: Stores): BEnv;

  def |(names: Iterable[SName]): BEnv;
}

trait Time extends Ordered[Time] {
  def succ(k: Int, call: Int): Time;
}

trait Store {
  def apply(addr: Stores): D = (this get addr) match {
    case Some(d) => d
    case None => throw new Exception("Could not find " + addr)
  }

  def getOrElse(addr: Stores, default: D): D = (this get addr) match {
    case Some(d) => d
    case None => default
  }

  def get(addr: Stores): Option[D];


  def wt(that: Store): Boolean;

  /**
  Weak update.
   */
  def +(addr: Stores, d: D): Store;

  /**
  Strong update if safe; weak update otherwise.
   */
  def update(addr: Stores, d: D): Store;

  def toList: List[(Stores, D)];
}

trait Kont

trait Value extends Ordered[Value] {
  def isProcedure: Boolean;

  def isObjectLocation: Boolean;

  protected def localCompare(that: Value): Int;

  def compare(that: Value): Int = {
    val thisClassName = this.getClass().getName()
    val thatClassName = that.getClass().getName()
    val cmp = thisClassName compare thatClassName
    if (cmp != 0)
      cmp
    else
      this.localCompare(that)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Value => (this compare that) == 0
    case _ => false
  }

  def toSourceLabel: Int;
}


case class BooleanValue(val value: Boolean) extends Value {
  override def toString = if (value) {
    "#t"
  } else {
    "#f"
  }

  def localCompare(that: Value): Int = that match {
    case BooleanValue(value2) => value compare value2
  }

  def isProcedure = false

  def isObjectLocation = false

  def toSourceLabel: Int = -2
}


case class NilValue() extends Value {
  def localCompare(that: Value): Int = 0

  def isProcedure = false

  def isObjectLocation = false

  def toSourceLabel: Int = -5
}

case class PrimValue(val op: Prim) extends Value {
  def localCompare(that: Value): Int = that match {
    case PrimValue(thatOp: Prim) => this.op compare thatOp
  }

  def isProcedure = true

  def isObjectLocation = false

  def toSourceLabel: Int = -3
}


/* Generics components */


object Store {


}


case class StoreUpdate(val isStrong: Boolean, val addr: Stores, val d: D) {
  def apply(sharp: Sharp): Sharp = {
    sharp match {
      case StoreSharp(store) =>
        if (isStrong)
          new StoreSharp(store(addr) = d)
        else
          new StoreSharp(store +(addr, d))
    }
  }
}

case class StoreUpdateDeltaSharp(val changeLog: List[StoreUpdate]) extends DeltaSharp {
  def isEmpty = changeLog.isEmpty

  def apply(sharp: Sharp): Sharp =
    changeLog.foldLeft(sharp)((sharp, update) => update(sharp))
}

class SentinelStore(val changeLog: List[StoreUpdate], val store: Store) extends Store {

  def this(store: Store) = this(List(), store)

  def get(addr: Stores) = store get addr

  def wt(that: Store) = that match {
    case thatStore: SentinelStore => store wt thatStore.store
    case _ => store wt that
  }

  def resetLog() =
    new SentinelStore(List(), store)

  def +(addr: Stores, d: D): SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(StoreUpdate(false, addr, d) :: changeLog, store +(addr, d))
      }
    }
  }

  def update(addr: Stores, d: D): SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(StoreUpdate(true, addr, d) :: changeLog, store(addr) = d)
      }
    }
  }

  override def toString = store.toString

  def toList = store.toList
}


class MapStore(val map: SortedMap[Stores, D]) extends Store {
  def this() = this(TreeMap())

  def get(addr: Stores): Option[D] = map get addr

  def wt(that: Store) = that match {
    case _ => throw new Exception("Unknown store type")
  }

  /**
  Weak update.
   */
  def +(addr: Stores, d: D): MapStore = {
    map get addr match {
      case Some(existingD) => new MapStore(map + ((addr, d join existingD)))
      case None => new MapStore(map + ((addr, d)))
    }
  }

  /**
  A simple store does not contain enough information to determine whether a strong update is safe or not, so
   this operation always performs a weak update.
   */
  def update(addr: Stores, d: D): Store = {
    // We don't have enough information to do a strong update, so we fall back to a weak update.
    this +(addr, d)
  }

  override def toString = "\n " + (map mkString "\n ")

  def toList: List[(Stores, D)] = map.toList
}


case class SortedSetD(set: SortedSet[Value]) extends D {
  def this() = this(TreeSet())

  def +(value: Value): D = SortedSetD(set + value)

  def join(that: D) = SortedSetD(set ++ that.toList)

  def wt(that: D): Boolean = {
    that match {
      case SortedSetD(set2) => set subsetOf set2
    }
  }

  def toList = set.toList

  override def toString = "{" + (set mkString ",") + "}"
}


case class StoreSharp(val store: Store) extends Sharp {

  def resetChangeLog = {
    if (store.isInstanceOf[SentinelStore])
      new StoreSharp(new SentinelStore(store.asInstanceOf[SentinelStore].store))
    else
      new StoreSharp(new SentinelStore(store))
  }

  def changeLog = {
    if (store.isInstanceOf[SentinelStore])
      StoreUpdateDeltaSharp(store.asInstanceOf[SentinelStore].changeLog)
    else
      throw new Exception()
  }

  def wt(that: Sharp): Boolean = that match {
    case StoreSharp(thatStore) => store wt thatStore
    case _ => throw new Exception("Can't compare sharps!")
  }
}


case class KTime(val last: List[Int]) extends Time {
  def compare(that: Time) = that match {
    case KTime(last2) => ComparisonUtils.compareLists(last, last2)
  }

  def succ(k: Int, call: Int): KTime = KTime((call :: last) take k)
}


case object UniTime extends Time {
  def compare(that: Time) =
    if (this eq that)
      0
    else
      -1

  def succ(k: Int, call: Int): Time = this
}


case class PrimAddr(val name: SName) extends Stores {
  def localCompare(that: Stores): Int = that match {
    case PrimAddr(thatName) => this.name compare thatName
  }
}


case class GlobalAddr(val name: SName) extends Stores {
  def localCompare(that: Stores): Int = that match {
    case GlobalAddr(thatName) => this.name compare thatName
  }
}

case class FlatBind(name: SName, bEnv: BEnv) extends Stores {
  def localCompare(that: Stores): Int = {
    that match {
      case FlatBind(thatName, thatBEnv: BEnv) =>
        ComparisonUtils.compare2(name, bEnv)(thatName, thatBEnv)
    }
  }
}

case class FlatBEnv(val labels: List[Int]) extends BEnv {
  def apply(name: SName) = FlatBind(name, this)

  def succ(m: Int, l: Int) = FlatBEnv((l :: labels) take m)

  def compare(that: BEnv) = that match {
    case that: FlatBEnv => ComparisonUtils.compareLists(this.labels, that.labels)
  }

  def update(name: SName, addr: Stores): BEnv = throw new Exception("Cannot extend flat binding environments.")

  def |(names: Iterable[SName]): BEnv = this
}

case class Clo(val lam: Lambda, val bEnv: BEnv) extends Value {
  def localCompare(that: Value): Int = that match {
    case Clo(lam2, bEnv2) => ComparisonUtils.compare2(lam.asInstanceOf[Exp], bEnv)(lam2.asInstanceOf[Exp], bEnv2)
  }

  def isProcedure = true

  def isObjectLocation = false

  def toSourceLabel: Int = lam.label
}

trait D {
  def +(value: Value): D;

  def join(that: D): D;

  def wt(that: D): Boolean;

  def toList: List[Value];
}


case class MapBind(val name: SName, val time: Time) extends Stores {
  def localCompare(that: Stores): Int = that match {
    case MapBind(thatName, thatTime) => ComparisonUtils.compare2(this.name, this.time)(thatName, thatTime)
  }
}

case class MapBEnv(val map: SortedMap[SName, Stores]) extends BEnv {
  def apply(name: SName) = map(name)

  def succ(m: Int, l: Int) = throw new Exception("Not appropriate in this context.")

  def compare(that: BEnv) = that match {
    case MapBEnv(thatMap) => ComparisonUtils.compareLists(this.map.toList, thatMap.toList)
  }

  def update(name: SName, addr: Stores): BEnv =
    MapBEnv(map + ((name, addr)))

  def |(names: Iterable[SName]): BEnv = MapBEnv(TreeMap[SName, Stores]() ++ (names map (n => (n, map(n)))))

}


case class CFlat(val exp: Exp, val bEnv: BEnv, val t: Time) extends Flat {
  def compare(that: Flat) = that match {
    case CFlat(exp2, bEnv2, t2) => ComparisonUtils.compare3(exp, bEnv, t)(exp2, bEnv2, t2)
  }
}



