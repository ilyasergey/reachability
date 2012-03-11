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

import org.ucombinator.scheme.syntax.Prim
import tools.nsc.io.Directory
import java.io.{FileWriter, File}
import org.ucombinator.util.StringUtils
import util.Parameters


/* Small-step abstract interpretation */


/**
A state in a small-step abstract interpretation.

 The <code>flat</code> component of a state has a flat partial order;
 the <code>sharp</code> component of a state has a true partial order.
 */
case class State(_flat: Flat, _sharp: Sharp) {
  def flat = _flat

  def sharp_=(_sharp: Sharp): State =
    State(_flat, _sharp)

  def sharp = _sharp
}

trait Flat extends Ordered[Flat] {
  override def equals(that: Any) = that match {
    case that: Flat => (this compare that) == 0
    case _ => false
  }
}

case object StuckFlat extends Flat {
  def compare(that: Flat) =
    that match {
      case StuckFlat => 0
      case _ => -1
    }
}


abstract class DeltaSharp {
  def isEmpty: Boolean;

  def apply(sharp: Sharp): Sharp;
}

/*
case object NullDeltaSharp extends DeltaSharp {
  override isEmpty = true

  def apply(sharp : Sharp) : Sharp = sharp
}
*/


trait Sharp {
  def wt(that: Sharp): Boolean;

  def resetChangeLog: Sharp;

  def changeLog: DeltaSharp;
}


trait SmallStepAbstractInterpretation {
  def initialState: State;

  def next(state: State): List[State];

  var printStates = true
  val tmpDirName = "./dumps"
  val dumpFileName = (fn: String) => fn + "-dump.cfa"
  val dumpFilePath = (s: String) => tmpDirName + File.separator + dumpFileName(s)

  var dumpFileWriter: java.io.FileWriter = null

  def dumpln(s: String) {
    if (printStates) {
      dumpFileWriter.write(s)
      dumpFileWriter.write("\n")
    }
  }

  var count = 0
  var globalSharp: Sharp = null

  def runWithGlobalSharp(filename: String) {
    if (printStates) {
      val d = new Directory(new File(tmpDirName))
      if (!d.exists) {
        d.createDirectory(force = true)
        d.createFile(failIfExists = false)
      }
      val path = dumpFilePath(StringUtils.trimFileName(filename))
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      dumpFileWriter = new FileWriter(path)
    }

    // seen records the last generation store seen with this flat.
    val seen = scala.collection.mutable.HashMap[Flat, Long]()
    var currentGeneration: Long = 1

    val init = initialState
    var todo = List(init)

    globalSharp = init.sharp
    var timeout = -1

    while (!todo.isEmpty && timeout != 0) {
      var newState = todo.head
      todo = todo.tail

      val flat = newState.flat
      val sharp = newState.sharp

      val lastSeenGeneration: Long = seen.getOrElse(flat, 0)

      if (currentGeneration <= lastSeenGeneration) {
        // println("Current generation: " + currentGeneration) // DEBUG
        // println("Last generation: " + lastSeenGeneration) // DEBUG
        // println("Not a new state: " + newState) // DEBUG
      }

      if (currentGeneration > lastSeenGeneration) {
        // globalSharp changed since we last saw this flat.
        if (timeout > 0)
          timeout -= 1

        count += 1

        if (this.printStates)
          dumpFileWriter.write("State: " + newState + "\n\n")

        // Install the global sharp:
        newState = (newState.sharp = globalSharp)

        //println("State:\n" + newState + "\n")

        // Reset the changeLog:
        newState = (newState.sharp = newState.sharp.resetChangeLog)

        // Explore successors:
        val succs = next(newState)

        // Mark this state as being seen with the current generation.
        seen(newState.flat) = currentGeneration

        // Check each successor for change:
        for (succ <- succs) {
          var sharp = succ.sharp
          var delta = sharp.changeLog
          if (!delta.isEmpty) {
            // Something changed!

            // Bump up the current sharp generation:
            currentGeneration += 1

            // Apply sharp changes to the global store:
            globalSharp = delta(globalSharp)
          }
          todo = todo ++ succs
        }
      }
    }

    if (timeout == 0)
      System.err.println("Timeout reached")

    if (printStates) {
      dumpFileWriter.close()
    }
  }
}

/* Flow analyses */

abstract class AbstractCFA_CPS extends SmallStepAbstractInterpretation {

  val botD: D;

  def applyAtomicPrimitive(prim: Prim, params: Parameters): D = {
    prim match {
      case _ if !prim.invocationMayPerformIO && !prim.invocationMayMutate && !prim.invocationMayAllocate => botD
      case _ => throw new Exception("Unknown primitive: " + prim)
    }
  }

}


abstract class AbstractLinkedCFA_CPS extends AbstractCFA_CPS {

}

