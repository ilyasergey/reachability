/*
 * Copyright (c) 2015,
 * Ilya Sergey, Christopher Earl, Matthew Might and David Van Horn
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *  Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 *  Neither the name of the project "Reachability" nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

