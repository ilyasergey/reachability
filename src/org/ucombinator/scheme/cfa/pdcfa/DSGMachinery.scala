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

package org.ucombinator.scheme.cfa.pdcfa

import collection.immutable.Set
import org.ucombinator.scheme.cfa.cesk._
import org.ucombinator.scheme.syntax._

/**
 * Implementation Dyck State Graphs
 * Main types and functions
 *
 * @author ilya
 */

// TODO: make generic for arbitrary states/analyses
trait DSGMachinery extends StateSpace {
  self: AnalysisRunner with IPDSMachinery =>

  /**
   * DSG Nodes
   */
  type S = ControlState
  type Nodes = Set[S]

  /**
   * DSG Edges
   */
  sealed case class Edge(source: S, g: StackAction, target: S)

  type Edges = Set[Edge]

  /**
   * DSG = (S, E, s0) -- a Dyck State Graph
   * implicitly parametrized with ? (a set of frames)
   * s0 \in S -- initial node
   */
  sealed case class DSG(nodes: Set[S], edges: Edges, s0: S)

  /**
   * Monotonic DSG iteration function
   * denoted as 'f' in the paper
   */

  def iterateDSG(dsg: DSG, helper: NewDSGHelper): (DSG, NewDSGHelper) = dsg match {
    case DSG(ss, ee, s0) => {
      val newNodesAndEdges = for {
        s <- ss
        kont <- helper.getRequiredKont(s, s0)
        possibleFrames = helper.getPossibleStackFrames(s)
        (g, s1) <- stepIPDS(s, kont, possibleFrames)
      } yield (s1, Edge(s, g, s1))

      val (obtainedStates, obtainedEdges) = newNodesAndEdges.unzip

      val newEdges = obtainedEdges -- ee
      //val newHelper = updateHelper(helper, newEdges, ee)
      helper.update(newEdges)

      // S' = ...
      val ss1: Nodes = ss ++ obtainedStates + s0

      // E' = ...
      val ee1 = (ee ++ newEdges)


      println(progressPrefix + " Dyck state graph: " + ss1.size + " nodes and " + ee1.size + " edges.")

      // return updated graph
      (DSG(ss1, ee1, s0), helper)
    }
  }

  /**
   * Compute the leas-fixed point by Kleene iteration
   */
  def evaluateDSG(e: Exp) = {
    val initial = initState(e)
    val initS = initial._1

    // Compute the LFP(iterateDSG) recursively
    def eval(first: DSG, next: DSG, helper: NewDSGHelper): (DSG, NewDSGHelper) = {
      if (first == next) {
        (next, helper)
      } else if (interrupt && next.edges.size > interruptAfter) {
        (next, helper)
      } else {
        val (next2, helper2) = iterateDSG(next, helper)
        eval(next, next2, helper2)
      }
    }

    val firstDSG = DSG(Set(initS), Set(), initS)
    val firstHelper = new NewDSGHelper
    val (nextDSG, nextHelper) = iterateDSG(firstDSG, firstHelper)

    val (resultDSG, _) = eval(firstDSG, nextDSG, nextHelper)
    resultDSG
  }

  sealed class NewDSGHelper {

    import scala.collection.mutable.{Map => MMap, HashMap => MHashMap}

    private val epsPreds: MMap[S, Nodes] = new MHashMap
    private val epsSuccs: MMap[S, Nodes] = new MHashMap
    private val topFrames: MMap[S, Set[Frame]] = new MHashMap

    /**
     * Let s1 --[+f]--> s2_1 --> .... --> s2_n --[-f]--> s3
     * Then predForPushFrame((s2_i, f)) contains s1
     */
    private val predForPushFrame: MMap[(S, Frame), Nodes] = new MHashMap
    private val nonEpsPreds: MMap[S, Nodes] = new MHashMap

    ////////////////// Public methods //////////////////

    def update(newEdges: Set[Edge]) {
      for (e <- newEdges) {
        e match {
          case Edge(s1, Eps, s2) => equalize(s1, s2)
          case Edge(s1, Pop(f), s2) => processPop(s1, f, s2)
          case Edge(s1, Push(f), s2) => processPush(s1, f, s2)
        }
      }
    }

    /**
     * Constructs a fake continuation with only a top frame (if any)
     */
    def getRequiredKont(s: S, s0: S): Set[Kont] = {
      val frames = gets(topFrames, s)
      if (frames.isEmpty) {
        Set(List())
      } else {
        if (mustHaveOnlyEmptyContinuation(s)) {
          Set(List())

          /**
           * (REMARK)
           * [Valid final candidate]
           * Should carry a value and be epsilon-reachable
           * from the initial state
           */
        } else if (canHaveEmptyContinuation(s)
          && (getEpsPredStates(s)).contains(s0)) {
          frames.map(f => List(f)) + List()
        } else {
          frames.map(f => List(f))
        }
      }
    }

    /**
     * Necessary for abstract GC
     *
     * (REMARK)
     * [Dyck property exploited]
     * Compute recursively all possible frames that can be
     * somewhere in the stack for a state 's'
     */
    def getPossibleStackFrames(s: S): Kont = {
      if (!shouldGC) {
        // We don't deed it if there is no --gc flag
        return Nil
      }

      // initial -- just top frames
      var workSet: Nodes = Set(s) ++ getEpsPredStates(s)

      // first iteration
      var frames = workSet.flatMap(s => gets(topFrames, s))

      // get non-eps preds
      val neps = workSet.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
      val toProcess = neps ++ neps.flatMap(getEpsPredStates(_))
      var newWorkSet: Nodes = workSet ++ toProcess

      def iterate(delta: Nodes) {
        if (!workSet.equals(newWorkSet)) {
          // compute new frames
          frames = frames ++ delta.flatMap(s => gets(topFrames, s))
          // update old working set
          workSet = newWorkSet
          // compute new states
          val neps1 = delta.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
          val delta1 = neps1 ++ neps1.flatMap(getEpsPredStates(_))
          newWorkSet = workSet ++ delta1

          iterate(delta1)
        }
      }

      iterate(toProcess)
      frames.toList
    }

    ///////////////// Inner methods ////////////////////

    private def getEpsPredStates(s: S): Nodes = gets(epsPreds, s)

    /**
     * "Equalize" eps-predecessors & eps-successors
     * when an eps-transition s1 --[eps]--> s2 is added
     */
    private def equalize(s1: S, s2: S) {
      val preds = Set(s1) ++ gets(epsPreds, s1)
      val nexts = Set(s2) ++ gets(epsSuccs, s2)

      // Add new successors
      for (s <- preds) {
        puts(epsSuccs, s, nexts)
      }

      // Add new predecessors and top frames
      val topFramesToAdd = preds.flatMap(x => gets(topFrames, x))
      for (s <- nexts) {
        puts(epsPreds, s, preds)
        puts(topFrames, s, topFramesToAdd)
        for (f <- gets(topFrames, s1)) {
          val predForPushForS1 = gets(predForPushFrame, (s1, f))
          puts(predForPushFrame, (s, f), predForPushForS1)
        }
      }
    }

    /**
     * Update topFrames and predForPushFrames for a new edge s1 --[+f]--> s2
     */
    private def processPush(s1: S, f: Frame, s2: S) {
      val nexts = Set(s2) ++ gets(epsSuccs, s2)
      for (s <- nexts) {
        puts(topFrames, s, Set(f))
        puts(predForPushFrame, (s, f), Set(s1))
        puts(nonEpsPreds, s, Set(s1))
      }
    }

    /**
     * Update eps-graphs for a new egde s1 --[-f]--> s2
     */
    private def processPop(s1: S, f: Frame, s2: S) {
      val newEpsPreds = gets(predForPushFrame, (s1, f))
      for (s <- newEpsPreds) {
        equalize(s, s2)
      }
    }

    /**
     * Utility function for multimaps
     */
    private def puts[A, B](map: MMap[A, Set[B]], key: A, newVals: Set[B]) {
      val oldVals = map.getOrElse(key, Set())
      val values = oldVals ++ newVals
      map += ((key, values))
    }

    private def gets[A, B](map: MMap[A, Set[B]], key: A): Set[B] = map.getOrElse(key, Set())

  }

  /**************************************************************
   * Some utility methods
   ***************************************************************/

  /**
   * The function exploits the balanced structure of paths in DSG
   * So any "new" stack action cannon affect the status of successor nodes,
   * only "close" predecessors might become epsilon-predecessors.
   */
  def stackActionsEquivalent(g1: Frame, g: Frame): Boolean = {
    (g, g1) match {
      case (SeqFrame(_, _, _, b), SeqFrame(_, _, _, b1))
        if b == b1 => true
      case _ => g == g1
    }
  }

  def transitiveClosure(m: S :-> Set[S]): S :-> Set[S] = {
    val transitive = m.map {
      case (s: S, v: Set[S]) => (s, v ++ v.flatMap(x => m.getOrElse(x, Set())))
    }
    if (transitive != m) {
      transitiveClosure(transitive)
    } else {
      transitive
    }
  }


}

class DSGException(s: String) extends PDCFAException(s)
