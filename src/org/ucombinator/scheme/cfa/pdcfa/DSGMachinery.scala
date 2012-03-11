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
import org.ucombinator.util.FancyOutput
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
  self: AnalysisRunner with IPDSMachinery with StackCESKMachinery with FancyOutput =>

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

  def iterateDSG(dsg: DSG, helper: DSGHelper): (DSG, DSGHelper) = dsg match {
    case DSG(ss, ee, s0) => {
      val newNodesAndEdges = for {
        s <- ss
        kont <- helper.getRequiredKont(s, s0)
        possibleFrames = helper.getPossibleStackFrames(s)
        (g, s1) <- stepIPDS(s, kont, possibleFrames)
      } yield (s1, Edge(s, g, s1))

      val (obtainedStates, obtainedEdges) = newNodesAndEdges.unzip

      val newEdges = obtainedEdges -- ee
      val newHelper = updateHelper(helper, newEdges, ee)
      // S' = ...
      val ss1: Nodes = ss ++ obtainedStates + s0

      // E' = ...
      val ee1 = (ee ++ newEdges)


      println(progressPrefix + " Dyck state graph: " + ss1.size + " nodes and " + ee1.size + " edges.")

      // return updated graph
      (DSG(ss1, ee1, s0), newHelper)
    }
  }

  /**
   * Compute the leas-fixed point by Kleene iteration
   */
  def evaluateDSG(e: Exp) = {
    val initial = initState(e)
    val initS = initial._1

    // Compute the LFP(iterateDSG) recursively
    def eval(first: DSG, next: DSG, helper: DSGHelper): (DSG, DSGHelper) = {
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
    val firstHelper = DSGHelper.empty
    val (nextDSG, nextHelper) = iterateDSG(firstDSG, firstHelper)

    val (resultDSG, _) = eval(firstDSG, nextDSG, nextHelper)
    resultDSG
  }

  /**
   * A helper class to construct
   * 1. 'top' stack frames for IPDS computations for a given state
   * 2. Sets of possible frames for a given state
   */
  sealed case class DSGHelper(epsPreds: S :-> Nodes,
                              topFrames: S :-> Set[Frame],
                              nonEpsPreds: S :-> Nodes) {

    /**
     * For a given control state s, return a set of control states s'
     * such that s' --g--> s and [g] = eps
     * @param s - given state in DSG
     */
    def getEpsPredStates(s: S): Nodes = epsPreds.getOrElse(s, Set())

    /**
     * Get possible top frames for a given control state s
     */
    private def getTopFrames(s: S): Set[Frame] = {
      val ownFrames: Set[Frame] = topFrames.getOrElse(s, Set())
      val epsPredTopFrames: Set[Frame] = getEpsPredStates(s).flatMap(x => topFrames.getOrElse(x, Set()))

      // top frame - pushed when coming to this state or any of its eps-predecessors
      val result = ownFrames ++ epsPredTopFrames
      result
    }

    /**
     * Constructs a fake continuation with only a top frame (if any)
     */
    def getRequiredKont(s: S, s0: S): Set[Kont] = {
      val topFrames = getTopFrames(s)
      if (topFrames.isEmpty) {
        Set(List())
      } else {
        val collected = topFrames.map(f => List(f))
        if (mustHaveOnlyEmptyContinuation(s)) {
          Set(List())

          /**
           * (REMARK)
           * [Valid final candidate]
           * Should carry a value and be epsilon-reachable
           * from the initial state
           */
        } else if (canHaveEmptyContinuation(s)
          && (getEpsPredStates(s) ++ Set(s)).contains(s0)) {
          collected + List()
        } else {
          collected
        }
      }
    }

    /**
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
      var frames = workSet.flatMap(getTopFrames(_))

      // get non-eps preds
      val neps = workSet.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
      val toProcess = neps ++ neps.flatMap(getEpsPredStates(_))
      var newWorkSet: Nodes = workSet ++ toProcess

      def iterate(delta: Nodes) {
        if (!workSet.equals(newWorkSet)) {
          // compute new frames
          frames = frames ++ delta.flatMap(getTopFrames(_))
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
  }


  object DSGHelper {
    def empty: DSGHelper = DSGHelper(Map.empty, Map.empty, Map.empty)
  }

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

  def updateHelper(helper: DSGHelper, newEdges: Set[Edge], oldEdges: Set[Edge]) = {

    val (newPushEdges, newOtherEdges) = newEdges.partition({
      case Edge(_, a, _) => kindOf(a) == StackActionKind.Push
    })

    /************************************************************
     * update eps-predecessors
     * it changes only if newEdges contain only eps- or pop- transitions
     ************************************************************/
    val oldEpsPreds = helper.epsPreds

    val newEpsPreds = newOtherEdges.foldLeft(oldEpsPreds)((accumPreds: S :-> Nodes, e: Edge) => e match {
      /* Taking all edges have one source, only one of the following cases will be
      applicable to all of them */

      // eps-transition
      case Edge(s, Eps, s1) => {
        // s --eps--> s1
        val pps: Nodes = accumPreds.getOrElse(s, Set()) // s' eps-predecessors
        val pps1: Nodes = accumPreds.getOrElse(s1, Set()) // s1's eps-predecessors

        val pps1_joined = (pps1 + s) ++ pps // add s and eps-predecessors of s to eps-preds of s1
        accumPreds + ((s1, pps1_joined))
      }

      // pop-transition, can add new eps-reachable states
      // because of s --gs--> s' and [gs] = eps
      case Edge(s, Pop(g), s1) => {
        // s --Pop(g)--> s'
        val pps1: Nodes = accumPreds.getOrElse(s1, Set()) // s1' eps-predecessors

        // s and its eps-predecessors are g-pushed
        val afterPushingG: Nodes = Set(s) ++ accumPreds.getOrElse(s, Set())
        // Keep only those states that have come to eps-equivalent of "s"
        // by pushing a frame g
        val suitablePushEdges = (oldEdges ++ newEdges).filter {
          case Edge(_, Push(g1), t) => stackActionsEquivalent(g1, g) && afterPushingG.contains(t)
          case _ => false
        }

        // Collect all "previous" states and their eps-predecessors
        val result: Nodes = suitablePushEdges.map({
          case Edge(t, _, _) => t
        }).flatMap(x => Set(x) /* the node before push */)

        val pps1_joined = pps1 ++ result
        accumPreds + ((s1, pps1_joined))
      }
      // push-transition doesn't add new eps-predecessors
      case _ => throw new DSGException("Unexpected push edge occurred: " + e.toString)
    })

    /**(REMARK) [Mind transitive closures]
     * Close computed eps-predecessors transitively
     */
    val transitiveEps: S :-> Set[S] = transitiveClosure(newEpsPreds)


    /************************************************************
     * update top-frames (non-closed over eps-transitions)
     * it changes only if newEdges contain only push-transitions
     ************************************************************/
    val oldTopFrames = helper.topFrames
    val newTopFrames = newPushEdges.foldLeft(oldTopFrames)((accumFrames: S :-> Set[Frame], e: Edge) => e match {
      /* Only push-transitions matter */
      case Edge(s, Push(g), s1) => {
        val tps1: Set[Frame] = accumFrames.getOrElse(s1, Set())
        val tps1_joined = tps1 ++ Set(g)
        accumFrames + ((s1, tps1_joined))
      }
      case _ => throw new DSGException("Unexpected non-push edge occurred: " + e.toString)
    })

    /************************************************************
     * update non-eps predecessors (non-closed over eps-transitions)
     * it changes only if newEdges contain only push-transitions
     ************************************************************/
    val oldNonEpsPreds = helper.nonEpsPreds
    val newNonEpsPreds = newPushEdges.foldLeft(oldNonEpsPreds)((accumNonEpsPreds: S :-> Nodes, e: Edge) => e match {
      /* Only push-transitions matter */
      case Edge(s, Push(g), s1) => {
        val nes1: Nodes = accumNonEpsPreds.getOrElse(s1, Set())
        // add a new non-epsilon previous state
        val nes1_joined: Nodes = nes1 ++ Set(s)
        accumNonEpsPreds + ((s1, nes1_joined))
      }
      case _ => throw new DSGException("Unexpected non-push edge occurred: " + e.toString)
    })


    // return an updated helper
    DSGHelper(transitiveEps, newTopFrames, newNonEpsPreds)
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
