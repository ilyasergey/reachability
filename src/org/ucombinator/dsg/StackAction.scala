package org.ucombinator.dsg

/**
 * @author ilya
 */

/********************************************************************
 * Stack-action markers
 ********************************************************************/
abstract sealed class StackAction[+F]

// Stack unchanged
case object Eps extends StackAction[Nothing]

// Push a frame
case class Push[F](frame: F) extends StackAction[F]

// Pop a frame
case class Pop[F](frame: F) extends StackAction[F]

// Pop one frame, push another one
case class Switch[F, S](popped: F, state: S, pushed: F) extends StackAction[F]

object StackActionKind extends Enumeration {
  type StackActionKind = Value
  val Eps, Pop, Push = Value
}

