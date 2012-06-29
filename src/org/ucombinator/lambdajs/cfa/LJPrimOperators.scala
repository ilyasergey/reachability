package org.ucombinator.lambdajs.cfa

import org.ucombinator.lambdajs.syntax.LJSyntax

/**
 * @author ilya
 */

trait LJPrimOperators extends LJFrames with LJSyntax {
  def delta(op: String, values: List[Value]): Set[Value] = op match {

    case "typeof" => values match {
      case Nil => opUndefined(op, values)
      case UndefValue :: Nil => set(StringValue("undefined"))
      case NullValue :: Nil => set(StringValue("undefined"))
      case AddrValue(_) :: Nil => set(StringValue("location"))
      case RecValue(_) :: Nil => set(StringValue("object"))
      case FunValue(_, _) :: Nil => set(StringValue("lambda"))
      case BoolValue(_) :: Nil => set(StringValue("boolean"))
      case (_: AbstractStringValue) :: Nil => set(StringValue("string"))
      case (_: AbstractNumValue) :: Nil => set(StringValue("number"))
      case _ => opUndefined(op, values)
    }

    case "===" => values match {
      case x :: y :: Nil if x == y => set(BoolValue(true))
      case UndefValue :: y :: Nil => Set(BoolValue(true), BoolValue(false))
      case x :: UndefValue :: Nil => Set(BoolValue(true), BoolValue(false))
      case _ => set(BoolValue(false))
    }

    case "prim->string" => values match {
      case (s@StringValue(_)) :: Nil  => set(s)
      case _ => opUndefined(op, values)
    }

    case o => {
      opUndefined(o, values)
    }
  }

  private def set[T](t: T): Set[T] = Set(t)

  def opUndefined(o: String, values: List[Value]): Set[Value] = {
    System.err.println("Undefined operation " + o + ", arguments: " + values)
    Set(UndefValue)
  }

}
