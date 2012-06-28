package org.ucombinator.lambdajs.parsing

import org.ucombinator.lambdajs.parsing.LambdaJSTokens._
import org.ucombinator.lambdajs.syntax.LJSyntax._


/**
 * @author ilya
 */

class LambdaJSParser {

  private var maxSerialNumber = 1

  def newStamp(): Int = {
    maxSerialNumber += 1
    maxSerialNumber
  }

  type Result[T] = (Either[T, String], List[LJToken])

  def success[T](e: T, rest: List[LJToken]): Result[T] = (Left(e), rest)

  def failure[T](s: String, rest: List[LJToken]): Result[Nothing] = {
    (Right(s), rest)
  }

  val RESERVED_NAMES: Set[String] = Set(
    "$makeException", "@newDirect"
  )

  def isBound(s: String, map: Map[String, Var]): Boolean = map.keySet.contains(s)


  def isFailure[T](r: Result[T]) = r match {
    case (Right(_), _) => true
    case _ => false
  }

  /**
   * Close parenthesis
   */
  def closePar[T](pair: (Either[T, String], List[LJToken])): Result[T] = pair match {
    case (r, rest) if r.isRight => pair
    case (r, rest) => rest match {
      case RPar :: tail => (r, tail)
      case x :: tail => {
        val msg = "')' expected, but " + x.toString + " is found"
        failure(msg, rest)
      }
      case Nil => {
        val msg = "')' expected, but input is empty"
        failure(msg, rest)
      }
    }
  }

  def extract[T](r: Result[T]): (T, List[LJToken]) = r match {
    case (Left(t: T), rest) => (t, rest)
  }

  def getRest[T](r: Result[T]) = r._2

  def params(input: List[LJToken]): Result[List[String]] = input match {
    case LPar :: rest => {
      val (pre, rest1) = rest.span(_.isInstanceOf[TIdent])
      success(pre.map {
        case TIdent(s) => s
      }, rest1)
    }
    case _ => failure("'(' expected", input)
  }


  def lambda(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val psResult = closePar(params(input))
    if (isFailure(psResult)) {
      failure("Parameters parsing failed", getRest(psResult))
    } else {
      val (ids, rest) = extract(psResult)
      val vars = ids.map {
        case s => Var(s, newStamp())
      }
      val newBound = vars.filter(_.stamp > 0).map(v => (v.name, v)).toSet

      val bodyResult = exp(rest, bv ++ newBound)
      if (isFailure(bodyResult)) {
        bodyResult
      } else {
        val (body, rest1) = extract(bodyResult)
        success(Fun(vars, body, newStamp()), rest1)
      }
    }
  }

  def entry(input: List[LJToken], bv: Map[String, Var]): Result[(String, Exp)] = input match {
    case Nil => failure("No entry", input)
    case LPar :: TString(s) :: rest => {
      val result = exp(rest, bv)
      if (isFailure(result)) {
        failure("Entry expression failed", getRest(result))
      } else {
        val (e, rest1) = extract(result)
        closePar(success((s, e), rest1))
      }
    }
  }

  def record(input: List[LJToken], bv: Map[String, Var]): Result[Record] = {
    var rest = input
    var ets: List[(String, Exp)] = List()
    while (!rest.isEmpty && rest.head == LPar &&
      !rest.tail.isEmpty && rest.tail.head.isInstanceOf[TString]) {
      val result = entry(rest, bv)
      if (isFailure(result)) {
        return failure("Entry parsing failed", getRest(result))
      } else {
        val (ent, rr) = extract(result)
        ets = ent :: ets
        rest = rr
      }
    }
    success(Record(ets.reverse, newStamp()), rest)
  }

  def isReservedName(name: String): Boolean = {
    RESERVED_NAMES.contains(name)
  }


  def binding(input: List[LJToken], bv: Map[String, Var]): Result[(Var, Exp)] = input match {
    case Nil => failure("No binding", input)
    case LPar :: TIdent(name) :: rest => {
      val result = exp(rest, bv)
      if (isFailure(result)) {
        failure("Binding expression failed", getRest(result))
      } else {
        val (e, rest1) = extract(result)
        val binder = if (isReservedName(name)) {
          Var(name, 0)
        } else {
          Var(name, newStamp())
        }
        closePar(success((binder, e), rest1))
      }
    }
  }

  def let(in: List[LJToken], bv: Map[String, Var]): Result[Exp] = in match {
    case LPar :: input => {
      var rest = input
      var bindings: List[(Var, Exp)] = List()
      while (!rest.isEmpty && rest.head == LPar &&
        !rest.tail.isEmpty && rest.tail.head.isInstanceOf[TIdent]) {
        val result = binding(rest, bv)
        if (isFailure(result)) {
          return failure("Binding parsing failed", getRest(result))
        } else {
          val (bind, rr) = extract(result)
          bindings = bind :: bindings
          rest = rr
        }
      }

      // done with bindings
      val newBound = bindings.map(_._1).filter(_.stamp > 0).map(v => (v.name, v)).toSet

      rest match {
        case RPar :: rest1 => {
          val eres = exp(rest1, bv ++ newBound)
          if (isFailure(eres)) {
            failure("Faled body parsing expression", input)
          } else {
            val (body, rest2) = extract(eres)
            val letResult = bindings.foldRight(body) {
              case ((x, e), acc) => Let(x, e, acc, newStamp())
            }
            success(letResult, rest2)
          }
        }
        case _ => failure("')' expected", rest)
      }
    }
    case _ => failure("'(' expected", in)
  }


  def twoExpressions(input: List[LJToken], bv: Map[String, Var]): Result[(Exp, Exp)] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e1, rest) = extract(result)
      val result2 = exp(rest, bv)
      if (isFailure(result2)) {
        failure("Faled parsing expression", rest)
      } else {
        val (e2, rest2) = extract(result2)
        success((e1, e2), rest2)
      }
    }
  }

  def threeExpressions(input: List[LJToken], bv: Map[String, Var]): Result[(Exp, Exp, Exp)] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e1, rest) = extract(result)
      val result2 = exp(rest, bv)
      if (isFailure(result2)) {
        failure("Faled parsing expression", rest)
      } else {
        val (e2, rest2) = extract(result2)
        val result3 = exp(rest2, bv)
        if (isFailure(result2)) {
          failure("Faled parsing expression", rest)
        } else {
          val (e3, rest3) = extract(result3)
          success((e1, e2, e3), rest3)
        }
      }
    }
  }


  def ref(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e, rest) = extract(result)
      success(Ref(e, newStamp()), rest)
    }
  }

  def deref(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e, rest) = extract(result)
      success(Deref(e, newStamp()), rest)
    }
  }

  def pthrow(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e, rest) = extract(result)
      success(Throw(e, newStamp()), rest)
    }
  }


  def asgn(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(Asgn(ee._1, ee._2, newStamp()), rest)
    }
  }

  def lookup(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(Lookup(ee._1, ee._2, newStamp()), rest)
    }
  }

  def delete(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(Del(ee._1, ee._2, newStamp()), rest)
    }
  }

  def label(lab: String, input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e, rest) = extract(result)
      success(Labelled(lab, e, newStamp()), rest)
    }
  }

  def break(lab: String, input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (e, rest) = extract(result)
      success(Break(lab, e, newStamp()), rest)
    }
  }

  def seq(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(Seq(ee._1, ee._2, newStamp()), rest)
    }
  }

  def pwhile(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(While(ee._1, ee._2, newStamp()), rest)
    }
  }

  def tryCatch(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv) //parse try-catch and finally as two exprs
    if (isFailure(result)) {
      failure("Faled parsing try-catch of finally in try-catch-finally", input)
    } else {
      val ((e1, e2), rest) = extract(result)
      e2 match {
        case Fun(List(x), body, stamp) => success(TryCatch(e1, x, body, stamp), rest)
        case notLambda => {
          failure("Catch-clause is not well-formed lambda: " + notLambda, input)
        }
      }
    }
  }

  def tryFinally(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = twoExpressions(input, bv) //parse try-catch and finally as two exprs
    if (isFailure(result)) {
      failure("Faled parsing try-catch of finally in try-catch-finally", input)
    } else {
      val ((e1, e2), rest) = extract(result)
      e1 match {
        case TryCatch(_, _, _, _) => success(TryFinally(e1, e2, newStamp()), rest)
        case notCatch => {
          failure("Catch-part is not well-formed: " + notCatch, input)
        }
      }
    }
  }

  def pif(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = threeExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(If(ee._1, ee._2, ee._3, newStamp()), rest)
    }
  }

  def update(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = threeExpressions(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing expression", input)
    } else {
      val (ee, rest) = extract(result)
      success(Update(ee._1, ee._2, ee._3, newStamp()), rest)
    }
  }

  def repExp(input: List[LJToken], bv: Map[String, Var]): Result[List[Exp]] = {
    var exprs: List[Exp] = List()
    var rest = input

    def loop() {
      val result = exp(rest, bv)
      if (!isFailure(result)) {
        val (e, rest1) = extract(result)
        exprs = e :: exprs
        rest = rest1
        loop()
      }
    }
    loop()
    success(exprs.reverse, rest)
  }

  def opApp(op: String, input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = repExp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing several expressions", input)
    } else {
      val (ee, rest) = extract(result)
      val stamp = newStamp()
      success(OpApp(Op(op, stamp), ee, stamp), rest)
    }
  }

  def app(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    val result = exp(input, bv)
    if (isFailure(result)) {
      failure("Faled parsing callee", input)
    } else {
      val (e, rest) = extract(result)
      val result1 = repExp(rest, bv)
      if (isFailure(result1)) {
        failure("Faled parsing arguments", input)
      } else {
        val (args, rest1) = extract(result1)
        success(App(e, args, newStamp()), rest1)
      }
    }
  }

  val eval_bomb = "eval-semantic-bomb"

  /**
   * Parse complex expressions
   */
  def realExp(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = input match {
    case Nil => failure("Undexpected end of input", input)
    case TLambda :: rest => lambda(rest, bv)
    case TRec :: rest => record(rest, bv)
    case TRef :: rest => ref(rest, bv)
    case TDeref :: rest => deref(rest, bv)
    case TThrow :: rest => deref(rest, bv)
    case TAsgn :: rest => asgn(rest, bv)
    case TIndex :: rest => lookup(rest, bv)
    case TWhile :: rest => pwhile(rest, bv)
    case TTryCatch :: rest => tryCatch(rest, bv)
    case TTryFinally :: rest => tryFinally(rest, bv)
    case TSeq :: rest => seq(rest, bv)
    case TDel :: rest => delete(rest, bv)
    case TIf :: rest => pif(rest, bv)
    case TUpdate :: rest => update(rest, bv)
    case TBreak :: TIdent(l) :: rest => break(l, rest, bv)
    case TLabel :: TIdent(l) :: rest => label(l, rest, bv)
    case TOp(op) :: rest => opApp(op, rest, bv)
    case TLet :: rest => let(rest, bv)
    case TIdent(_) :: _ => app(input, bv)
    case LPar :: _ => app(input, bv)
    case _ => {
      failure("Wrong input!", input)
    }
  }

  def exp(input: List[LJToken], bv: Map[String, Var]): Result[Exp] = {
    input match {
      case Nil => failure("Undexpected end of input", input)
      case h :: t => h match {
        case TString(s) => success(EString(s), t)
        case TFloat(f) => success(EFloat(f), t)
        case TTrue => success(EBool(true), t)
        case TFalse => success(EBool(false), t)
        case TUndef => success(EUndef, t)
        case TNull => success(ENull, t)
        case TOp("eval-semantic-bomb") => success(EEval, t)
        case TNan => success(ENan, t)
        case TInfP => success(EInfP, t)
        case TInfM => success(EInfM, t)

        case TIdent(id) => {
          val v = if (isBound(id, bv)) {
            bv(id)
          } else if (isReservedName(id)) {
            Var(id, 0)
          } else {
            System.err.println("Unbound variable: " + id)
            Var(id, 0)
          }
          success(v, t)
        }
        case LPar => {
          closePar(realExp(t, bv))
        }
        case _ => failure("NYI", input)
      }
    }
  }

  def parseAll(input: List[LJToken]): Exp = {
    val (result, rest) = exp(input, Map.empty)
    if (result.isLeft && rest.isEmpty) {
      val Left(r) = result
      r
    } else {
      throw new Exception("Parsing failed on a token " + rest.head)
    }
  }

  def parseText(text: String): Exp = {
    val lexer = new LambdaJSLexer
    val input = lexer.parseAll(text)
    val (result, rest) = exp(input, Map.empty)
    if (result.isLeft && rest.isEmpty) {
      val Left(r) = result
      r
    } else {
      throw new Exception("Parsing failed on a token " + rest.head)
    }
  }

  def parseAllIn(filename: String): Exp = {

    System.err.println("Reading file...")

    val firstTime = (new java.util.Date()).getTime
    val lexer = new LambdaJSLexer
    val input = lexer.parseAllIn(filename)
    val secondTime = (new java.util.Date()).getTime
    val delta = secondTime - firstTime

    System.err.println("Input read in " + delta + " ms.")


    parseAll(input)
  }

}
