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

package org.ucombinator.scheme.compile

import scala.collection.immutable.{Map => ImmMap, SortedMap, TreeMap, SortedSet, TreeSet}

import org.ucombinator.scheme.syntax._
import org.ucombinator.scheme.transform._
import org.ucombinator.scheme.cfa.mcfa._
import org.ucombinator.scheme.parsing.RnRSParser

/**
Emits C code for closure-converted, lifted CPS.
 */

class CPSEmitC {

  var prog: Program = null;

  private def mangle(string: String): String = {
    var s = "";
    for (c <- string) {
      // println("mangling: " + c)  // DEBUG
      // println("isLoD:    " + Character.isLetterOrDigit(c)) // DEBUG
      s += (c match {
        case _ if Character.isLetterOrDigit(c) => c.toString
        case _ => {
          // println("MANGLING") // DEBUG
          "_" + char2int(c)
        }
      })
    }
    // println("mangle: " + name + " => " + s) // DEBUG
    s
  }

  private def mangle(name: SName): String = mangle(name.toString)

  private def mangle(prefix: String, name: SName): String =
    prefix + mangle(name.toString)

  private def mangle(kw: SKeyword): String = "__kw_" + mangle(kw.string)

  private def emit(s: String) {
    print(s)
  }


  private def emitln(s: String) {
    emit(s);
    emit("\n");
  }

  private def emitDeclarations(prog: Program) {
    emitln("\n /* Declarations: */ ");
    for (v <- prog.variables) {
      emitln(" Value " + mangle(v) + ";");
    }
    emitln("");
    /*
    prog match {
      case Program(List(), defs, init) => {
        for (d <- defs) {
          emitln(" Value " + mangle(d.name) + ";") ;
        }
      }
    }
    */
  }

  private def emitInitialization(prog: Program) {
    // Emit non-lambda global variable initialization code.

    // Emit jump to init.
    emitln(" goto __INIT ;");
  }

  private def emitProgramBody(prog: Program) {
    // Emit labels.

    emitln("\n /* Program body: */");
    prog match {
      case Program(_, defs, init) => {
        for (d <- defs) {
          d match {
            case VarDef(name, lam: Lambda) => {
              emitln("\n " + mangle(name) + ": ");
              emitLambda(lam);
            }
            case _ => {
              // Should be emitted elsewhere.
            }
          }
        }

        emitln("\n __INIT: ");
        emitCall(init);
        emitln(" return 0 ;");
      }
    }
  }

  private def emitLambda(lam: Lambda) = {
    lam match {
      case Lambda(formals, Body(List(), List(call))) => {
        emitFormals(formals);
        emitCall(call);
      }
    }
  }

  private def emitFormals(formals: Formals) = {
    formals match {
      case Formals(forms, rest) => {
        var i = 1;

        for (f <- forms) {
          f match {
            case PosFormal(name) => {
              emitln("  " + mangle(name) + " = " + "__a" + i + ";")
              i = i + 1;
            }
            case KeywordFormal(kw, name) => {
              emitln("  " + mangle(name) + " = " + mangle(kw) + ";")
            }
          }
        }
        if (!rest.isEmpty) {
          throw new Exception("Handle rest parameters")
        }
      }
    }
  }


  private def emitCall(call: Exp) {
    // Emit a call.
    call match {
      case App(Ref(f), args) => {
        // We can assume f is a top-level name.
        emitArguments(args);
        emitln("  goto " + mangle(f) + " ;");
      }

      case Call(clo, key, args) => {
        emitArguments(args);
        emitln("  " + mangle(key) + " = " + compile(clo) + ";");
        emitln("  goto *" + mangle(key) + ".asClosure.data->fn;")
      }

      // BUG: Should be first.mustReturn
      case Sequence(first, rest) if first.isPure && first.mustReturnOrFail => {
        emitCall(rest);
      }

      case Sequence(first, rest) => {
        emitCall(first);
        emitCall(rest);
      }

      case Let1(name, value, ExpBody(call2)) => {
        emitAssignment(name, value);
        emitCall(call2);
      }

      case SetVar(name, value) => {
        emitln("  " + mangle(name) + " = " + compile(value) + ";");
      }

      case App(Prim("set-cell!", false), Arguments(List(PosArgument(cell), PosArgument(value)), None)) => {
        emitln("  *(" + compile(cell) + ").asCell.value = " + compile(value) + ";");
      }

      // Side-effecting primitives:
      case App(Prim("display", false), Arguments(List(PosArgument(v)), None)) => {
        emitln("  __display(" + compile(v) + ");");
      }

      case App(Prim("newline", _), Arguments(List(), None)) => {
        emitln("  printf(\"\\n\");");
      }

      case _ if call.isPure && call.mustReturnOrFail => {
        // End the program.
        emitln("  return 0; ");
      }

      case App(f, args) =>
        throw new Exception("Can't translate application of " + f.getClass() + ";\n in " + call + "\n with: " + args)

      case _ => {
        throw new Exception("Can't translate call: " + call)
      }
    }
  }

  def emitAssignment(name: SName, value: Exp) {
    emitln("  " + mangle(name) + " = " + compile(value) + ";");
  }

  def emitArguments(arguments: Arguments) {
    arguments match {
      case Arguments(args, rest) => {
        var i = 1;
        for (a <- args) {
          a match {
            case PosArgument(e) => {
              emitln("  __a" + i + " = " + compile(e) + ";")
              i = i + 1
            }
            case KeywordArgument(kw, e) => {
              emitln("  " + mangle(kw) + " = " + compile(e) + ";")
            }
          }
          rest match {
            case None => {}
            case Some(_) =>
              throw new Exception("cannot compile rest: " + rest)
          }
        }
      }
    }
  }

  def emitArgumentRegisters(prog: Program) {
    for (i <- 1 to 10) {
      emitln(" Value __a" + i + ";")
    }
    for (kw <- prog.keywords) {
      emitln(" Value " + mangle(kw) + ";");
    }
    //for (v <- prog.variables) {
    //emitln(" Value " + mangle(v) + ";") ;
    //}
    emitln(" Value __arest;"); // Any rest parameters.
    emitln(" int __an ;"); // Number of arguments passed.
  }

  def compile(exp: Exp): String = {
    exp match {
      case Unspecified() =>
        //"MakeUnspecified()"
        "Unspecified"

      case Ref(name) =>
        mangle(name)

      case SelfLit(SInt(n)) =>
        "MakeInt(" + n.toString + ")"

      case SelfLit(SText(s)) =>
        // BUG: Add escapes.
        "MakeCString(\"" + s.toString + "\")"

      case QuoteLit(name: SName) =>
        // BUG: Add escapes.
        "MakeSymbol(\"" + name.string + "\"," + name.string.length + ")"

      case Closure(Ref(name), ty, List()) =>
        "MakeClosure(&&" + mangle(name) + ",0)"

      case Closure(Ref(name), ty, exps) =>
        "MakeClosure(&&" + mangle(name) + "," + exps.length + "," + ((exps map compile) mkString ", ") + ")"

      case StructGet(base, field, ty) => {
        prog.typeOf(ty) match {
          case ClosureStruct(fields) => {
            val index = fields.indexOf(field)
            compile(base) + ".asClosure.data->values[" + index + "]"
          }
          case StrictStruct(fields) => {
            val index = fields.indexOf(field)
            compile(base) + ".asStruct.data->values[" + index + "]"
          }
          case t => {
            throw new Exception("Unhandled type: " + ty + "/" + t)
          }
        }
      }

      case MakeStruct(NamedType(name), values) =>
        "MakeStruct(" + mangle("ty_", name) + "," + values.length + "," + ((values map compile) mkString ", ") + ")"

      case MakeCell(value) =>
        "MakeCell(" + compile(value) + ")"

      case CellGet(cell) =>
        "(*" + compile(cell) + ".asCell.value)"

      case App(Prim("+", false), Arguments(List(PosArgument(a), PosArgument(b)), None)) =>
        "MakeInt(" + compile(a) + ".asInteger.value + " + compile(b) + ".asInteger.value)"

      case App(TypePredicate(NamedType(ty)), Arguments(List(PosArgument(exp)), None)) =>
        "MakeBoolean(" + (mangle("ty_", ty)) + "==" + compile(exp) + ".tag" + ")"

      case _ =>
        throw new Exception("Cannot compile " + exp)
    }
  }

  // BUG: Calculate this based on the number of structs.
  var STRUCT_START = 1 << 10;

  private var typeCodes: scala.collection.mutable.Map[SName, Int] = null

  private def emitTypeCodes() {
    typeCodes = scala.collection.mutable.HashMap[SName, Int]()

    var i = 0
    prog match {
      case Program(decs, defs, init) => {
        for (d <- decs) {
          d match {
            case TypeDec(name, ty) => {
              emitln("unsigned int " + mangle("ty_", name) + " = " + (STRUCT_START + i) + ";");
              i = i + 1
            }
          }
        }
      }
    }
  }


  def apply(prog: Program) {
    this.prog = prog;

    emitln("#include \"scheme-cps.h\"\n\n");

    emitln("/* The following breaks with more than 2^9 structs. */");
    emitTypeCodes();

    emitln("\n")

    emitln("int main(int argc, char* argv[]) { ");
    emitArgumentRegisters(prog);
    emitDeclarations(prog);
    emitln(" InitializeRuntime();");
    emitInitialization(prog);
    emitProgramBody(prog);
    emitln("}");
  }
}


object CCompile {

  private def write(file: java.io.File, contents: String) {
    val fw = new java.io.FileWriter(file);
    fw.write(contents);
    fw.close()
  }

  private def write(fileName: String, contents: String) {
    write(new java.io.File(fileName), contents)
  }

  def main(args: Array[String]) {

    /*

     Passes:

     + Preamblification
     + Alphatization
     + Desugaring
     + Primitive transformation (*)
     + A-normalization
     + Mutable-variable elimination
     + Exception elimination (*)
     + CPS conversion
     + Closure conversion
     + Allocation normalization
     + Global value lifting
     + Type normalization
     + Emission

     */

    val filename = args(0)

    var sexps: List[SExp] = Nil

    if (filename == "-") {
      sexps = SExp.parseAll(scala.io.Source.fromInputStream(System.in).getLines mkString "")
    } else {
      sexps = SExp.parseAllIn(filename)
    }

    // println("input:\n" + (sexps mkString "\n"))

    var ast = RnRSParser(sexps)
    write("tmp/original.scm", ast.toString);

    val preamblifier = new Preamblifier
    ast = preamblifier(ast)
    write("tmp/preamblified.scm", ast.toString);

    val desugarer = new Desugarer(false)
    ast = desugarer(ast)
    write("tmp/desugared.scm", ast.toString);

    val alphatizer = new Alphatizer
    ast = alphatizer(ast)
    write("tmp/alphatized.scm", ast.toString);

    val anormalizer = new ANormalizer()
    ast = anormalizer(ast)
    write("tmp/a-normalized.scm", ast.toString);

    val immutator = new MutableVariableEliminator()
    ast = immutator(ast)
    write("tmp/immutated.scm", ast.toString);

    val cpsConverter = new CPSConverter()
    ast = cpsConverter(ast)
    write("tmp/cps-converted.scm", ast.toString);

    val CFA = new KCFA_CPS(ast, new MapBEnv(TreeMap()), KTime(List()), new MapStore(), new SortedSetD())
    CFA.runWithGlobalSharp(filename)

    val closureConverter = new FlatClosureConverter()
    ast = closureConverter(ast)
    write("tmp/closure-converted.scm", ast.toString);

    val allocNormalizer = new ANormalizer()
    allocNormalizer.atomicsCanAllocate = false
    ast = allocNormalizer(ast)
    write("tmp/alloc-normalized.scm", ast.toString);

    val lifter = new Lifter()
    ast = lifter(ast)
    write("tmp/lifted.scm", ast.toString);

    val typeNormalizer = new TypeNormalizer()
    ast = typeNormalizer(ast)
    write("tmp/type-normalized.scm", ast.toString);

    val emitter = new CPSEmitC()
    emitter(ast)

  }
}


