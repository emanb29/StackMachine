import jdk.nashorn.internal.codegen.CompilationException

object StackMachineLanguage {

  trait StackMachineError extends Exception {
    val msg: String
  }

  case class ExecutionError(msg: String) extends RuntimeException(msg) with StackMachineError

  case class CompilationError(msg: String) extends StackMachineError

  sealed abstract class AST

  sealed abstract class Instruction extends AST

  sealed abstract class Instruction0() extends Instruction

  sealed abstract class Instruction1(id: Id) extends Instruction

  case object MINUS extends Instruction0

  case object PLUS extends Instruction0

  case class CONST(n: Number) extends Instruction1(n)

  case class LOAD(varName: VarName) extends Instruction1(varName)

  case class STORE(varName: VarName) extends Instruction1(varName)

  abstract class Id extends AST

  case class Number(n: Int) extends Id {
    override def toString: String = n.toString

    def +(rhs: Number): Number = n + rhs.n

    def -(rhs: Number): Number = n + -rhs.n
  }

  object Number {
    implicit def intToNumber(n: Int): Number = Number(n)

    implicit def numToInt(n: Number): Int = n.n
  }

  case class VarName(name: String) extends Id

  object VarName {
    implicit def strToVarName(name: String): VarName = VarName(name)
  }

}
