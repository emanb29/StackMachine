import jdk.nashorn.internal.codegen.CompilationException

object StackMachineLanguage {
  trait StackMachineError extends Exception {
    val msg: String
  }

  case class ExecutionError(msg: String) extends RuntimeException(msg) with StackMachineError

  case class CompilationError(msg: String) extends StackMachineError

  case class VarName(value: String)

  trait Instruction

  case class CONST(n: Int) extends Instruction

  case class LOAD(variable: VarName) extends Instruction

  case class STORE(variable: VarName) extends Instruction

  case object MINUS extends Instruction

  case object PLUS extends Instruction

}
