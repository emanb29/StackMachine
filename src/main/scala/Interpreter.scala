import scala.annotation.tailrec
import StackMachineLanguage._

object Interpreter {
  /**
    * Execute a StackMachineLanguage program
    *
    * @param program     A program represented in the StackMachineLanguage ADT
    * @param initialVars The initial state of the machine -- key => value sets
    * @return The final state of the machine -- key => value sets
    */
  def execute(program: List[Instruction], initialVars: PartialFunction[VarName, Number]): PartialFunction[VarName, Number] = {
    @tailrec
    def recurse(instructions: List[Instruction], stack: List[Number], vars: PartialFunction[VarName, Number]): PartialFunction[VarName, Number] = instructions match {
      case instruction :: unexecuted => instruction match {
        case CONST(n) => recurse(unexecuted, n :: stack, vars)
        case LOAD(variable) if vars.isDefinedAt(variable) => recurse(unexecuted, vars(variable) :: stack, vars)
        case LOAD(variable) => throw ExecutionError(s"Variable '$variable' is undefined")
        case STORE(variable) if stack.nonEmpty => recurse(unexecuted, stack.tail, {
          case x: VarName if x == variable => stack.head
          case x: VarName => vars(x)
        })
        case STORE(_) => throw ExecutionError("tried to STORE from an empty stack")
        case PLUS =>
          val newStack = stack match {
            case s1 :: s2 :: tail => s2 + s1 :: tail
            case _ => throw ExecutionError("tried to PLUS on a stack with less than 2 elements")
          }
          recurse(unexecuted, newStack, vars)
        case MINUS =>
          val newStack = stack match {
            case s1 :: s2 :: tail => s2 - s1 :: tail
            case _ => throw ExecutionError("tried to MINUS on a stack with less than 2 elements")
          }
          recurse(unexecuted, newStack, vars)
        case _ => throw CompilationError("Interpreter received an invalid instruction -- there was an error in compilation")
      }
      case Nil => vars
    }

    recurse(program, List.empty, initialVars)
  }
}
