import StackMachineLanguage._

object Driver extends App {

  val demo0 = // c = 3 - a + (b - 7)
    List(
      CONST(3),
      LOAD("a"),
      MINUS,
      LOAD("b"),
      CONST(7),
      MINUS,
      PLUS,
      STORE("c")
    )
  val initState0 = Map[VarName, Number](VarName("a") -> 100, VarName("b") -> 200)

  //  val result: PartialFunction[VarName, Int] = Interpreter.execute(demo0, initState0)

  val demo01 =
    """
      CONST(3)
      LOAD(a)
      MINUS
      LOAD(b)
      CONST(7)
      MINUS
      PLUS
      STORE(c)
    """
  val demo01Parsed = Parser.doParse(demo01)
  val result = Interpreter.execute(demo01Parsed, initState0)
  println(result("a"))
  println(result("b"))
  println(result("c"))
  println(demo0 == demo01Parsed)
}
