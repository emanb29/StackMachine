import StackMachineLanguage._

object Driver extends App {

  val demo0 = // c = 3 - a + (b - 7)
    List(
      CONST(3),
      LOAD(VarName("a")),
      MINUS,
      LOAD(VarName("b")),
      CONST(7),
      MINUS,
      PLUS,
      STORE(VarName("c"))
    )
  val initState0: PartialFunction[VarName, Int] = Map(VarName("a") -> 100, VarName("b") -> 200)

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
    """.stripMargin
  val demo01Parsed = Parser.doParse(demo01)
  val result = Interpreter.execute(demo01Parsed, initState0)
  println(result(VarName("a")))
  println(result(VarName("b")))
  println(result(VarName("c")))
  println(demo0 == demo01Parsed)
}
