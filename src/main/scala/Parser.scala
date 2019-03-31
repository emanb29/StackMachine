import StackMachineLanguage._
import fastparse._, MultiLineWhitespace._

object Parser {
  def PVarName[_: P]: P[VarName] = P(CharIn("a-zA-Z").rep(1).!.map(VarName.apply))

  def PUnaryAppl[_: P]: P[Instruction] = P((PStore | PLoad) ~~ "(" ~/ PVarName ~ ")").map {
    case (
      fnt: (VarName => Instruction),
      varname: VarName
      ) => fnt(varname)
  }

  def PIntAppl[_: P]: P[Instruction] = P((PConst) ~~ "(" ~/ CharIn("0-9").rep.!.map(_.toInt) ~ ")").map {
    case (
      fnt: (Int => Instruction),
      int: Int
      ) => fnt(int)
  }

  def PStore[_: P]: P[VarName => Instruction] = P(StringIn("STORE")).map(_ => STORE.apply(_))

  def PLoad[_: P]: P[VarName => Instruction] = P(StringIn("LOAD")).map(_ => LOAD.apply(_))

  def PConst[_: P]: P[Int => Instruction] = P(StringIn("CONST")).map(_ => CONST.apply(_))

  def PNullaryAppl[_: P]: P[Instruction] = P(PMinus | PPlus)

  def PMinus[_: P]: P[Instruction] = P(StringIn("MINUS")).map(_ => MINUS)

  def PPlus[_: P]: P[Instruction] = P(StringIn("PLUS")).map(_ => PLUS)


  def PStatement[_: P]: P[Instruction] = P(PIntAppl | PUnaryAppl | PNullaryAppl)

  def PProgram[_: P]: P[Seq[Instruction]] = P(Start ~ PStatement.rep ~ End)

  def doParse(program: String): List[Instruction] = parse(program, PProgram(_)).get.value.toList
}
