package aoc2022

object Day21Part2 {
  case class Op(kind: String, value: Long)

  sealed trait Expression {
    def name: String
  }
  sealed case class Eq(name: String,
                       leftSide: String,
                       rightSide: String,
                       operation: String) extends Expression

  sealed case class Num(name: String, value: Long) extends Expression
  sealed case class X(name: String, ops: List[Op]) extends Expression

  def performOps(ops: Seq[Op], v1: Long): Long = {
    ops.reverse.foldLeft(v1) { case (acc, op) =>
      op.kind match {
        case "*" => acc * op.value
        case "+" => acc + op.value
        case ">-" => acc - op.value
        case "-<" => op.value - acc
        case ">/" => acc / op.value
        case "/<" => op.value / acc
      }
    }
  }

  def solve(variable: String, eq: Map[String, Expression]): Expression = {
    eq(variable) match {
      case x: X => x
      case v: Num => v
      case Eq(eqResult, leftSide, rightSide, operation) =>
        operation match {
          case "*" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (X(nx, ops), Num(_, v1)) => X(nx, ops :+ Op(">/", v1))
              case (Num(_, v1), X(nx, ops)) => X(nx, ops :+ Op(">/", v1))
              case (Num(_, v1), Num(_, v2)) => Num(eqResult, v1 * v2)
            }
          case "-" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (X(nx, ops), Num(_, v1)) => X(nx, ops :+ Op("+", v1))
              case (Num(_, v1), X(nx, ops)) => X(nx, ops :+ Op("-<", v1))
              case (Num(_, v1), Num(_, v2)) => Num(eqResult, v1 - v2)
            }
          case "/" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (X(nx, ops), Num(_, v1)) => X(nx, ops :+ Op("*", v1))
              case (Num(_, v1), X(nx, ops)) => X(nx, Op("/<", v1) +: ops)
              case (Num(_, v1), Num(_, v2)) => Num(eqResult, v1 / v2)
            }
          case "+" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (X(nx, ops), Num(_, v1)) => X(nx, ops :+ Op(">-", v1))
              case (Num(_, v1), X(nx, ops)) => X(nx, ops :+ Op(">-", v1))
              case (Num(_, v1), Num(_, v2)) => Num(eqResult, v1 + v2)
            }
          case "=" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (X(nx, ops), Num(_, v1)) => Num(nx, performOps(ops, v1))
              case (Num(_, v1), X(nx, ops)) => Num(nx, performOps(ops, v1))
            }
        }
    }
  }

  def parseInput(input: String): Map[String, Expression] = {
    val ValuePattern = "([a-z]+): ([0-9]+)".r
    val SimplePattern = "([a-z]+): ([a-z]+) ([=*/\\-+]) ([a-z]+)".r

    input.split("\n").map(_.trim).filterNot(_.isBlank).map {
      case ValuePattern(variableName, value) =>
        if (variableName == "humn") { X(variableName, List.empty) }
        else { Num(variableName, value.toLong) }
      case SimplePattern(variableName, leftSide, operation, rightSide) =>
        if (variableName == "root") { Eq(variableName, leftSide, rightSide, "=") }
        else { Eq(variableName, leftSide, rightSide, operation) }
    }.map(eq => (eq.name, eq)).toMap
  }

  def main(args: Array[String]): Unit = {
    val input = readFile(s"src/main/resources/day21/input")
    val equations = parseInput(input)
    val rootVal = solve("root", equations)
    println(s"root = $rootVal")
  }
}
