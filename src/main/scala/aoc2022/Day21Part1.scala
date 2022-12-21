package aoc2022

object Day21Part1 {

  sealed trait Equation {
    def variable: String
  }
  sealed case class Simple(variable: String,
                           leftSide: String,
                           rightSide: String,
                           operation: String) extends Equation
  sealed case class Value(variable: String, value: Long) extends Equation

  def solve(variable: String, equations: Map[String, Equation]): Long = {
    equations(variable) match {
      case Value(_, value) => value
      case Simple(_, leftSide, rightSide, operation) =>
        val leftSideValue = solve(leftSide, equations)
        val rightSideValue = solve(rightSide, equations)
        operation match {
          case "*" => leftSideValue * rightSideValue
          case "-" => leftSideValue - rightSideValue
          case "/" => leftSideValue / rightSideValue
          case "+" => leftSideValue + rightSideValue
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val ValuePattern = "([a-z]+): ([0-9]+)".r
    val SimplePattern = "([a-z]+): ([a-z]+) ([*/\\-+]) ([a-z]+)".r

    val equations = readFileToLines(s"src/main/resources/day21/input").map {
      case ValuePattern(variableName, value) =>
        Value(variableName, value.toLong)
      case SimplePattern(variableName, leftSide, operation, rightSide) =>
        Simple(variableName, leftSide, rightSide, operation)
    }.map(eq => (eq.variable, eq)).toMap

    val rootVal = solve("root", equations)
    println(s"root = $rootVal")
  }
}
