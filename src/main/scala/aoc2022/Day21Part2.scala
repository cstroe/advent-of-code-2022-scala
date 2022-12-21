package aoc2022

import scala.+:
import scala.collection.mutable

object Day21Part2 {

  case class Op(kind: String, value: Long)

  sealed trait Equation {
    def variable: String
  }
  sealed case class Simple(variable: String,
                           leftSide: String,
                           rightSide: String,
                           operation: String) extends Equation
  sealed case class Value(variable: String, value: Long) extends Equation {
    override def toString: String = s"$variable = $value"
  }
  sealed case class Unknown(variable: String, ops: List[Op]) extends Equation

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

  def solve(variable: String, eq: mutable.Map[String, Equation]): Equation = {
    eq(variable) match {
      case Unknown(a, op) => Unknown(a, op)
      case v: Value => v
      case Simple(currVarName, leftSide, rightSide, operation) =>
        operation match {
          case "*" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (Unknown(unk, ops), Value(_, v1)) =>
                Unknown(unk, ops :+ Op(">/", v1))
              case (Value(_, v1), Unknown(unk, ops)) =>
                Unknown(unk, ops :+ Op(">/", v1))
              case (Value(_, v1), Value(_, v2)) =>
                val value = Value(currVarName, v1 * v2)
                println(value)
                value
              case _ =>
                ???
            }
          case "-" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (Unknown(unk, ops), Value(_, v1)) =>
                Unknown(unk, ops :+ Op("+", v1))
              case (Value(_, v1), Unknown(unk, ops)) =>
                Unknown(unk, ops :+ Op("-<", v1))
              case (Value(_, v1), Value(_, v2)) =>
                val value = Value(currVarName, v1 - v2)
                println(value)
                value
              case _ =>
                ???
            }
          case "/" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (Unknown(unk, ops), Value(_, v1)) =>
                Unknown(unk, ops :+ Op("*", v1))
              case (Value(_, v1), Unknown(unk, ops)) =>
                Unknown(unk, Op("/<", v1) +: ops)
              case (Value(_, v1), Value(_, v2)) =>
                val value = Value(currVarName, v1 / v2)
                println(value)
                value
              case _ =>
                ???
            }
          case "+" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (Unknown(unk, ops), Value(_, v1)) =>
                Unknown(unk, ops :+ Op(">-", v1))
              case (Value(_, v1), Unknown(unk, ops)) =>
                Unknown(unk, ops :+ Op(">-", v1))
              case (Value(_, v1), Value(_, v2)) =>
                val value = Value(currVarName, v1 + v2)
                println(value)
                value
              case _ =>
                ???
            }
          case "=" =>
            (solve(leftSide, eq), solve(rightSide, eq)) match {
              case (Unknown(unk, ops), Value(_, v1)) =>
                val value = performOps(ops, v1)
                val resVal = Value(unk, value)
                println(resVal)
                resVal
              case (Value(_, v1), Unknown(unk, ops)) =>
                val value = performOps(ops, v1)
                Value(unk, value)
              case (Value(_, v1), Value(_, v2)) =>
                ???
              case _ =>
                ???
            }
        }
    }
  }

  def parseInput(input: String): Map[String, Equation] = {
    val ValuePattern = "([a-z]+): ([0-9]+)".r
    val SimplePattern = "([a-z]+): ([a-z]+) ([=*/\\-+]) ([a-z]+)".r

    input.split("\n").filterNot(_.isBlank).map {
      case ValuePattern(variableName, value) =>
        if (variableName == "humn") {
          Unknown(variableName, List.empty)
        } else {
          Value(variableName, value.toLong)
        }
      case SimplePattern(variableName, leftSide, operation, rightSide) =>
        if (variableName == "root") {
          Simple(variableName, leftSide, rightSide, "=")
        } else {
          Simple(variableName, leftSide, rightSide, operation)
        }
    }.map(eq => (eq.variable, eq)).toMap
  }

  def main(args: Array[String]): Unit = {
    val input = readFile(s"src/main/resources/day21/input")
    val equations = parseInput(input)

    equations.foreach(println)

    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(equations)
    val rootVal = solve("root", mutEq.result())
    println(s"root = $rootVal")
  }
}
