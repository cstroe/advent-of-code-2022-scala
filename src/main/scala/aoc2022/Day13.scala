package aoc2022

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day13 {
  sealed trait Comparison
  object Smaller extends Comparison
  object Equal extends Comparison
  object Bigger extends Comparison
  object LeftOutOfItems extends Comparison
  object RightOutOfItems extends Comparison

  sealed trait TermType
  object ListTerm extends TermType
  object IntTerm extends TermType

  def getType(str: String): TermType = {
    if (str.startsWith("[")) { ListTerm }
    else { IntTerm }
  }

  def listToTerms(list: String): List[String] = {
    if (list == "") { List(list) }
    else {
      val buffer = mutable.ArrayBuffer[String]()
      var termDepth = 0
      val terms = (1 until list.length - 1).foldLeft(List.empty[String]) {
        case (acc, i) =>
          list.substring(i, i + 1) match {
            case "," if termDepth == 0 =>
              val term = buffer.mkString("")
              buffer.clear()
              acc :+ term
            case c if c == "[" =>
              termDepth += 1
              buffer.append(c)
              acc
            case c if c == "]" =>
              termDepth -= 1
              buffer.append(c)
              acc
            case c =>
              buffer.append(c)
              acc
          }
      }

      if (buffer.nonEmpty) {
        terms :+ buffer.mkString("")
      } else {
        terms
      }
    }
  }

  def compare(left: String, right: String): Comparison = {
    (getType(left), getType(right)) match {
      case (IntTerm, IntTerm) =>
        left.toInt.compare(right.toInt) match {
          case -1 => Smaller
          case 0  => Equal
          case 1  => Bigger
        }
      case (ListTerm, IntTerm) => compare(left, s"[$right]")
      case (IntTerm, ListTerm) => compare(s"[$left]", right)
      case (ListTerm, ListTerm) =>
        val leftTerms = listToTerms(left)
        val rightTerms = listToTerms(right)

        var comparison: Comparison = Equal
        breakable {
          leftTerms.zipWithIndex.foreach { case (leftTerm, i) =>
            if (i > rightTerms.length - 1) {
              comparison = RightOutOfItems
              break()
            }
            val rightTerm = rightTerms(i)

            compare(leftTerm, rightTerm) match {
              case c @ (Smaller | Bigger | LeftOutOfItems | RightOutOfItems) =>
                comparison = c
                break()
              case Equal =>
            }
          }
        }
        if (comparison == Equal && leftTerms.length < rightTerms.length) {
          comparison = LeftOutOfItems
        }
        comparison
    }
  }

  def inOrder(left: String, right: String): Boolean = {
    compare(left, right) match {
      case Smaller | Equal | LeftOutOfItems => true
      case Bigger | RightOutOfItems         => false
    }
  }

  def main(args: Array[String]): Unit = {
    val answer =
      readFile("src/main/resources/day13/input")
        .split("\n\n")
        .zipWithIndex
        .map { case (line, index) => (line, index + 1) }
        .map { case (pairs, index) =>
          val pairsList = pairs.split("\n")
          val left = pairsList(0)
          val right = pairsList(1)
          (inOrder(left, right), index)
        }
        .filter(c => c._1)
        .map(_._2)
        .sum
    assert(answer == 5625)
  }
}
