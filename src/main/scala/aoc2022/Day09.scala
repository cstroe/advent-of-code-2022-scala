package aoc2022

import scala.collection.mutable

/**
 * According to Reddit discussion, should have used complex numbers here...
 *
 * Example from https://github.com/fuglede/adventofcode/blob/master/2022/day09/solutions.py#L13-L18
 *
 * def new_tail(head, tail):
 *   if abs(head - tail) < 2:
 *     return tail
 *   tail += sign(head.real - tail.real)
 *   tail += sign(head.imag - tail.imag) * 1j
 *   return tail
 *
 */
object Day09 {
  def moveHead(head: (Int, Int), dir: String): (Int, Int) = {
    dir match {
      case "R" => (head._1 + 1, head._2)
      case "L" => (head._1 - 1, head._2)
      case "U" => (head._1, head._2 - 1)
      case "D" => (head._1, head._2 + 1)
    }
  }

  def isTouching(head: (Int, Int), tail: (Int, Int)): Boolean = {
    head._1 == tail._1 && head._2 == tail._2 || // directly on top
    head._1 - 1 == tail._1 && head._2 == tail._2 || // head Right of tail
    head._1 + 1 == tail._1 && head._2 == tail._2 || // head left of tail
    head._1 == tail._1 && head._2 - 1 == tail._2 || // head below tail
    head._1 == tail._1 && head._2 + 1 == tail._2 || // head above tail
    head._1 - 1 == tail._1 && head._2 - 1 == tail._2 || // head Right and below tail
    head._1 - 1 == tail._1 && head._2 + 1 == tail._2 ||
    head._1 + 1 == tail._1 && head._2 - 1 == tail._2 || // head Left and below tail
    head._1 + 1 == tail._1 && head._2 + 1 == tail._2
  }

  def moveTail(head: (Int, Int), tail: (Int, Int)): (Int, Int) = {
    if (head._1 == tail._1) { // same row
      if (head._2 > tail._2) { // head is right of tail
        (tail._1, tail._2 + 1)
      } else {
        (tail._1, tail._2 - 1)
      }
    } else if (head._2 == tail._2) { // same col
      if (head._1 > tail._1) { // head below tail
        (tail._1 + 1, tail._2)
      } else {
        (tail._1 - 1, tail._2)
      }
    } else {
      val tailX = if (head._1 > tail._1) {
        tail._1 + 1
      } else { tail._1 - 1 }
      val tailY = if (head._2 > tail._2) {
        tail._2 + 1
      } else { tail._2 - 1 }
      (tailX, tailY)
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day09/input")

    val positions = mutable.Set[(Int, Int)]()
    positions.add((0,0))
    contents
      .flatMap { line =>
        val split = line.split(" ")
        val dir = split(0)
        val count = split(1).toInt
        List.fill(count)(dir)
      }
      .foldLeft(((0,0),(0,0))) { case ((head, tail), dir) =>
      val newHead = moveHead(head, dir)

      val newTail = if (!isTouching(newHead, tail)) {
        moveTail(newHead, tail)
      } else {
        tail
      }

      positions.add(newTail)
      (newHead,newTail)
    }

    assert(positions.size == 5902)
    println(positions.size)
  }
}

object Day09Part2 {
  def moveHead(head: (Int, Int), dir: String): (Int, Int) = {
    dir match {
      case "R" => (head._1 + 1, head._2)
      case "L" => (head._1 - 1, head._2)
      case "U" => (head._1, head._2 - 1)
      case "D" => (head._1, head._2 + 1)
    }
  }

  def isTouching(head: (Int, Int), tail: (Int, Int)): Boolean = {
    head._1 == tail._1 && head._2 == tail._2 || // directly on top
      head._1 - 1 == tail._1 && head._2 == tail._2 || // head Right of tail
      head._1 + 1 == tail._1 && head._2 == tail._2 || // head left of tail
      head._1 == tail._1 && head._2 - 1 == tail._2 || // head below tail
      head._1 == tail._1 && head._2 + 1 == tail._2 || // head above tail
      head._1 - 1 == tail._1 && head._2 - 1 == tail._2 || // head Right and below tail
      head._1 - 1 == tail._1 && head._2 + 1 == tail._2 ||
      head._1 + 1 == tail._1 && head._2 - 1 == tail._2 || // head Left and below tail
      head._1 + 1 == tail._1 && head._2 + 1 == tail._2
  }

  def moveTail(head: (Int, Int), tail: (Int, Int)): (Int, Int) = {
    if (head._1 == tail._1) { // same row
      if (head._2 > tail._2) { // head is right of tail
        (tail._1, tail._2 + 1)
      } else {
        (tail._1, tail._2 - 1)
      }
    } else if (head._2 == tail._2) { // same col
      if (head._1 > tail._1) { // head below tail
        (tail._1 + 1, tail._2)
      } else {
        (tail._1 - 1, tail._2)
      }
    } else {
      val tailX = if (head._1 > tail._1) {
        tail._1 + 1
      } else {
        tail._1 - 1
      }
      val tailY = if (head._2 > tail._2) {
        tail._2 + 1
      } else {
        tail._2 - 1
      }
      (tailX, tailY)
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day09/input")

    val positions = mutable.Set[(Int, Int)]()
    positions.add((0, 0))
    contents
      .flatMap { line =>
        val split = line.split(" ")
        val dir = split(0)
        val count = split(1).toInt
        List.fill(count)(dir)
      }
      .foldLeft(List.fill(10)((0,0))) { case (knots, dir) =>
        val newList = mutable.ArrayBuffer[(Int, Int)]()
        newList.addAll(knots)
        newList.indices.foreach { i =>
          if (i == 0) {
            val head = newList(0)
            newList(0) = moveHead(head, dir)
          } else {
            val knotBefore = newList(i-1)
            val currentKnot = newList(i)
            val newKnot = if (!isTouching(knotBefore, currentKnot)) {
              moveTail(knotBefore, currentKnot)
            } else {
              currentKnot
            }
            newList(i) = newKnot
          }
        }
        positions.add(newList.last)
        newList.toList
      }

    assert(positions.size == 2445, s"${positions.size} != 2445")
    println(positions.size)
  }
}
