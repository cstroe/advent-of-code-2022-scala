package aoc2022

object Day05 {
  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day05/input")
    /*
    [T]             [P]     [J]
    [F]     [S]     [T]     [R]     [B]
    [V]     [M] [H] [S]     [F]     [R]
    [Z]     [P] [Q] [B]     [S] [W] [P]
    [C]     [Q] [R] [D] [Z] [N] [H] [Q]
    [W] [B] [T] [F] [L] [T] [M] [F] [T]
    [S] [R] [Z] [V] [G] [R] [Q] [N] [Z]
    [Q] [Q] [B] [D] [J] [W] [H] [R] [J]
     1   2   3   4   5   6   7   8   9
     */
    val stackRepresentation = contents.takeWhile(_.nonEmpty).toList
    //  1   2   3   4   5   6   7   8   9
    val numStacks =
      stackRepresentation.last.split(" ").filterNot(_.isEmpty).length

    val stacks = (0 until numStacks).map { _ =>
      scala.collection.mutable.Stack[String]()
    }

    (stackRepresentation.length - 2 to (0, -1))
      .map(stackRepresentation(_))
      .foreach { line =>
        (0 until numStacks).foreach { stackNum =>
          val crate = line.substring((stackNum * 4) + 1, (stackNum * 4) + 2)
          if (crate != " ") {
            stacks(stackNum).push(crate)
          }
        }
      }

    val instrRegex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
    contents.drop(stackRepresentation.length + 1).foreach { instr =>
      for (patternMatch <- instrRegex.findAllMatchIn(instr)) {
        val moveThisMany = patternMatch.group(1).toInt
        val fromStack = patternMatch.group(2).toInt - 1
        val toStack = patternMatch.group(3).toInt - 1
        (0 until moveThisMany).foreach { _ =>
          val crate = stacks(fromStack).pop()
          stacks(toStack).push(crate)
        }
      }
    }

    val output = stacks.map(_.top).mkString("")
    assert(output == "BZLVHBWQF")
    println(s"Output is: $output")
  }
}

object Day05Part2 {
  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day05/input")
    /*
    [T]             [P]     [J]
    [F]     [S]     [T]     [R]     [B]
    [V]     [M] [H] [S]     [F]     [R]
    [Z]     [P] [Q] [B]     [S] [W] [P]
    [C]     [Q] [R] [D] [Z] [N] [H] [Q]
    [W] [B] [T] [F] [L] [T] [M] [F] [T]
    [S] [R] [Z] [V] [G] [R] [Q] [N] [Z]
    [Q] [Q] [B] [D] [J] [W] [H] [R] [J]
     1   2   3   4   5   6   7   8   9
     */
    val stackRepresentation = contents.takeWhile(_.nonEmpty).toList
    //  1   2   3   4   5   6   7   8   9
    val numStacks =
      stackRepresentation.last.split(" ").filterNot(_.isEmpty).length

    val stacks = (0 until numStacks).map { i =>
      scala.collection.mutable.Stack[String]()
    }

    (stackRepresentation.length - 2 to (0, -1))
      .map(stackRepresentation(_))
      .foreach { line =>
        (0 until numStacks).foreach { stackNum =>
          val crate = line.substring((stackNum * 4) + 1, (stackNum * 4) + 2)
          if (crate != " ") {
            stacks(stackNum).push(crate)
          }
        }
      }

    val instrRegex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
    contents.drop(stackRepresentation.length + 1).foreach { instr =>
      for (patternMatch <- instrRegex.findAllMatchIn(instr)) {
        val moveThisMany = patternMatch.group(1).toInt
        val fromStack = patternMatch.group(2).toInt - 1
        val toStack = patternMatch.group(3).toInt - 1
        val tempStack = scala.collection.mutable.Stack[String]()
        (0 until moveThisMany).foreach { _ =>
          val crate = stacks(fromStack).pop()
          tempStack.push(crate)
        }
        (0 until moveThisMany).foreach { _ =>
          val crate = tempStack.pop()
          stacks(toStack).push(crate)
        }
      }
    }

    val output = stacks.map(_.top).mkString("")
    assert(output == "TDGJQTZSL")
    println(s"Output is: $output")
  }
}
