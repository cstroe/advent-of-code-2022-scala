package aoc2022

object Day03 {
  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day03/input")
    val output = contents
      .map { line =>
        val first = line.substring(0, line.length / 2)
        val second = line.substring(line.length / 2, line.length)
        assert(first.size == second.size)
        (first, second)
      }
      .map { tuple =>
        val other = tuple._2.chars().toArray.toSet
        val second = tuple._1.chars().toArray.toSet

        val common = other.intersect(second)
        assert(common.size == 1)
        common.toList.head
      }
      .map { i =>
        if (i >= 'a' && i <= 'z') {
          val p: Int = i - 'a' + 1
          p
        } else if (i >= 'A' && i <= 'Z') {
          val p: Int = i - 'A' + 27
          p
        } else {
          throw new AssertionError("nope")
        }
      }
      .sum

    println(output)
  }
}

object Day03Part2 {
  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day03/input")
    val output = contents
      .grouped(3)
      .map { lines =>
        assert(lines.length == 3)
        val firstSet = lines(0).toCharArray.toSet
        val secondSet = lines(1).toCharArray.toSet
        val thirdSet = lines(2).toCharArray.toSet

        val badgeSet = firstSet.intersect(secondSet).intersect(thirdSet)
        assert(badgeSet.size == 1)
        badgeSet.toList.head
      }
      .map { i =>
        if (i >= 'a' && i <= 'z') {
          val p: Int = i - 'a' + 1
          p
        } else if (i >= 'A' && i <= 'Z') {
          val p: Int = i - 'A' + 27
          p
        } else {
          throw new AssertionError("nope")
        }
      }
      .sum

    println(output)
  }
}
