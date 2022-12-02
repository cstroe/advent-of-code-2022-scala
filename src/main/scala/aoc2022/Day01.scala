package aoc2022

import com.google.common.base.Charsets
import com.google.common.io.Files

import java.io.File
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Puzzle01 {
  def main(args: Array[String]): Unit = {
    val pwd = sys.props.get("user.dir").get
    val filePath = s"$pwd/src/main/resources/day01/puzzle01/input"
    assert(new File(filePath).exists(), s"Could not find input file at: $filePath")

    var mostCalories = 0
    Using(Source.fromFile(filePath)) { file =>
      var currentElfCalories = 0
      for (line <- file.getLines()) {
        if (line.trim.isBlank) {
          if (currentElfCalories > mostCalories) {
            mostCalories = currentElfCalories
          }
          currentElfCalories = 0
        } else {
          currentElfCalories += line.trim.toInt
        }
      }
    }
    println(s"The highest number of calories: ${mostCalories}")
  }
}

object Puzzle01Again {
  case class Elf(item: List[Int]) {
    val calories: Int = item.sum
  }
  case class Acc(elves: List[Elf] = List.empty, currentPack: List[Int] = List.empty) {
    def newElf(): Acc = Acc(elves :+ Elf(currentPack))
    def newItem(item: Int): Acc = Acc(elves, currentPack :+ item)
  }

  def main(args: Array[String]): Unit = {
    val pwd = sys.props.get("user.dir").get
    val filePath = s"$pwd/src/main/resources/day01/puzzle01/input"
    assert(new File(filePath).exists(), s"Could not find input file at: $filePath")

    Using(Source.fromFile(filePath)) { file =>
      val mostCalories = file.getLines().foldLeft(Acc()) { case (acc, item) =>
        if (item.trim.isBlank) {
          acc.newElf()
        } else {
          acc.newItem(item.trim.toInt)
        }
      }.elves.map(_.calories).max

      println(s"The highest number of calories: $mostCalories")
    }
  }
}

object Puzzle01Smaller {
  def main(args: Array[String]): Unit = {
    val pwd = sys.props.get("user.dir").get
    val filePath = s"$pwd/src/main/resources/day01/puzzle01/input"
    assert(new File(filePath).exists(), s"Could not find input file at: $filePath")

    val mostCalories = Files.asCharSource(new File(filePath), Charsets.UTF_8)
      .read()
      .split("\n\n")
      .map(b => b.split("\n").map(_.toInt).sum)
      .max

    println(s"The highest number of calories: $mostCalories")
  }
}

object Puzzle02 {
  def main(args: Array[String]): Unit = {
    val pwd = sys.props.get("user.dir").get
    val filePath = s"$pwd/src/main/resources/day01/puzzle01/input"
    assert(new File(filePath).exists(), s"Could not find input file at: $filePath")

    val pq = new mutable.PriorityQueue[Int]()
    Using(Source.fromFile(filePath)) { file =>
      var currentElfCalories = 0
      for (line <- file.getLines()) {
        if (line.trim.isBlank) {
          pq.addOne(currentElfCalories)
          currentElfCalories = 0
        } else {
          currentElfCalories += line.trim.toInt
        }
      }
    }
    val top3 = pq.take(3).toList
    println(s"The highest number of calories: ${top3(0)}")
    println(s"The 2nd highest number of calories: ${top3(1)}")
    println(s"The 3rd highest number of calories: ${top3(2)}")
    println(s"Total: ${top3.sum}")
  }
}

object Puzzle02Again {
  def main(args: Array[String]): Unit = {
    val pwd = sys.props.get("user.dir").get
    val filePath = s"$pwd/src/main/resources/day01/puzzle01/input"
    assert(new File(filePath).exists(), s"Could not find input file at: $filePath")

    val top3Sum = Files.asCharSource(new File(filePath), Charsets.UTF_8)
      .read()
      .split("\n\n")
      .map(b => b.split("\n").map(_.toInt).sum)
      .sorted.reverse
      .take(3)
      .sum

    println(s"The top 3 sum: $top3Sum")
  }
}