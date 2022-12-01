package aoc2022

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