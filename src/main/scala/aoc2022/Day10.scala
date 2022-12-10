package aoc2022

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day10 {
  sealed trait Command
  object Noop extends Command
  sealed case class Addx(num: Int) extends Command

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day10/input")

    val interestingCycles = (20 to (220, 40)).toSet

    var signalStrengths = new ArrayBuffer[(Int, Int)]()

    val commands = contents
      .flatMap { line =>
        line.substring(0, 4) match {
          case "addx" =>
            val n = line.split(" ")(1).toInt
            List(Noop, Addx(n))
          case _ =>
            List(Noop)
        }
      }
      .zipWithIndex
      .map { case (command, cycle) => (command, cycle + 1) }
      .take(240)
      .toList

    commands.foldLeft(1) { case (register, (command, cycle)) =>
      command match {
        case Noop =>
          if (interestingCycles.contains(cycle)) {
            signalStrengths.addOne((cycle, register * cycle))
          }
          register
        case Addx(n) =>
          if (interestingCycles.contains(cycle)) {
            signalStrengths.addOne((cycle, register * cycle))
          }
          register + n
      }
    }

    println("Signal strengts:" + signalStrengths.map(_._2).sum)
  }
}

object Day10Part2 {
  sealed trait Command
  object Noop extends Command
  sealed case class Addx(num: Int) extends Command

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day10/input")

    val screen: Array[String] = Array.fill(240)(" ")

    val commands = contents
      .flatMap { line =>
        line.substring(0, 4) match {
          case "addx" =>
            val n = line.split(" ")(1).toInt
            List(Noop, Addx(n))
          case _ =>
            List(Noop)
        }
      }
      .zipWithIndex
      .map { case (command, cycle) => (command, cycle + 1) }
      .take(240)
      .toList

    commands.foldLeft(1) { case (register, (command, cycle)) =>
      val currentPixelBeingDrawn = cycle - 1
      val pixelHorizontalPos = currentPixelBeingDrawn % 40

      if ((register - 1) to (register + 1) contains pixelHorizontalPos) {
        // pixel is lit
        screen(currentPixelBeingDrawn) = "█"
      }
      command match {
        case Noop    => register
        case Addx(n) => register + n
      }
    }

    println("Screen:")
    println("-" * 40)
    screen.grouped(40).foreach { arr => println(arr.mkString("")) }
    println("-" * 40)
  }
}
