package aoc2022

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Day22Part1 {
  case class Tile(row: Int, col: Int)

  sealed trait Direction {
    def value: Int
  }
  case object Right extends Direction {
    def value = 0
  }
  case object Down extends Direction {
    def value = 1
  }
  case object Left extends Direction {
    def value = 2
  }
  case object Up extends Direction {
    def value = 3
  }

  case class Maze(map: Array[Array[Char]]) {
    val width: Int = map(0).length
    val height: Int = map.length

    def getTile(row: Int, col: Int, direction: Direction): Option[Tile] = {
      var (newRow, newCol): (Int, Int) = direction match {
        case Right => (row, col + 1)
        case Down => (row + 1, col)
        case Left => (row, col - 1)
        case Up => (row - 1, col)
      }

      if (newRow >= height) { newRow = 0 }
      if (newRow < 0) { newRow = height - 1 }
      if (newCol >= width) { newCol = 0 }
      if (newCol < 0) { newCol = width - 1 }

      map(newRow)(newCol) match {
        case ' ' => getTile(newRow, newCol, direction)
        case '.' => Option(Tile(newRow, newCol))
        case '#' => None
      }
    }
  }

  case class Player(row: Int, col: Int, facing: Direction) {
    def turnClockwise(): Player = {
      facing match {
        case Right => Player(row, col, Down)
        case Down => Player(row, col, Left)
        case Left => Player(row, col, Up)
        case Up => Player(row, col, Right)
      }
    }

    def turnCounterClockwise(): Player = {
      facing match {
        case Right => Player(row, col, Up)
        case Up => Player(row, col, Left)
        case Left => Player(row, col, Down)
        case Down => Player(row, col, Right)
      }
    }

    def move(maze: Maze, numSteps: Int): Player = {
      println(s"numsteps = ${numSteps}")
      if (numSteps == 0) {
        this
      } else {
        maze.getTile(row, col, facing) match {
          case None => debug(this)
          case Some(Tile(row, col)) => debug(Player(row, col, facing)).move(maze, numSteps - 1)
        }
      }
    }

    def code: Int = 1000 * (row + 1) + 4 * (col + 1) + facing.value
  }

  def parseInstructions(instructionArray: String): List[String] = {
    val instructions = mutable.ArrayBuffer[String]()
    val stringBuilder = new mutable.StringBuilder()
    instructionArray.toCharArray.foreach { char =>
      if (char >= '0' && char <= '9') {
        stringBuilder.append(char)
      } else if (char == 'R' || char == 'L') {
        if (stringBuilder.nonEmpty) {
          instructions.append(stringBuilder.toString())
          stringBuilder.clear()
        }
        instructions.append(char.toString)
      }
    }
    if (stringBuilder.nonEmpty) {
      instructions.append(stringBuilder.toString())
    }

    instructions.toList
  }

  def debug(p: Player): Player = {
    println(p)
    p
  }

  def main(args: Array[String]): Unit = {
    val input = readFile(s"src/main/resources/day22/input")

    val inputArr = input.split("\n\n")
    val mazeLines = inputArr(0).split("\n")

    val numRows = mazeLines.length
    val numCols = mazeLines.map(_.length).max

    val map: Array[Array[Char]] = Array.fill(numRows, numCols)(' ')
    val maze = Maze(map)

    mazeLines.zipWithIndex.map { case (line, row) =>
      val charArr = line.toCharArray
      charArr.indices.map { col =>
        map(row)(col) = charArr(col)
      }
    }

    val startAtCol = map(0).zipWithIndex.find { case (char, _) => char == '.' }.map(_._2).get

    val finalPlayer = parseInstructions(inputArr(1)).foldLeft(debug(Player(0, startAtCol, Right))) {
      case (player, instruction) =>
        println(s"instruction: $instruction")
        Try(instruction.toInt) match {
          case Success(steps) => debug(player.move(maze, steps))
          case Failure(_) => instruction match {
            case "R" => debug(player.turnClockwise())
            case "L" => debug(player.turnCounterClockwise())
          }
        }
    }

    println(finalPlayer.code)
  }
}
