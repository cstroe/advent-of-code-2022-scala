package aoc2022

import aoc2022.Day22Part1.parseInstructions

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Day22Part2 {
  case class Tile(cubeNum: Int, row: Int, col: Int)

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

  def getNextPosition(cubeNum: Int, row: Int, col: Int, direction: Direction): (Int, Int, Int, Direction) = {
    val width = 50
    val height = 50

    Seq((cubeNum, row, col, direction))
      .map { case (cubeNum, row, col, direction) =>
        direction match {
          case Right => (cubeNum, row, col + 1, direction)
          case Down => (cubeNum, row + 1, col, direction)
          case Left => (cubeNum, row, col - 1, direction)
          case Up => (cubeNum, row - 1, col, direction)
        }
      }
      .map { case (cubeNum, row, col, direction) =>
        if (row >= height) { // down
          cubeNum match {
            case 0 => (2, 0, col, Down)
            case 1 => (2, col, 49, Left)
            case 2 => (4, 0, col, Down)
            case 3 => (5, 0, col, Down)
            case 4 => (5, col, 49, Left)
            case 5 => (1, 0, col, Down)
          }
        } else if (row < 0) { // up
          cubeNum match {
            case 0 => (5, col, 0, Right)
            case 1 => (5, 49, col, Up)
            case 2 => (0, 49, col, Up)
            case 3 => (2, col, 0, Right)
            case 4 => (2, 49, col, Up)
            case 5 => (3, 49, col, Up)
          }
        } else if (col >= width) { // right
          cubeNum match {
            case 0 => (1, row, 0, Right)
            case 1 => (4, 49 - row, 49, Left)
            case 2 => (1, 49, row, Up)
            case 3 => (4, row, 0, Right)
            case 4 => (1, 49 - row, 49, Left)
            case 5 => (4, 49, row, Up)
          }
        } else if (col < 0) { // left
          cubeNum match {
            case 0 => (3, 49 - row, 0, Right)
            case 1 => (0, row, 49, Left)
            case 2 => (3, 0, row, Down)
            case 3 => (0, 49 - row, 0, Right)
            case 4 => (3, row, 49, Left)
            case 5 => (0, 0, row, Down)
          }
        } else { (cubeNum, row, col, direction) }
      }
      .head
  }


  case class Maze(cube: Array[Array[Array[Char]]]) {
    val width: Int = 50
    val height: Int = 50

    def getTile(cubeNum: Int, row: Int, col: Int, direction: Direction): Option[Tile] = {
        val (newCubeNum, newRow, newCol, newDirection) =
          getNextPosition(cubeNum, row, col, direction)

      cube(newCubeNum)(newRow)(newCol) match {
        case ' ' => getTile(newCubeNum, newRow, newCol, newDirection)
        case '.' => Option(Tile(newCubeNum, newRow, newCol))
        case '#' => None
      }
    }
  }

  case class Player(cubeNum: Int, row: Int, col: Int, facing: Direction) {
    def turnClockwise(): Player = {
      facing match {
        case Right => Player(cubeNum, row, col, Down)
        case Down => Player(cubeNum, row, col, Left)
        case Left => Player(cubeNum, row, col, Up)
        case Up => Player(cubeNum, row, col, Right)
      }
    }

    def turnCounterClockwise(): Player = {
      facing match {
        case Right => Player(cubeNum, row, col, Up)
        case Up => Player(cubeNum, row, col, Left)
        case Left => Player(cubeNum, row, col, Down)
        case Down => Player(cubeNum, row, col, Right)
      }
    }

    def move(maze: Maze, numSteps: Int): Player = {
      println(s"numsteps = ${numSteps}")
      if (numSteps == 0) {
        this
      } else {
        maze.getTile(cubeNum, row, col, facing) match {
          case None => debug(this)
          case Some(Tile(cn, r, c)) => debug(Player(cn, r, c, facing)).move(maze, numSteps - 1)
        }
      }
    }

    def code: Int = 1000 * (row + 1) + 4 * (col + 1) + facing.value
  }

  def debug(p: Player): Player = {
    println(p)
    p
  }

  def parseMaze(input: String): Maze = {
    val mazeLines = input.split("\n")
    val numRows = mazeLines.length
    val numCols = mazeLines.map(_.length).max

    val map: Array[Array[Char]] = Array.fill(numRows, numCols)(' ')
    mazeLines.zipWithIndex.map { case (line, row) =>
      val charArr = line.toCharArray
      charArr.indices.map { col =>
        map(row)(col) = charArr(col)
      }
    }

    val cube = (0 to 5).map { _ => Array.ofDim[Char](50, 50) }.toArray

    (0 until 50).foreach { row =>
      (50 until 100).foreach { col => cube(0)(row)(col - 50) = map(row)(col) }
      (100 until 150).foreach { col => cube(1)(row)(col - 100) = map(row)(col) }
    }
    (50 until 100).foreach { row =>
      (50 until 100).foreach { col => cube(2)(row - 50)(col - 50) = map(row)(col) }
    }
    (100 until 150).foreach { row =>
      (0 until 50).foreach { col => cube(3)(row - 100)(col) = map(row)(col) }
      (50 until 100).foreach { col => cube(4)(row - 100)(col - 50) = map(row)(col) }
    }
    (150 until 200).foreach { row =>
      (0 until 50).foreach { col => cube(5)(row - 150)(col) = map(row)(col) }
    }

    Maze(cube)
  }

  def printCube(cube: Array[Array[Char]]): String = {
    cube.map { row => row.mkString("") }.mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val input = readFile(s"src/main/resources/day22/input")

    val inputArr = input.split("\n\n")

    val maze = parseMaze(inputArr(0))
    val instructions = parseInstructions(inputArr(1).trim)


    val startAtCol = maze.cube(0)(0).zipWithIndex.find { case (char, _) => char == '.' }.map(_._2).get

    println(s"Starting at cube 0: (0, $startAtCol)")

    (0 to 5).foreach { cubeNum =>
      println(s"=============================== Cube $cubeNum =========================")
      println(printCube(maze.cube(cubeNum)))
      println(s"=======================================================================")
    }

    val finalPlayer = instructions.foldLeft(debug(Player(0, 0, startAtCol, Right))) {
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
