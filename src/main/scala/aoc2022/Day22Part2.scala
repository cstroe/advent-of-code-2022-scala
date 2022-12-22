package aoc2022

import aoc2022.Day22Part1.parseInstructions

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Day22Part2 {
  case class Tile(cubeNum: Int, row: Int, col: Int, direction: Direction)

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
      .map { case (cubeNum, row, col, dir) =>
        dir match {
          case Right => (cubeNum, row, col + 1, dir)
          case Down => (cubeNum, row + 1, col, dir)
          case Left => (cubeNum, row, col - 1, dir)
          case Up => (cubeNum, row - 1, col, dir)
        }
      }
      .map { case (cubeNum, row, col, dir) =>
        // the tuples here are hardcoded to match the parsing
        if (row >= height) { // down
          assert(dir == Down)
          cubeNum match {
            case 0 => (2, col, 49, Left)
            case 1 => (2, 0, col, Down)
            case 2 => (3, 0, col, Down)
            case 3 => (5, col, 49, Left)
            case 4 => (5, 0, col, Down)
            case 5 => (0, 0, col, Down)
          }
        } else if (row < 0) { // up
          assert(dir == Up)
          cubeNum match {
            case 0 => (5, 49, col, Up)
            case 1 => (5, col, 0, Right)
            case 2 => (1, 49, col, Up)
            case 3 => (2, 49, col, Up)
            case 4 => (2, col, 0, Right)
            case 5 => (4, 49, col, Up)
          }
        } else if (col >= width) { // right
          assert(dir == Right)
          cubeNum match {
            case 0 => (3, 49 - row, 49, Left)
            case 1 => (0, row, 0, Right)
            case 2 => (0, 49, row, Up)
            case 3 => (0, 49 - row, 49, Left)
            case 4 => (3, row, 0, Right)
            case 5 => (3, 49, row, Up)
          }
        } else if (col < 0) { // left
          assert(dir == Left)
          cubeNum match {
            case 0 => (1, row, 49, Left)
            case 1 => (4, 49 - row, 0, Right)
            case 2 => (4, 0, row, Down)
            case 3 => (4, row, 49, Left)
            case 4 => (1, 49 - row, 0, Right)
            case 5 => (1, 0, row, Down)
          }
        } else { (cubeNum, row, col, dir) }
      }
      .head
  }


  case class Maze(cube: Array[Array[Array[Char]]], cubeLocations: Map[Int, (Int, Int)], debugCube: Array[Array[Array[Char]]]) {
    val width: Int = 50
    val height: Int = 50

    def getTile(cubeNum: Int, row: Int, col: Int, direction: Direction): Option[Tile] = {
        val (newCubeNum, newRow, newCol, newDirection) =
          getNextPosition(cubeNum, row, col, direction)

      cube(newCubeNum)(newRow)(newCol) match {
        case ' ' => getTile(newCubeNum, newRow, newCol, newDirection)
        case '.' =>
          debugCube(cubeNum)(row)(col) = direction match {
            case Up => '^'
            case Down => 'V'
            case Left => '<'
            case Right => '>'
          }
          debugCube(newCubeNum)(newRow)(newCol) = '*'
          Option(Tile(newCubeNum, newRow, newCol, newDirection))
        case '#' =>
          debugCube(cubeNum)(row)(col) = '*'
          println(s"=============================== Debug Cube $cubeNum =========================")
          println(printCube(debugCube(cubeNum)))
          println(s"=============================================================================")


          None
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
      if (numSteps == 0) {
        println(s"=============================== Debug Cube $cubeNum =========================")
        println(printCube(maze.debugCube(cubeNum)))
        println(s"=============================================================================")
        this
      } else {
        maze.getTile(cubeNum, row, col, facing) match {
          case None =>
            println(s"hit wall at $numSteps steps")
            debug(this)
          case Some(Tile(cn, r, c, newDir)) =>
            val stepsLeft = numSteps - 1
            if (stepsLeft > 0) {
              debug(Player(cn, r, c, newDir)).move(maze, numSteps - 1)
            } else {
              Player(cn, r, c, newDir)
            }
        }
      }
    }

    def code(cubeLocations: Map[Int, (Int, Int)]): Int = {
      val cubeLocation = cubeLocations(cubeNum)
      val gRow = cubeLocation._1 + row
      val gCol = cubeLocation._2 + col
      1000 * (gRow + 1) + 4 * (gCol + 1) + facing.value
    }
  }

  var playerMoves = mutable.ArrayBuffer[Player]()
  def debug(p: Player): Player = {
    if (playerMoves.isEmpty || playerMoves.last != p) {
      playerMoves.append(p)
      println(p)
    }
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

    val cubeLocations: Map[Int, (Int, Int)] = Map(
      0 -> (0, 100),
      1 -> (0, 50),
      2 -> (50, 50),
      3 -> (100, 50),
      4 -> (100, 0),
      5 -> (150, 0),
    )

    val cube = (0 to 5).map { _ => Array.ofDim[Char](50, 50) }.toArray
//    cubeLocations.foreach { case (cubeNum,(startRow, startCol)) =>
//      (startRow until (startRow + 50)).foreach { row =>
//        (startCol until (startCol + 50)).foreach { col =>
//          cube(cubeNum)(row)(col) = map(row)(col)
//        }
//      }
//    }



    (0 until 50).foreach { row =>
      (100 until 150).foreach { col => cube(0)(row)(col - 100) = map(row)(col) }
      (50 until 100).foreach { col => cube(1)(row)(col - 50) = map(row)(col) }
    }
    (50 until 100).foreach { row =>
      (50 until 100).foreach { col => cube(2)(row - 50)(col - 50) = map(row)(col) }
    }
    (100 until 150).foreach { row =>
      (50 until 100).foreach { col => cube(3)(row - 100)(col - 50) = map(row)(col) }
      (0 until 50).foreach { col => cube(4)(row - 100)(col) = map(row)(col) }
    }
    (150 until 200).foreach { row =>
      (0 until 50).foreach { col => cube(5)(row - 150)(col) = map(row)(col) }
    }

    val debugcube = (0 to 5).map { _ => Array.ofDim[Char](50, 50) }.toArray
    debugcube.indices.foreach { cubeNum =>
      val nthCube = debugcube(cubeNum)
      nthCube.indices.foreach { rowNum =>
        val nthRow = nthCube(rowNum)
        nthRow.indices.foreach { colNum =>
          nthRow(colNum) = cube(cubeNum)(rowNum)(colNum)
        }
      }
    }


    Maze(cube, cubeLocations, debugcube)
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

    val finalPlayer = instructions.foldLeft(debug(Player(1, 0, startAtCol, Right))) {
      case (player, instruction) =>
        println(s"instruction: $instruction")
        Try(instruction.toInt) match {
          case Success(steps) =>
            println(s"move $steps ${player.facing}")
            debug(player.move(maze, steps))
          case Failure(_) => instruction match {
            case "R" => debug(player.turnClockwise())
            case "L" => debug(player.turnCounterClockwise())
          }
        }
    }

    println(finalPlayer.code(maze.cubeLocations))
    //assert(finalPlayer.code > 44200)
  }
}
