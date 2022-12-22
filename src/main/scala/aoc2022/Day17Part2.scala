package aoc2022

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.collection.mutable

object Day17Part2 {
  class RockStore(var store: Array[Char] = Array.fill(32768)(0),
                  private var rowsToSkip: Long = 0,
                  private var maxRow: Long = -1L) {
    def add(rock: FallingRock): Unit = {
      val bottomRow = rock.row - rock.chars.length + 1

      var i = 0
      while(i < rock.chars.length) {
        val currentRow = bottomRow + i
        val storeArrayIndex = (currentRow - rowsToSkip).toInt
        if (storeArrayIndex >= store.length) { growArray() }
        val existingChar = store(storeArrayIndex)
        val maskByte: Char = rock.chars(i)
        val newChr: Char = (maskByte | existingChar).toChar
        store(storeArrayIndex) = newChr

        i += 1
      }

      if (rock.row > maxRow) {
        maxRow = rock.row
      }
    }

    private def growArray(): Unit = {
      val newSize = store.length * 2
      val newStore: Array[Char] = Array.fill(newSize)(0x0)
      Array.copy(store, 0, newStore, 0, store.length)
      println(s"Array grew to ${newStore.length}")
      store = newStore
    }

    def getCharByRowNum(rowNum: Long): Char = {
      val arrayIndex = rowNum - rowsToSkip
      if (arrayIndex >= store.length) { 0x00 }
      else if (arrayIndex < 0) { 0xFF }
      else { store(arrayIndex.toInt) }
    }

    def getTop(n: Int): Option[String] = {
      val chars = Array.ofDim[Char](n)
      (0 until n).foreach { i =>
        val currentArrayIndex = maxRow - rowsToSkip
        chars(i) = store((currentArrayIndex - i).toInt) // will error on Int overflow
      }

      val fullRowsExist = chars.sliding(3).exists { w =>
        w.foldLeft(0x00) { case (acc, chr) => (acc | chr).toChar } == 0x7F
      }
      if (fullRowsExist) { Option(chars.mkString("")) }
      else { None }
    }

    def isEmpty: Boolean = maxRow == -1L
    def getMaxRow: Long = maxRow

    def skipAheadBy(height: Long): Unit = {
      rowsToSkip += height
      maxRow += height
    }

    def printTop(startAtRow: Long, showN: Long): String = {
      val str = new mutable.StringBuilder()
      val topRow = startAtRow
      val bottomRow = startAtRow - showN
      (topRow until (bottomRow, -1)).foreach { row =>
        str.append('|')
        val char = getCharByRowNum(row)
        (0 to 6).foreach { col =>
          val mask = 0x40 >> col
          if ((char & mask) != 0) {
            str.append("█")
          } else { str.append(".")}
        }
        str.append(s"| $row\n")
      }
      str.append("+-------+\n")
      str.toString()
    }
  }


  sealed trait Jet

  case object LeftPush extends Jet {
    override def toString: String = "<"
  }

  case object RightPush extends Jet {
    override def toString: String = ">"
  }

  sealed trait RockShape {
    def encoded: Array[Char]
    def width: Int
    def height: Int = encoded.length
    def getCharsAtCol(col: Int): Array[Char]
    override def toString: String = getClass.getSimpleName
  }

  // ####...
  sealed case class FlatRock(encoded: Array[Char] = Array(0x78), width: Int = 4) extends RockShape {
    val charsAtCol: Array[Array[Char]] =
      (0 to 3).map(computeChars(encoded, _)).toArray

    override def getCharsAtCol(col: Int): Array[Char] = charsAtCol(col)
  }

  // .#..... 0x20
  // ###.... 0x70
  // .#..... 0x20
  sealed case class CrossRock(encoded: Array[Char] = Array(0x20, 0x70, 0x20),
                              width: Int = 3) extends RockShape {
    val charsAtCol: Array[Array[Char]] =
      (0 to 4).map(computeChars(encoded, _)).toArray

    override def getCharsAtCol(col: Int): Array[Char] = charsAtCol(col)
  }
  // ..#.... 0x10
  // ..#.... 0x10
  // ###.... 0x70
  sealed case class LRock(encoded: Array[Char] = Array(0x70, 0x10, 0x10),
                          width: Int = 3) extends RockShape {
    val charsAtCol: Array[Array[Char]] =
      (0 to 4).map(computeChars(encoded, _)).toArray

    override def getCharsAtCol(col: Int): Array[Char] = charsAtCol(col)
  }

  // #...... 0x40
  // #...... 0x40
  // #...... 0x40
  // #...... 0x40
  sealed case class TallRock(encoded: Array[Char] = Array(0x40, 0x40, 0x40, 0x40),
                             width: Int = 1) extends RockShape {
    val charsAtCol: Array[Array[Char]] =
      (0 to 6).map(computeChars(encoded, _)).toArray

    override def getCharsAtCol(col: Int): Array[Char] = charsAtCol(col)
  }

   // ##..... 0x60
   // ##..... 0x60
  sealed case class BlockRock(encoded: Array[Char] = Array(0x60, 0x60),
                              width: Int = 2) extends RockShape {
     val charsAtCol: Array[Array[Char]] =
       (0 to 5).map(computeChars(encoded, _)).toArray

     override def getCharsAtCol(col: Int): Array[Char] = charsAtCol(col)
   }

  val rockShapes: List[RockShape] = List(
    FlatRock(), CrossRock(), LRock(), TallRock(), BlockRock()
  )

  def computeChars(blueprint: Array[Char], col: Int): Array[Char] =
    blueprint.map(_ >>> col).map(_.toChar)

  class FallingRock(var shape: RockShape, var row: Long, var col: Int, var chars: Array[Char]) {
    def moveLeft(): Unit = {
      col -= 1
      chars = shape.getCharsAtCol(col)
    }
    def moveRight(): Unit = {
      col += 1
      chars = shape.getCharsAtCol(col)
    }
    def moveDown(): Unit = { row -= 1 }

    def isValidMove(room: TallRoom, row: Long, chars: Array[Char]): Boolean = {
      val bottomRow = (row - chars.length + 1).toInt

      var i = 0
      var retVal = true
      while(retVal && i < chars.length) {
        val currentRow = bottomRow + i
        val rowChar = room.rocks.getCharByRowNum(currentRow)
        val rockChar = chars(i)
        if ((rowChar & rockChar) != 0x0) {
          retVal = false
        }

        i += 1
      }
      retVal
    }

    def canMoveDown(room: TallRoom): Boolean = {
      if ((row - 1) - (shape.height - 1) < 0) {
        false
      } else {
        isValidMove(room, row - 1, chars)
      }
    }
  }

  class TallRoom(val rocks: RockStore) {
    def height: Long = if (rocks.isEmpty) {
      0
    } else {
      rocks.getMaxRow + 1L
    }

    def width: Int = 7

    def nextSpawnRow(shape: RockShape): Long = height + 3 + (shape.encoded.length - 1)
  }

  def parseInput(fileName: String): Array[Jet] = {
    readFile(s"src/main/resources/day17/$fileName").trim.toCharArray.map {
      case '<' => LeftPush
      case '>' => RightPush
      case _ => throw new RuntimeException("invalid character")
    }
  }

  def placeRock(room: TallRoom, jetsIter: Iterator[(Jet, Int)], rock: FallingRock, debug: Boolean): Int = {
    var rockCanMove = true
    var jetNum: Int = 0
    while (rockCanMove) {
      val jet = jetsIter.next()
      jetNum = jet._2

      if (jet._1 == LeftPush) {
        if (rock.col - 1 >= 0 && rock.isValidMove(room, rock.row, rock.shape.getCharsAtCol(rock.col - 1))) {
          rock.moveLeft()
        }
      } else {
        if (rock.shape.width + rock.col <= 6 && rock.isValidMove(room, rock.row, rock.shape.getCharsAtCol(rock.col + 1))) {
          rock.moveRight()
        }
      }

      if (rock.canMoveDown(room)) {
        rock.moveDown()
      } else {
        rockCanMove = false
      }
    }

    jetNum
  }

  def findHeight(jets: Array[Jet], totalRocks: Long): Long = {
    val room = new TallRoom(rocks = new RockStore())

    val stateGenerationLookback = 12
    val jetsWithIndex = jets.zipWithIndex
    val shapesWithIndex = rockShapes.zipWithIndex

    val shapesIter = Iterator.unfold(0) { i =>
      if (i >= shapesWithIndex.length) {
        Option(shapesWithIndex.head, 1)
      } else {
        Option(shapesWithIndex(i), i + 1)
      }
    }

    val jetsIter = Iterator.unfold(0) { i =>
      if (i >= jetsWithIndex.length) {
        Option((jetsWithIndex(0), 1))
      } else {
        Option(jetsWithIndex(i), i + 1)
      }
    }

    val stateCache = mutable.HashMap[(String, Int, Int), (Long, Long)]()

    var currentRockNum: Long = 1L
    //var cyclesFound = 0L

    val rock = new FallingRock(FlatRock(), 0, 2, Array.empty) // this object will be mutated
    while(currentRockNum <= totalRocks) {
      val nextShape = shapesIter.next()
      rock.shape = nextShape._1
      rock.row = room.nextSpawnRow(rock.shape)
      rock.col = 2
      rock.chars = rock.shape.getCharsAtCol(2)
      //println(s"Placing rock at ${rock.row}")
      val jetNum = placeRock(room, jetsIter, rock, debug = false)
      room.rocks.add(rock)
      currentRockNum += 1

      // check for cycle
      if (currentRockNum > stateGenerationLookback) {
        room.rocks.getTop(stateGenerationLookback).foreach { topN =>
          val currentState = (topN, nextShape._2, jetNum)
          stateCache.get(currentState) match {
            case None => stateCache.put(currentState, (currentRockNum, room.height))
            case Some((previousRockNum, previousHeight)) =>
              stateCache.clear() // don't need these entries anymore
              stateCache.put(currentState, (currentRockNum, room.height))
              val currentHeight: Long = room.height
              println(s"Found cycle after rock number $currentRockNum with height $currentHeight.")
              room.rocks.printTop(currentHeight, stateGenerationLookback)
              println(s"Cycle started at rock number $previousRockNum with height $previousHeight")
              room.rocks.printTop(previousHeight, stateGenerationLookback)
              val rockNumDelta: Long = currentRockNum - previousRockNum
              val heightDelta: Long = currentHeight - previousHeight
              println(s"Cycle found, rock num delta: $rockNumDelta, height delta: $heightDelta")
              val rocksLeft: Long = totalRocks - currentRockNum
              println(s"$rocksLeft rocks left")
              val cyclesToSkipAheadBy: Long = rocksLeft / rockNumDelta
              println(s"Skipping ahead $cyclesToSkipAheadBy by cycles")
              val rocksToAdvance: Long = cyclesToSkipAheadBy * rockNumDelta
              println(s"Advancing by $rocksToAdvance rocks")
              currentRockNum += rocksToAdvance
              val heightToGrowBy: Long = cyclesToSkipAheadBy * heightDelta
              println(s"Growing room by: $heightToGrowBy")
              room.rocks.skipAheadBy(heightToGrowBy)
              println(s"Skipped ahead to rock #$currentRockNum, current height at ${room.height}")
              println(s"Iterations left: ${totalRocks - currentRockNum}")
          }
        }
      }


    }

    room.height
  }

  def main(args: Array[String]): Unit = {
    val startTime = ZonedDateTime.now

    val jets = parseInput("input")
    val height = findHeight(jets, 1_000_000_000_000L)

    println(s"Room height: $height")

    val durationSections = startTime.until(ZonedDateTime.now, ChronoUnit.SECONDS)
    println(s"Execution took $durationSections seconds")
  }
}
