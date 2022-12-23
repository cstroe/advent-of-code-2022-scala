package aoc2022

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object Day23Part1 {
  sealed trait ElfMove
  case object North extends ElfMove
  case object South extends ElfMove
  case object East extends ElfMove
  case object West extends ElfMove
  val directions: Array[ElfMove] = Array(North, South, West, East)

  def newProposalIter(): Iterator[ElfMove] = {
    Iterator.unfold(0) { i =>
      if (i < directions.length) {
        Option((directions(i), i + 1))
      }
      else {
        Option(directions(0), 1)
      }
    }
  }

  def containsElf(location: (Int, Int), coords: Map[Int, Map[Int, Array[Elf]]]): Boolean = {
    coords.contains(location._1) && coords(location._1).contains(location._2)
  }

  case class Elf(name: Char, location: (Int, Int)) {
    // returns only moves that the Elf can move to
    def proposedMoves(coords: Map[Int, Map[Int, Array[Elf]]], proposalIter: Iterator[ElfMove]): Option[ElfMove] = {
      // If no other Elves are in one of those eight positions, the Elf does not do anything during this round.
      if (!getSurroundingCoords().exists(c => containsElf(c, coords))) {
        println(s"elf ${name} has noone around them, so they will do nothing")
        None
      } else {
        val proposedDirections = (1 to 4).map(_ => proposalIter.next()).toArray
        // TODO: replace with deltas array
        proposedDirections.find {
          case North =>
            val northIsOccupied =
              containsElf((location._1 - 1, location._2), coords) ||
              containsElf((location._1 - 1, location._2 - 1), coords) ||
              containsElf((location._1 - 1, location._2 + 1), coords)
            !northIsOccupied
          case South =>
            val southIsOccupied =
              containsElf((location._1 + 1, location._2), coords) ||
              containsElf((location._1 + 1, location._2 - 1), coords) ||
              containsElf((location._1 + 1, location._2 + 1), coords)
            !southIsOccupied
          case West =>
            val westIsOccupied =
              containsElf((location._1, location._2 - 1), coords) ||
              containsElf((location._1 - 1, location._2 - 1), coords) ||
              containsElf((location._1 + 1, location._2 - 1), coords)
            !westIsOccupied
          case East =>
            val eastIsOccupied =
              containsElf((location._1, location._2 + 1), coords) ||
              containsElf((location._1 - 1, location._2 + 1), coords) ||
              containsElf((location._1 + 1, location._2 + 1), coords)
            !eastIsOccupied
        }
      }
    }

    def getCoord(direction: ElfMove): (Int, Int) = {
      direction match {
        case North => (location._1 - 1, location._2)
        case South => (location._1 + 1, location._2)
        case West => (location._1, location._2 - 1)
        case East => (location._1, location._2 + 1)
      }
    }

    def getSurroundingCoords(): Array[(Int, Int)] = {
      (-1 to 1).flatMap { dx =>
        (-1 to 1).map { dy =>
          (location._1 + dx, location._2 + dy)
        }
      }
        .filterNot(_ == location)
        .toArray
    }
  }

  def simulateRound(roundNum: Int, elves: Array[Elf], proposalIter: Iterator[ElfMove]): Array[Elf] = {
    val coords = getCoordinateMap(elves)

    val proposedMoves = elves
      .flatMap { elf => elf.proposedMoves(coords, proposalIter).zipWithIndex.map{ case (move, i) => (elf, (i, move)) } } // each elf proposes their valid moves

    println(s"$roundNum| ==================================")
    println(s"$roundNum| Proposed moves:")
    proposedMoves.sortBy(_._1.name).foreach { case (elf, move) =>
      println(s"$roundNum| elf ${elf.name} is at ${elf.location} and proposes to move $move to ${elf.getCoord(move._2)} with priority ${move._1}")
    }

    val moveTracker = mutable.HashMap[Int, mutable.HashMap[Int, Int]]()
    proposedMoves.foreach { case (elf, move) =>
      val (x, y) = elf.getCoord(move._2)
      if (moveTracker.contains(x) && moveTracker(x).contains(y)) {
        val currentValue = moveTracker(x)(y)
        val firstMap = moveTracker(x)
        firstMap.put(y, currentValue + 1)
      } else if (moveTracker.contains(x)) {
        val firstMap = moveTracker(x)
        firstMap.put(y, 1)
      } else {
        val firstMap = mutable.HashMap[Int, Int]()
        moveTracker.put(x, firstMap)
        firstMap.put(y, 1)
      }
    }

    println(s"$roundNum| ==================================")

    val validMoves: Array[(Elf, ElfMove)] = proposedMoves.filter { case (elf, move) =>
      val (x, y) = elf.getCoord(move._2)
      val count = moveTracker(x)(y)
      if (count >= 2) {
       println(s"$roundNum| ($x,$y) is attempted to be occupied by $count elves")
      }
      count < 2
    }
      .groupBy(_._1.name)
      .map { case (name, p) =>
        val elf = p.head._1
        val proposedMove = p.map(_._2).minBy(_._1)._2
        (name, (elf, proposedMove))
      }.values.toArray

    println(s"$roundNum| ==================================")
    println(s"$roundNum| Moves that were accepted:")
    validMoves.sortBy(_._1.name).foreach { case (elf, move) =>
      println(s"$roundNum| elf ${elf.name} at ${elf.location} should move $move to ${elf.getCoord(move)}")
    }

    val elvesThatShouldMoveNames = validMoves.map(_._1.name).toSet

    val (elvesThatShouldMove, elvesThatDidntMove) =
      elves.partition { elf => elvesThatShouldMoveNames.contains(elf.name) }

    println(s"$roundNum| ${elvesThatDidntMove.length} elves didn't move")
    val elvesThatMoved = validMoves.map { case (elf, move) =>
      Elf(elf.name, elf.getCoord(move))
    }
    println(s"$roundNum| ${elvesThatMoved.length} elves moved")

    elvesThatMoved ++ elvesThatDidntMove
  }

  def getCoordinateMap(elves: Array[Elf]): Map[Int, Map[Int, Array[Elf]]] = {
      elves.groupBy(_.location._1).map {
        case (key, elves) => (key, elves.groupBy(_.location._2)) }
  }

  // returns bottom left point and top right point
  def getRectangle(elves: Array[Elf]): (Int, Int, Int, Int) = {
    val highX = elves.map(_.location._1).max
    val lowX = elves.map(_.location._1).min
    val highY = elves.map(_.location._2).max
    val lowY = elves.map(_.location._2).min

    (lowX, lowY, highX, highY)
  }

  def computeEmptyGround(elves: Array[Elf]): Int = {
    val coords = getCoordinateMap(elves)
    val (lowX, lowY, highX, highY) = getRectangle(elves)

    val counter = new AtomicInteger()
    (lowX to highX).foreach { x =>
      (lowY to highY).foreach { y =>
        if (!containsElf((x, y), coords)) {
          counter.incrementAndGet()
        }
      }
    }

    counter.get()
  }

  def printElves(elves: Array[Elf]): String = {
    val coords = getCoordinateMap(elves)
    val (lowX, lowY, highX, highY) = getRectangle(elves)

    (lowX to highX).map { x =>
      (lowY to highY).map { y =>
        val isOccupied = coords.contains(x) && coords(x).contains(y)
        if (isOccupied) {
          val elvesHere = coords(x)(y)
          if (elvesHere.length != 1) {
            val message = s"Found elves that are occupying the same space: ${elvesHere.map(_.name).mkString(", ")}"
            println(message)
            throw new RuntimeException(message)
          }
          elvesHere(0).name.toString
        } else { "." }
      }.mkString("")
    }.mkString("\n")
  }

  def parseElves(lines: Array[String]): Array[Elf] = {
    val elfNamesIter = Iterator.unfold('A') { currentName =>
      Option((currentName, (currentName + 1).toChar))
    }

    lines
      .filterNot(_.isBlank)
      .zipWithIndex
      .flatMap { case (line, row) =>
        line.toCharArray.zipWithIndex.flatMap { case (chr, col) =>
          chr match {
            case '#' => Option(Elf(elfNamesIter.next(), (row, col)))
            case _ => None
          }
        }
      }
  }

  def main(args: Array[String]): Unit = {
    val lines = readFileToLines("src/main/resources/day23/testinput")
    val elves: Array[Elf] = parseElves(lines)

    println(s"============ Initial =====================")
    println(printElves(elves))
    println(s"Num elves: ${elves.length}")
    val proposalIter: Iterator[ElfMove] = newProposalIter()
    val finalRound = (1 to 10).foldLeft(elves) { case (elves, i) =>
      val newElves = simulateRound(i, elves, proposalIter)
      proposalIter.next() // next time we start with one advanced
      println(s"$i| ============ Round $i =====================")
      println(printElves(newElves))
      println(s"$i| Num elves: ${elves.length}")
      newElves
    }

    val emptyGround = computeEmptyGround(finalRound)
    println(emptyGround)
  }
}
