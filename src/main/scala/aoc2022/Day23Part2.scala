package aoc2022

import aoc2022.Day23Part1.{Elf, ElfMove, containsElf, getCoordinateMap, newProposalIter, parseElves}

import java.lang.RuntimeException
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object Day23Part2 {
  def simulateRound(roundNum: Int, elves: Array[Elf], proposalIter: Iterator[ElfMove]): Array[Elf] = {
    val coords = getCoordinateMap(elves)

    val proposedMoves = elves
      .flatMap { elf => elf.proposedMoves(coords, proposalIter).zipWithIndex.map{ case (move, i) => (elf, (i, move)) } } // each elf proposes their valid moves

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

    //    println(s"$roundNum| ==================================")

    val validMoves: Array[(Elf, ElfMove)] = proposedMoves.filter { case (elf, move) =>
      val (x, y) = elf.getCoord(move._2)
      moveTracker(x)(y) < 2
    }
      .groupBy(_._1.name)
      .map { case (name, p) =>
        val elf = p.head._1
        val proposedMove = p.map(_._2).minBy(_._1)._2
        (name, (elf, proposedMove))
      }.values.toArray

    val elvesThatShouldMoveNames = validMoves.map(_._1.name).toSet

    val (elvesThatShouldMove, elvesThatDidntMove) =
      elves.partition { elf => elvesThatShouldMoveNames.contains(elf.name) }

    if (elvesThatDidntMove.length == elves.length) {
      throw new RuntimeException(s"Elves didn't have to move at round $roundNum")
    }
    val elvesThatMoved = validMoves.map { case (elf, move) =>
      Elf(elf.name, elf.getCoord(move))
    }
    println(s"$roundNum| ${elvesThatMoved.length} elves moved")

    elvesThatMoved ++ elvesThatDidntMove
  }

  def main(args: Array[String]): Unit = {
    val lines = readFileToLines("src/main/resources/day23/input")
    val elves: Array[Elf] = parseElves(lines)

    //    println(s"============ Initial =====================")
    //    println(printElves(elves))
    //    println(s"Num elves: ${elves.length}")
    val proposalIter: Iterator[ElfMove] = newProposalIter()
    try {
      val finalRound = (1 to 1_000_000).foldLeft(elves) { case (elves, i) =>
        val newElves = simulateRound(i, elves, proposalIter)
        proposalIter.next() // next time we start with one advanced
        //      println(s"$i| ============ Round $i =====================")
        //      println(printElves(newElves))
        //     println(s"$i| Num elves: ${elves.length}")
        newElves
      }
    } catch {
      case e: RuntimeException =>
        if (e.getMessage.startsWith("Elves didn't have to move at round")) {
          println(e.getMessage)
        } else {
          throw e
        }
    }
  }
}
