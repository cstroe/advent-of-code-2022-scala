package aoc2022

import aoc2022.Day23Part1.{Elf, ElfMove, newProposalIter, parseElves, printElves, simulateRound}
import org.junit.Test
import org.scalatest.matchers.should.Matchers

class Day23P1Test extends Matchers {
  @Test
  def smallExampleTest(): Unit = {
    val elves = parseElves("""
      .....
      ..##.
      ..#..
      .....
      ..##.
      .....
    """.split("\n").map(_.trim))

    val proposalIter: Iterator[ElfMove] = newProposalIter()

    val round1Result = simulateRound(1, elves, proposalIter)
    proposalIter.next()
    "\n" + printElves(round1Result) + "\n" shouldBe
      "\n" + """
        AB
        ..
        C.
        .E
        D.
      """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"

    val round2Result = simulateRound(2, round1Result, proposalIter)
    proposalIter.next()
    "\n" + printElves(round2Result) + "\n" shouldBe
      "\n" +
        """
        .AB.
        C...
        ...E
        ....
        .D..
      """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"

    val round3Result = simulateRound(3, round2Result, proposalIter)
    proposalIter.next()
    "\n" + printElves(round3Result) + "\n" shouldBe
      "\n" +
        """
        ..A..
        ....B
        C....
        ....E
        .....
        ..D..
      """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"
  }

  @Test
  def sampleInputTest(): Unit = {
    /*
    ....A..
    ..BCD.E
    F...G.H
    .I...JK
    L.MNO..
    PQ.R.ST
    .U..V..
     */
    val lines = readFileToLines("src/main/resources/day23/testinput")
    val elves: Array[Elf] = parseElves(lines)
    val proposalIter: Iterator[ElfMove] = newProposalIter()
    val round1Result = simulateRound(1, elves, proposalIter)
    proposalIter.next()
    println("======= round 1 ========")
    println(printElves(round1Result))
    ("\n" + printElves(round1Result) + "\n") shouldBe
    "\n" + """
      .....A...
      ...B...E.
      .F..C.D..
      .....G..H
      ..I.N.JK.
      L..M.O...
      P.Q.R.ST.
      .........
      ..U..V...
    """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"

    val round2Result = simulateRound(2, round1Result, proposalIter)
    proposalIter.next()
    println("======= round 2 ========")
    println(printElves(round2Result))
    ("\n" + printElves(round2Result) + "\n") shouldBe
      "\n" + """
        ......A....
        ...B.....E.
        ..F..C.D...
        ......G...H
        ..I..N.J...
        L...M.O.K..
        ...........
        .P.Q.R.ST..
        ...U..V....
    """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n") + "\n"

    val round3Result = simulateRound(3, round2Result, proposalIter)
    proposalIter.next()
    println("======= round 3 ========")
    println(printElves(round3Result))
    """
        ......A....
        ....B....E.
        ....C...D..
        ..F...G...H
        ..I..N.J...
        L..M.....K.
        ......OS...
        .PQ.R....T.
        ...........
        ...U..V....
    """.split("\n").map(_.trim).filterNot(_.isBlank).mkString("\n")

    /*
    ......A....
    ....B....E.
    .F..C...D..
    ......#...#
    ..#..#.#...
    #..#.....#.
    ......##...
    .##.#....#.
    ..#........
    ......#....

     */
  }
}
