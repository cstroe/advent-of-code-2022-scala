package aoc2022

import aoc2022.Day21Part2.{Num, parseInput, solve}
import org.junit.Assert.assertTrue
import org.junit.Test

//noinspection AccessorLikeMethodIsUnit,TypeAnnotation
class Day21Part2Test {
  @Test
  def canSolveSimpleAddition(): Unit = {
    implicit val input = parseInput("""
      aaaa: 11
      root: aaaa = bbbb
      bbbb: humn + cccc
      humn: 0
      cccc: 6
    """)
    assertTrue(solve("root") == Num("humn", 5L))
  }

  @Test
  def canSolveSimpleAddition2(): Unit = {
    implicit val input = parseInput("""
      aaaa: 11
      root: aaaa = bbbb
      bbbb: cccc + humn
      humn: 0
      cccc: 6
    """)
    assertTrue(solve("root") == Num("humn", 5L))
  }

  @Test
  def canSolveSimpleDivision(): Unit = {
    implicit val input = parseInput("""
      aaaa: 6
      root: aaaa = bbbb
      bbbb: humn / cccc
      humn: 0
      cccc: 6
    """)
    val expected = Num("humn", 36L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSimpleDivision2(): Unit = {
    implicit val input = parseInput("""
      aaaa: 2
      root: aaaa = bbbb
      bbbb: cccc / humn
      humn: 0
      cccc: 12
    """)
    val expected = Num("humn", 6L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSubtraction(): Unit = {
    implicit val input = parseInput("""
      root: aaaa + bbbb
      aaaa: 2
      bbbb: humn - cccc
      cccc: 10
      humn: 0
    """)

    val expected = Num("humn", 12L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSubtraction2(): Unit = {
    implicit val input = parseInput("""
        root: aaaa + bbbb
        aaaa: 3
        bbbb: cccc - humn
        cccc: 10
        humn: 0
    """)

    val expected = Num("humn", 7L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSimpleMultiplication(): Unit = {
    implicit val input = parseInput("""
      root: aaaa = bbbb
      aaaa: 20
      bbbb: humn * cccc
      cccc: 10
      humn: 0
    """)

    val expected = Num("humn", 2L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSimpleMultiplication2(): Unit = {
    implicit val input = parseInput("""
      root: aaaa = bbbb
      aaaa: 20
      bbbb: cccc * humn
      cccc: 10
      humn: 0
    """)

    val expected = Num("humn", 2L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def solvesTestInput(): Unit = {
    implicit val input = parseInput("""
      root: pppw + sjmn
      dbpl: 5
      cczh: sllz + lgvd
      zczc: 2
      ptdq: humn - dvpt
      dvpt: 3
      lfqf: 4
      humn: 5
      ljgn: 2
      sjmn: drzm * dbpl
      sllz: 4
      pppw: cczh / lfqf
      lgvd: ljgn * ptdq
      drzm: hmdt - zczc
      hmdt: 32
    """)

    val expected = Num("humn", 301L)
    val actual = solve("root")
    assertTrue(s"$actual != $expected", actual == expected)
  }

}