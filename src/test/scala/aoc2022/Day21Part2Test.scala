package aoc2022

import aoc2022.Day21Part2.{Equation, Simple, Unknown, Value, parseInput, solve}
import org.junit.Assert.assertTrue
import org.junit.Test

import scala.collection.mutable

class Day21Part2Test {
  @Test
  def canSolveSimpleAddition(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(Seq(
      ("aaaa", Value("aaaa", 11)),
      ("root", Simple("root", "aaaa", "bbbb", "=")),
      ("bbbb", Simple("bbbb", "humn", "cccc", "+")),
      ("humn", Unknown("humn", List.empty)),
      ("cccc", Value("cccc", 6L)),
    ))
    assertTrue(solve("root", mutEq.result()) == Value("humn", 5L))
  }

  @Test
  def canSolveSimpleAddition2(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(Seq(
      ("aaaa", Value("aaaa", 11)),
      ("root", Simple("root", "aaaa", "bbbb", "=")),
      ("bbbb", Simple("bbbb", "cccc", "humn", "+")),
      ("humn", Unknown("humn", List.empty)),
      ("cccc", Value("cccc", 6L)),
    ))
    assertTrue(solve("root", mutEq.result()) == Value("humn", 5L))
  }

  @Test
  def canSolveSimpleDivision(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(Seq(
      ("aaaa", Value("aaaa", 6L)),
      ("root", Simple("root", "aaaa", "bbbb", "=")),
      ("bbbb", Simple("bbbb", "humn", "cccc", "/")),
      ("humn", Unknown("humn", List.empty)),
      ("cccc", Value("cccc", 6L)),
    ))
    val expected = Value("humn", 36L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSimpleDivision2(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(Seq(
      ("aaaa", Value("aaaa", 2L)),
      ("root", Simple("root", "aaaa", "bbbb", "=")),
      ("bbbb", Simple("bbbb", "cccc", "humn", "/")),
      ("humn", Unknown("humn", List.empty)),
      ("cccc", Value("cccc", 12L)),
    ))
    val expected = Value("humn", 6L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSubtraction(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(parseInput(
      """
        |root: aaaa + bbbb
        |aaaa: 2
        |bbbb: humn - cccc
        |cccc: 10
        |humn: 0
        |""".trim.stripMargin))

    val expected = Value("humn", 12L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSubtraction2(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(parseInput(
      """
        |root: aaaa + bbbb
        |aaaa: 3
        |bbbb: cccc - humn
        |cccc: 10
        |humn: 0
        |""".trim.stripMargin))

    val expected = Value("humn", 7L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSimpleMultiplication(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(parseInput(
      """
        |root: aaaa = bbbb
        |aaaa: 20
        |bbbb: humn * cccc
        |cccc: 10
        |humn: 0
        |""".trim.stripMargin))

    val expected = Value("humn", 2L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def canSolveSimpleMultiplication2(): Unit = {
    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(parseInput(
      """
        |root: aaaa = bbbb
        |aaaa: 20
        |bbbb: cccc * humn
        |cccc: 10
        |humn: 0
        |""".trim.stripMargin))

    val expected = Value("humn", 2L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

  @Test
  def solvesTestInput(): Unit = {
    val input = """
      |root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32
      |""".trim.stripMargin

    val mutEq = mutable.HashMap.newBuilder[String, Equation]
    mutEq.addAll(parseInput(input))

    val expected = Value("humn", 301L)
    val actual = solve("root", mutEq.result())
    assertTrue(s"$actual != $expected", actual == expected)
  }

}