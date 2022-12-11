package aoc2022

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day11 {

  sealed trait Operation {
    def apply(input: Int): Int
  }

  sealed case class OldTimesN(n: Int) extends Operation {
    override def apply(input: Int): Int = input * n
    override def toString: String = s"multiplied by $n"
  }

  sealed case class OldPlusN(n: Int) extends Operation {
    override def apply(input: Int): Int = input + n
    override def toString: String = s"increases by $n"
  }

  object Squared extends Operation {
    override def apply(input: Int): Int = input * input
    override def toString: String = "multiplied by itself"
  }

  sealed trait MonkeyTest {
    def apply(input: Int): Boolean
  }

  sealed case class DivisibleBy(n: Int) extends MonkeyTest {
    override def apply(input: Int): Boolean = input % n == 0
  }

  case class Monkey(id: Int,
                    op: Operation,
                    test: MonkeyTest,
                    onTrueThrowTo: Int,
                    onFalseThrowTo: Int,
                    items: mutable.ArrayBuffer[Int] = new ArrayBuffer[Int](),
                    inspections: AtomicInteger = new AtomicInteger(0))

  def toOperation(input: String): Operation = {
    val TimesPattern = "old \\* ([0-9]+)".r
    val PlusPattern = "old \\+ ([0-9]+)".r
    input match {
      case TimesPattern(n) => OldTimesN(n.toInt)
      case PlusPattern(n) => OldPlusN(n.toInt)
      case "old * old" => Squared
    }
  }

  def toTest(input: String): MonkeyTest = {
    val DividiblePattern = "divisible by ([0-9]+)".r
    input match {
      case DividiblePattern(n) => DivisibleBy(n.toInt)
    }
  }

  def runRound(roundNum: Int, monkeys: List[Monkey]) = {
    println(s"------------------- Round ${roundNum} ---------------------")
    monkeys.foreach { monkey =>
      println(s"Monkey ${monkey.id}:")
      monkey.items.foreach { item =>
        println(s"  Monkey inspects an item with worry level of $item.")
        monkey.inspections.incrementAndGet()
        val newWorryLevel = monkey.op(item)
        println(s"    Worry level is ${monkey.op} to $newWorryLevel")
        val boredWorryLevel = newWorryLevel / 3
        println(s"    Monkey gets bored with item. Worry level is divided by 3 to $boredWorryLevel.")
        if (monkey.test(boredWorryLevel)) {
          println(s"(T) Item with worry level $boredWorryLevel is thrown to monkey ${monkey.onTrueThrowTo}.")
          monkeys(monkey.onTrueThrowTo).items.addOne(boredWorryLevel)
        } else {
          println(s"(F) Item with worry level $boredWorryLevel is thrown to monkey ${monkey.onFalseThrowTo}.")
          monkeys(monkey.onFalseThrowTo).items.addOne(boredWorryLevel)
        }
      }
      monkey.items.clear()
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day11/input")
    val monkeys = contents.split("\n\n").map { monkeySpec =>
      val specLines = monkeySpec.split("\n")

      val monkeyId = specLines(0).split(" ")(1).stripSuffix(":").toInt
      val monkeyItems = specLines(1).split(":")(1).split(",").map(_.trim.toInt).toList
      val operation = toOperation(specLines(2).split(":")(1).split("=")(1).trim())
      val test = toTest(specLines(3).split(":")(1).trim)
      val trueMonkey = specLines(4).split(" ").last.toInt
      val falseMonkey = specLines(5).split(" ").last.toInt

      Monkey(monkeyId, operation, test, trueMonkey, falseMonkey,
        new ArrayBuffer[Int]().addAll(monkeyItems)
      )
    }.toList

    (1 to 20).foreach { round =>
      runRound(round, monkeys)
    }

    val topMonkeys = monkeys.map { monkey =>
      monkey.inspections.get()
    }.sorted.reverse.take(2).toList

    println("Day 11 Part 1")
    println("Monkey business:" + (topMonkeys.head * topMonkeys(1)))
  }
}




object Day11Part2 {

  sealed trait Operation {
    def apply(input: BigInt): BigInt
  }

  sealed case class OldTimesN(n: BigInt) extends Operation {
    override def apply(input: BigInt): BigInt = input * n
    override def toString: String = s"multiplied by $n"
  }

  sealed case class OldPlusN(n: BigInt) extends Operation {
    override def apply(input: BigInt): BigInt = input + n
    override def toString: String = s"increases by $n"
  }

  object Squared extends Operation {
    override def apply(input: BigInt): BigInt = input * input
    override def toString: String = "multiplied by itself"
  }

  sealed case class DivisibleBy(n: BigInt) {
    def apply(input: BigInt): Boolean = input % n == 0
  }

  case class Monkey(id: Int,
                    op: Operation,
                    test: DivisibleBy,
                    onTrueThrowTo: Int,
                    onFalseThrowTo: Int,
                    items: mutable.ArrayBuffer[BigInt] = new ArrayBuffer[BigInt](),
                    inspections: AtomicLong = new AtomicLong(0))

  def toOperation(input: String): Operation = {
    val TimesPattern = "old \\* ([0-9]+)".r
    val PlusPattern = "old \\+ ([0-9]+)".r
    input match {
      case TimesPattern(n) => OldTimesN(n.toLong)
      case PlusPattern(n) => OldPlusN(n.toLong)
      case "old * old" => Squared
    }
  }

  def toTest(input: String): DivisibleBy = {
    val DividiblePattern = "divisible by ([0-9]+)".r
    input match {
      case DividiblePattern(n) => DivisibleBy(n.toLong)
    }
  }

  def runRound(roundNum: Int, monkeys: List[Monkey]) = {
    println(s"------------------- Round ${roundNum} ---------------------")
    monkeys.foreach { monkey =>
      //println(s"Monkey ${monkey.id}:")
      monkey.items.foreach { item =>
        //println(s"  Monkey inspects an item with worry level of $item.")
        monkey.inspections.incrementAndGet()
        val newWorryLevel = monkey.op(item)
        //println(s"    Worry level is ${monkey.op} to $newWorryLevel")
        if (monkey.test(newWorryLevel)) {
          //println(s"(T) Item with worry level $newWorryLevel is thrown to monkey ${monkey.onTrueThrowTo}.")
          monkeys(monkey.onTrueThrowTo).items.addOne(newWorryLevel)
        } else {
          //println(s"(F) Item with worry level $newWorryLevel is thrown to monkey ${monkey.onFalseThrowTo}.")
          monkeys(monkey.onFalseThrowTo).items.addOne(newWorryLevel)
        }
      }
      monkey.items.clear()
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day11/input")
    val monkeys = contents.split("\n\n").map { monkeySpec =>
      val specLines = monkeySpec.split("\n")

      val monkeyId = specLines(0).split(" ")(1).stripSuffix(":").toInt
      val monkeyItems = specLines(1).split(":")(1).split(",").map(_.trim.toLong).map(i => BigInt(i)).toList
      val operation = toOperation(specLines(2).split(":")(1).split("=")(1).trim())
      val test = toTest(specLines(3).split(":")(1).trim)
      val trueMonkey = specLines(4).split(" ").last.toInt
      val falseMonkey = specLines(5).split(" ").last.toInt

      Monkey(monkeyId, operation, test, trueMonkey, falseMonkey,
        new ArrayBuffer[BigInt]().addAll(monkeyItems)
      )
    }.toList

    val upperLimit: BigInt = monkeys.map(_.test.n).foldLeft(BigInt(1)) { case (acc, n) => acc * n }
    println(s"Upper limit is: ${upperLimit}")


    (1 to 10000).foreach { round =>
      runRound(round, monkeys)

      var run = true

      while(run) {
        monkeys.flatMap(_.items).find(w => w < upperLimit) match {
          case Some(_) => println("Not at upper limit yet")
            run = false
          case None =>
            println(s"${round} At upper limit, let's downsize")
            monkeys.foreach { monkey =>
              val newItems = monkey.items.map { item => item % upperLimit }
              monkey.items.clear()
              monkey.items.addAll(newItems)
            }
        }
      }

    }

    val topMonkeys: List[BigInt] = monkeys.map { monkey =>
      monkey.inspections.get()
    }.sorted.reverse.take(2).map(BigInt(_))

    println("Day 11 Part 2")
    println("Monkey business:" + (topMonkeys.head * topMonkeys(1)))
  }
}
