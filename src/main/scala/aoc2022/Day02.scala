package aoc2022

object Puzzle03 {
  sealed trait Sign
  case object Rock extends Sign
  case object Paper extends Sign
  case object Scissors extends Sign
  case class Round(them: Sign, you: Sign) {
    def score: Int = {
      val shapeScore = you match {
        case Rock => 1
        case Paper => 2
        case Scissors =>3
      }
      val winScore = (them, you) match {
        case (Rock, Rock) => 3
        case (Rock, Paper) => 6
        case (Rock, Scissors) => 0
        case (Paper, Rock) => 0
        case (Paper, Paper) => 3
        case (Paper, Scissors) => 6
        case (Scissors, Rock) => 6
        case (Scissors, Paper) => 0
        case (Scissors, Scissors) => 3
      }

      shapeScore + winScore
    }
  }
  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day02/input")
    val sum = contents.map { line =>
      val responses = line.split(" ")
      val them = responses(0) match {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      }
      val you = responses(1) match {
        case "X" => Rock
        case "Y" => Paper
        case "Z" => Scissors
      }

      Round(them, you).score
    }.sum

    println(s"The sum of the scores is ${sum}")
  }
}

object Puzzle04 {
  sealed trait Sign

  case object Rock extends Sign
  case object Paper extends Sign
  case object Scissors extends Sign

  sealed trait Outcome
  case object Lose extends Outcome
  case object Draw extends Outcome
  case object Win extends Outcome

  case class Round(them: Sign, outcome: Outcome) {
    def score: Int = {
      val you = (them, outcome) match {
        case (Rock, Lose) => Scissors
        case (Rock, Draw) => Rock
        case (Rock, Win) => Paper
        case (Paper, Lose) => Rock
        case (Paper, Draw) => Paper
        case (Paper, Win) => Scissors
        case (Scissors, Lose) => Paper
        case (Scissors, Draw) => Scissors
        case (Scissors, Win) => Rock
      }
      val winScore = (them, you) match {
        case (Rock, Rock) => 3
        case (Rock, Paper) => 6
        case (Rock, Scissors) => 0
        case (Paper, Rock) => 0
        case (Paper, Paper) => 3
        case (Paper, Scissors) => 6
        case (Scissors, Rock) => 6
        case (Scissors, Paper) => 0
        case (Scissors, Scissors) => 3
      }

      val shapeScore = you match {
        case Rock => 1
        case Paper => 2
        case Scissors => 3
      }

      shapeScore + winScore
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day02/input")
    val sum = contents.map { line =>
      val responses = line.split(" ")
      val them = responses(0) match {
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      }
      val you = responses(1) match {
        case "X" => Lose
        case "Y" => Draw
        case "Z" => Win
      }

      Round(them, you).score
    }.sum

    println(s"The sum of the scores is ${sum}")
  }
}