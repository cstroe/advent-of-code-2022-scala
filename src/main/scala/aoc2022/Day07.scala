package aoc2022

object Day07 {
  sealed trait Command
  sealed case class Cd(dir: String) extends Command
  sealed case class Ls(output: List[Listing]) extends Command

  sealed trait Listing
  sealed case class DirListing(name: String) extends Listing
  sealed case class FileListing(name: String, size: Int) extends Listing

  case class Dir(path: String)

  def getSize(dirs: Map[Dir, List[Listing]], dir: Dir): Int = {
    dirs.get(dir) match {
      case None => 0
      case Some(listing) =>
        listing.map {
          case DirListing(name) =>
            getSize(dirs, Dir(dir.path + "/" + name))
          case FileListing(_, size) =>
            size
        }.sum
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day07/input")

    val dirs = scala.collection.mutable.Map[Dir, List[Listing]]()

    contents
      .split("\\$ ")
      .filterNot(_.trim.isEmpty)
      .map { command =>
        if (command.startsWith("cd ")) {
          val dir = command.substring(3).trim
          Cd(dir)
        } else if (command.startsWith("ls")) {
          val listing: List[Listing] = command
            .split("\n")
            .drop(1)
            .map { node =>
              if (node.startsWith("dir ")) {
                val dirName = node.substring(4).trim
                DirListing(dirName)
              } else {
                val fileParts = node.split(" ")
                FileListing(fileParts(1), fileParts(0).toInt)
              }
            }
            .toList
          Ls(listing)
        } else {
          throw new AssertionError(s"unknown command: $command")
        }
      }
      .foldLeft(List.empty[String]) { case (acc, cmd) =>
        cmd match {
          case Cd(dir) =>
            if (dir == "/") {
              List("")
            } else if (dir == "..") {
              acc.dropRight(1)
            } else {
              acc :+ dir
            }
          case Ls(output) =>
            output.foreach { listing =>
              val pwd = Dir(acc.mkString("/"))
              dirs.get(pwd) match {
                case None =>
                  dirs.put(pwd, List(listing))
                case Some(l) =>
                  dirs.put(pwd, l :+ listing)
              }
            }
            acc
        }
      }

    val output = dirs.keys.toList
      .sortBy(_.path)
      .map { dir =>
        val size = getSize(dirs.toMap, dir)
        println(dir + ": " + size.toString)
        (dir, size)
      }
      .filter { case (_, size) => size <= 100_000 }
      .map { case (dir, size) => println(dir + ": " + size.toString); size }
      .sum

    println(output)
  }
}

object Day07Part2 {
  sealed trait Command
  sealed case class Cd(dir: String) extends Command
  sealed case class Ls(output: List[Listing]) extends Command

  sealed trait Listing
  sealed case class DirListing(name: String) extends Listing
  sealed case class FileListing(name: String, size: Int) extends Listing

  case class Dir(path: String)

  def getSize(dirs: Map[Dir, List[Listing]], dir: Dir): Int = {
    dirs.get(dir) match {
      case None => 0
      case Some(listing) =>
        listing.map {
          case DirListing(name) =>
            getSize(dirs, Dir(dir.path + "/" + name))
          case FileListing(_, size) =>
            size
        }.sum
    }
  }

  def main(args: Array[String]): Unit = {
    val contents = readFile("src/main/resources/day07/input")

    val dirs = scala.collection.mutable.Map[Dir, List[Listing]]()

    contents
      .split("\\$ ")
      .filterNot(_.trim.isEmpty)
      .map { command =>
        if (command.startsWith("cd ")) {
          val dir = command.substring(3).trim
          Cd(dir)
        } else if (command.startsWith("ls")) {
          val listing: List[Listing] = command
            .split("\n")
            .drop(1)
            .map { node =>
              if (node.startsWith("dir ")) {
                val dirName = node.substring(4).trim
                DirListing(dirName)
              } else {
                val fileParts = node.split(" ")
                FileListing(fileParts(1), fileParts(0).toInt)
              }
            }
            .toList
          Ls(listing)
        } else {
          throw new AssertionError(s"unknown command: $command")
        }
      }
      .foldLeft(List.empty[String]) { case (acc, cmd) =>
        cmd match {
          case Cd(dir) =>
            if (dir == "/") {
              List("")
            } else if (dir == "..") {
              acc.dropRight(1)
            } else {
              acc :+ dir
            }
          case Ls(output) =>
            output.foreach { listing =>
              val pwd = Dir(acc.mkString("/"))
              dirs.get(pwd) match {
                case None =>
                  dirs.put(pwd, List(listing))
                case Some(l) =>
                  dirs.put(pwd, l :+ listing)
              }
            }
            acc
        }
      }

    val spaceNeeded = getSize(dirs.toMap, Dir("")) - 40_000_000
    println(s"Space needed: $spaceNeeded")

    val output = dirs.keys.toList
      .sortBy(_.path)
      .map { dir =>
        val size = getSize(dirs.toMap, dir)
        println(dir.path + ": " + size.toString)
        (dir, size)
      }
      .filter { case (_, size) => size >= spaceNeeded }
      .sortBy(_._2)
      .map { case (dir, size) => println(dir + ": " + size.toString); size }
      .min

    println(output)
  }
}
