package aoc2022

/** Notes: Need to get much better at iterating with accumulation + early
  * termination.
  */
object Day08 {
  sealed trait Direction
  case object Horizontal extends Direction
  case object Vertical extends Direction

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day08/input")

    val map = contents
      .filterNot(line => line.trim.isEmpty)
      .map { line =>
        line.toCharArray.map { chr => chr - '0' }.toList
      }
      .toList

    val isVisible = Array.ofDim[Boolean](map.length, map.head.length)

    isVisible.indices.foreach { row =>
      isVisible(row)(0) = true
      isVisible(row)(isVisible.head.length - 1) = true
    }

    isVisible.head.indices.foreach { col =>
      isVisible(0)(col) = true
      isVisible(isVisible.length - 1)(col) = true
    }

//    isVisible.indices.foreach { row =>
//      println(isVisible(row).map(v => if (v) "t" else ".").mkString(""))
//    }

    val directions = (0 until map.length).map(x => (x, Horizontal)).toList ++
      (0 until map.head.length).map(y => (y, Vertical))

    directions.foreach {
      case (i, Horizontal) =>
        var maxHeight = map(i).head
        map(i).indices.foreach { col =>
          val currentHeight = map(i)(col)
          if (!isVisible(i)(col)) {
            isVisible(i)(col) = currentHeight > maxHeight
          }
          maxHeight = Math.max(maxHeight, currentHeight)
        }

        maxHeight = map(i).last
        map(i).indices.reverse.drop(1).foreach { col =>
          val currentHeight = map(i)(col)
          if (!isVisible(i)(col)) {
            isVisible(i)(col) = currentHeight > maxHeight
          }
          maxHeight = Math.max(maxHeight, currentHeight)
        }
      case (j, Vertical) =>
        val list = (0 until map.length).map { row =>
          map(row)(j)
        }.toList

        var maxHeight = list.head
        list.indices.foreach { row =>
          val currentHeight = map(row)(j)
          if (!isVisible(row)(j)) {
            isVisible(row)(j) = currentHeight > maxHeight
          }
          maxHeight = Math.max(maxHeight, currentHeight)
        }

        maxHeight = list.last
        list.indices.reverse.foreach { row =>
          val currentHeight = map(row)(j)
          if (!isVisible(row)(j)) {
            isVisible(row)(j) = currentHeight > maxHeight
          }
          maxHeight = Math.max(maxHeight, currentHeight)
        }
    }

    isVisible.indices.foreach { row =>
      println(isVisible(row).map(v => if (v) "t" else ".").mkString(""))
    }

    print(isVisible.map { _.count(_ == true) }.sum)
  }
}

object Day08Part2 {
  sealed trait Direction
  case object Horizontal extends Direction
  case object Vertical extends Direction

  type ScenicMap = List[List[Int]]

  def computeScoreLookingUp(map: ScenicMap, row: Int, col: Int): Int = {
    if (row == 0) {
      0
    } else {
      val treeHeight = map(row)(col)
      var treesVisible = 0
      val sum = ((row - 1) to (0, -1))
        .takeWhile { currentRow =>
          treesVisible = treesVisible + 1
          val currentTreeHeight = map(currentRow)(col)
          currentTreeHeight < treeHeight
        }
        .map(_ => 1)
        .sum
      // println(sum)
      treesVisible
    }
  }

  def computeScoreLookingDown(heightMap: ScenicMap, row: Int, col: Int): Int = {
    if (row == heightMap.length - 1) {
      0
    } else {
      val treeHeight = heightMap(row)(col)
      var treesVisible = 0
      val sum = (row + 1 until heightMap.length)
        .takeWhile { currentRow =>
          treesVisible = treesVisible + 1
          val currentTreeHeight = heightMap(currentRow)(col)
          currentTreeHeight < treeHeight
        }
        .map(_ => 1)
        .sum
      // println(sum)
      treesVisible
    }
  }

  def computeScoreLookingLeft(map: ScenicMap, row: Int, col: Int): Int = {
    if (col == 0) {
      0
    } else {
      val treeHeight = map(row)(col)
      var treesVisible = 0
      val indices = ((col - 1) to (0, -1))
      val sum = indices
        .takeWhile { currentCol =>
          treesVisible = treesVisible + 1
          val currentTreeHeight = map(row)(currentCol)
          currentTreeHeight < treeHeight
        }
        .map(_ => 1)
        .sum
      // println(sum)
      treesVisible
    }
  }

  def computeScoreLookingRight(map: ScenicMap, row: Int, col: Int): Int = {
    if (col == map.head.length - 1) {
      0
    } else {
      val treeHeight = map(row)(col)
      var treesVisible = 0
      val sum = ((col + 1) until map.head.length)
        .takeWhile { currentCol =>
          treesVisible = treesVisible + 1
          val currentTreeHeight = map(row)(currentCol)
          currentTreeHeight < treeHeight
        }
        .map(_ => 1)
        .sum
      // println(sum)
      treesVisible
    }
  }

  def computeScenicScore(map: ScenicMap, row: Int, col: Int): Int = {
    val up = computeScoreLookingUp(map, row, col)
    val down = computeScoreLookingDown(map, row, col)
    val left = computeScoreLookingLeft(map, row, col)
    val right = computeScoreLookingRight(map, row, col)

    up * down * left * right
  }

  def assertScenicScore(
      scoreMap: Array[Array[Int]],
      row: Int,
      col: Int,
      value: Int
  ): Unit = {
    assert(
      scoreMap(row)(col) == value,
      s"Row $row, Col $col: ${scoreMap(row)(col)} != $value"
    )
  }

  def main(args: Array[String]): Unit = {
    val contents = readFileToLines("src/main/resources/day08/input")

    val map = contents
      .filterNot(line => line.trim.isEmpty)
      .map { line =>
        line.toCharArray.map { chr => chr - '0' }.toList
      }
      .toList

    val scenicScore = Array.ofDim[Int](map.length, map.head.length)

    map.indices.foreach { row =>
      map(row).indices.foreach { col =>
        scenicScore(row)(col) = computeScenicScore(map, row, col)
      }
    }

    // some assertions before I refactor
    assertScenicScore(scenicScore, 0, 0, 0)
    assertScenicScore(scenicScore, 0, map.head.length - 1, 0)
    assertScenicScore(scenicScore, map.length - 1, map.head.length - 1, 0)
    assertScenicScore(scenicScore, 50, 50, 8)
    assertScenicScore(scenicScore, 25, 25, 1)
    assertScenicScore(scenicScore, 2, 1, 1)
    assertScenicScore(scenicScore, 75, 30, 4)
    assertScenicScore(scenicScore, 97, 45, 4)
    assertScenicScore(scenicScore, 97, 86, 24)
    assertScenicScore(scenicScore, 97, 57, 432)
    assertScenicScore(scenicScore, 96, 65, 20328)
    assertScenicScore(scenicScore, 76, 40, 125_840)
    assertScenicScore(scenicScore, 9, 54, 648)
    assertScenicScore(scenicScore, 14, 11, 5040)

    val highest = scenicScore.map(_.max).max
    assert(highest == 313_200)
    println(s"Highest: $highest")
  }
}
