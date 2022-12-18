package aoc2022

import scala.collection.mutable

object Day18Part1 {
  case class Cube(x: Int, y: Int, z: Int)
  case class Face(planeAxis: String, axisValue: Int,
                  dp: Int, // planeAxis(y) => x, planeAxis(z) => x, planeAxis(x) => y
                  ds: Int) // planeAxis(y) => z  planeAxis(z) => y, planeAxis(x) => z

  def cubeToFaces(cube: Cube): Array[Face] = {
    Array(
      Face("y", cube.y - 1, cube.x, cube.z),
      Face("y", cube.y, cube.x, cube.z),
      Face("z", cube.z - 1, cube.x, cube.y),
      Face("z", cube.z, cube.x, cube.y),
      Face("x", cube.x - 1, cube.y, cube.z),
      Face("x", cube.x, cube.y, cube.z),
    )
  }

  def computeArea(faces: Array[Face]): Int = {
    val removeIndices = mutable.HashSet[Int]()
    for {
      i <- faces.indices
      j <- (i + 1) until faces.length
    } {
      val iface = faces(i)
      val jface = faces(j)
      if (iface == jface) {
        println(s"$iface touches $jface")
        removeIndices.add(i)
        removeIndices.add(j)
      }
    }

    faces.length - removeIndices.size
  }

  def main(args: Array[String]): Unit = {
    val faces: Array[Face] = readFileToLines(s"src/main/resources/day18/input")
      .filterNot(_.isBlank)
      .map { cubeStr =>
        val ints = cubeStr.split(",").map(_.trim.toInt)
        Cube(ints(0), ints(1), ints(2))
      }.flatMap(cubeToFaces)

    println(computeArea(faces))
  }
}
