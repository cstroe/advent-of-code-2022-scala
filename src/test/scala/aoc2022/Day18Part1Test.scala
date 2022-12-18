package aoc2022

import aoc2022.Day17Part1.{CrossRock, FallingRock, FlatRock, Point}
import aoc2022.Day18Part1.{Cube, Face, computeArea, cubeToFaces}
import org.scalatest.matchers.should.Matchers

class Day18Part1Test extends org.scalatest.flatspec.AnyFlatSpec with Matchers {
  behavior of "Cubes"

  it should "convert Cubes to faces" in {
    val cube = Cube(1, 1, 1)
    val faces = cubeToFaces(cube)
    faces.length shouldBe 6
    faces should contain(Face("y", 0, 1, 1))
    faces should contain(Face("y", 1, 1, 1))
    faces should contain(Face("z", 0, 1, 1))
    faces should contain(Face("z", 1, 1, 1))
    faces should contain(Face("x", 0, 1, 1))
    faces should contain(Face("x", 1, 1, 1))

    val cube2 = Cube(2, 1, 1)
    val faces2 = cubeToFaces(cube2)
    faces2.length shouldBe 6
    faces2 should contain(Face("y", 0, 2, 1))
    faces2 should contain(Face("y", 1, 2, 1))
    faces2 should contain(Face("z", 0, 2, 1))
    faces2 should contain(Face("z", 1, 2, 1))
    faces2 should contain(Face("x", 1, 1, 1))
    faces2 should contain(Face("x", 2, 1, 1))
    
    val cube3 = Cube(1, 2, 1)
    val faces3 = cubeToFaces(cube3)
    faces3.length shouldBe 6
    faces3 should contain(Face("y", 1, 1, 1))
    faces3 should contain(Face("y", 2, 1, 1))
    faces3 should contain(Face("z", 0, 1, 2))
    faces3 should contain(Face("z", 1, 1, 2))
    faces3 should contain(Face("x", 0, 2, 1))
    faces3 should contain(Face("x", 1, 2, 1))
  }

  it should "compute the un-shared faces" in {
    val faces = Array(Cube(1, 1, 1), Cube(2, 1, 1), Cube(1, 2, 1)).flatMap(cubeToFaces)
    computeArea(faces) shouldBe 14
  }
}
