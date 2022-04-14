package ua.lviv.mel2.scala.lab1

import ua.lviv.mel2.scala.lab1.Lab1.{Matrix, Vector}

object TestUtils {
  implicit class ImprovedVect(val vect: Vector) {
    def isClose(other: Vector, eps: Double): Boolean = {
      (vect - other).length < eps
    }
  }

  implicit class ImprovedMat(val matrix: Matrix) {
    def isClose(other: Matrix, eps: Double): Boolean = {
      matrix.arr.zip(other.arr).forall { case (a, b) => Vector(a).isClose(b, eps) }
    }
  }
}
