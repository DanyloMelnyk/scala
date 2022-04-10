package ua.lviv.mel2.scala.lab1

import ua.lviv.mel2.scala.lab1.Lab1.{Matrix, _}
import ua.lviv.mel2.scala.lab1.exceptions.BadMatrixShapeException

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

object Gauss {
  def forward(matrix: Matrix): Matrix = {
    var tempArr = matrix.arr
    tempArr.indices.foreach(k => {
      val mainRow = tempArr(k) / tempArr(k)(k)
      tempArr = tempArr.zipWithIndex.map { case (row, i) =>
        if (i > k) (row - (mainRow * row(k))).vect else row
      }
    })

    tempArr
  }

  def forward(a: Matrix, b: Vector): Try[(Matrix, Vector)] = {
    if (a.shape._1 != b.vect.length) {
      return Failure(BadMatrixShapeException)
    }

    val extended = a.arr.transpose.concat(Array(b.vect)).transpose

    val resExtendedTransposed = forward(extended).arr.transpose

    val a_res = resExtendedTransposed.init.transpose

    val b_res = resExtendedTransposed.last

    Success(Matrix(a_res) -> Vector(b_res))
  }

  def backward(a: Matrix, b: Vector): Vector = {
    val rows = a.shape._1
    val A = a.arr

    val x = ArrayBuffer.fill[Double](rows)(0)

    b.vect.indices.reverse.foreach(k => {
      val t = b.vect(k) - (A(k).slice(k + 1, rows) * x.slice(k + 1, rows).toArray).vect.sum

      x(k) = t / A(k)(k)
    })

    x.toArray
  }

  def gauss(a: Matrix, b: Vector): Try[Vector] = {
    forward(a, b).map { case (a, b) => backward(a, b) }
  }
}
