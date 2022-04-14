package ua.lviv.mel2.scala.lab1

import ua.lviv.mel2.scala.lab1.Lab1.{Matrix, _}
import ua.lviv.mel2.scala.lab1.exceptions.{BadMatrixShapeException, DegenerateMatrixException, NotSquareMatrixException}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

object Gauss {
  val EPS = 0.000_001

  def forward(matrix: Matrix): Try[Matrix] = {
    var tempArr = matrix.arr
    tempArr.indices.foreach(k => {
      if (Math.abs(tempArr(k)(k)) < EPS) {
        return Failure(DegenerateMatrixException)
      }

      val mainRow = tempArr(k) / tempArr(k)(k)
      tempArr = tempArr.zipWithIndex.map { case (row, i) =>
        if (i > k) (row - (mainRow * row(k))).vect else row
      }
    })

    Success(tempArr)
  }

  def forward(a: Matrix, b: Vector): Try[(Matrix, Vector)] = {
    if (a.rows != b.vect.length) {
      return Failure(BadMatrixShapeException)
    }

    a.concatH(Array(b.vect).transpose)
      .flatMap(forward)
      .map(_.splitH(a.cols))
      .map { case (a, b) =>
        a -> b.arr.flatten
      }
  }

  def backward(a: Matrix, b: Vector): Vector = {
    val A = a.arr

    val x = ArrayBuffer.fill[Double](a.rows)(0)

    b.vect.indices.reverse.foreach(k => {
      val t = b.vect(k) - (A(k).slice(k + 1, a.rows) * x.slice(k + 1, a.rows).toArray).vect.sum

      x(k) = t / A(k)(k)
    })

    x.toArray
  }

  def gauss(a: Matrix, b: Vector): Try[Vector] = {
    forward(a, b)
      .map { case (a, b) =>
        backward(a, b)
      }
  }

  def inverseForward(a: Matrix): Try[(Matrix, Matrix)] = {
    a.concatH(Matrix.ones(a.rows))
      .flatMap(forward)
      .map(_.splitH(a.rows))
  }

  def inverse(a: Matrix): Try[Matrix] = {
    if (a.cols != a.rows) {
      return Failure(NotSquareMatrixException)
    }

    inverseForward(a).map { case (a, e) => e.arr
      .transpose
      .map(col => backward(a, col).vect)
      .transpose
    }
  }
}
