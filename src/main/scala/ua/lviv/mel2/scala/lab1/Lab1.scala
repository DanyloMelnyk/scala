package ua.lviv.mel2.scala.lab1

import ua.lviv.mel2.scala.lab1.exceptions.{IncompatibleMatricesShapes, NotEqualRowLengthException, NotSquareMatrixException}

import java.io.FileNotFoundException
import scala.io.Source
import scala.io.StdIn.readLine
import scala.math.sqrt
import scala.util.{Failure, Success, Try}

object Lab1 extends App {
  implicit class Vector(val vect: Array[Double]) {

    def +(double: Double): Vector = vect.map(_ + double)

    def -(double: Double): Vector = vect.map(_ - double)

    def *(double: Double): Vector = vect.map(_ * double)

    def /(double: Double): Vector = vect.map(_ / double)

    def +(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map { case (i, j) => i + j }
    }

    def -(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map { case (i, j) => i - j }
    }

    def *(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map { case (i, j) => i * j }
    }

    def /(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map { case (i, j) => i / j }
    }

    def ==(vector: Vector): Boolean = {
      vector.length == vect.length &&
        vect.zip(vector.vect)
          .forall { case (i, j) => i == j }
    }

    def length: Double = {
      sqrt(vect.map(i => i * i).sum)
    }

    override def toString: String = vect.map(i => f"$i%.3f").mkString("  ")
  }

  implicit class Matrix(var arr: Array[Array[Double]]) {
    checkRowLength()

    def checkRowLength(): Unit = {
      if (arr.map(_.length)
        .reduce((l, r) => if (l == r) l else -1) == -1) {
        throw NotEqualRowLengthException
      }
    }

    override def toString: String = {
      arr.map(Vector).mkString("\n")
    }

    def *(double: Double): Matrix = {
      arr.map(i => (i * double).vect)
    }

    def *(matrix: Matrix): Try[Matrix] = {
      if (cols != matrix.rows) {
        return Failure(IncompatibleMatricesShapes)
      }

      val otherTransposed = matrix.arr.transpose
      val t = Array.tabulate(rows, matrix.cols) { case (i, j) =>
        (arr(i) * otherTransposed(j)).vect.sum
      }

      Success(Matrix(t))
    }

    def det: Try[Double] = {
      if (rows != cols) {
        return Failure(NotSquareMatrixException)
      }
      Gauss.forward(this)
        .map(_.arr.zipWithIndex.map { case (row, i) => row(i) }.product)
    }

    def shape: (Int, Int) = {
      arr.length -> arr.head.length
    }

    def rows: Int = arr.length

    def cols: Int = arr.head.length

    def ==(matrix: Matrix): Boolean = {
      matrix.shape == shape && arr.zip(matrix.arr)
        .forall { case (i1, i2) =>
          i1 sameElements i2
        }
    }

    def concatV(matrix: Matrix): Try[Matrix] = {
      if (cols != matrix.cols) {
        Failure(IncompatibleMatricesShapes)
      } else {
        Success(arr.concat(matrix.arr))
      }
    }

    def concatH(matrix: Matrix): Try[Matrix] = {
      if (rows != matrix.rows) {
        Failure(IncompatibleMatricesShapes)
      } else {
        Success(arr.transpose.concat(matrix.arr.transpose).transpose)
      }
    }

    def splitH(leftCols: Int): (Matrix, Matrix) = {
      val left = arr.transpose.slice(0, leftCols).transpose
      val right = arr.transpose.slice(leftCols, cols + 1).transpose

      Matrix(left) -> right
    }
  }

  object Matrix {
    def ones(size: Int): Matrix = {
      Array.tabulate(size, size) {
        case (i, j) if i == j => 1.0
        case _ => 0.0
      }
    }
  }


  def readMatrix(filename: String): Try[Matrix] = Try {
    val file = Source.fromFile(filename)
    val array = file
      .getLines()
      .map(parseLine).toArray

    file.close()
    array
  }

  def parseLine(line: String): Array[Double] = {
    line.split("\\s+")
      .filterNot(_.isBlank)
      .map(_.toDouble)
  }

  println(
    readMatrix(readLine("Enter matrix filename:\n"))
      .flatMap(Gauss.inverse)
      .fold(
        {
          case _: FileNotFoundException => "Can't find file!"
          case e: NumberFormatException => "Can't parse - " + e.getMessage
          case e =>
            "Can't find inverse matrix: " + e.getMessage
        },
        inverse => "Inverse matrix:\n" + inverse
      )
  )
}
