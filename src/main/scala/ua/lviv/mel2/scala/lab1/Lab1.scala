package ua.lviv.mel2.scala.lab1

import ua.lviv.mel2.scala.lab1.exceptions.{BadMatricesShapeException, BadMatrixShapeException, NotEqualRowLengthException}

import java.util
import scala.io.Source
import scala.math.sqrt
import scala.util.{Failure, Success, Try}

object Lab1 extends App {
  implicit class Vector(val vect: Array[Double]) {
    def +(double: Double): Vector = {
      vect.map(_ + double)
    }

    def -(double: Double): Vector = {
      vect.map(_ - double)
    }

    def *(double: Double): Vector = {
      vect.map(_ * double)
    }

    def /(double: Double): Vector = {
      vect.map(_ / double)
    }

    def +(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map(i => i._1 + i._2)
    }

    def -(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map(i => i._1 - i._2)
    }

    def *(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map(i => i._1 * i._2)
    }

    def /(vector: Vector): Vector = {
      vect.zip(vector.vect)
        .map(i => i._1 / i._2)
    }

    def ==(vector: Vector): Boolean = {
      vect.zip(vector.vect)
        .forall { case (i1, i2) => i1 == i2 }
    }

    def length: Double = {
      sqrt(vect.map(i => i * i).sum)
    }

    override def toString: String = {
      util.Arrays.toString(vect)
    }
  }

  implicit class Matrix(var arr: Array[Array[Double]]) {
    checkRowLength()

    def checkRowLength(): Unit = {
      if (arr.map(_.length)
        .reduce((l, r) => if (l == r) l else -1) == -1) {
        throw new NotEqualRowLengthException()
      }
    }

    override def toString: String = {
      arr.map(_.mkString(" ")).mkString("\n")
    }

    def *(double: Double): Matrix = {
      arr.map(_.map(_ * double))
    }

    def *(matrix: Matrix): Try[Matrix] = {
      if (shape._2 != matrix.shape._1) {
        return Failure(BadMatricesShapeException)
      }


      val otherTransposed = matrix.arr.transpose
      val t = Array.tabulate(shape._1, matrix.shape._2) { case (i, j) =>
        (arr(i) * otherTransposed(j)).vect.sum
      }

      Success(Matrix(t))
    }

    def det: Try[Double] = {
      if (shape._1 != shape._2) {
        return Failure(BadMatrixShapeException)
      }
      Success(Gauss.forward(this).arr.zipWithIndex.map { case (row, i) => row(i) }.product)
    }

    def shape: (Int, Int) = {
      arr.length -> arr.head.length
    }

    def ==(matrix: Matrix): Boolean = {
      arr.zip(matrix.arr)
        .forall { case (i1, i2) =>
          i1 sameElements i2
        }
    }
  }


  def readMatrix(filename: String): Array[Array[Double]] = {
    val file = Source.fromFile(filename)
    val array = file
      .getLines()
      .map(parseLine).toArray

    // TODO check size?
    file.close()
    array
  }

  def parseLine(line: String): Array[Double] = {
    line.split("\\s+")
      .filterNot(_.isBlank)
      .map(_.toDouble)
  }

  val inputFile = "data/test1.txt" //readLine("Enter matrix filename:\n")
  val matrix = readMatrix(inputFile)
  println(Matrix(matrix) + "\n")
  // FIXME
  //  println(matrix.equivalentTriangle + "\n")
  println("Det:\n" + matrix.det)
}
