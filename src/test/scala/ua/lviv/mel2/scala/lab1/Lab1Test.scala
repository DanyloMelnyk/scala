package ua.lviv.mel2.scala.lab1

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import ua.lviv.mel2.scala.lab1.Lab1._
import ua.lviv.mel2.scala.lab1.TestUtils._
import ua.lviv.mel2.scala.lab1.exceptions.{IncompatibleMatricesShapes, NotEqualRowLengthException, NotSquareMatrixException}

import scala.util.{Failure, Success}


class Lab1Test extends AnyWordSpec {

  "Vector" should {
    "add 2 vectors" in {
      val t = Array(1.0, 2.0, 3.0) + Array(1.0, 2.0, -3.0)
      assert(t.isClose(Array(2.0, 4.0, 0.0), 0.001))
    }

    "add const" in {
      val t = Array(1.0, 2.0, 3.0) + 5
      assert(t.isClose(Array(6.0, 7.0, 8.0), 0.001))
    }

    "sub 2 vectors" in {
      val t = Array(1.0, 2.0, 3.0) - Array(1.0, 3.0, -3.0)
      assert(t.isClose(Array(0.0, -1.0, 6.0), 0.001))
    }

    "sub const" in {
      val t = Array(1.0, 2.0, 3.0) - 1
      assert(t.isClose(Array(0.0, 1.0, 2.0), 0.001))
    }

    "mul 2 vectors" in {
      val t = Array(1.0, 2.0, 3.0) * Array(2.0, 3.0, 4.0)
      assert(t.isClose(Array(2.0, 6.0, 12.0), 0.001))
    }

    "mul const" in {
      val t = Array(1.0, 2.0, 3.0) * 2
      assert(t.isClose(Array(2.0, 4.0, 6.0), 0.001))
    }

    "div 2 vectors" in {
      val t = Array(10.0, -9.0, 4.0) / Array(2.0, 3.0, 4.0)
      assert(t.isClose(Array(5.0, -3.0, 1.0), 0.001))
    }

    "div const" in {
      val t = Array(1.0, 2.0, 3.0) / 2
      assert(t.isClose(Array(0.5, 1.0, 1.5), 0.001))
    }
  }

  "Matrix" should {

    "check row length" in {
      assertThrows[NotEqualRowLengthException.type] {
        Matrix(Array(Array(1.0, 1.0), Array(1.0)))
      }
    }

    "return true size" in {
      assertResult((2, 3)) {
        Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)).shape
      }
    }

    "mul check matrices shapes" in {
      val a = Array(
        Array(1.0, 2.0, 3.0),
        Array(4.0, 5.0, 6.0))

      val b = Array(Array(1.0, 2.0, 3.0))

      (a * b) shouldBe Failure(IncompatibleMatricesShapes)
    }

    "mul matrices" in {
      val a = Array(Array(4.0, 2.0), Array(9.0, 0.0))
      val b = Array(Array(3.0, 1.0), Array(-3.0, 4.0))
      val expected = Matrix(Array(Array(6.0, 12.0), Array(27.0, 9.0)))

      assert((a * b).filter(_ == expected).isSuccess)
    }

    "mul bigger matrices" in {
      val a = Array(Array(2.0, 1.0), Array(-3.0, 0.0), Array(4.0, -1.0))
      val b = Array(Array(5.0, -1.0, 6.0), Array(-3.0, 0.0, 7.0))
      val expected = Array(Array(7.0, -2.0, 19.0), Array(-15.0, 3.0, -18.0), Array(23.0, -4.0, 17.0))

      assert((a * b).get.isClose(expected, 0.001))
    }

    "mul const" in {
      val a = Array(Array(4.0, 2.0), Array(9.0, 0.0))
      val expected = Array(Array(12.0, 6.0), Array(27.0, 0.0))

      assert((a * 3).isClose(expected, 0.001))
    }

    "determinant for not square matrix" in {
      val a = Array(
        Array(1.0, 2.0, 3.0),
        Array(4.0, 5.0, 6.0))

      a.det shouldBe Failure(NotSquareMatrixException)
    }

    "determinant 2x2" in {
      val a = Array(
        Array(5.0, 7.0),
        Array(-4.0, 1.0))

      a.det shouldBe Success(33)
    }

    "determinant 3x3" in {
      val a = Array(
        Array(5.0, 7.0, 1.0),
        Array(-4.0, 1.0, 0),
        Array(2.0, 0.0, 3.0)
      )

      a.det shouldBe Success(97)
    }

    "determinant 4x4" in {
      val a = Array(
        Array(2.0, 4.0, 1.0, 1.0),
        Array(0.0, 2.0, 1.0, 0.0),
        Array(2.0, 1.0, 1.0, 3.0),
        Array(4.0, 0.0, 2.0, 3.0)
      )

      a.det shouldBe Success(-26)
    }

  }

}
