package ua.lviv.mel2.scala.lab1

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import ua.lviv.mel2.scala.lab1.Lab1._
import ua.lviv.mel2.scala.lab1.TestUtils._
import ua.lviv.mel2.scala.lab1.exceptions.DegenerateMatrixException

import scala.util.{Failure, Random}

class GaussTest extends AnyWordSpec {

  "Gauss" should {
    "SLAR 3" in {
      val a = Array(
        Array(4.0, 2.0, -1.0),
        Array(5.0, 3.0, -2.0),
        Array(3.0, 2.0, -3.0),
      )
      val b = Array(1.0, 2.0, 0.0)

      assert(Gauss.gauss(a, b).get.isClose(Array(-1.0, 3.0, 1.0), 0.001))
    }


    "SLAR 4" in {
      val a = Array(
        Array(1.0, -1.0, 3.0, 1.0),
        Array(4.0, -1.0, 5.0, 4.0),
        Array(2.0, -2.0, 4.0, 1.0),
        Array(1.0, -4.0, 5.0, -1.0),
      )
      val b = Array(5.0, 4.0, 6.0, 3.0)

      assert(Gauss.gauss(a, b).get.isClose(Array(9.0, 18.0, 10.0, -16.0), 0.001))
    }

    "inverse 2" in {
      val a = Array(
        Array(3.0, 4.0),
        Array(5.0, 7.0)
      )

      assert((Gauss.inverse(a).get * a).get.isClose(Matrix.ones(2), 0.001))
    }

    "inverse 20" in {
      val size = 20
      val a = Array.tabulate(size, size) { case (_, _) => Random.nextDouble() }

      if (a.det.get != 0) {
        assert((Gauss.inverse(a).get * a).get.isClose(Matrix.ones(size), 0.001))
      } else {
        Gauss.inverse(a) shouldBe Failure(DegenerateMatrixException)
      }

    }

    "inverse degenerate matrix" in {
      val a = Array(
        Array(-25.0, -5.0, -10.0),
        Array(14.0, 3.0, 5.0),
        Array(5.0, 1.0, 2.0)
      )

      Gauss.inverse(a) shouldBe Failure(DegenerateMatrixException)
    }

    // FIXME add SLAR without finite x
  }

}
