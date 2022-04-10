package ua.lviv.mel2.scala.lab1

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import ua.lviv.mel2.scala.lab1.Lab1._

class GaussTest extends AnyWordSpec {

  "Gauss" should {
    "SLAR 3" in {
      val a = Array(
        Array(4.0, 2.0, -1.0),
        Array(5.0, 3.0, -2.0),
        Array(3.0, 2.0, -3.0),
      )
      val b = Array(1.0, 2.0, 0.0)

      val result = Gauss.gauss(a, b)
      assert(result.isSuccess)
      result.get.vect shouldBe Array(-1.0, 3.0, 1.0)
    }


    "SLAR 4" in {
      val a = Array(
        Array(1.0, -1.0, 3.0, 1.0),
        Array(4.0, -1.0, 5.0, 4.0),
        Array(2.0, -2.0, 4.0, 1.0),
        Array(1.0, -4.0, 5.0, -1.0),
      )
      val b = Array(5.0, 4.0, 6.0, 3.0)

      val result = Gauss.gauss(a, b)

      assert(result.isSuccess)
      result.get.vect shouldBe Array(9.0, 18.0, 10.0, -16.0)
    }

    // FIXME add SLAR without finite x
  }

}
