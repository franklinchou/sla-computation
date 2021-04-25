import org.scalatest.funspec.AnyFunSpec
import util.Comparable._

import scala.language.implicitConversions

class ComparableSpec extends AnyFunSpec {

  describe("Comparable tests") {

    // Adding an implicit conversion doesn't work here.
    // Implicit conversion of tuple elements does not create implicit conversion of a whole tuples
    it("should properly identify greater than") {
      val tests =
        Map(
          (2.0, 2.0) -> false,
          (3.0, 4.1) -> false,
          (4.1, 3.0) -> true
        )

      tests.foreach { mi => assert(GreaterThan(mi._1._1, mi._1._2) == mi._2) }
    }

    it("should properly identify less than") {
      val tests =
        Map(
          (2.0, 2.0) -> false,
          (0.991, 0.999) -> true,
          (1.0, 0.99) -> false
        )

      tests.foreach { mi => assert(LessThan(mi._1._1, mi._1._2) == mi._2) }
    }

    it("should properly identify greater than or equal to") {
      val tests =
        Map(
          (2.0, 2.0) -> true,
          (0.991, 0.999) -> false,
          (1.0, 0.99) -> true,
          (4.0, 1.0) -> true
        )

      tests.foreach { mi => assert(GreaterThanEqualTo(mi._1._1, mi._1._2) == mi._2) }
    }

    it("should properly identify less than or equal to") {
      val tests =
        Map(
          (2.0, 2.0) -> true,
          (0.991, 0.999) -> true,
          (1.0, 0.99) -> false
        )

      tests.foreach { mi => assert(LessThanEqualTo(mi._1._1, mi._1._2) == mi._2) }
    }
  }

}
