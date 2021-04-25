import org.scalatest.funspec.AnyFunSpec
import util.Comparable._

import scala.language.implicitConversions
import scala.math.ScalaNumber
import scala.math.BigInt.int2bigInt

class ComparableSpec extends AnyFunSpec {

  describe("Comparable tests") {

    it ("should properly identify greater than") {
      Map[(ScalaNumber, ScalaNumber), Boolean](
        (2, 2) -> false,
        (10, 9) -> true,
        (BigDecimal(10), BigDecimal(9)) -> true,
        (10, BigDecimal(9)) -> true,
        (BigDecimal(10), 9) -> true,
        (11, 2.2) -> true,
        (BigDecimal(11), 2.2) -> true,
      ).foreach { mi =>
        assert(GreaterThan(mi._1._1, mi._1._2) == mi._2 )
      }
    }

    it ("should properly identify less than") {

    }

    it ("should properly identify greater than or equal to") {

    }

    it ("should properly identify less than or equal to") {

    }
  }

}
