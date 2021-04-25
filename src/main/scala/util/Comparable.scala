package util

object Comparable {

  val GreaterThan: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a > b

  val GreaterThanEqualTo: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a >= b

  val LessThan: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a < b

  val LessThanEqualTo: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a <= b
}
