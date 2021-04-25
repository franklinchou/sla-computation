package util

object Comparable {

  // Need to add support for other numeric types
  val GreaterThan: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a > b

  val GreaterThanEqualTo: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a >= b

  val LessThan: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a < b

  val LessThanEqualTo: (BigDecimal, BigDecimal) => Boolean =
    (a, b) => a <= b
}
