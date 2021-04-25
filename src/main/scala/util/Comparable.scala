package util

import scala.math.ScalaNumber

object Comparable {

  val GreaterThan: (ScalaNumber, ScalaNumber) => Boolean =
    (a, b) => a.asInstanceOf[BigDecimal] > b.asInstanceOf[BigDecimal]

}
