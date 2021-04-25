package util

import java.time.{LocalDate, Period}

object DateUtil {

  def inclusiveBetween(a: LocalDate, b: LocalDate): Period = {
    var i: Int = 0
    var ptr = a
    if (b.isAfter(a)) {
      while (ptr.compareTo(a) >= 0 && ptr.compareTo(b) <= 0) {
        i = i + 1
        ptr = ptr.plusDays(1)
      }
      Period.ofDays(i)
    } else if (a.isEqual(b)) {
      Period.ofDays(1)
    }else {
      Period.ZERO
    }
  }

}
