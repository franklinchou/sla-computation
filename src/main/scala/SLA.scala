import util.DateUtil

import java.time.{Duration, LocalDate}
import scala.math.ScalaNumber
import util.Comparable._

case class SLA(description: String,
               allocationPercentage: BigDecimal,
               serviceLevelTarget: BigDecimal,
               evaluationCriteria: (BigDecimal, BigDecimal) => Boolean,
               measurementWindow: Duration,
               effectiveDate: LocalDate,
               observationDate: LocalDate,
               observations: Seq[SLAObservation] = Seq.empty[SLAObservation]) {

  private val endOfMeasurementWindow: LocalDate =
    effectiveDate.plusDays(measurementWindow.toDays)

  private val measurementWindowDates: Seq[LocalDate] =
    effectiveDate.toEpochDay.until(endOfMeasurementWindow.toEpochDay).map(LocalDate.ofEpochDay)

  private  val measurementsInWindow =
    observations.filter(f => measurementWindowDates.contains(f.observationDate))

  // Get only the observations applicable to the measurement window
  // If there are multiple observations for a date get the least favorable observation (the max value if the goal is
  // to minimize, the min value if the goal is to maximize)
  private val relevantObservations: Seq[SLAObservation] =
    evaluationCriteria match {
      case x if x.equals(GreaterThan) || x.equals(GreaterThanEqualTo) =>
        measurementsInWindow.groupBy(_.observationDate).map(o => o._2.minBy(_.serviceLevelObserved)).toSeq
      case x if x.equals(LessThan) || x.equals(LessThanEqualTo) =>
        measurementsInWindow.groupBy(_.observationDate).map(o => o._2.maxBy(_.serviceLevelObserved)).toSeq
    }

  private val mostRecent: Option[LocalDate] =
    relevantObservations match {
      case Nil => None
      case x :: Nil => Some(x.observationDate)
      case xs => Some(xs.maxBy(_.observationDate).observationDate)
    }

  // Retrieve the number of dates actually elapsed during the measurement window
  // Inclusive between can be used here because the assumption is that if the date exists, data
  // has been collected for that observation date
  private val applicableMeasurementDates: Int =
    mostRecent.map { m => DateUtil.inclusiveBetween(observationDate, m).getDays }.getOrElse(0)

  val serviceLevelInMeasurementWindow: BigDecimal =
    applicableMeasurementDates match {
      case 0 => BigDecimal(0)
      case x if x < 0 => BigDecimal(0)
      case _ => relevantObservations.map(_.serviceLevelObserved).sum / applicableMeasurementDates
    }

  val isPassing: Boolean =
    evaluationCriteria(serviceLevelInMeasurementWindow, serviceLevelTarget)

  val isFailing: Boolean = !isPassing
}