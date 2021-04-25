import util.DateUtil

import java.time.{Duration, LocalDate}
import scala.math.ScalaNumber

case class SLA(description: String,
               allocationPercentage: BigDecimal,
               serviceLevelTarget: BigDecimal,
               evaluationCriteria: (ScalaNumber, ScalaNumber) => Boolean,
               measurementWindow: Duration,
               effectiveDate: LocalDate,
               observationDate: LocalDate,
               observations: Seq[SLAObservation] = Seq.empty[SLAObservation]) {

  private val endOfMeasurementWindow: LocalDate =
    effectiveDate.plusDays(measurementWindow.toDays)

  private val measurementWindowDates: Seq[LocalDate] =
    effectiveDate.toEpochDay.until(endOfMeasurementWindow.toEpochDay).map(LocalDate.ofEpochDay)

  // Get only the observations applicable to the measurement window
  private val relevantObservations: Seq[SLAObservation] =
    observations.filter(f => measurementWindowDates.contains(f.observationDate))

  private val latestMeasurement: Option[SLAObservation] =
    relevantObservations match {
      case Nil => None
      case x :: Nil => Some(x)
      case xs => Some(xs.maxBy(_.observationDate))
    }

  // Retrieve the number of dates actually elapsed during the measurement window
  // Inclusive between can be used here because the assumption is that if the date exists, data
  // has been collected for that observation date
  private val applicableMeasurementDates: Int =
  latestMeasurement.map { m => DateUtil.inclusiveBetween(observationDate, m.observationDate).getDays }.getOrElse(0)

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