import org.scalatest.funspec.AnyFunSpec

import java.time.{Duration, LocalDate, Period}

class SLAModeSpec extends AnyFunSpec {

  describe("An SLA model") {

    it("should properly handle no historic observations") {

      val testSLA =
        SLA("Test of nothing",
          allocationPercentage = BigDecimal(0.15),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(90),
          effectiveDate = LocalDate.now(),
          observationDate = LocalDate.now()
        )

      // An SLA with no observations should fail
      assert(!testSLA.isPassing)
    }

    it("should properly handle out of bounds historic observations - no observations") {

      val daysOfHistory = 10

      val startDate = LocalDate.of(1997, 8, 29)

      val history: Seq[SLAObservation] =
        (0 to daysOfHistory)
          .map { d => startDate.plus(Period.ofDays(d)) }
          .map { d => SLAObservation(d, BigDecimal(0.99)) }

      val testSLA =
        SLA("Test of nothing",
          allocationPercentage = BigDecimal(0.15),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(90),
          effectiveDate = LocalDate.now(),
          observationDate = LocalDate.now(),
          observations = history
        )

      // An SLA with no observations should fail
      assert(!testSLA.isPassing)
      assert(testSLA.serviceLevelInMeasurementWindow == BigDecimal(0))
    }

    it("should properly handle out of bounds historic observations - some observations") {

      val startDate = LocalDate.of(2021, 4, 4)

      // If there is no data for a date where an SLA measurement should have been made,
      // the measurement date will still be counted. E.g., there is no data for 4/4/2021,
      // so the three day window starting on 4/4/2021, will have a three day denominator but
      // only 2 measurements.
      val history: Seq[SLAObservation] =
        Seq(
          SLAObservation(LocalDate.of(2021, 4, 5), BigDecimal(0.9)),
          SLAObservation(LocalDate.of(2021, 4, 6), BigDecimal(0.9)),
          SLAObservation(LocalDate.of(2021, 3, 1), BigDecimal(0.1))
        )

      val testSLA =
        SLA("Test of nothing",
          allocationPercentage = BigDecimal(0.15),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(3),
          effectiveDate = startDate,
          observationDate = startDate,
          observations = history
        )

      assert(!testSLA.isPassing)
      assert(testSLA.serviceLevelInMeasurementWindow == BigDecimal(0.6))
    }

    it("should properly handle multiple observations - maximize") {

      val startDate = LocalDate.of(2021, 4, 1)

      val history: Seq[SLAObservation] =
      Seq(
        SLAObservation(LocalDate.of(2021, 4, 1), BigDecimal(0.9)),
        SLAObservation(LocalDate.of(2021, 4, 1), BigDecimal(0.95)),
        SLAObservation(LocalDate.of(2021, 4, 1), BigDecimal(0.3))
      )

      val testSLA =
        SLA("Test of nothing",
          allocationPercentage = BigDecimal(0.15),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(1),
          effectiveDate = startDate,
          observationDate = startDate,
          observations = history
        )

      assert(testSLA.serviceLevelInMeasurementWindow == BigDecimal(0.3))
      assert(!testSLA.isPassing)
    }

    it("should properly handle multiple observations - minimize") {

      val startDate = LocalDate.of(2021, 4, 1)

      val history: Seq[SLAObservation] =
        Seq(
          SLAObservation(LocalDate.of(2021, 4, 1), BigDecimal(0.9)),
          SLAObservation(LocalDate.of(2021, 4, 1), BigDecimal(0.95)),
          SLAObservation(LocalDate.of(2021, 4, 1), BigDecimal(0.3))
        )

      val testSLA =
        SLA("Test of nothing",
          allocationPercentage = BigDecimal(0.15),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.LessThan,
          measurementWindow = Duration.ofDays(1),
          effectiveDate = startDate,
          observationDate = startDate,
          observations = history
        )

      assert(testSLA.serviceLevelInMeasurementWindow == BigDecimal(0.95))
      assert(!testSLA.isPassing)
    }

    it("should properly evaluate passing criteria") {

      val r = scala.util.Random

      val passing = 0.9

      val daysOfHistory = 10

      val startDate = LocalDate.now().minus(Period.ofDays(daysOfHistory))

      val history: Seq[SLAObservation] =
        (0 to daysOfHistory)
          .map { d => LocalDate.now().minus(Period.ofDays(d)) }
          .map { d => SLAObservation(d, BigDecimal(r.between(passing, 1.0))) }

      val testSLA =
        SLA("Server uptime in 3 months > 0.9",
          allocationPercentage = BigDecimal(0.15),
          serviceLevelTarget = BigDecimal(passing),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(90),
          effectiveDate = startDate,
          observationDate = startDate,
          observations = history
        )

      assert(testSLA.isPassing)
      assert(!testSLA.isFailing)
      assert(testSLA.serviceLevelInMeasurementWindow < 1)
    }
  }
}
