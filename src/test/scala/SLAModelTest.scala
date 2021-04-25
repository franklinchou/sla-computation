import org.scalatest.funspec.AnyFunSpec

import java.time.{Duration, LocalDate, Period}

class SLAModelTest extends AnyFunSpec {

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

    it("should properly handle out of bounds historic observations") {

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
