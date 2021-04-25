import org.scalatest.funspec.AnyFunSpec

import java.time.{Duration, LocalDate, Period}

class ContractModelSLA extends AnyFunSpec {

  describe("A contract model") {

    val daysOfHistory = 5

    val observationDate = LocalDate.now()

    val startDate = observationDate.minus(Period.ofDays(5))

    val history =
      (0 to daysOfHistory)
        .map { d => startDate.plus(Period.ofDays(d)) }
        .map { d => SLAObservationModel(d, BigDecimal(0.0019)) }

    val sla1 =
      SLAModel("Total free space >= 3%, rolling 5 day average",
        slaType = CPI,
        allocationPercentage = BigDecimal(0.15),
        serviceLevelTarget = BigDecimal(0.03),
        evaluationCriteria = util.Comparable.GreaterThanEqualTo,
        measurementWindow = Duration.ofDays(5),
        effectiveDate = startDate,
        observationDate = observationDate,
        observations = history
      )

    val lru1 =
      LRUModel("LRU 1",
        serviceLevelAgreements = Set(sla1),
        allocationPercentage = BigDecimal(.1)
      )

    val c1 =
      ContractModel("Test contract 1",
        logicalResourceUnits = Set(lru1)
      )

    it("should compute the total at risk amount") {
      assert(c1.atRiskAmount == BigDecimal(0.13 * 5000000))
    }

    it("should properly map service levels to credits") {
      assert(c1.serviceLevelCreditTotal == BigDecimal(3900))
    }

  }

}
