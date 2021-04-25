import org.scalatest.funspec.AnyFunSpec

import java.time.{Duration, LocalDate}

class LRUModelSpec extends AnyFunSpec {

  describe("LRU Model") {
    it("should validate a single SLA") {

      val sla1 =
        SLAModel("test1",
          slaType = CPI,
          allocationPercentage = BigDecimal(0.66),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(90),
          effectiveDate = LocalDate.now(),
          observationDate = LocalDate.now()
        )

      val lru =
        LRUModel("test lru 1", serviceLevelAgreements = List(sla1), allocationPercentage = BigDecimal(1))

      assert(!lru.validateSLA())
      assert(lru.totalAllocationPercentage == BigDecimal(0.66))
    }

    it("should validate multiple SLAs") {

      val sla1 =
        SLAModel("test1",
          slaType = CPI,
          allocationPercentage = BigDecimal(0.63),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(90),
          effectiveDate = LocalDate.now(),
          observationDate = LocalDate.now()
        )

      val sla2 =
        SLAModel("test2",
          slaType = CPI,
          allocationPercentage = BigDecimal(0.63),
          serviceLevelTarget = BigDecimal(0.9),
          evaluationCriteria = util.Comparable.GreaterThan,
          measurementWindow = Duration.ofDays(90),
          effectiveDate = LocalDate.now(),
          observationDate = LocalDate.now()
        )

      val lru =
        LRUModel("test lru 1", serviceLevelAgreements = List(sla1, sla2), allocationPercentage = BigDecimal(1))

      assert(lru.validateSLA())
      assert(lru.totalAllocationPercentage == BigDecimal(1.26))
    }
  }


}
