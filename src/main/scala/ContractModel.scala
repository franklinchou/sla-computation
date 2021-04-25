case class ContractModel(name: String,
                         atRiskRatio: BigDecimal = BigDecimal(0.13),
                         totalContractValue: BigDecimal = BigDecimal(5000000),
                         logicalResourceUnits: Set[LRUModel]) {

  // Service level credits need to be computed at the contract model level
  // because the total contract value and at risk amount are at the contract model level
  val mapServiceLevelCreditToServiceLevel: Map[SLAModel, BigDecimal] =
    logicalResourceUnits.flatMap { lru =>
      lru.serviceLevelAgreements.map { sla =>
        sla -> serviceLevelCreditFormula(sla, lru, BigDecimal(0.4))
      }
    }.toMap

  def serviceLevelCreditFormula(s: SLAModel, lru: LRUModel, defaultFactor: BigDecimal): BigDecimal = {
    val atRiskAmount = atRiskRatio * totalContractValue
    val cpiAllocationPercentage = s.allocationPercentage
    val lruAllocationPercentage = lru.allocationPercentage
    atRiskAmount * lruAllocationPercentage * cpiAllocationPercentage * defaultFactor
  }

  val serviceLevelCreditTotal: BigDecimal = mapServiceLevelCreditToServiceLevel.values.sum

  val atRiskAmount: BigDecimal = totalContractValue * atRiskRatio

}

