case class ContractModel(name: String,
                         atRiskRatio: BigDecimal = BigDecimal(0.13),
                         totalContractValue: BigDecimal = BigDecimal(5000000),
                         logicalResourceUnits: List[LRUModel]) {

  private val mapLRUs2LRUAllocations =
    Map(
      1 -> BigDecimal(1),
      2 -> BigDecimal(0.8),
      3 -> BigDecimal(0.7),
      4 -> BigDecimal(0.65),
      5 -> BigDecimal(0.6),
      6 -> BigDecimal(0.5)
    )

  private val maxLRUs: Int = mapLRUs2LRUAllocations.maxBy(_._1)._1

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

  def totalLRUs: Int = logicalResourceUnits.size

  def validateLRU(): Boolean =
    if (mapLRUs2LRUAllocations.keySet.contains(totalLRUs)) {
      totalLRUs < mapLRUs2LRUAllocations(totalLRUs)
    } else if (totalLRUs > maxLRUs) {
      totalLRUs < mapLRUs2LRUAllocations(maxLRUs)
    } else {
      false
    }

  val serviceLevelCreditTotal: BigDecimal = mapServiceLevelCreditToServiceLevel.values.sum

  val atRiskAmount: BigDecimal = totalContractValue * atRiskRatio

}

