case class ContractModel(name: String,
                         atRiskAmount: BigDecimal = BigDecimal(0.13),
                         totalContractValue: BigDecimal = BigDecimal(5000000),
                         logicalResourceUnits: Set[LRUModel])