case class LRUModel(name: String,
                    serviceLevelAgreements: List[SLAModel],
                    allocationPercentage: BigDecimal) {

  val totalAllocationPercentage: BigDecimal =
    serviceLevelAgreements.map(_.allocationPercentage).sum

  def validateSLA(): Boolean =
    !serviceLevelAgreements.exists(_.allocationPercentage > BigDecimal(0.63))

}
