import java.time.LocalDate

case class SLAObservationModel(observationDate: LocalDate,
                               serviceLevelObserved: BigDecimal)
