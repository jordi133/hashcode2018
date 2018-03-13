package hashcode2018

object OutputData {
  def fromPlanning(planning: Planning): OutputData = {
    val plan = for (vehPlan <- planning.vehicles) yield VehiclePlan(vehPlan.map(_.id))
    OutputData(plan)
  }
}

case class OutputData(vehiclePlans: Seq[VehiclePlan]) {

  /**
    * Throws IllegalArgumentException when this object is invalid (rides are assigned multiple times)
    */
  def validate: Unit = {
    val allRides = vehiclePlans.flatMap(_.rideNumbers)
    val rideSet = allRides.toSet
    require(allRides.size == allRides.toSet.size, s"Invalid: rides assigned multiple times: ${allRides.diff(rideSet.toSeq).toSet.mkString(", ")}")
  }

  /**
    * Calculates the score
    */
  def score(input: InputData): Int = {

    val score = vehiclePlans.map { plan =>
      val rides = plan.rideNumbers.map(nr => input.rides(nr))
      scorePerVehicle(rides, input.bonus)
    }.sum

    score
  }


  def getOutputString: String = {
    vehiclePlans.map { plan =>
      s"${plan.numberOfRides} ${plan.rideNumbers.mkString(" ")}"
    }.mkString(System.lineSeparator())
  }
}
