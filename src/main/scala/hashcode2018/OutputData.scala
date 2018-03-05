package hashcode2018

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
    def scorePerVehice(plannedRides: Seq[Ride], location: Location = (0, 0), time: Int = 0, score: Int = 0): Int = plannedRides match {
      case ride +: rs =>
        val timeWaited = Math.max(0, ride.start - time)
        val timeAtStart = time + (ride.from - location)
        val newLocation = ride.to
        val timeSpent = timeWaited + (ride.from - location) + (ride.to - ride.from)
        val rideScore =
          if (timeAtStart <= ride.start) ride.from - ride.to
          else 0
        val bonus =
          if (time + timeSpent < ride.finish) input.bonus
          else 0
        val scoreGained = rideScore + bonus
        scorePerVehice(rs, newLocation, time + timeSpent, score + scoreGained)
      case Nil =>
        score
    }

    val score = vehiclePlans.map { plan =>
      val rides = plan.rideNumbers.map(nr => input.rides(nr))
      scorePerVehice(rides)
    }.sum

    score
  }

  def getOutputString: String = {
    vehiclePlans.map{plan =>
      s"${plan.numberOfRides} ${plan.rideNumbers.mkString(" ")}"
    }.mkString(System.lineSeparator())
  }
}
