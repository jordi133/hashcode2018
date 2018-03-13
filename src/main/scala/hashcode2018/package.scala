package object hashcode2018 {

  type Location = (Int, Int)

  case class Ride(id: Int, from: Location, to: Location, start: Int, finish: Int) {
    val duration = to - from
  }

  case class VehiclePlan(rideNumbers: Seq[Int]) {
    def numberOfRides: Int = rideNumbers.size
  }

    def scorePerVehicle(plannedRides: Seq[Ride], rideBonus: Int, location: Location = (0, 0), time: Int = 0, score: Int = 0): Int = plannedRides match {
      case ride +: rs =>
        val timeWaited = Math.max(0, ride.start - time)
        val timeAtStart = time + (ride.from - location)
        val newLocation = ride.to
        val timeSpent = timeWaited + (ride.from - location) + (ride.to - ride.from)
        val rideScore =
          if (time + timeSpent < ride.finish) ride.from - ride.to
          else 0
        val bonus: Int =
          if (timeAtStart <= ride.start) rideBonus
          else 0
        val scoreGained = rideScore + bonus
        scorePerVehicle(rs, bonus, newLocation, time + timeSpent, score + scoreGained)
      case Nil =>
        score
    }


  implicit class TupleOps(tuple: Location) {
    /**
      * @return the distance between 2 locations
      */
    def -(other: Location): Int = Math.abs(tuple._1 - other._1) + Math.abs(tuple._2 - other._2)
  }

}
