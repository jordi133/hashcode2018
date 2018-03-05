package object hashcode2018 {

  type Location = (Int, Int)

  case class Ride(id: Int, from: Location, to: Location, start: Int, finish: Int) {
    val duration = to - from
  }

  case class VehiclePlan(numberOfRides: Int, rideNumbers: Seq[Int])

  implicit class TupleOps(tuple: Location) {
    /**
      * @return the distance between 2 locations
      */
    def -(other: Location): Int = Math.abs(tuple._1 - other._1) + Math.abs(tuple._2 - other._2)
  }

}
