package object hashcode2018 {

  case class Ride(from: (Int, Int), to: (Int, Int), start: Int, finish: Int)

  case class VehiclePlan(numberOfRides: Int, rideNumbers: Seq[Int])

  implicit class TupleOps(tuple: (Int, Int)) {
    /**
      * @return the distance between 2 locations
      */
    def -(other: (Int, Int)): Int = Math.abs(tuple._1 - other._1) + Math.abs(tuple._2 - other._2)
  }
}
