package hashcode2018

import scala.util.Random

object RandomAssignment {

  val random = new Random(0)

  def doAssignment(input: InputData): OutputData = {
    // sort rides on start time
    val vehicleLocations = (0 to input.vehicles) map (id => Vehicle(id) -> ((0, 0), 0))
    val assignment = assignR(input.rides, vehicleLocations.toMap)
    println(assignment)
    OutputData(assignment)
  }

  def assignR(ridesToAssign: Seq[Ride],
              vehicleLocationsAndTimes: Map[Vehicle, (Location, Int)],
              assignments: RideAssignments = RideAssignments(Map.empty)): Seq[VehiclePlan] = ridesToAssign match {

    case ride +: rs =>
      val selectedCar = vehicleLocationsAndTimes.keys.toVector(random.nextInt(vehicleLocationsAndTimes.size))
      val newAssignments = assignments.assignRideToCar(selectedCar, ride)
      val newVehicleTime = vehicleLocationsAndTimes(selectedCar)._2 + ride.duration
      val newVehicleLocationsAndTimes = vehicleLocationsAndTimes.updated(selectedCar, (ride.to, newVehicleTime))
      assignR(rs, newVehicleLocationsAndTimes, newAssignments)
    case Nil =>
      assignments.assignments.values.toList.reverse
  }

  case class RideAssignments(assignments: Map[Vehicle, VehiclePlan]) {
    def assignRideToCar(vehicle: Vehicle, ride: Ride) = {
      val currentVehiclePlan = assignments.getOrElse(vehicle, VehiclePlan(0, Seq.empty))
      val newVehiclePlan = VehiclePlan(currentVehiclePlan.numberOfRides + 1, ride.id +: currentVehiclePlan.rideNumbers)
      val newAssignments = assignments.updated(vehicle, newVehiclePlan)
      RideAssignments(newAssignments)
    }
  }

  case class Vehicle(id: Int)

}
