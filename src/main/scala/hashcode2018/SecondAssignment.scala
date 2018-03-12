package hashcode2018

object SecondAssignment {

  def doAssignment(input: InputData): OutputData = {
    // sort rides on start time
    val sortedRides = input.rides.sortBy(_.start)

    val vehicleLocations = (0 to input.vehicles) map (id => Vehicle(id) -> ((0, 0), 0))

    val assignment = assignR(sortedRides, vehicleLocations.toMap)

    OutputData(assignment)
  }

  def assignR(ridesToAssign: Seq[Ride],
              vehicleLocationsAndTimes: Map[Vehicle, (Location, Int)],
              assignments: RideAssignments = RideAssignments(Map.empty)): Seq[VehiclePlan] = ridesToAssign match {

    case ride +: rs =>
      // find cars that can make it in time
      val possibleCars: Map[Vehicle, (Location, Int)] = vehicleLocationsAndTimes filter { case (_, (location, time)) =>
        time + ride.duration + (ride.from - location) < ride.finish
      }

      if (possibleCars.nonEmpty) {
        // see if one can get bonus: sort by arrival time - start time
        val carsWithArrival: Map[Vehicle, Int] = possibleCars map { case (veh, (loc, time)) =>
          val timeToStart = (ride.from - loc) + time
          val delta = timeToStart - ride.start
          veh -> delta
        }
        val canGetBonus = carsWithArrival.exists(_._2 <= 0)
        // select one of these
          val selectedCar = if (canGetBonus) {
          carsWithArrival.filter(_._2 <= 0).minBy{case (veh, delta) =>
            val travelDist = vehicleLocationsAndTimes(veh)._1 - ride.from
            val idleTime = ride.start - vehicleLocationsAndTimes(veh)._2
            travelDist * idleTime
          }._1
        } else {
          possibleCars.head._1
        }

        val newAssignments = assignments.assignRideToCar(selectedCar, ride)
        val newVehicleTime = vehicleLocationsAndTimes(selectedCar)._2 + ride.duration
        val newVehicleLocationsAndTimes = vehicleLocationsAndTimes.updated(selectedCar, (ride.to, newVehicleTime))
        assignR(rs, newVehicleLocationsAndTimes, newAssignments)
      } else {

        assignR(rs, vehicleLocationsAndTimes, assignments)
      }

    case Nil =>
      assignments.assignments.values.toList.reverse
  }

  case class RideAssignments(assignments: Map[Vehicle, VehiclePlan]) {
    def assignRideToCar(vehicle: Vehicle, ride: Ride) = {
      val currentVehiclePlan = assignments.getOrElse(vehicle, VehiclePlan(Seq.empty))
      val newVehiclePlan = VehiclePlan(ride.id +: currentVehiclePlan.rideNumbers)
      val newAssignments = assignments.updated(vehicle, newVehiclePlan)
      RideAssignments(newAssignments)
    }
  }

  case class Vehicle(id: Int)

}
