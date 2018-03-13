package hashcode2018.swarm

import java.util.concurrent.atomic.AtomicInteger

import hashcode2018._

object Optimizer {
  def optimize(input: InputData): Planning = {
    /**
      *
      * @param t Current time
      * @param vehicles Set of vehicles including rides they are committed to
      * @param ridesToDo Rides that should still be looked at
      * @param ridesPlanned Rides that are planned to be done by a vehicle, but not yet final/committed
      * @return
      */
    def optimizeR(t: Int = 0,
                  vehicles: Seq[Vehicle] = (0 to input.vehicles).map(_ => Vehicle()),
                  ridesToDo: Seq[Ride] = input.rides.sortBy(_.start),
                  ridesPlanned: Map[Vehicle, Ride] = Map.empty
                 ): Planning = {
      if (t == input.steps) {
        Planning(vehicles.map(_.history.reverse))
      } else {
        val freeVehicles = vehicles.filter(_.timeReady == t)
        val ridesPlannedThisIteration = assignRidesToVehicles(ridesToDo, freeVehicles, ridesPlanned)
        val totalRidesPlanned = ridesPlanned ++ ridesPlannedThisIteration
        // update rides assigned by adding all planned rides that start at this t
        val newVehicles = for (v <- vehicles) yield {
          val rideToAssign = totalRidesPlanned.get(v).filter(_.start == t)
          v.addRideOption(rideToAssign)
        }

        val newRidesPlanned = totalRidesPlanned.filter(_._2.start != t)
        val newRidesToPlan = ridesToDo diff newRidesPlanned.values.toSeq

        optimizeR(t + 1, newVehicles, newRidesToPlan, newRidesPlanned)
      }
    }

    /**
      * Vehicles opt for their preferred rides and then there's some selection policy (for now the closest one gets them???)
      *
      * @param freeVehicles
      * @return
      */
    def assignRidesToVehicles(ridesToPlan: Seq[Ride], freeVehicles: Seq[Vehicle], ridesPlanned: Map[Vehicle, Ride]) = {
      // assign a ride based on:
      // - points to be gained by the ride
      // - crowdedness of target location at timestep of arrival

      // per ride:
      // if no vehicle can do it, skip it

      ???
    }

    optimizeR()
  }

  object Vehicle {
    val idCounter = new AtomicInteger()
  }

  case class Vehicle(location: Location = (0, 0), timeReady: Int = 0, history: Seq[Ride] = Seq.empty, id: Int = Vehicle.idCounter.getAndIncrement()) {

    def addRideOption(rideOption: Option[Ride]) = rideOption match {
      case Some(ride) => addRide(ride)
      case None => this
    }

    def addRide(r: Ride) = {
      this.copy(
        location = r.to,
        timeReady = this.timeReady + (r.from - location) + r.duration,
        history = r +: this.history
      )
    }
  }

}
