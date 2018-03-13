package hashcode2018.swarm

import java.util.concurrent.atomic.AtomicInteger

import hashcode2018._


/**
  *
  * TODO:
  *
  * - add multiple features to selection process
  * -- one is gain per time unit
  * -- other is length of ride
  * -- other is whether you end up in a (planned to be) crowded location (measuring the average distance to the closest X other cars (where multiple X values give multiple features))
  *
  */

object GreedyOptimizer {
  def optimize(input: InputData): Planning = {
    /**
      *
      * @param vehiclesToDo     Set of vehicles including rides they are committed to
      * @param ridesToDo    Rides that should still be looked at
      * @param vehiclesDone Rides that are planned to be done by a vehicle, but not yet final/committed
      * @return
      */
    def optimizeR(vehiclesToDo: Seq[Vehicle] = (0 to input.vehicles).map(_ => Vehicle()),
                  ridesToDo: IndexedSeq[Ride] = input.rides.toVector.sortBy(_.finish),
                  vehiclesDone: Seq[Vehicle] = Seq.empty,
                 ): Planning = vehiclesToDo match {
      case v +: vs if ridesToDo.nonEmpty => // per vehicle, plan an ideal trip (maximize profit)
        // per ride in ridesToDo, calculate "profit per timestep" for this vehicle
        getIndexOfRideForVehicle(v, ridesToDo) match {
          case index if index >= 0 =>
            val ride = ridesToDo(index)
            val newVehicles = (v.addRide(ride) +: vs).sortBy(_.timeReady)
            val newRidesToDo = ridesToDo.take(index) ++ ridesToDo.drop(index + 1)
            val profitableRides = newRidesToDo.dropWhile(_.finish < newVehicles.head.timeReady)
            optimizeR(newVehicles, profitableRides, vehiclesDone)
          case _ =>
            optimizeR(vs, ridesToDo, v +: vehiclesDone)
        }
      case _ =>
        Planning((vehiclesToDo ++ vehiclesDone).map(_.history.reverse))
    }

    def getIndexOfRideForVehicle(vehicle: Vehicle, ridesToDo: IndexedSeq[Ride]): Int = {
      val profitsPerRide = ridesToDo.indices map { index =>
        val ride = ridesToDo(index)
        val profitPerTimeStep = vehicle.getProfitForRide(ride, input.bonus).toDouble / ((ride.from - vehicle.location) + ride.duration)
        index -> profitPerTimeStep
      }
      val optimalRide = profitsPerRide.maxBy(_._2)
      if (optimalRide._2 > 0) optimalRide._1
      else -1
    }

    optimizeR()
  }

  object Vehicle {
    val idCounter = new AtomicInteger()
  }

  case class Vehicle(location: Location = (0, 0), timeReady: Int = 0, history: Seq[Ride] = Seq.empty, id: Int = Vehicle.idCounter.getAndIncrement()) {

    def addRide(r: Ride): Vehicle = {
      this.copy(
        location = r.to,
        timeReady = this.timeReady + (r.from - location) + r.duration,
        history = r +: this.history
      )
    }

    def getProfitForRide(ride: Ride, rideBonus: Int): Int = {
      val rideStartTime = (ride.from - location) + timeReady
      val profit =
        if (ride.duration + rideStartTime <= ride.finish) ride.duration
        else 0
      val bonus =
        if (rideStartTime <= ride.start) rideBonus
        else 0
      profit + bonus
    }
  }

}
