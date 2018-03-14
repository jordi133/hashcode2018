package hashcode2018.swarm

import java.util.concurrent.atomic.AtomicInteger

import hashcode2018._


/**
  *
  * TODO:
  * - Add other greedy appraoch in which 1 vehicle first plans multiple rides in an optimal way
  * - Then take combination of both greedy approaches
  */

object GreedyOptimizer {
  def optimizeBreadthFirst(input: InputData): Planning = {
    /**
      *
      * @param vehiclesToDo Set of vehicles including rides they are committed to
      * @param ridesToDo    Rides that should still be looked at
      * @param vehiclesDone Rides that are planned to be done by a vehicle, but not yet final/committed
      * @return
      */
    def optimizeBreadthFirstR(vehiclesToDo: IndexedSeq[Vehicle] = (0 to input.vehicles).map(_ => Vehicle()),
                              ridesToDo: IndexedSeq[Ride] = input.rides.toVector.sortBy(_.finish),
                              vehiclesDone: IndexedSeq[Vehicle] = Vector.empty,
                             ): Planning = vehiclesToDo match {
      case v +: vs if ridesToDo.nonEmpty => // per vehicle, plan an ideal trip (maximize profit)
        // per ride in ridesToDo, calculate "profit per timestep" for this vehicle
        getIndexOfOptimalRideForVehicle(v, ridesToDo, input.bonus) match {
          case index if index >= 0 =>
            val ride = ridesToDo(index)
            val newVehicles = insertInSortedSeq(v.addRide(ride), vs)(_.timeReady)
            val newRidesToDo = ridesToDo.take(index) ++ ridesToDo.drop(index + 1)
            val profitableRides = newRidesToDo.dropWhile(_.finish < newVehicles.head.timeReady)
            optimizeBreadthFirstR(newVehicles, profitableRides, vehiclesDone)
          case _ =>
            optimizeBreadthFirstR(vs, ridesToDo, v +: vehiclesDone)
        }
      case _ =>
        Planning((vehiclesToDo ++ vehiclesDone).map(_.getPlanning))
    }

    optimizeBreadthFirstR()
  }

  def optimizeDepthFirst(input: InputData): Planning = {
    def optimizeDepthFirstR(vehiclesToDo: IndexedSeq[Vehicle] = (0 to input.vehicles).map(_ => Vehicle()),
                            ridesToDo: IndexedSeq[Ride] = input.rides.toVector.sortBy(_.finish),
                            vehiclesDone: IndexedSeq[Vehicle] = Vector.empty,
                           ): Planning = vehiclesToDo match {
      case v +: vs =>
        val plannedVehicle = getSingleOptimalVehiclePlan(v, ridesToDo, input.bonus)
        val newRidesToDo = removeItemsFromSortedSeq[Ride](ridesToDo, plannedVehicle.history.reverse.toVector)(_.finish)
        val newVehiclesDone = plannedVehicle +: vehiclesDone
        optimizeDepthFirstR(vs, newRidesToDo, newVehiclesDone)
      case Vector() =>
        Planning(vehiclesDone.map(_.getPlanning))
    }

    optimizeDepthFirstR()
  }


  def getIndexOfOptimalRideForVehicle(vehicle: Vehicle, ridesToDo: IndexedSeq[Ride], bonus: Int): Int = {
    // TODO: early termination: at some point the price per timeunit for rides will steadily decrease, then there's
    // no need to continue searching
    val profitsPerRide = ridesToDo.indices map { index =>
      val ride = ridesToDo(index)
      val profitPerTimeStep = vehicle.getProfitForRide(ride, bonus).toDouble / ((ride.from - vehicle.location) + ride.duration)
      index -> profitPerTimeStep
    }
    val optimalRide = profitsPerRide.maxBy(_._2)
    if (optimalRide._2 > 0) optimalRide._1
    else -1
  }

  def getSingleOptimalVehiclePlan(veh: Vehicle, rides: IndexedSeq[Ride], bonus: Int): Vehicle = {
    def getSingleOptimalVehiclePlanR(v: Vehicle, ridesToDo: IndexedSeq[Ride]): Vehicle = {
      if (ridesToDo.isEmpty) {
        v
      }
      else {
        val nextRide = getIndexOfOptimalRideForVehicle(v, ridesToDo, bonus)
        if (nextRide < 0) {
          v
        } else {
          val newVehicle = v.addRide(ridesToDo(nextRide))
          val newRidesToDo = ridesToDo.take(nextRide) ++ ridesToDo.drop(nextRide + 1)
          getSingleOptimalVehiclePlanR(newVehicle, newRidesToDo)
        }
      }
    }

    getSingleOptimalVehiclePlanR(veh, rides)
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

    def getPlanning = history.reverse

  }

}
