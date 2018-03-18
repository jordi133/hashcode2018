package hashcode2018

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec


/**
  *
  * TODO:
  * - Add multiple features in choosing next ride in breadth first approach
  */

class GreedyOptimizer(input: InputData) {

  /**
    * Combines depth and breadth first approach, using the indicated portion of all vehicles for depth first
    */
  def optimizeCombined(depthFirstFactor: Double): Planning = {
    val vehicles = (0 to input.vehicles).map(_ => Vehicle())

    val (depthFirstVehicles, breathFirstVehicles) = vehicles.splitAt((input.vehicles * depthFirstFactor).toInt)

    val depthFirstPlanning = optimizeDepthFirst(vehiclesToDo = depthFirstVehicles)
    val allRides = input.rides.toVector.sortBy(_.finish)
    val ridesToDo = removeItemsFromSortedSeq[Ride](allRides, depthFirstPlanning.vehicles.flatten.toVector)(_.finish)

    val breadthFirstPlanning = optimizeBreadthFirst(breathFirstVehicles, ridesToDo)

    val result = Planning(depthFirstPlanning.vehicles ++ breadthFirstPlanning.vehicles)

    result
  }

  /**
    * @param vehiclesToDo Set of vehicles including rides they are committed to
    * @param ridesToDo    Rides that should still be looked at
    * @param vehiclesDone Rides that are planned to be done by a vehicle, but not yet final/committed
    * @return
    */
  @tailrec
  final def optimizeBreadthFirst(vehiclesToDo: IndexedSeq[Vehicle] = (0 to input.vehicles).map(_ => Vehicle()),
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
          optimizeBreadthFirst(newVehicles, profitableRides, vehiclesDone)
        case _ =>
          optimizeBreadthFirst(vs, ridesToDo, v +: vehiclesDone)
      }
    case _ =>
      Planning((vehiclesToDo ++ vehiclesDone).map(_.getPlanning))
  }

  @tailrec
  final def optimizeDepthFirst(vehiclesToDo: IndexedSeq[Vehicle] = (0 to input.vehicles).map(_ => Vehicle()),
                               ridesToDo: IndexedSeq[Ride] = input.rides.toVector.sortBy(_.finish),
                               vehiclesDone: IndexedSeq[Vehicle] = Vector.empty,
                              ): Planning = vehiclesToDo match {
    case v +: vs =>
      val plannedVehicle = getSingleOptimalVehiclePlan(v, ridesToDo, input.bonus)
      val newRidesToDo = removeItemsFromSortedSeq[Ride](ridesToDo, plannedVehicle.history.reverse.toVector)(_.finish)
      val newVehiclesDone = plannedVehicle +: vehiclesDone
      optimizeDepthFirst(vs, newRidesToDo, newVehiclesDone)
    case Vector() =>
      Planning(vehiclesDone.map(_.getPlanning))
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
