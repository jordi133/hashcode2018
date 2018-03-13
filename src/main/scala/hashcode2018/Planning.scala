package hashcode2018

object Planning {
  def fromInputAndOutput(input: InputData, output: OutputData): Planning = {
    val plan: Seq[VehiclePlan] = output.vehiclePlans

    val vehiclePlansByIndex: Seq[Seq[Int]] = plan.map(_.rideNumbers)
    val vehiclePlans: Seq[Seq[Ride]] = for {
      plan <- vehiclePlansByIndex
    } yield plan.map(input.rides(_))

    Planning(vehiclePlans)
  }
}

case class Planning(vehicles: Seq[Seq[Ride]])
