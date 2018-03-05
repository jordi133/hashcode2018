package hashcode2018.genetics

import hashcode2018.OutputData

import scala.util.Random

object Combinator {
  val random = new Random(0)
  def combine(out1: OutputData, out2: OutputData, f: Double = 0.3): OutputData = {
    // pick f of out1 ride assignments
    val fromOut1 = random.shuffle(out1.vehiclePlans).take((f * out1.vehiclePlans.size).toInt)

    // find those vehicles of out2 that have the least in common with the selected plans

    ???
  }

}
