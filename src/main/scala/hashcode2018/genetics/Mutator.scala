package hashcode2018.genetics

import hashcode2018._

import scala.util.Random

object Mutator {
  val random = new Random(0)
  def combine(p: Planning, f: Double = 0.1): Planning = {
    val (vehiclesToMutate, rest) = random.shuffle(p.vehicles).splitAt(1 + (p.vehicles.size * f - 1).toInt)

    // pair each vehicle with another one
    val (othersToMutate, unchanged) = rest.splitAt(vehiclesToMutate.size)

    val changed = (vehiclesToMutate zip othersToMutate) map { case (plan1, plan2) =>
        mergeVehiclePlans(plan1, plan2)
    }

    Planning(changed.flatten ++ unchanged)
  }

  def mergeVehiclePlans(p1: Seq[Ride], p2: Seq[Ride], acc1: Seq[Ride] = Seq.empty, acc2: Seq[Ride] = Seq.empty, switchChange: Double = 0.1): Seq[Seq[Ride]] = {
    def addToRandomAcc(r: Ride, r1s: Seq[Ride], r2s: Seq[Ride], fromR1: Boolean): Seq[Seq[Ride]] = {
      if (random.nextDouble() > switchChange ) {
        mergeVehiclePlans(r1s, r2s, r +: acc1, acc2)
      } else {
        mergeVehiclePlans(r1s, r2s, acc1, r +: acc2)
      }
    }

    (p1, p2) match {
      case (Nil, Nil) =>
        Seq(acc1.reverse, acc2.reverse)
      case (Nil, r +: rs) =>
        mergeVehiclePlans(r +: rs, Nil, acc1, acc2)
      case (r +: rs, Nil) =>
        addToRandomAcc(r, rs, Nil, true)
      case (r1 +: r1s, r2 +: r2s) =>
        if (random.nextBoolean()) {
          addToRandomAcc(r1, r1s, r2 +: r2s, true)
        } else {
          addToRandomAcc(r2, r1 +: r1s, r2s, false)
        }
    }
  }
}
