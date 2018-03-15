package object hashcode2018 {

  type Location = (Int, Int)

  case class Ride(id: Int, from: Location, to: Location, start: Int, finish: Int) {
    val duration = to - from
  }

  case class VehiclePlan(rideNumbers: Seq[Int]) {
    def numberOfRides: Int = rideNumbers.size
  }

  def scorePerVehicle(plannedRides: Seq[Ride], rideBonus: Int, location: Location = (0, 0), time: Int = 0, score: Int = 0): Int = plannedRides match {
    case ride +: rs =>
      val timeWaited = Math.max(0, ride.start - time)
      val timeAtStart = time + (ride.from - location)
      val newLocation = ride.to
      val timeSpent = timeWaited + (ride.from - location) + (ride.to - ride.from)
      val rideScore =
        if (time + timeSpent < ride.finish) ride.from - ride.to
        else 0
      val bonus: Int =
        if (timeAtStart <= ride.start) rideBonus
        else 0
      val scoreGained = rideScore + bonus
      scorePerVehicle(rs, bonus, newLocation, time + timeSpent, score + scoreGained)
    case Nil =>
      score
  }

  def insertInSortedSeq[T](item: T, seq: IndexedSeq[T])(f: T => Int): IndexedSeq[T] = {
    def insertInSortedSeqR(x: Int, y: Int): IndexedSeq[T] = {
      val m = (x + y) / 2
      if (x + 1 == y) {
        seq.take(m + 1) ++ (item +: seq.drop(m + 1))
      } else if (f(item) <= f(seq(m))) {
        insertInSortedSeqR(x, m)
      } else /*if (f(item) > f(seq(m)))*/ {
        insertInSortedSeqR(m, y)
      }
    }

    if (seq.isEmpty || f(item) < f(seq.head)) item +: seq
    else insertInSortedSeqR(0, seq.length)
  }

  def removeItemsFromSortedSeq[T](seq: IndexedSeq[T], toRemove: IndexedSeq[T], acc: IndexedSeq[T] = Vector.empty)(f: T => Int): IndexedSeq[T] = (seq, toRemove) match {
    case (s +: ss, r +: rs) =>
      if (f(r) == f(s)) removeItemsFromSortedSeq(ss, rs, acc)(f)
      else if (f(s) < f(r)) removeItemsFromSortedSeq(ss, r +: rs, s +: acc)(f)
      else /* f(s) > f(r) */ removeItemsFromSortedSeq(s +: ss, rs, acc)(f)
    case (s +: ss, Vector()) => removeItemsFromSortedSeq(ss, Vector(), s +: acc)(f)
    case (Vector(), _) => acc.reverse
  }

  implicit class TupleOps(tuple: Location) {
    /**
      * @return the distance between 2 locations
      */
    def -(other: Location): Int = Math.abs(tuple._1 - other._1) + Math.abs(tuple._2 - other._2)
  }

}
