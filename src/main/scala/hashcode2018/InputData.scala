package hashcode2018

import scala.io.Source

object InputData {

  val filenames = Seq("a_example.in", "b_should_be_easy.in", "c_no_hurry.in", "d_metropolis.in", "e_high_bonus.in")

  def fromFile(s: String): InputData = {
    val data = Source.fromFile(s"input/$s").getLines().toVector

    val Array(rows, cols, vehicles,nrOfRides, bonus, steps) = data.head.split(" ").map(_.toInt)
    val rides = for (i <- data.tail.indices) yield {
      val line = data.tail(i)
      val Array(fromRow, fromCol, toRow, toCol, start, finish) = line.split(" ").map(_.toInt)
      Ride(i, (fromRow, fromCol), (toRow, toCol), start, finish)
    }

    InputData(rows, cols, vehicles, nrOfRides, bonus, steps, rides)
  }
}

case class InputData(rows: Int, cols: Int, vehicles: Int, nrOfRides: Int, bonus: Int, steps: Int, rides: Seq[Ride])
