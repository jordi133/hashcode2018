package hashcode2018.genetics

import hashcode2018._
import hashcode2018.swarm.GreedyOptimizer

class Optimizer(input: InputData) {

  def optimize(popSize: Int = 50, offspringFactor: Int = 50, maxIterations: Int = 1000, maxIterationsWithoutImprovement: Int = 1) = {
    val initialOutput = OutputData.fromPlanning(GreedyOptimizer.optimize(input))
//    val initialOutput = SecondAssignment.doAssignment(input)

    val initialPlanning = Planning.fromInputAndOutput(input, initialOutput)

    var pop = Seq(initialPlanning)
    var i = 0
    var iterationsNoImprovement = 0
    var bestScore = 0
    while (i < maxIterations && iterationsNoImprovement < maxIterationsWithoutImprovement) {
      val newPop = select(pop ++ multiply(pop, offspringFactor), popSize)
      val newBestScore = scorePlanning(pop.head)
      println(s"best score in round $i: $newBestScore")
      if (bestScore < newBestScore) {
        iterationsNoImprovement = 0
        bestScore = newBestScore
      } else {
        iterationsNoImprovement += 1
      }
      i += 1
      pop = newPop
    }

    OutputData.fromPlanning(pop.head)
  }

  def multiply(pop: Seq[Planning], offspringFactor: Int): Seq[Planning] = {
    (for {
      planning <- pop.par
      _ <- 0 to offspringFactor
    } yield Mutator.combine(planning)).seq
  }


  def select(pop: Seq[Planning], popSize: Int) = {
    pop.sortBy(-scorePlanning(_)).take(popSize)

  }

  def scorePlanning(planning: Planning): Int = planning.vehicles.map(scorePerVehicle(_, input.bonus)).sum

}
