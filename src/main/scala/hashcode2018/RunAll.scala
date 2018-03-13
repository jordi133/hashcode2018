package hashcode2018

import hashcode2018.swarm.GreedyOptimizer
import hashcode2018.genetics.Optimizer

object RunAll extends App {

  override def main(args: Array[String]) = {

    val totalScore = for (file <- InputData.filenames) yield {
      val input = InputData.fromFile(file)

//      val output = SecondAssignment.doAssignment(input)
//      val output = OutputData.fromPlanning(GreedyOptimizer.optimize(input))
      val opt = new Optimizer(input)
      val output = opt.optimize()

      val score = output.score(input)

      println(s"Score on file $file: $score")

      score

    }
    println(s"Total score: ${totalScore.sum}")

  }
}
