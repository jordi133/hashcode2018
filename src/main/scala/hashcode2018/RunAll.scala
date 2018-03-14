package hashcode2018

import hashcode2018.swarm.GreedyOptimizer

object RunAll extends App {

  override def main(args: Array[String]) = {

    val t0 = System.currentTimeMillis()
    val totalScore = for (file <- InputData.filenames) yield {
      val input = InputData.fromFile(file)
      val output = OutputData.fromPlanning(GreedyOptimizer.optimize(input))

      val score = output.score(input)

      println(s"Score on file $file: $score")

      score

    }
    println(s"Total score: ${totalScore.sum}, time taken: ${System.currentTimeMillis() - t0}")

  }
}
