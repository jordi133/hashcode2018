package hashcode2018

import hashcode2018.swarm.GreedyOptimizer

object RunAll extends App {

  override def main(args: Array[String]) = {

    val t0 = System.currentTimeMillis()
    val totalScore = for (file <- InputData.filenames) yield {
      val input = InputData.fromFile(file)
      val opt = new GreedyOptimizer(input)

      val scores = for (f <- 0 to 10) yield {
        val factor = 0.1 * f
//        val output = OutputData.fromPlanning(opt.optimizeBreadthFirst())
        val output = OutputData.fromPlanning(opt.optimizeCombined(factor))

        val score = output.score(input)

        println(s"Score on file $file with factor $factor: $score")

        score

      }
      println(s"Score on file $file: ${scores.max}")
      scores.max

    }
    println(s"Total score: ${totalScore.sum}, time taken: ${System.currentTimeMillis() - t0}")

  }
}
