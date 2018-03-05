package hashcode2018

object RunAll extends App {

  override def main(args: Array[String]) = {

    val totalScore = for (file <- InputData.filenames) yield {
      val input = InputData.fromFile(file)

      val output = SecondAssignment.doAssignment(input)

      val score = output.score(input)

      println(s"Score on file $file: $score")

      score
    }
    println(s"Total score: ${totalScore.sum}")

  }
}
