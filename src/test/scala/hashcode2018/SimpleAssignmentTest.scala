package hashcode2018

import org.scalatest.{Matchers, WordSpec}

class SimpleAssignmentTest extends WordSpec with Matchers {

  "doAssignment" should {
    "give some valid assignment" in {
      val input = InputData.fromFile(InputData.filenames.head)

      val output = SimpleAssignment.doAssignment(input)

      noException should be thrownBy output.validate

      println(output.score(input))
    }
  }
}
