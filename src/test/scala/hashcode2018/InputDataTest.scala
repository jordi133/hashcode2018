package hashcode2018

import org.scalatest.{Matchers, WordSpec}

class InputDataTest extends WordSpec with Matchers {

  "fromFile" should {
    "read the a_example file correctly" in {
      val input = InputData.fromFile("a_example.in")
      val expectedRides = Seq(Ride(0, (0, 0), (1, 3), 2, 9), Ride(1, (1, 2), (1, 0), 0, 9), Ride(2, (2, 0), (2, 2), 0, 9))
      val expectedInputData = InputData(3, 4, 2, 3, 2, 10, expectedRides)
      input shouldBe expectedInputData
    }
    "read the correct number of rides in each file" in {
      for (file <- InputData.filenames) {
        val input = InputData.fromFile(file)
        input.nrOfRides shouldBe input.rides.size
      }
    }
  }
}
