package hashcode2018

import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class OutputDataTest extends WordSpec with Matchers {

  "validate" should {
    "fail when a ride is assigned twice" in {
      val veh1 = VehiclePlan(2, Seq(0, 1, 2))
      val veh2 = VehiclePlan(2, Seq(1, 2, 3))
      val outputData = OutputData(Seq(veh1, veh2))

      val expectedMessage = "requirement failed: Invalid: rides assigned multiple times: 1, 2"

      the [IllegalArgumentException] thrownBy outputData.validate should have message expectedMessage
    }
    "succeed when no ride is assigned twice" in {
      val veh1 = VehiclePlan(2, Seq(0, 1))
      val veh2 = VehiclePlan(2, Seq(2, 3))
      val outputData = OutputData(Seq(veh1, veh2))

      noException should be thrownBy outputData.validate
    }
  }

  "score" should {
    "result in 10 for the example" in {
      val veh1 = VehiclePlan(1, Seq(0))
      val veh2 = VehiclePlan(2, Seq(2, 1))
      val outputData = OutputData(Seq(veh1, veh2))

      val expectedScore = 10

      outputData.score(InputData.fromFile(InputData.filenames.head)) shouldBe expectedScore
    }
  }

  "getOutputString" should {
    "return the correct string for the example" in {
      val veh1 = VehiclePlan(1, Seq(0))
      val veh2 = VehiclePlan(2, Seq(2, 1))
      val expectedOut = Source.fromURL(getClass.getResource("/example.out")).mkString

      val outputData = OutputData(Seq(veh1, veh2))

      outputData.getOutputString shouldBe expectedOut
    }
  }
}
