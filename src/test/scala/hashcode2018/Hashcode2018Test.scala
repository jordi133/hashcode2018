package hashcode2018

import org.scalatest.{Matchers, WordSpec}

class Hashcode2018Test extends WordSpec with Matchers {
  "insertInSortedSeq" should {
    "insert 4 at the correct place in Seq(1,2,3,5,6)" in {
      val s: IndexedSeq[Int] = Vector(1,2,3,5,6,7,8)
      val expected = Vector(1,2,3,4,5,6,7,8)

      val result = insertInSortedSeq[Int](4, s)(identity(_))

      result shouldBe expected
    }
    "insert 0 at the correct place in Seq(1,2,3,4,5,6)" in {
      val s: IndexedSeq[Int] = Vector(1,2,3,4,5,6)
      val expected = Vector(0,1,2,3,4,5,6)

      val result = insertInSortedSeq[Int](0, s)(identity(_))

      result shouldBe expected
    }
    "insert 7 at the correct place in Seq(1,2,3,4,5,6)" in {
      val s: IndexedSeq[Int] = Vector(1,2,3,4,5,6)
      val expected = Vector(1,2,3,4,5,6,7)

      val result = insertInSortedSeq[Int](7, s)(identity(_))

      result shouldBe expected
    }
  }

  "removeItemsFromSortedSeq" should {
    "remove (1,2,3) from (1,2,3,4,5,6)" in {
      val toRemove = Vector(1,2,3)
      val initial = Vector(1,2,3,4,5,6)
      val expected = Vector(4,5,6)

      val actual = removeItemsFromSortedSeq(initial, toRemove)(identity(_))

      actual shouldBe expected
    }
    "remove (1,2) from (3,4,5,6)" in {
      val toRemove = Vector(1,2)
      val initial = Vector(3,4,5,6)
      val expected = Vector(3,4,5,6)

      val actual = removeItemsFromSortedSeq(initial, toRemove)(identity(_))

      actual shouldBe expected
    }
    "remove (1,2) from (1,2)" in {
      val toRemove = Vector(1,2)
      val initial = Vector(1,2)
      val expected = Vector()

      val actual = removeItemsFromSortedSeq(initial, toRemove)(identity(_))

      actual shouldBe expected
    }
  }

}
