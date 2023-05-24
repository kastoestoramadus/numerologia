import org.scalatest.matchers.should.Matchers
import ww86.numerology.domain.NPair
import org.scalatest.wordspec

class TestCases extends wordspec.AnyWordSpec with Matchers {
  val testCase1 = List("PIOTR", "DARIUSZ", "WOJCIECH", "SZCZAWNICKI") // podliczby, 13, 14, 33, 13, 22, 19; 67/4 95/5 162/9
  val testCase2 = List("WALDEMAR", "GRZEGORZ", "MELCHIOR", "WOSIŃSKI") // "HEKTOR" should be the anwser in 1,9,1

  "counting NDigit" should {
    "keep master number" in {
      NPair.toNDigitOneStep(38) shouldBe 11
    }
    "not keep subnumbers" in {
      NPair.toBasicNDigit(38) shouldBe 2
    }
  }
  "Program" should {
    "count profile with subnumbers for long name" in {
      val suspect = NPair(testCase1)
      suspect.vowelsSum shouldBe 67
      suspect.vowelsDigit shouldBe 4
      suspect.consonantsSum shouldBe 95
      suspect.consonantsDigit shouldBe 5
      suspect.bothSum shouldBe 162
      suspect.bothDigit shouldBe 9

      val expected = Set(
        33 -> "PIOTR",
        13 -> "DARIUSZ",
        22 -> "DARIUSZ",
        19 -> "SZCZAWNICKI",
        13 -> testCase1.mkString(" "),
        14 -> testCase1.mkString(" ")
      )
      suspect.subnumbersRecursively should contain theSameElementsAs expected
    }
    "Propose names transforming to better profiles" in {

    }
  }
}