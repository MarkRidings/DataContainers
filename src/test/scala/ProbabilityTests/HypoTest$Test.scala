package ProbabilityTests

import Probability.HypoTest
import org.scalatest.{ShouldMatchers, FunSuite}

class HypoTest$Test extends FunSuite with ShouldMatchers {

  test("testNormalProbabilityAbove") {
    val p = HypoTest.normalProbabilityAbove(.25)

    p should be (0.401294 +- 0.02)
  }

  test("testNormalProbabilityBetween") {
    val p = HypoTest.normalProbabilityBetween(-0.2, 0.2)

    p should be (0.158519 +- 0.01)
  }

  test("testNormalProbabilityOutside") {
    val p = HypoTest.normalProbabilityOutside(-0.2, 0.2)

    p should be (0.841481 +- 0.01)
  }

  test("testNormalUpperBound") {
    val z = HypoTest.normalUpperBound(.95)

    z should be(1.644854 +- 0.01)
  }

  test("testNormalUpperBoundIllegalPValue") {
    try {
      val z = HypoTest.normalUpperBound(1.25)

      fail("should have caught an exception")
    }
    catch  {
      case e: Exception => assert(true)
    }
  }

}
