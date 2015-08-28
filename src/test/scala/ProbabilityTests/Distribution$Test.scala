package ProbabilityTests

import Probability.Distribution
import org.scalatest.{ShouldMatchers, FunSuite}

class Distribution$Test extends FunSuite with ShouldMatchers {

  test("testNormalPdf") {
    val z = Distribution.normalPdf(.25)

    z should be (.386668 +- 0.01)
  }

  test("testNormalCdf") {
    val z = Distribution.normalCdf(.25)

    z should be (0.598706 +- 0.01)
  }

  test("testInverseNormalCdf") {
    val z = Distribution.inverseNormalCdf(.598706)

    z should be (0.25 +- 0.01)
  }
}
