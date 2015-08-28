package DataVectorTests

import DataVector.DataVector
import org.scalatest.{FunSuite, ShouldMatchers}

class DataVectorTest extends FunSuite with ShouldMatchers {

  test("testIsEmpty") {
    val emptyVector = new DataVector(List())

    assert(emptyVector.isEmpty)
  }

  test("testSize") {
    val vector_1 = new DataVector(List(1, 5, 4, 3, 6))

    vector_1.size should be (5)
  }

  test("testEquals") {
    val vector_1 = new DataVector(List(4, 3, 1))
    val vector_2 = new DataVector(List(1, 3, 4))
    val vector_3 = new DataVector(List(4, 3, 1))

    assert(vector_1 equals vector_3)
    vector_1 equals vector_2 should not be true
  }

  test("test$plus") {
    val vector_1 = new DataVector(List(-5, 4, 3, 0 , 9))
    val vector_2 = new DataVector(List(0, 4, -1 , 3, 11))

    val expectedVector = new DataVector(List(-5, 8, 2, 3, 20))

    assert((vector_1 + vector_2) equals expectedVector)
  }

  test("test$minus") {
    val vector_1 = new DataVector(List(2, 3, 4))
    val vector_2 = new DataVector(List(-9, 4, 3))

    val expectedVector = new DataVector(List(11, -1, 1))

    assert(vector_1 - vector_2 equals expectedVector)
  }

  test("test$times") {
    val vector = new DataVector(List(9, 4, 2, 1))

    val expectedVector = new DataVector(List(63, 28, 14 ,7))

    assert(vector * 7 equals expectedVector)
  }

  test("testDot") {
    val vector_1 = new DataVector(List(-1, 2, 0))
    val vector_2 = new DataVector(List(6, 7, 5))


    vector_1 dot vector_2 should be (8)
  }

  test("testAt") {
    val vector_1 = new DataVector(List(0, 9, -5, 4))

    vector_1.at(0) should be (0)
    vector_1.at(2) should be (-5)
  }

  test("testSquared") {
    val vector = new DataVector(List(9, 7, 1))

    vector.squared should be (131)
  }

  test("testMagnitude") {
    val vector = new DataVector(List(3, 4))

    vector.magnitude should be (5)
  }

  test("testMedian") {
    val vector_1 = new DataVector(List(5, 6, 0, 1))
    val vector_2 = new DataVector(List(1, 5, 3, -9 ,9))

    vector_1.median should be (3)
    vector_2.median should be (3)
  }

  test("testQuartile") {
    val vector_1 = new DataVector(List(4, 7, 11, -9, 6, 21, 5, 2, 1, 1, 2))

    vector_1.quartile(.7) should be (5)
    vector_1.quartile(1) should be (21)
    vector_1.quartile(.00001) should be (-9)
  }

  test("testInterQuartileRange") {
    val vector_1 = new DataVector(List(8, 5, 1, 3, 2, 2, -9, -4, 11))

    vector_1.interQuartileRange should be (7)
  }

  test("testRange") {
    val vector_1 = new DataVector(List(6, 5, 2, -9, -11, 3))

    vector_1.range should be (17)
  }

  test("testMean") {
    val vector_1 = new DataVector(List(9, 6, 4, 2, -1, 3))

    vector_1.mean should be (3.83 +- .02)
  }

  test("testVariance") {
    val vector_1 = new DataVector(List(9, 8, -5, 1, 0, 3, 2))

    vector_1.variance should be (22.95 +- .02)
  }

  test("testStandardDev") {
    val vector_1 = new DataVector(List(8, 9, 11, 0, 2, -1, -8, 11))

    vector_1.standardDev should be (6.84 +- 0.02)
  }

  test("testCovariance") {
    val vector_1 = new DataVector(List(5, 4, 1, 9, 11))
    val vector_2 = new DataVector(List(1, 11, -9, 5, 4))

    vector_1 covariance vector_2 should be (14.25 +- 0.02)
  }

  test("testCorrelation") {
    val vector_1 = new DataVector(List(8, 6, 3, 4, 11, 3))
    val vector_2 = new DataVector(List(0, 6, -4, 4, 4, 2))

    vector_1 correlation vector_2 should be (0.35 +- 0.02)
  }

  test("toListTest") {
    val vector = new DataVector(List(9, 3, 0))

    val expectedValue = List(9, 3, 0)

    assert(vector.toList equals expectedValue)
  }
}
