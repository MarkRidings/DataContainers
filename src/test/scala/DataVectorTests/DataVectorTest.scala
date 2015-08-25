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

    vector_1 should be === vector_3
    vector_1 equals vector_2 should not be true
  }

  test("test$plus") {
    val vector_1 = new DataVector(List(-5, 4, 3, 0 , 9))
    val vector_2 = new DataVector(List(0, 4, -1 , 3, 11))

    val expectedVector = new DataVector(List(-5, 8, 2, 3, 20))

    vector_1 + vector_2 should be === expectedVector
  }

  test("test$minus") {
    val vector_1 = new DataVector(List(2, 3, 4))
    val vector_2 = new DataVector(List(-9, 4, 3))

    val expectedVector = new DataVector(List(11, -1, 1))

    vector_1 - vector_2 should be === expectedVector
  }

  test("test$times") {
    val vector = new DataVector(List(9, 4, 2, 1))

    val expectedVector = new DataVector(List(63, 28, 14 ,7))

    vector * 7 should be === expectedVector
  }

  test("testDot") {
    val vector_1 = new DataVector(List(-1, 2, 0))
    val vector_2 = new DataVector(List(6, 7, 5))


    vector_1 dot vector_2 should be (8)
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

  test("toListTest") {
    val vector = new DataVector(List(9, 3, 0))

    val expectedValue = List(9, 3, 0)

    vector.toList should be === expectedValue
  }
}
