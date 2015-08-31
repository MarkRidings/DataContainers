package DataMatrixTests

import DataMatrix.DataMatrix
import DataVector.DataVector
import org.scalatest.{ShouldMatchers, FunSuite}

class DataMatrixTest extends FunSuite with ShouldMatchers {

  test("testShape") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val m = DataMatrix(List(v1, v2, v3 ,v4))

    assert(m.shape == (4, 3))
  }

  test("testIllegalConstruction") {
    val v1 = DataVector(List(1, 2, 3, 4))
    val v2 = DataVector(List(2, 5, 4, 2))
    val v3 = DataVector(List(1, 5, 4))
    val v4 = DataVector(List(3, 5, 2, 3))
    val v5 = DataVector(List(-9, -3, -4, 5))

    try  {
      val m = DataMatrix(List(v1, v2, v3, v4, v5))

      fail("Should have thrown an exception for illegal construction")
    }
    catch {
      case e: Exception => assert(true)
    }
  }

  test("testGetRow") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val m = DataMatrix(List(v1, v2, v3, v4))

    assert(m.getRow(2) equals v3)
  }

  test("testGetColumn") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val m = DataMatrix(List(v1, v2, v3, v4))
    val expectedColum = DataVector(List(5, 9, 5, 4))

    assert(m.getColumn(1) equals expectedColum)
  }

  test("testIllegalIndexGetRow") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val m = DataMatrix(List(v1, v2, v3, v4))

    try {
      m.getRow(4)

      fail("Should have thrown IndexOutOfBoundsException")
    }
    catch {
      case i: IndexOutOfBoundsException => assert(true)
      case e: Exception => fail("Throw an unexpected exception: " + e.getClass + " " + e.getMessage)
    }

    try {
      m.getRow(-1)

      fail("Should have thrown IndexOutOfBoundsException")
    }
    catch {
      case i: IndexOutOfBoundsException => assert(true)
      case e: Exception => fail("Throw an unexpected exception: " + e.getClass + " " + e.getMessage)
    }

  }

  test("testIllegalIndexGetColumn") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val m = DataMatrix(List(v1, v2, v3, v4))

    try {
      m.getColumn(3)

      fail("Should have thrown IndexOutOfBoundsException")
    }
    catch {
      case i: IndexOutOfBoundsException => assert(true)
      case e: Exception => fail("Throw an unexpected exception: " + e.getClass + " " + e.getMessage)
    }

    try {
      m.getColumn(-1)

      fail("Should have thrown IndexOutOfBoundsException")
    }
    catch {
      case i: IndexOutOfBoundsException => assert(true)
      case e: Exception => fail("Throw an unexpected exception: " + e.getClass + " " + e.getMessage)
    }
  }

  test("testEquals") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val m1 = DataMatrix(List(v1, v2, v3, v4))
    val m2 = DataMatrix(List(v1, v2, v3, v4))
    val m3 = DataMatrix(List(v2, v3, v4, v1))
    val m4 = DataMatrix(List(v1, v2, v3, v4, v1))

    assert(m1 equals m2)
    m1 equals m3 should not be true
    m1 equals m4 should not be true
  }

  test("testAddition") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val v5 = DataVector(List(7, 2, 1))
    val v6 = DataVector(List(9, 8, -2))
    val v7 = DataVector(List(5,-1, 14))
    val v8 = DataVector(List(1, 2, 3))

    val m1 = DataMatrix(List(v1, v2, v3, v4))
    val m2 = DataMatrix(List(v5, v6, v7, v8))

    val e1 = DataVector(List(8, 7, 5))
    val e2 = DataVector(List(9, 17, 1))
    val e3 = DataVector(List(4, 4, 18))
    val e4 = DataVector(List(8, 6, 5))

    val expectedMatrix = DataMatrix(List(e1, e2, e3, e4))

    assert(m1 + m2 equals expectedMatrix)
  }

  test("testSubtraction") {
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val v5 = DataVector(List(7, 2, 1))
    val v6 = DataVector(List(9, 8, -2))
    val v7 = DataVector(List(5,-1, 14))
    val v8 = DataVector(List(1, 2, 3))

    val m1 = DataMatrix(List(v1, v2, v3, v4))
    val m2 = DataMatrix(List(v5, v6, v7, v8))

    val e1 = DataVector(List(-6, 3, 3))
    val e2 = DataVector(List(-9, 1, 5))
    val e3 = DataVector(List(-6, 6, -10))
    val e4 = DataVector(List(6, 2, -1))

    val expectedMatrix = DataMatrix(List(e1, e2, e3, e4))

    assert(m1 - m2 equals expectedMatrix)
  }

  test("testScalarMultiplication") {
    val scalar = 2
    val v1 = DataVector(List(1, 5, 4))
    val v2 = DataVector(List(0, 9 ,3))
    val v3 = DataVector(List(-1, 5, 4))
    val v4 = DataVector(List(7, 4, 2))

    val e1 = DataVector(List(2, 10, 8))
    val e2 = DataVector(List(0, 18, 6))
    val e3 = DataVector(List(-2, 10, 8))
    val e4 = DataVector(List(14, 8, 4))


    val m = DataMatrix(List(v1, v2, v3, v4))
    val expectedMatrix = DataMatrix(List(e1, e2, e3, e4))

    assert(m *  scalar equals expectedMatrix)
  }

  test("testDot") {
    val v1 = DataVector(List(2, 5, 7, 8))
    val v2 = DataVector(List(1, 3, 5, 7))
    val v3 = DataVector(List(0, 9, -1, -2))

    val v4 = DataVector(List(1, 2, 3))
    val v5 = DataVector(List(3, 4, 5))
    val v6 = DataVector(List (0, 9, 4))
    val v7 = DataVector(List(-1, -2, -3))

    val e1 = DataVector(List(9, 71, 35))
    val e2 = DataVector(List(3, 45, 17))
    val e3 = DataVector(List(29, 31, 47))

    val m1 = DataMatrix(List(v1, v2, v3))
    val m2 = DataMatrix(List(v4, v5, v6, v7))
    val expectedMatrix = DataMatrix(List(e1, e2, e3))

    assert(m1 dot m2 equals expectedMatrix)
  }

}
