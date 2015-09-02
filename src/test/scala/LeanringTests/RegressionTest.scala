package LeanringTests

import DataMatrix.DataMatrix
import IO.FileHandling
import Learning.Regression
import org.scalatest.{ShouldMatchers, FunSuite}

class RegressionTest extends FunSuite with ShouldMatchers {

  test("testLeastSquaresFit") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    coefficients._1 should be (0.16217 +- 0.01)
    coefficients._2 should be (0.39251896 +- 0.1)

  }

  test("testRSquared") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val r = Regression.rSquared(coefficients, data.getColumn(4), data.getColumn(5))

    r should be (0.035988963 +- 0.01)
  }

  test("testPredict") {

  }



}
