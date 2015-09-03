package LeanringTests

import DataMatrix.DataMatrix
import IO.FileHandling
import Learning.Regression
import Probability.Distribution
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

  test("testSumOfSquaresError") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val sse = Regression.sumOfSquaresErrors(coefficients, data.getColumn(4), data.getColumn(5))

    sse should be (0.917245865 +- 0.01)

  }

  test("testSumOfSquaresRegression") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val ssr = Regression.sumOfSquaresRegression(coefficients, data.getColumn(4), data.getColumn(5))

    ssr should be (0.034243101 +- 0.01)

  }

  test("testMeanSSE") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val mse = Regression.meanSSE(coefficients, data.getColumn(4), data.getColumn(5))

    mse should be (0.003527869 +- 0.001)
  }

  test("testIndStdError") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val v = Regression.indStdError(coefficients, data.getColumn(4), data.getColumn(5))

    v should be (0.125988325 +- 0.01)

  }

  test("testDepStdError") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val v = Regression.depStdError(coefficients, data.getColumn(4), data.getColumn(5))

    v should be (0.010664962 +- 0.1)

  }

  test("fValueTest") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val f = Regression.fValue(coefficients, data.getColumn(4), data.getColumn(5))

    f should be (9.706455649 +- 0.01)
  }

  test("pValueTest") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val p = Regression.pValue(coefficients, data.getColumn(4), data.getColumn(5))

    p should be (0.002041986 +- 0.01)
  }

  test("fSigTest") {
    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"
    val data = DataMatrix(FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true))

    val coefficients = Regression.LeastSquaresFit(data.getColumn(4), data.getColumn(5))

    val fSig = Regression.fSignificance(coefficients, data.getColumn(4), data.getColumn(5))

    fSig should be (0.002041986 +- 0.01)
  }

}
