package Learning

import DataVector.DataVector
import Probability.Distribution

case object Regression {

  def LeastSquaresFit(indVector: DataVector, depVector: DataVector): (Double, Double) = {
    val beta = indVector.correlation(depVector) * depVector.standardDev / indVector.standardDev
    val alpha = depVector.mean - beta * indVector.mean

    (alpha, beta)
  }

  // coefficients is (alpha, beta)
  def predict(coefficients: (Double, Double), dataPoint: Double): Double = {
    coefficients._2 * dataPoint + coefficients._1
  }

  def error(coefficients: (Double, Double), dataPoint: Double, expectedResult: Double): Double = {
    expectedResult - predict(coefficients, dataPoint)
  }

  def sumOfSquaresErrors(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    (indVector.toList zip depVector.toList map { case (a, b) => math.pow(error(coefficients, a, b), 2) }).sum
  }

  def sumOfSquaresRegression(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    depVector.sumOfSquares - sumOfSquaresErrors(coefficients, indVector, depVector)
  }

  def meanSSE(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    sumOfSquaresErrors(coefficients, indVector, depVector) / (indVector.size - 2)
  }

  def indStdError(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    math.sqrt(meanSSE(coefficients, indVector, depVector) / indVector.sumOfSquares)
  }

  def depStdError(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double ={
    val mse = meanSSE(coefficients, indVector, depVector)
    val invCount: Double = 1.0 / indVector.size.toDouble

    math.sqrt(mse * (invCount + (math.pow(indVector.mean, 2) / indVector.sumOfSquares)))
  }

  def fValue(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    sumOfSquaresRegression(coefficients, indVector, depVector) / meanSSE(coefficients, indVector, depVector)
  }

  def fSignificance(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    val f = fValue(coefficients, indVector, depVector)
    1 - Distribution.fDistCdf(f, 1, indVector.size - 2)
  }

  def pValue(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    val tScore = math.sqrt(fValue(coefficients, indVector, depVector))
    Distribution.tDistCdf(tScore, indVector.size - 2)
  }

  def rSquared(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    1.0 - (sumOfSquaresErrors(coefficients, indVector, depVector) / depVector.sumOfSquares)
  }
}
