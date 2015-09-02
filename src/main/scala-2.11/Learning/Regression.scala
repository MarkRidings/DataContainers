package Learning

import DataVector.DataVector

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

  def rSquared(coefficients: (Double, Double), indVector: DataVector, depVector: DataVector): Double = {
    1.0 - (sumOfSquaresErrors(coefficients, indVector, depVector) / depVector.sumOfSquares)
  }
}
