package Probability

object HypoTest {

  def normalProbabilityAbove(lo: Double, mu: Double = 0.0, sigma:Double = 1.0): Double = {
    1 - Distribution.normalCdf(lo, mu, sigma)
  }

  def normalProbabilityBelow(hi: Double, mu: Double = 0.0, sigma: Double = 1.0): Double = {
    Distribution.normalCdf(hi, mu, sigma)
  }

  def normalProbabilityBetween(lo: Double, hi: Double, mu: Double = 0.0, sigma:Double = 1.0): Double = {
    Distribution.normalCdf(hi, mu, sigma) - Distribution.normalCdf(lo, mu, sigma)
  }

  def normalProbabilityOutside(lo: Double, hi: Double, mu: Double = 0.0, sigma: Double = 1.0): Double = {
    1 - normalProbabilityBetween(lo, hi, mu, sigma)
  }

  def normalUpperBound(p: Double, mu:Double = 0.0, sigma: Double = 1.0):Double = {

    if (p > 1.0 || p < 0.0)
      throw new Exception("p-value must be between 0.0 and 1.0")

    Distribution.inverseNormalCdf(p, mu, sigma)
  }

  def normalLowerBound(p: Double, mu: Double = 0.0, sigma: Double = 1.0): Double = {

    if (p > 1.0 || p < 0.0)
      throw new Exception("p-value must be between 0.0 and 1.0")

    Distribution.inverseNormalCdf(1 - p, mu, sigma)
  }

  def normalTwoSidedBound(p: Double, mu: Double = 0.0, sigma: Double = 1.0): (Double, Double) = {
    val tailProbability = (1 - p ) / 2

    val upperBound = normalLowerBound(tailProbability, mu, sigma)
    val lowerBound = normalUpperBound(tailProbability, mu, sigma)

    (lowerBound, upperBound)
  }

  def twoSidedP(x: Double, mu: Double = 0.0, sigma: Double = 1.0): Double = {

    if (x > mu) {
      2 * normalProbabilityAbove(x, mu, sigma)
    }
    else {
      2 * normalProbabilityBelow(x, mu, sigma)
    }
  }
}
