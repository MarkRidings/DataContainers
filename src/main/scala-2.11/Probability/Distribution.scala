package Probability

import org.apache.commons.math3.distribution.{FDistribution, TDistribution}

import scala.util.control.Breaks._

object Distribution {

  def normalPdf(x: Double, mu: Double = 0.0, sigma: Double = 1.0 ): Double = {
    val a = 1.0 / math.sqrt(math.Pi * 2) * sigma
    val b = math.exp( -1 * ((x - mu) * (x - mu)) / 2 * sigma * sigma)

    a * b
  }

  def normalCdf(x: Double, mu:Double = 0.0, sigma: Double = 1.0): Double = {
    val t = (x - mu) / math.sqrt(2) / sigma
    (1 + MathHelpers.erf(t)) / 2
  }

  def inverseNormalCdf(p: Double, mu:Double = 0.0, sigma: Double = 1.0, tolerance:Double = 0.00001):Double = {

    if (mu != 0 || sigma != 1) {
      return mu + sigma * inverseNormalCdf(p, tolerance = tolerance)
    }

    var lowZ: Double = -10.0
    var lowP: Double = 0
    var hiZ: Double = 10.0
    var hiP: Double = 1
    var midZ: Double = 0.0

    breakable {
      while (hiZ - lowZ > tolerance) {
        midZ = (lowZ + hiZ) / 2.0
        val midP = normalCdf(midZ)

        if (midP < p) {
          lowZ = midZ
          lowP = midP
        }
        else if (midP > p) {
          hiZ = midZ
          hiP = midP
        }
        else {
          break()
        }
      }
    }

    midZ
  }

  def binomialNormalApprox(n: Int, p: Double): (Double, Double) = {
    val mu = p * n
    val sigma = math.sqrt(p * (1 - p) * n)

    (mu, sigma)
  }

  def tDistCdf(x: Double, dof: Double): Double = {
    val tDist = new TDistribution(dof)

    (1 - tDist.cumulativeProbability(x)) * 2
  }

  def fDistCdf(x: Double, numDof: Double, denDof: Double): Double = {
    val fDist = new FDistribution(numDof, denDof)

    fDist.cumulativeProbability(x)
  }
}
