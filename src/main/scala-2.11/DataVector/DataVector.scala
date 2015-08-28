package DataVector

case class DataVector (vector: List[Double]) {

  val v = vector

  def size: Int = {
    v.length
  }

  def isEmpty: Boolean = {
    v.isEmpty
  }

  def +(that: DataVector): DataVector = {
    new DataVector((this.v, that.v).zipped.map(_ + _))
  }

  def -(that: DataVector): DataVector = {
    new DataVector((this.v, that.v).zipped.map(_ - _))
  }

  def *(that: Double): DataVector = {
    new DataVector(this.v.map(x => x * that))
  }

  def dot(that: DataVector): Double = {
    (this.v, that.v).zipped.map(_ * _).sum
  }

  def at(index: Int): Double = {
    v.apply(index)
  }

  def squared: Double = {
    this dot this
  }

  def magnitude: Double = {
    math.sqrt(this.squared)
  }

  def mean: Double = {
    v.sum / size
  }

  def median: Double = {
    if (size % 2 == 0) {
      (v.sorted.apply(size / 2) + v.sorted.apply((size / 2) - 1)) / 2.0
    }
    else {
      v.sorted.apply(size / 2)
    }
  }

  def range: Double = {
    v.max - v.min
  }

  def quartile(q: Double): Double = {

    val index = (size * q).toInt - 1

    if (index <= 0)
      v.sorted.head
    else
      v.sorted.apply(index)
  }

  def interQuartileRange: Double = {
    quartile(.75) - quartile(.25)
  }

  def variance: Double = {
    val m = mean
    v.map(x => (x - m) * (x - m)).sum / (size - 1).toDouble
  }

  def standardDev: Double = {
    math.sqrt(variance)
  }

  def covariance(that: DataVector): Double = {
    val m1 = this.mean
    val m2 = that.mean

    (this.v zip that.v map { case(a, b) => (a - m1) * (b - m2) }).sum / (size - 1).toDouble
  }

  def correlation(that: DataVector): Double = {
    val std1 = this.standardDev
    val std2 = that.standardDev

    if (std1 > 0 && std2 > 0) {
      (this covariance that) / std1 / std2
    }
    else {
      0.0
    }
  }

  def equals(that: DataVector): Boolean = {
    this.v.equals(that.v)
  }

  def toList: List[Double] = {
    this.v
  }

}
