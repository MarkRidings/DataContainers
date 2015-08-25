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

  def squared: Double = {
    this dot this
  }

  def magnitude: Double = {
    math.sqrt(this.squared)
  }

  def median: Double = {
    if (size % 2 == 0) {
      (v.sorted.apply(size / 2) + v.sorted.apply((size / 2) - 1)) / 2.0
    }
    else {
      v.sorted.apply(size / 2)
    }

  }

  def equals(that: DataVector): Boolean = {
    this.v.equals(that.v)
  }

  def toList: List[Double] = {
    this.v
  }

}
