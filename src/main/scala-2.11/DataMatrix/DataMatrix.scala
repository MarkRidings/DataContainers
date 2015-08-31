package DataMatrix

import DataVector.DataVector

import scala.collection.mutable.ListBuffer

case class DataMatrix (matrix: List[DataVector]) {

  private val _matrix = matrix

  for (m <- _matrix) {
    if (m.size != shape._2)
      throw new Exception("All DataVectors in a matrix must be the same size")
  }

  def shape: (Int, Int) = {
    if (_matrix.isEmpty)
      (0, 0)
    else {
      (_matrix.length, _matrix.head.size)
    }
  }

  def getRow(index: Int): DataVector = {
    _matrix(index)
  }

  def getColumn(index: Int): DataVector = {
    DataVector(_matrix.map(_.at(index)))
  }

  def getMatrix: List[DataVector] = {
    _matrix
  }

  def equals(that: DataMatrix): Boolean = {

    if (this.shape != that.shape) {
      return false
    }

    for (index <- _matrix.indices) {
      if (!getRow(index).equals(that.getRow(index))) {
        return false
      }
    }

    true
  }

  def +(that: DataMatrix): DataMatrix = {

    if (this.shape != that.shape)
      throw new Exception("Addition requires matrices have the same shape")

    DataMatrix(this.getMatrix zip that.getMatrix map { case (a, b) => a + b })
  }

  def -(that: DataMatrix): DataMatrix = {

    if (this.shape != that.shape)
      throw new Exception("Subtraction requires matrices have the same shape")

    DataMatrix(this.getMatrix zip that.getMatrix map { case (a, b) => a - b })
  }

  def *(that: Double): DataMatrix = {
    DataMatrix(_matrix.map(_ * that))
  }

  def dot (that: DataMatrix): DataMatrix = {

    if (this.shape._2 != that.shape._1)
      throw new Exception("Illegal shape for Matrix Multiplication")

    val rows = new ListBuffer[DataVector]

    for (row <- 0 until this.shape._1) {
      val aux = new ListBuffer[Double]
      for (col <- 0 until that.shape._2) {
        aux.append(this.getRow(row) dot that.getColumn(col))
      }
      rows.append(DataVector(aux.toList))
    }

    DataMatrix(rows.toList)
  }

}
