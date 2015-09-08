package Learning

import DataMatrix.DataMatrix
import DataVector.DataVector

import scala.collection.mutable.ListBuffer

case class NeuralLayer (numInputs: Int, numNeurons: Int) {

  var bias = new DataVector()
  var weights = new DataMatrix()

  InitializeLayer()

  def InitializeLayer(): Unit = {
    bias = DataVector(List.range(1, numNeurons + 1).map(_.toDouble))

    val w = new ListBuffer[DataVector]
    for (i <- 0 until numNeurons) {
      val aux = List.fill(numInputs)((i + 1).toDouble)
      w.append(DataVector(aux))
    }

    weights = new DataMatrix(w.toList)
  }

  def sigmoid(n: Double): Double = {
    1.0 / (1.0 + math.exp(-n))
  }

  def hardLimit(n: Double): Double = {
    if (n < 0)
      0.0
    else
      1.0
  }

  def evaluateNeuron(neuron: DataVector, bias: Double, inputs: DataVector): Double = {
    sigmoid((neuron dot inputs) + bias)
  }

  def adjustWeights(outputs: DataVector, inputs: DataVector, target: Double): Unit = {

    val newWeights = new ListBuffer[DataVector]
    val newBias = new ListBuffer[Double]

    for (i <- weights.matrix.indices) {
      val error = target - outputs.at(i)
      newWeights.append(weights.matrix.apply(i) + (inputs * error))
      newBias.append(bias.at(i) + error)
    }

    weights = DataMatrix(newWeights.toList)
    bias = DataVector(newBias.toList)

  }

  def evaluateLayer(inputs: DataVector): DataVector = {
    DataVector(weights.matrix zip bias.toList map { case(a, b) => evaluateNeuron(a, b, inputs) })
  }

}
