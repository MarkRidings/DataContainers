package Learning

import DataMatrix.DataMatrix
import DataVector.DataVector

import scala.collection.mutable.ListBuffer

object NeuralNetwork {

  def stepFunction(x: Double): Int = {
    if (x >= 0)
      1
    else
      0
  }

  def output(weights: DataVector, bias: Double, x: DataVector): Int = {
    stepFunction((weights dot x) + bias)
  }

  def sigmoid(t: Double): Double = {
    1.0 / (1.0 + math.exp(-t))
  }

  def neuron_output(weights: DataVector, inputs: DataVector): Double = {
    sigmoid(weights dot inputs)
  }

  def feedForward(neuralNetwork: DataMatrix, input: DataVector, bias: DataVector): DataVector = {

    var inputVector = input
    val outputs = new ListBuffer[DataVector]()

    neuralNetwork.matrix.foreach(x => {
      val inputWithBias = input append bias
      val output = x.v.map(a => neuron_output(x, inputWithBias))
      outputs.append(DataVector(output))
      inputVector = DataVector(output)
    })

    null
  }

}
