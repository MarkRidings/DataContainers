package Learning

import DataVector.DataVector

import scala.collection.mutable.ListBuffer

case class NeuralNetwork (numberInputs: Int, numberHiddenLayers: Int, sizeHiddenLayers: Int, sizeOutputLayer: Int) {

  val hiddenLayers: List[NeuralLayer] = initHiddenLayers

  val outputLayer: NeuralLayer =
    if (numberHiddenLayers > 0)
      NeuralLayer(sizeHiddenLayers, sizeOutputLayer)
    else
      NeuralLayer(numberInputs, sizeOutputLayer)

  def initHiddenLayers: List[NeuralLayer] = {

    val aux = new ListBuffer[NeuralLayer]
    for (i <- 0 until numberHiddenLayers) {
      if (i == 0) {
        aux.append(NeuralLayer(numberInputs, sizeHiddenLayers))
      }
      else {
        aux.append(NeuralLayer(sizeHiddenLayers, sizeHiddenLayers))
      }
    }

    aux.toList
  }

  def feedForward(inputs: DataVector): DataVector = {

    var aux = inputs
    for (i <- hiddenLayers.indices) {
      if (i == 0) {
        aux = hiddenLayers.head.evaluateLayer(inputs)
      }
      else {
        aux = hiddenLayers.apply(i).evaluateLayer(aux)
      }
    }

    outputLayer.evaluateLayer(aux)
  }

  def adjustWeights(inputs: DataVector, outputs: DataVector, target: Double): Unit = {
    hiddenLayers.foreach(_.adjustWeights(outputs, inputs, target))
    outputLayer.adjustWeights(outputs, inputs, target)
  }

  def printWeights(): Unit = {
    for (i <- hiddenLayers.indices) {
      println(s"Hidden Layer $i")
      hiddenLayers.apply(i).weights.matrix.foreach(println)
      println(hiddenLayers.apply(i).bias)
      println()
    }

    println("Output Layer")
    outputLayer.weights.matrix.foreach(println)
    println("bias -> " + outputLayer.bias)
    println()
  }

}
