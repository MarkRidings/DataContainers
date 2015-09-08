package LeanringTests

import DataVector.DataVector
import Learning.NeuralNetwork
import org.scalatest.{FunSuite, ShouldMatchers}

class NeuralNetworkTest extends FunSuite with ShouldMatchers {

  test("testFeedForward") {

    val inputs = DataVector(List(1, 1))
    val numberHiddenLayers = 2
    val sizeHiddenLayers = 5
    val numberOutputLayers = 1

    val n = NeuralNetwork(inputs, numberHiddenLayers, sizeHiddenLayers, numberOutputLayers)
    val outputs = n.feedForward

    outputs.at(0) should be (0.731059 +- 0.000001)
    outputs.size should be (1)

  }

}
