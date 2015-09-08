

import Charts.{ScatterPlot, Pie}
import DataMatrix.DataMatrix
import DataVector.DataVector
import IO.FileHandling
import Learning.{NeuralNetwork, NearestNeighbor}

import scala.io.Source


object Main  {

  def main (args: Array[String]) {

    val input1 = DataVector(List(0, 0))
    val input2 = DataVector(List(1, 0))
    val input3 = DataVector(List(0, 1))
    val input4 = DataVector(List(1, 1))

    val inputs = DataMatrix(List(input1, input2, input3, input4))
    val targets = List(0, 0, 0, 1)

    val n = NeuralNetwork(2, 0, 0, 1)

    n.printWeights()

    println("---------- training -----------------")
    for (j <- 0 until 1000) {
      for (i <- inputs.matrix.indices) {

        val output = n.feedForward(inputs.getRow(i))
        n.adjustWeights(inputs.getRow(i), output, targets.apply(i))
      }
    }

    n.printWeights()

    println("------------- results ----------------")
    for (i <- inputs.matrix.indices) {
      val output = n.feedForward(inputs.getRow(i))
      inputs.getRow(i).toList.foreach(x => print(x + " "))
      println(" -> " + output)
    }

  }

}


