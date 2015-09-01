

import Charts.{ScatterPlot, Pie}
import DataVector.DataVector
import IO.FileHandling
import Learning.NearestNeighbor

import scala.io.Source


object Main  {

  def main (args: Array[String]) {
    val dx = DataVector(List(5, 3, 2, 1))
    val dy = DataVector(List(6, 1, 2, 4))

    val names = List("C", "C++", "Java", "Ruby")

    /* val pieChart = new Pie
    pieChart.setData(d)
    pieChart.setLabels(names)
    pieChart.setTitle("Languages")
    pieChart.show() */

    /*val scatterPlot = new ScatterPlot
    scatterPlot.setTitle("Test Stuff")
    scatterPlot.setXData(dx)
    scatterPlot.setYData(dy)
    scatterPlot.show()
  */

    val fileName = "src\\test\\scala\\TestData\\bbTest.csv"

    val data = FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true)

    val testData = DataVector(List(0.167842031, 0.046544429, 0.002820874, 0.011283498, 0.133991537, 0.156558533))

    val bestFit = NearestNeighbor.kClassify(1, data, testData)

    println(bestFit.classification)
  }

}


