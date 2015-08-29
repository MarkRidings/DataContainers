

import Charts.{ScatterPlot, Pie}
import DataVector.DataVector


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

    val scatterPlot = new ScatterPlot
    scatterPlot.setTitle("Test Stuff")
    scatterPlot.setXData(dx)
    scatterPlot.setYData(dy)
    scatterPlot.show()

  }

}


