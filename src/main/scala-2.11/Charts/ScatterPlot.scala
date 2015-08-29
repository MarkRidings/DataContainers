package Charts

import DataVector.DataVector

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Side
import scalafx.scene.Scene
import scalafx.scene.chart.{XYChart, NumberAxis, ScatterChart}

class ScatterPlot {

  var _title: String = ""
  var _xData: DataVector = DataVector(List())
  var _yData: DataVector = DataVector(List())
  var _yAxis: String = "Y"
  var _xAxis: String = "X"
  var _xMin: Double = 0.0
  var _xMax: Double = 10.0
  var _xStep: Double = 1.0
  var _yMin: Double = 0.0
  var _yMax: Double = 10.0
  var _yStep: Double = 1.0

  def setTitle(title: String): Unit = {
    _title = title
  }

  def setXData(data: DataVector): Unit = {
    _xData = data
  }

  def setYData(data: DataVector): Unit = {
    _yData = data
  }



  def show(): Unit = {
    new makeChart().main(Array())
  }

  private class makeChart extends JFXApp {

    stage = new PrimaryStage {
      title = _title
      scene = new Scene {
        root = new ScatterChart(NumberAxis(_xAxis, _xMin, _xMax, _xStep), NumberAxis(_yAxis, _yMin, _yMax, _yStep)) {
          title = _title
          legendSide = Side.RIGHT
          data = ObservableBuffer(
            createSeries("Series 1", _xData, _yData)
          )
        }
      }
    }

    def createSeries(name: String, xData: DataVector, yData: DataVector) =
      XYChart.Series[Number, Number](name, ObservableBuffer(
        xData.toList zip yData.toList map { case (a, b) => XYChart.Data[Number, Number](a, b)}
      ))
  }

}
