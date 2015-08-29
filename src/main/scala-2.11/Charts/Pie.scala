package Charts

import DataVector.DataVector

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.PieChart

class Pie {

  private var _data: DataVector = new DataVector(List())
  private var _labels: List[String] = List()
  private var _title: String = ""
  private var _clockwise: Boolean = false

  def show(): Unit = {
    new makeChart().main(Array())
  }

  def setData(data: DataVector): Unit = {
    _data = data
  }

  def setLabels(labels: List[String]): Unit = {
    _labels = labels
  }

  def setTitle(title: String): Unit = {
    _title = title
  }

  def setClockwise(b: Boolean): Unit = {
    _clockwise = b
  }

  private class makeChart extends JFXApp {

    stage = new PrimaryStage {
      title = _title
      scene = new Scene {
        root = new PieChart() {
          title = _title
          clockwise = _clockwise
          data = ObservableBuffer(_data.toList zip _labels map { case (a, b) => PieChart.Data(b, a) })
        }
      }
    }
  }

 }


