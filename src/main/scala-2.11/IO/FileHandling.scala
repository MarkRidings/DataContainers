package IO

import DataVector.DataVector

import scala.collection.mutable.ListBuffer
import scala.io.Source

object FileHandling {

  def LoadCsv(fileName: String, hasHeader: Boolean = false, hasLabels: Boolean = false): List[DataVector] = {

    val file = Source.fromFile(fileName).getLines().toList

    if (hasHeader) {
      val header = file.head.split(',')
      CreateVectors(file.tail, hasLabels, header)
    }
    else {
      CreateVectors(file, hasLabels)
    }
  }

  def CreateVectors(data: List[String], hasLabels: Boolean, header:Array[String] = null): List[DataVector] = {

    val dataList = new ListBuffer[DataVector]()

    for (line <- data) {
      val items = line.split(',')
      var vec = DataVector(List())

      if (hasLabels) {
        vec = DataVector(ConvertDataToDouble(items.tail))
        vec.classification = items.head
      }
      else {
        vec = DataVector(ConvertDataToDouble(items))
      }

      if (header != null) {
        vec.header = header.toList
      }

      dataList.append(vec)
    }

    dataList.toList
  }

  def ConvertDataToDouble(data: Array[String]): List[Double] = {

    data.map(_.toDouble).toList
  }

}
