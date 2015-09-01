package Learning

import DataVector.DataVector
import IO.FileHandling
import org.scalatest.FunSuite

class NearestNeighbor$Test extends FunSuite {

  test("testKClassify") {

    val fileName = "src\\test\\scala\\TestData\\classData.csv"

    val data = FileHandling.LoadCsv(fileName, hasHeader = false, hasLabels = true)

    val testData = DataVector(List(6, 5, 4, 2))

    testData.classification = NearestNeighbor.kClassify(5, data, testData).classification

    assert(testData.classification == "D")
  }

}
