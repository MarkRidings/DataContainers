package Learning

import DataVector.DataVector

case object NearestNeighbor {

  def kClassify(k: Int, vectors: List[DataVector], newVector: DataVector): DataVector = {

    if (k > vectors.length)
      throw new Exception("k can not be larger then the list of DataVectors")

    Counter(vectors.sortBy(x => x.distanceTo(newVector)).slice(0, k))
  }

  def Counter(neighbors: List[DataVector]): DataVector = {

    if (neighbors.length == 1)
      return neighbors.head

    val winners = neighbors.map(x => (x, neighbors.count(n => n.classification == x.classification))).sortBy(- _._2)

    if (winners.head._2 == winners(1)._2) {
      Counter(winners.map(_._1).toList.slice(0, winners.length -1))
    }
    else {
      winners.head._1
    }
  }
}
