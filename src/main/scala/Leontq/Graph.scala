package Leontq

trait Graph[V] {
  def vertices: List[V]

  def edges: List[(V, V)]

  def addEdges(a: V, b: V): Graph[V]

  def neighbours(vertex: V): List[V]

  // find total distance from source node till destination node
  def findTotalDistance(list: List[V]): Option[Int]
}
