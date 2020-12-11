package Leontq

case class WeightedEdge[V](destination: V, weight: Int)

class WeightedGraph[V](adjList:Map[V, List[WeightedEdge[V]]]) extends Graph[V]{
  override def vertices: List[V] = adjList.keys.toList

  def addEdges(a: V, b: WeightedEdge[V]): WeightedGraph[V] = {
    val adjNeighbours = b +: adjList.getOrElse(a, Nil)
    new WeightedGraph(adjList + (a -> adjNeighbours))
  }

  override def neighbours(vertex: V): List[V] = adjList.getOrElse(vertex, Nil).map(v => v.destination)

  override def addEdges(a: V, b: V): WeightedGraph[V] = addEdges(a, new WeightedEdge(b, 0))

  override def edges: List[(V, V)] = adjList.flatMap{
    case (v, edgeList) => edgeList.map(n => (v, n.destination))
  }.toList
}