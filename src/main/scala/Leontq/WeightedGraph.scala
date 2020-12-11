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

  private def findDistance(source: V, destination: V): Option[Int] = {
    if(neighbours(source) contains destination){
      val edges = adjList get source
      Some(edges.get.filter(edge => edge.destination == destination).map(edge => edge.weight).foldLeft(0)(_ + _))
    } else {
      None
    }
  }

  // If there is no route between any 2 consecutive nodes return None
  // else return total distance from source to destination
  override def findTotalDistance(list: List[V]): Option[Int] = {
    (list zip list.tail).foldLeft(Option[Int](0))(
      (distance: Option[Int] , pair) => {
        distance match {
          case Some(i: Int) => {
            findDistance(pair._1 , pair._2) match {
              case Some(j: Int) => Some(i + j)
              case _ => None
            }
          }
          case _ => None
        }
      }
    )
  }
}