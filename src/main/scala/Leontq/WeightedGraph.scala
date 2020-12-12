package Leontq

import scala.annotation.tailrec
import scala.collection.{BitSet, mutable}

case class WeightedEdge[V](destination: V, weight: Int)

class WeightedGraph[V](adjList: Map[V, List[WeightedEdge[V]]]) extends Graph[V] {
  override def vertices: List[V] = adjList.keys.toList

  def addEdges(a: V, b: WeightedEdge[V]): WeightedGraph[V] = {
    val adjNeighbours = b +: adjList.getOrElse(a, Nil)
    new WeightedGraph(adjList + (a -> adjNeighbours))
  }

  override def neighbours(vertex: V): List[V] =
    adjList.getOrElse(vertex, Nil).map(v => v.destination)

  def neighboursWithWeight(vertex: V): List[WeightedEdge[V]] = adjList.getOrElse(vertex, Nil)

  override def addEdges(a: V, b: V): WeightedGraph[V] = addEdges(a, new WeightedEdge(b, 0))

  override def edges: List[(V, V)] =
    adjList.flatMap {
      case (v, edgeList) => edgeList.map(n => (v, n.destination))
    }.toList

  private def findDistance(source: V, destination: V): Option[Int] = {
    if (neighbours(source) contains destination) {
      val edges = adjList get source
      Some(
        edges.get
          .filter(edge => edge.destination == destination)
          .map(edge => edge.weight)
          .foldLeft(0)(_ + _)
      )
    } else {
      None
    }
  }

  // If there is no route between any 2 consecutive nodes return None
  // else return total distance from source to destination
  def findTotalDistance(list: List[V]): Option[Int] = {
    (list zip list.tail).foldLeft(Option[Int](0))((distance: Option[Int], pair) => {
      distance match {
        case Some(i: Int) => {
          findDistance(pair._1, pair._2) match {
            case Some(j: Int) => Some(i + j)
            case _            => None
          }
        }
        case _ => None
      }
    })
  }

  // return total number of trips possible from source to destination in exactly  `stops`
  // number of stops
  def findExactTrips(source: V, destination: V, stops: Int): Int = {
    if (stops < 0) 0
    else if (stops == 0 && source == destination) 1
    else
      neighbours(source).foldLeft(0)((routes, neighbour) =>
        (routes + findExactTrips(neighbour, destination, stops - 1))
      )
  }

  // find maximum possible trips for given number of trials form source to destination
  def findMaxTotalTrips(source: V, destination: V, maxStops: Int): Int = {
    if (maxStops <= 0) 0
    else if (maxStops == 0 && source == destination) 0
    else {
      if (source == destination && maxStops != 0)
        1 + neighbours(source).foldLeft(0)((routes, neighbour) =>
          (routes + findMaxTotalTrips(neighbour, destination, maxStops - 1))
        )
      else
        neighbours(source).foldLeft(0)((routes, neighbour) =>
          (routes + findMaxTotalTrips(neighbour, destination, maxStops - 1))
        )
    }
  }

  def findTripsInRange(source: V, destination: V, range: Int, start: Boolean): Int = {
    if (range <= 0) 0
    else {
      val total = neighboursWithWeight(source).foldLeft(0)((routes, neighbour) =>
        (routes + findTripsInRange(
          neighbour.destination,
          destination,
          range - neighbour.weight,
          false
        ))
      )
      if (source == destination && start != true) {
        1 + total
      } else total
    }
  }

  def shortestPath(source: V, destination: V, visited: Set[V]): Int = {
    val newVisited = visited + source
    if (source == destination) 0
    else {
      var pathLength = for (i <- adjList(source)) yield {
        val sLen = if (!visited.contains(i.destination)) {
          shortestPath(i.destination, destination, newVisited)
        } else Int.MaxValue
        if (i.weight > Int.MaxValue - sLen) Int.MaxValue else i.weight + sLen
      }
      if (pathLength.isEmpty) Int.MaxValue else pathLength.min
    }
  }
}
