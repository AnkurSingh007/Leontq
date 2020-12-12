package main

import Leontq.{WeightedEdge, WeightedGraph}
import com.typesafe.config.{Config, ConfigFactory}

import java.io.File
import scala.io.Source

case class ServiceConf(
    input: List[String]
)

object Main {
  def main(args: Array[String]): Unit = {
    val config = readConfig()
    val map    = prepareMap(config)
    val graph  = new WeightedGraph[Char](map)
    testForTotalDistance(graph, List("AED", "AD", "ABC", "ADC", "AEBCD"))
    testForExactStops(graph, 'A', 'C', 4)
    testForMaximumStops(graph, 'C', 'C', 3)
    testForShortestLength(graph, 'B', 'B')
    testForShortestLength(graph, 'A', 'C')
    testForRoutesInRange(graph, 'C', 'C', 30)
  }

  def prepareMap(list: List[String]): Map[Char, List[WeightedEdge[Char]]] = {
    try {
      list
        .map(s => (s.charAt(0), s.charAt(1), s.substring(2).toInt))
        .map(a => a._1 -> WeightedEdge[Char](a._2, a._3))
        .groupMap(a => a._1)(a => a._2)
    } catch {
      case e: Exception => Map.empty[Char, List[WeightedEdge[Char]]]
    }
  }

  def readConfig(): List[String] = {
    Source.fromFile("application.conf").getLines().toList
  }

  // handles queries for total distance for given nodes
  def testForTotalDistance(graph: WeightedGraph[Char], list: List[String]): Unit = {
    for (input <- list) {
      graph.findTotalDistance(input.toList) match {
        case Some(i) => println("Total distance for route " + input + " is " + i.toString)
        case None    => println("NO SUCH ROUTE possible for route " + input)
      }
    }
  }

  def testForExactStops(
      graph: WeightedGraph[Char],
      source: Char,
      destination: Char,
      stops: Int
  ): Unit = {
    println(
      "Trips between " + source.toString + " and " + destination.toString
        + " for exact" + stops + " stops is "
        + graph.findExactTrips(source, destination, stops)
    )
  }

  def testForMaximumStops(
      graph: WeightedGraph[Char],
      source: Char,
      destination: Char,
      stops: Int
  ): Unit = {
    println(
      "Max trips between " + source.toString + " and " + destination.toString
        + " for " + stops + " stops is "
        + graph.findMaxTotalTrips(source, destination, stops)
    )
  }

  def testForShortestLength(
      graph: WeightedGraph[Char],
      source: Char,
      destination: Char
  ): Unit = {
    println(
      "Shortest distance from " + source.toString + " to " + destination.toString +
        " is " + graph.shortestPath(source, destination, Set()).toString
    )
  }

  def testForRoutesInRange(
      graph: WeightedGraph[Char],
      source: Char,
      destination: Char,
      range: Int
  ): Unit = {
    println(
      "Total routes between " + source.toString + " and " + destination.toString +
        " within range " + range + " is " + graph
        .findTripsInRange(source, destination, range, true)
        .toString
    )
  }
}
