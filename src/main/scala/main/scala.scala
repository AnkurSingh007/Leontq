package main

import Leontq.{WeightedEdge, WeightedGraph}

object scala {
  def main(args: Array[String]): Unit = {
    val config = List("AB5", "BC4", "CD8", "DC8", "DE6", "AD5", "CE2", "EB3", "AE7")
    val map = prepareMap(config)
    val graph = new WeightedGraph[Char](map)

  }

  // preCondition: each string is in correct format
  // postCondition: return empty map in case of any error
  def prepareMap(list : List[String]): Map[Char, List[WeightedEdge[Char]]] ={
    try {
      list
        .map(s => (s.charAt(0), s.charAt(1), s.substring(2).toInt))
        .map(
          a => (a._1 -> WeightedEdge[Char](a._2, a._3))
        ).
        groupMap(a => a._1)(a => a._2)
    }
    catch{
      case e : Exception => Map.empty[Char, List[WeightedEdge[Char]]]
    }
  }

}
