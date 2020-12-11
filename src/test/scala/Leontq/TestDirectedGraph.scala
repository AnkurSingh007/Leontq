package Leontq
import org.scalatest.{BeforeAndAfter, FunSuite}

class WeightedGraphTest extends FunSuite with BeforeAndAfter{
  var graph: WeightedGraph[Char] = _
  var graph2: WeightedGraph[Char] = _
  before {
    // Graph 1 -> 2
    //  2 -> 3
    //  2 -> 4
    graph = new WeightedGraph[Char](Map('A' -> List(new WeightedEdge[Char]('B', 10))))
    graph2 = new WeightedGraph[Char](
      Map('A' -> List(new WeightedEdge[Char]('B', 10)),
      'B' -> List(new WeightedEdge[Char]('A', 11), new WeightedEdge[Char]('C', 11))
      )
    )
  }

    test("Graph is initialised"){
      assert(graph != Nil)
    }

    test("Graph has 2 vertices"){
      assert(graph.vertices.length == 1)
    }

    test("Graph has 2 edges"){
      assert(graph.edges.length == 1)
    }

    test("B and C are neighbours of A"){
      val neighbours = graph.neighbours('A')
      assert(neighbours.contains('B'))
    }

    test("distance between A and B is 10 units"){
      assert(graph.findTotalDistance(List('A', 'B')).get == 10)
    }

  test("calculate distance operation between A and D should return None"){
    assert(graph.findTotalDistance(List('A', 'D')) == None)
  }

  test("exact total trips from A to A in 2 stops should be 1"){
    assert(graph2.findExactTrips('A', 'A', 2) == 1)
  }

  test("max total trips from A to B in 4 stops should be 2"){
    assert(graph2.findMaxTotalTrips('A', 'B', 4) == 2)
  }

  test("max total trips from A to A in 1 stops should be 1"){
    assert(graph2.findMaxTotalTrips('A', 'A', 1) == 1)
  }

  test("max total trips within 21 units from A to A should be 1"){
    assert(graph2.findTripsInRange('A', 'A', 21) == 2)
  }

  test("Shortest distance between "){
    assert(graph2.shortestPath('A', 'C', Set()) == 21)
  }


}