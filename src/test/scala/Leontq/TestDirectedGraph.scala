package Leontq
import org.scalatest.{BeforeAndAfter, FunSuite}

class WeightedGraphTest extends FunSuite with BeforeAndAfter{
  var graph: WeightedGraph[Char] = _
  before {
    // Graph 1 -> 2
    //  2 -> 3
    //  2 -> 4
    graph = new WeightedGraph[Char](Map('A' -> List(new WeightedEdge[Char]('B', 10))))
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


}