package Leontq
import org.scalatest.{BeforeAndAfter, FunSuite}

class WeightedGraphTest extends FunSuite with BeforeAndAfter {
  var graph: WeightedGraph[Char]  = _
  var graph2: WeightedGraph[Char] = _
  var graph3: WeightedGraph[Char] = _
  before {
    // Graph 1 -> 2
    //  2 -> 3
    //  2 -> 4
    graph = new WeightedGraph[Char](Map('A' -> List(new WeightedEdge[Char]('B', 10))))
    graph2 = new WeightedGraph[Char](
      Map(
        'A' -> List(new WeightedEdge[Char]('B', 10)),
        'B' -> List(new WeightedEdge[Char]('A', 11), new WeightedEdge[Char]('C', 11))
      )
    )

    graph3 = new WeightedGraph[Char](
      Map(
        'A' -> List(
          new WeightedEdge[Char]('B', 5),
          new WeightedEdge[Char]('D', 5),
          new WeightedEdge[Char]('E', 7)
        ),
        'B' -> List(new WeightedEdge[Char]('C', 4)),
        'C' -> List(new WeightedEdge[Char]('D', 8), new WeightedEdge[Char]('E', 2)),
        'D' -> List(new WeightedEdge[Char]('C', 8), new WeightedEdge[Char]('E', 6)),
        'E' -> List(new WeightedEdge[Char]('B', 3))
      )
    )

  }

  test("Graph is initialised") {
    assert(graph != Nil)
  }

  test("Graph has 2 vertices") {
    assert(graph.vertices.length == 1)
  }

  test("Graph has 2 edges") {
    assert(graph.edges.length == 1)
  }

  test("B and C are neighbours of A") {
    val neighbours = graph.neighbours('A')
    assert(neighbours.contains('B'))
  }

  test("distance between A and B is 10 units") {
    assert(graph.findTotalDistance(List('A', 'B')).get == 10)
  }

  test("calculate distance operation between A and D should return None") {
    assert(graph.findTotalDistance(List('A', 'D')) == None)
  }

  test("exact total trips from A to A in 2 stops should be 1") {
    assert(graph2.findExactTrips('A', 'A', 2) == 1)
  }

  test("max total trips from A to B in 4 stops should be 2") {
    assert(graph2.findMaxTotalTrips('A', 'B', 4) == 2)
  }

  test("max total trips from A to A in 1 stops should be 1") {
    assert(graph2.findMaxTotalTrips('A', 'A', 1) == 1)
  }

  test("max total trips within 22 units from A to A should be 1") {
    assert(graph2.findTripsInRange('A', 'A', 22, true) == 1)
  }

  test("Shortest distance between A to C is 21") {
    assert(graph2.shortestPath('A', 'C', Set()) == 21)
  }

  test("distance of the path A-B-C should be 9") {
    assert(graph3.findTotalDistance("ABC".toList).get == 9)
  }

  test("distance of the path A-D should be 5") {
    assert(graph3.findTotalDistance("AD".toList).get == 5)
  }

  test("distance of the path A-D-C should be 13") {
    assert(graph3.findTotalDistance("ADC".toList).get == 13)
  }

  test("distance of the path A-E-B-C-D should be 22") {
    assert(graph3.findTotalDistance("AEBCD".toList).get == 22)
  }

  test("distance of the path A-E-D should be None") {
    assert(graph3.findTotalDistance("AED".toList) == None)
  }

  test("number of trips starting at C and ending at C with a maximum of 3 stops should be 2") {
    assert(graph3.findMaxTotalTrips('C', 'C', 3) == 2)
  }

  test("number of trips starting at A and ending at C with exactly 4 stops should be 3") {
    assert(graph3.findExactTrips('A', 'C', 4) == 3)
  }

  test("length of the shortest route (in terms of distance to travel) from A to C should be 9") {
    assert(graph3.shortestPath('A', 'C', Set()) == 9)
  }

  test("length of the shortest route (in terms of distance to travel) from B to B should be 0") {
    assert(graph3.shortestPath('B', 'B', Set()) == 0)
  }

  test(" number of different routes from C to C with a distance of less than 30 should be 7") {
    assert(graph3.findTripsInRange('C', 'C', 30, true) == 7)
  }

}
