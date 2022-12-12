import scala.collection.mutable

object Day12 extends IDay {
  type HeightMap = Array[Array[Int]]
  type Point = (Int, Int)

  override def execute(input: String): (Any, Any) = {
    def readHeightMap(input: String): (HeightMap, Point, Point) = {
      def readChar(c: Char): Int = c match {
        case 'S' => 1
        case 'E' => 26
        case c => c - 'a' + 1
      }

      def findCoords(search: Char): Point = {
        val x: ((Char, Int), Int) = Helper.readLines(input, identity)
          .zipWithIndex
          .map(outer => outer._1.zipWithIndex.zip(LazyList.continually(outer._2)))
          .filter(outer => outer.exists(_._1._1 == search))
          .head
          .filter(inner => inner._1._1 == search)
          .head
        (x._1._2, x._2)
      }

      (Helper.readLines(input, _.map(readChar).toArray).toArray, findCoords('S'), findCoords('E'))
    }

    val (heightMap, start, end) = readHeightMap(input)
    (part1(heightMap, start, end), part2(heightMap, end))
  }

  def getHeight(heightMap: HeightMap, point: Point): Int = {
    heightMap(point._2)(point._1)
  }

  def part1(heightMap: HeightMap, start: Point, end: Point): Int = {
    def addToFrontier(visited: Set[Point], frontier: mutable.PriorityQueue[(Point, Int)], curr: Point)(p: Point): Boolean =
      getHeight(heightMap, p) <= getHeight(heightMap, curr) + 1 && !visited.contains(p)

    val ordering = Ordering.by({ p: (Point, Int) => -(p._2 + ((p._1._1 - end._1).abs + (p._1._2 - end._2).abs)) })

    hillClimb(mutable.PriorityQueue((start, 0))(ordering), _ == end, addToFrontier)(heightMap)
  }

  def part2(heightMap: HeightMap, end: Point): Int = {
    def addToFrontier(visited: Set[Point], frontier: mutable.PriorityQueue[(Point, Int)], curr: Point)(p: Point): Boolean =
      getHeight(heightMap, p) >= getHeight(heightMap, curr) - 1 &&
        !visited.contains(p) &&
        !frontier.exists(q => q._1 == p)

    val ordering = Ordering.by({ p: (Point, Int) => -(p._2 - getHeight(heightMap, p._1) / 2) })

    hillClimb(mutable.PriorityQueue((end, 0))(ordering), getHeight(heightMap, _) == 1, addToFrontier)(heightMap)
  }

  def hillClimb(startFrontier: mutable.PriorityQueue[(Point, Int)], successCondition: Point => Boolean,
                frontierCondition: (Set[Point], mutable.PriorityQueue[(Point, Int)], Point) => Point => Boolean)
               (heightMap: HeightMap): Int = {
    val frontier: mutable.PriorityQueue[(Point, Int)] = startFrontier
    var visited: Set[Point] = Set()

    def neighbours(point: Point): Iterable[Point] =
      List((1, 0), (-1, 0), (0, 1), (0, -1))
        .map(d => (point._1 + d._1, point._2 + d._2))
        .filter(p => p._1 >= 0 && p._1 < heightMap.head.length && p._2 >= 0 && p._2 < heightMap.length)

    while (frontier.nonEmpty) {
      val (point, currDist) = frontier.dequeue()
      if (successCondition(point)) return currDist

      visited = visited + point

      neighbours(point)
        .filter(p => frontierCondition(visited, frontier, point)(p))
        .foreach(p => frontier.enqueue((p, currDist + 1)))
    }

    sys.error("failed to find path")
  }
}
