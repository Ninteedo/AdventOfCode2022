import scala.collection.mutable

object Day12 extends IDay {
  type HeightMap = Array[Array[Int]]
  type Frontier = mutable.PriorityQueue[(Point2D, Int)]

  override def execute(input: String): (Any, Any) = {
    def readHeightMap(input: String): (HeightMap, Point2D, Point2D) = {
      def readChar(c: Char): Int = c match {
        case 'S' => 1
        case 'E' => 26
        case c => c - 'a' + 1
      }

      def findCoords(search: Char): Point2D = {
        val x: ((Char, Int), Int) = Helper.readLines(input, identity)
          .zipWithIndex
          .map(outer => outer._1.zipWithIndex.zip(LazyList.continually(outer._2)))
          .filter(outer => outer.exists(_._1._1 == search))
          .head
          .filter(inner => inner._1._1 == search)
          .head
        new Point2D(x._1._2, x._2)
      }

      (Helper.readLines(input, _.map(readChar).toArray).toArray, findCoords('S'), findCoords('E'))
    }

    val (heightMap, start, end) = readHeightMap(input)
    (part1(heightMap, start, end), part2(heightMap, end))
  }

  def getHeight(heightMap: HeightMap, point: Point2D): Int = {
    heightMap(point.y)(point.x)
  }

  def part1(heightMap: HeightMap, start: Point2D, end: Point2D): Int = {
    def addToFrontier(visited: Set[Point2D], frontier: Frontier, curr: Point2D)(point: Point2D): Boolean =
      getHeight(heightMap, point) <= getHeight(heightMap, curr) + 1 && !visited.contains(point)

    val ordering: Ordering[(Point2D, Int)] = Ordering.by(pair => -(pair._2 + pair._1.mannDist(end)))

    hillClimb(mutable.PriorityQueue((start, 0))(ordering), _ == end, addToFrontier)(heightMap)
  }

  def part2(heightMap: HeightMap, end: Point2D): Int = {
    def addToFrontier(visited: Set[Point2D], frontier: Frontier, curr: Point2D)(point: Point2D): Boolean =
      getHeight(heightMap, point) >= getHeight(heightMap, curr) - 1 &&
        !visited.contains(point) &&
        !frontier.exists(_._1 == point)

    val ordering: Ordering[(Point2D, Int)] = Ordering.by(pair => -(pair._2 - getHeight(heightMap, pair._1) / 2))

    hillClimb(mutable.PriorityQueue((end, 0))(ordering), getHeight(heightMap, _) == 1, addToFrontier)(heightMap)
  }

  def hillClimb(startFrontier: Frontier, successCondition: Point2D => Boolean,
                frontierCondition: (Set[Point2D], Frontier, Point2D) => Point2D => Boolean)
               (heightMap: HeightMap): Int = {
    val frontier: Frontier = startFrontier
    var visited: Set[Point2D] = Set()
    val heightMapSizeLimit: Point2D = new Point2D(heightMap.head.length - 1, heightMap.length - 1)

    def neighbours(point: Point2D): Iterable[Point2D] =
      List(Point2D.xVec(1), Point2D.xVec(-1), Point2D.yVec(1), Point2D.yVec(-1))
        .map(_ + point)
        .filter(_.inArea(Point2D.zero, heightMapSizeLimit))

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
