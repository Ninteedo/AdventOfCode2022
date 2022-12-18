object Day14 extends IDay {
  type Path = Array[Point2D]

  override def execute(input: String): (Any, Any) = {
    def readPath(pathStr: String): Path = pathStr
      .split(" -> ")
      .map(Helper.splitPair(_, ","))
      .map(p => new Point2D(p._1.toInt, p._2.toInt))

    val paths: Iterable[Path] = Helper.readLines(input, readPath)
    (part1(paths), part2(paths))
  }

  def checkInRange(x: Int, l: Int, r: Int): Boolean = if (l < r) x >= l && x <= r else x >= r && x <= l

  def countSandDrops(paths: Iterable[Path], maxY: Int, bottomBarrier: Boolean): Int = {
    var pathBlocked: Set[Point2D] = Set()
    val origin: Point2D = new Point2D(500, 0)
    var sand: Set[Point2D] = Set()

    def sandUnitDestination(paths: Iterable[Path], origin: Point2D): List[Point2D] = {
      def isPointInPath(point: Point2D, path: Path): Boolean = {
        path.init.zip(path.tail).exists({ pair: (Point2D, Point2D) =>
          if (pair._1.x != pair._2.x)
            pair._1.y == point.y && checkInRange(point.x, pair._1.x, pair._2.x)
          else
            pair._1.x == point.x && checkInRange(point.y, pair._1.y, pair._2.y)
        })
      }

      def unblocked(point: Point2D): Boolean = {
        if (pathBlocked.contains(point) || sand.contains(point)) false
        else if (bottomBarrier && point.y == maxY) false
        else {
          val isBlocked = paths.exists(path => isPointInPath(point, path))
          if (isBlocked) pathBlocked = pathBlocked + point
          !isBlocked
        }
      }

      var curr: Option[Point2D] = Some(origin)
      var prev: List[Point2D] = List()

      while (curr.isDefined && curr.get.y <= maxY) {
        prev = curr.get :: prev
        curr = LazyList(new Point2D(0, 1), new Point2D(-1, 1), new Point2D(1, 1)).map(_ + curr.get).find(unblocked)
      }
      if (curr.isEmpty || bottomBarrier) prev
      else List()
    }

    var continue = true
    var toVisit: List[Point2D] = List(origin)

    while (continue && toVisit.nonEmpty) {
      val result: List[Point2D] = sandUnitDestination(paths, toVisit.head)
      if (result.isEmpty) continue = false
      else {
        sand = sand + result.head
        toVisit = result.tail ++ toVisit.tail
        if (result.head == origin) continue = false
      }
    }

    sand.size
  }

  def part1(paths: Iterable[Path]): Int =
    countSandDrops(paths, paths.map(_.map(_.y).max).max, bottomBarrier = false)

  def part2(paths: Iterable[Path]): Int =
    countSandDrops(paths, paths.map(_.map(_.y).max).max + 2, bottomBarrier = true)
}
