object Day14 extends IDay {
  type Point = (Int, Int)
  type Path = Array[Point]

  override def execute(input: String): (Any, Any) = {
    def readPath(pathStr: String): Path = pathStr
      .split(" -> ")
      .map(Helper.splitPair(_, ","))
      .map(p => (p._1.toInt, p._2.toInt))

    val paths: Iterable[Path] = Helper.readLines(input, readPath)
    (part1(paths), part2(paths))
  }

  def addPoints(a: Point, b: Point): Point = (a._1 + b._1, a._2 + b._2)

  def checkInRange(x: Int, l: Int, r: Int): Boolean = if (l < r) x >= l && x <= r else x >= r && x <= l

  def countSandDrops(paths: Iterable[Path], maxY: Int, bottomBarrier: Boolean): Int = {
    var pathBlocked: Set[Point] = Set()
    val origin: Point = (500, 0)
    var sand: Set[Point] = Set()

    def sandUnitDestination(paths: Iterable[Path], origin: Point): List[Point] = {
      def isPointInPath(point: Point, path: Path): Boolean = {
        path.init.zip(path.tail).exists({ pair: (Point, Point) =>
          if (pair._1._1 != pair._2._1)
            pair._1._2 == point._2 && checkInRange(point._1, pair._1._1, pair._2._1)
          else
            pair._1._1 == point._1 && checkInRange(point._2, pair._1._2, pair._2._2)
        })
      }

      def unblocked(point: Point): Boolean = {
        if (pathBlocked.contains(point) || sand.contains(point)) false
        else if (bottomBarrier && point._2 == maxY) false
        else {
          val isBlocked = paths.exists(path => isPointInPath(point, path))
          if (isBlocked) pathBlocked = pathBlocked + point
          !isBlocked
        }
      }

      var curr: Option[Point] = Some(origin)
      var prev: List[Point] = List()

      while (curr.isDefined && curr.get._2 <= maxY) {
        prev = curr.get :: prev
        curr = LazyList((0, 1), (-1, 1), (1, 1)).map(dir => addPoints(curr.get, dir)).find(unblocked)
      }
      if (curr.isEmpty || bottomBarrier) prev
      else List()
    }

    var continue = true
    var toVisit: List[Point] = List(origin)

    while (continue && toVisit.nonEmpty) {
      val result: List[Point] = sandUnitDestination(paths, toVisit.head)
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
    countSandDrops(paths, paths.map(_.map(_._2).max).max, bottomBarrier = false)

  def part2(paths: Iterable[Path]): Int =
    countSandDrops(paths, paths.map(_.map(_._2).max).max + 2, bottomBarrier = true)
}
