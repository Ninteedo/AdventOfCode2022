object Day18 extends IDay {
  type Point = (Int, Int, Int) // (x,y,z)

  override def execute(input: String): (Any, Any) = {
    val droplet: List[Point] = Helper.readLines(input, readPoint).toList
    (part1(droplet), part2(droplet))
  }

  def readPoint(str: String): Point = {
    val pattern = "(\\d+),(\\d+),(\\d+)".r
    str match {
      case pattern(x, y, z) => (x.toInt, y.toInt, z.toInt)
    }
  }

  val DIRECTIONS: LazyList[Point] = LazyList((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))

  def addPoints(a: Point, b: Point): Point = (a._1 + b._1, a._2 + b._2, a._3 + b._3)

  def neighbourPoints(point: Point): Iterable[Point] =
    DIRECTIONS.map(addPoints(point, _))

  def dropletBounds(droplet: List[Point]): (Point, Point) = (
    (droplet.map(_._1).min, droplet.map(_._2).min, droplet.map(_._3).min),
    (droplet.map(_._1).max, droplet.map(_._2).max, droplet.map(_._3).max)
  )

  def getDirComponent(point: Point, dir: Point): Int = {
    if (dir._1 != 0) point._1
    else if (dir._2 != 0) point._2
    else point._3
  }

  def boundInDirection(dir: Point, bounds: (Point, Point)): Int = {
    val minMaxBounds = if (dir._1 + dir._2 + dir._3 == -1) bounds._1 else bounds._2
    getDirComponent(minMaxBounds, dir)
  }

  def inBounds(dir: Point, curr: Point, bound: Int): Boolean = {
    if (dir._1 + dir._2 + dir._3 == -1) bound < getDirComponent(curr, dir)
    else getDirComponent(curr, dir) < bound
  }

  def dropletFaces(droplet: List[Point]): List[Point] = droplet.flatMap(neighbourPoints).filter(!droplet.contains(_))

  def part1(droplet: List[Point]): Int = dropletFaces(droplet).size

  def part2(droplet: List[Point]): Int = {
    val filteringPoints: List[FilteringPoint] = dropletFaces(droplet).map(new FilteringPoint(_))
    val bounds: (Point, Point) = dropletBounds(droplet)
    filteringPoints.foreach(_.checkLineOfSight(droplet, bounds, filteringPoints))

    var anyChange: Boolean = true
    while (anyChange) {
      anyChange = filteringPoints.exists(_.checkDependentsExterior())
    }

    filteringPoints.count(_.isExterior)
  }

  class FilteringPoint(point: Point) {
    var dependentOn: List[FilteringPoint] = List()
    var isExterior: Boolean = false

    def addDependent(filteringPoint: FilteringPoint): Unit = dependentOn = filteringPoint :: dependentOn

    def getDependents: List[FilteringPoint] = dependentOn

    def isPoint(test: Point): Boolean = point == test

    def checkLineOfSight(droplet: List[Point], dropletBounds: (Point, Point),
                         filteringPoints: List[FilteringPoint]): Boolean = {
      def checkInDirection(dir: Point): Boolean = {
        val bound: Int = boundInDirection(dir, dropletBounds)
        var curr: Point = point
        var added: Boolean = false
        while (inBounds(dir, curr, bound)) {
          if (droplet.contains(curr)) return false
          if (curr != point && !added) {
            val filteringPoint: Option[FilteringPoint] = filteringPoints.find(_.isPoint(curr))
            if (filteringPoint.isDefined) {
              if (filteringPoint.get.isExterior) {
                isExterior = true
                return true
              }
              added = true
              addDependent(filteringPoint.get)
            }
          }
          curr = addPoints(curr, dir)
        }
        true
      }

      val result: Boolean = DIRECTIONS.exists(checkInDirection)
      if (result) isExterior = true
      result
    }

    def checkDependentsExterior(): Boolean = {
      if (isExterior) return false
      if (getDependents.exists(_.isExterior)) {
        isExterior = true
        return true
      }
      false
    }
  }
}
