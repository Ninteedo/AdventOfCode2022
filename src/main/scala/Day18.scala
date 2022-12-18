object Day18 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val droplet: List[Point3D] = Helper.readLines(input, readPoint).toList
    (part1(droplet), part2(droplet))
  }

  def readPoint(str: String): Point3D = {
    val pattern = "(\\d+),(\\d+),(\\d+)".r
    str match {
      case pattern(x, y, z) => new Point3D(x.toInt, y.toInt, z.toInt)
    }
  }

  def neighbourPoints(point: Point3D): Iterable[Point3D] = Point3D.directions.map(_ + point)

  def dropletBounds(droplet: List[Point3D]): (Point3D, Point3D) = (
    new Point3D(droplet.map(_.x).min, droplet.map(_.y).min, droplet.map(_.z).min),
    new Point3D(droplet.map(_.x).max, droplet.map(_.y).max, droplet.map(_.z).max)
  )

  def inBounds(curr: Point3D, bounds: (Point3D, Point3D)): Boolean = curr.inVolume(bounds._1, bounds._2)

  def dropletFaces(droplet: List[Point3D]): List[Point3D] = droplet.flatMap(neighbourPoints).filter(!droplet.contains(_))

  def part1(droplet: List[Point3D]): Int = dropletFaces(droplet).size

  def part2(droplet: List[Point3D]): Int = {
    val filteringPoints: List[FilteringPoint] = dropletFaces(droplet).map(new FilteringPoint(_))
    val bounds: (Point3D, Point3D) = dropletBounds(droplet)
    filteringPoints.foreach(_.checkLineOfSight(droplet, bounds, filteringPoints))

    var anyChange: Boolean = true
    while (anyChange) anyChange = filteringPoints.exists(_.checkDependentsExterior())

    filteringPoints.count(_.isExterior)
  }

  class FilteringPoint(point: Point3D) {
    var dependentOn: List[FilteringPoint] = List()
    var isExterior: Boolean = false

    def addDependent(filteringPoint: FilteringPoint): Unit = dependentOn = filteringPoint :: dependentOn

    def getDependents: List[FilteringPoint] = dependentOn

    def isPoint(test: Point3D): Boolean = point == test

    def checkLineOfSight(droplet: List[Point3D], dropletBounds: (Point3D, Point3D),
                         filteringPoints: List[FilteringPoint]): Boolean = {
      def checkInDirection(dir: Point3D): Boolean = {
        var curr: Point3D = point
        var added: Boolean = false
        while (inBounds(curr, dropletBounds)) {
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
          curr += dir
        }
        true
      }

      val result: Boolean = Point3D.directions.exists(checkInDirection)
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
