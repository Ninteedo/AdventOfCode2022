import Day23.Direction.Direction

object Day23 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val initialElves: ElfMap = ElfMap.read(input)
    (part1(initialElves), part2(initialElves))
  }

  def part1(initialElves: ElfMap): Int = {
    var curr: ElfMap = initialElves
    while (curr.time < 10) curr = curr.next
    curr.countSpace
  }

  def part2(initialElves: ElfMap): Int = {
    var curr: ElfMap = initialElves
    var prev: ElfMap = null
    while (curr.time == 0 || curr.elvesByRow != prev.elvesByRow) {
      prev = curr
      curr = curr.next
    }
    curr.time
  }

  def calculateSurroundingRectangle(elves: Iterable[Point2D]): (Point2D, Point2D) = {
    (new Point2D(elves.map(_.x).min, elves.map(_.y).min),
      new Point2D(elves.map(_.x).max, elves.map(_.y).max))
  }

  class ElfMap(val elvesByRow: RowMapSet, val time: Int) {
    val surroundingRectangle: (Point2D, Point2D) = {
      calculateSurroundingRectangle(elvesByRow.toIterable)
    }

    val directionOrder: Iterable[Direction] = Direction.order(time)

    def checkOccupied(point: Point2D): Boolean = {
      point.inArea(surroundingRectangle._1, surroundingRectangle._2) && elvesByRow.contains(point)
    }

    def next: ElfMap = {
      def notOccupiedInDirection(elf: Point2D)(direction: Direction): Boolean = direction match {
        case Direction.North => (elf.x - 1 to elf.x + 1).forall(col => !checkOccupied(new Point2D(col, elf.y - 1)))
        case Direction.South => (elf.x - 1 to elf.x + 1).forall(col => !checkOccupied(new Point2D(col, elf.y + 1)))
        case Direction.West => (elf.y - 1 to elf.y + 1).forall(row => !checkOccupied(new Point2D(elf.x - 1, row)))
        case Direction.East => (elf.y - 1 to elf.y + 1).forall(row => !checkOccupied(new Point2D(elf.x + 1, row)))
      }

      def firstOccupiedDirection(elf: Point2D): Option[Direction] = {
        val unoccupiedDirections = directionOrder.filter(notOccupiedInDirection(elf))
        if (unoccupiedDirections.size == 4) None
        else unoccupiedDirections.headOption
      }

      def newPosition(elf: Point2D): Point2D = firstOccupiedDirection(elf) match {
        case Some(direction) => elf + Direction.component(direction)
        case None => elf
      }

      var occupiedByRow: Map[Int, Map[Int, Point2D]] = Map()
      var newBlockedElvesByRow: RowMapSet = new RowMapSet(Map())

      def attemptMove(elf: Point2D): Unit = {
        val position: Point2D = newPosition(elf)

        if (occupiedByRow.contains(position.y) && occupiedByRow(position.y).contains(position.x)) {
          newBlockedElvesByRow += elf
          newBlockedElvesByRow += occupiedByRow(position.y)(position.x)
        } else {
          if (!occupiedByRow.contains(position.y)) occupiedByRow += position.y -> Map()
          occupiedByRow += position.y -> (occupiedByRow(position.y) + (position.x -> elf))
        }
      }

      elvesByRow.toIterable.foreach(attemptMove)

      def isElfClear(elf: Point2D): Boolean = !newBlockedElvesByRow.contains(elf)

      val unblockedElvesByRow: RowMapSet = {
        var result: Map[Int, Set[Int]] = Map()
        occupiedByRow.keys.foreach({ row =>
          result += row -> Set.from(occupiedByRow(row).filter(pair => isElfClear(pair._2)).keySet)
        })
        newBlockedElvesByRow.contents.keys.foreach({ row =>
          result += row -> Set.from(result.getOrElse(row, Set()) ++ newBlockedElvesByRow.contents(row))
        })
        new RowMapSet(result)
      }

      if (unblockedElvesByRow.size != elvesByRow.size)
        sys.error(s"elf count changed from ${elvesByRow.size} to ${unblockedElvesByRow.size}")

      new ElfMap(unblockedElvesByRow, time + 1)
    }

    def countSpace: Int = {
      val rectSize: Point2D = surroundingRectangle._2 - surroundingRectangle._1 + new Point2D(1, 1)
      rectSize.x * rectSize.y - elvesByRow.size
    }

    override def toString: String = "time=" + time + ", map=[\n" + printElfMap + "\n]"

    def printElfMap: String = {
      (surroundingRectangle._1.y to surroundingRectangle._2.y).map(y =>
        (surroundingRectangle._1.x to surroundingRectangle._2.x).map(x =>
          if (elvesByRow.contains(new Point2D(x, y))) '#' else '.'
        ).mkString("")
      ).mkString("\n")
    }
  }

  object ElfMap {
    def read(input: String): ElfMap = {
      val indexedLines: Iterable[Iterable[(Char, Point2D)]] = Helper.indexByPoint(Helper.readLines(input, identity).map(_.toCharArray))
      val elves: Set[Point2D] = indexedLines.flatMap(line => line.filter(_._1 == '#').map(_._2)).toSet

      val surroundingRectangle: (Point2D, Point2D) = calculateSurroundingRectangle(elves)
      var rowResult: Map[Int, Set[Int]] = Map()
      for (row <- surroundingRectangle._1.y to surroundingRectangle._2.y) {
        rowResult += row -> Set.from(elves.filter(_.y == row).map(_.x))
      }

      new ElfMap(new RowMapSet(rowResult), 0)
    }
  }

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, West, East = Value

    def order(offset: Int): Iterable[Direction] = {
      val originalOrder = List(North, South, West, East)
      originalOrder.drop(offset % 4) ++ originalOrder.take(offset % 4)
    }

    def component(direction: Direction): Point2D = direction match {
      case North => new Point2D(0, -1)
      case South => new Point2D(0, 1)
      case West => new Point2D(-1, 0)
      case East => new Point2D(1, 0)
    }
  }

  class RowMapSet(val contents: Map[Int, Set[Int]]) {
    def +(point: Point2D): RowMapSet = {
      new RowMapSet(contents + (point.y -> (contents.getOrElse(point.y, Set.empty[Int]) + point.x)))
    }

    def -(point: Point2D): RowMapSet = {
      new RowMapSet(contents + (point.y -> (contents.getOrElse(point.y, Set.empty[Int]) - point.x)))
    }

    def contains(point: Point2D): Boolean = {
      contents.contains(point.y) && contents(point.y).contains(point.x)
    }

    var iterableCache: Option[Iterable[Point2D]] = None

    def toIterable: Iterable[Point2D] = {
      if (iterableCache.isEmpty)
        iterableCache = Some(contents.flatMap(pair => pair._2.toList.map(col => new Point2D(col, pair._1))))
      iterableCache.get
    }

    var sizeCache: Option[Int] = None

    def size: Int = {
      if (sizeCache.isEmpty)
        sizeCache = Some(toIterable.size)
      sizeCache.get
    }

    override def equals(obj: Any): Boolean = {
      if (obj == null || !obj.isInstanceOf[RowMapSet]) false
      else {
        val other: RowMapSet = obj.asInstanceOf[RowMapSet]
        toIterable.forall(other.contains)
      }
    }
  }
}
