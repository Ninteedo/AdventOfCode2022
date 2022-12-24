object Day24 extends IDay {
  type Blizzards = List[(Point2D, Point2D)]

  override def execute(input: String): (Any, Any) = {
    // val input = "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"
    val startState: MazeState = MazeState.read(input)
    (part1(startState), part2(startState))
  }

  def part1(startState: MazeState): Int = {
    startState.bestFirstSearch().get.getResult
  }

  def part2(startState: MazeState): Int = {
    val firstTripEnd = startState.bestFirstSearch().get
    val returnTripStart = new MazeState(originalGoal, originalStart, firstTripEnd.getResult, null)
    val returnTripEnd = returnTripStart.bestFirstSearch().get
    val secondTripStart = new MazeState(originalStart, originalGoal, returnTripEnd.getResult, null)
    val secondTripEnd = secondTripStart.bestFirstSearch().get
    secondTripEnd.getResult
  }

  var areaSize: Point2D = _
  var originalStart: Point2D = _
  var originalGoal: Point2D = _
  var blizzardsCache: Map[Int, Blizzards] = Map()

  def blizzardsAtTime(time: Int): Blizzards = {
    if (!blizzardsCache.contains(time)) blizzardsCache += time -> calculateNextBlizzards(blizzardsCache(time - 1))
    blizzardsCache(time)
  }

  def calculateNextBlizzards(blizzards: Blizzards): Blizzards = {
    var result: Blizzards = Nil

    def wrapAround(current: Point2D, direction: Point2D): Point2D = {
      val next: Point2D = current + direction
      if (next.inArea(new Point2D(1, 1), areaSize)) next
      else if (next.x == 0) new Point2D(areaSize.x, next.y)
      else if (next.y == 0) new Point2D(next.x, areaSize.y)
      else if (next.x == areaSize.x + 1) new Point2D(1, next.y)
      else if (next.y == areaSize.y + 1) new Point2D(next.x, 1)
      else sys.error(s"blizzard in unexpected location: size=$areaSize, next=$next")
    }

    /*
    for (blizzardPair <- blizzards) {
      val location: Point2D = blizzardPair._1
      for (blizzard <- blizzardPair._2) {
        val nextLocation: Point2D = wrapAround(location, blizzard)
        result += nextLocation -> (blizzard :: result.getOrElse(nextLocation, Nil))
      }
    }
    */

    for ((location, direction) <- blizzards) {
      val nextLocation = wrapAround(location, direction)
      result = (nextLocation, direction) :: result
    }

    if (result.length != blizzards.size) sys.error(s"blizzards count changed from ${blizzards.size} to ${result.length}")

    result
  }

  class MazeState(val location: Point2D, val goal: Point2D, val timePassed: Int, val parent: MazeState)
    extends SearchNode[MazeState] {
    override def calculateOrderingValue(): Int = -(location.mannDist(goal) + timePassed)

    override def descendents(): Iterable[MazeState] = {
      val nextBlizzards = blizzardsAtTime(timePassed + 1)
      val dirs = List(new Point2D(0, 0), new Point2D(-1, 0), new Point2D(1, 0), new Point2D(0, -1), new Point2D(0, 1))

      def isClear(loc: Point2D): Boolean = {
        (loc.inArea(new Point2D(1, 1), areaSize) || loc == originalStart || loc == originalGoal) &&
          nextBlizzards.forall(_._1 != loc)
      }

      dirs.map(location + _).filter(isClear).map(new MazeState(_, goal, timePassed + 1, this))
    }

    override def atGoal: Boolean = location == goal

    override def getParent: MazeState = parent

    override def getResult: Int = timePassed

    override def toString: String = s"MazeState($location, $timePassed, ordering=$orderingValue)"

    override def isDuplicateOf(other: SearchNode[MazeState]): Boolean =
      location == other.asInstanceOf[MazeState].location && timePassed >= other.asInstanceOf[MazeState].timePassed

    override def filterDuplicates: Boolean = true
  }

  object MazeState {
    def read(input: String): MazeState = {
      val lines: Iterable[Array[Char]] = Helper.readLines(input, _.toCharArray)
      areaSize = new Point2D(lines.head.length - 2, lines.size - 2)
      originalStart = new Point2D(1, 0)
      originalGoal = areaSize + new Point2D(0, 1)
      val indexedInput: Iterable[Iterable[(Char, Point2D)]] = Helper.indexByPoint(lines.map(_.toList))

      var blizzards: Blizzards = Nil
      for (line <- indexedInput) {
        for (pair <- line) {
          val direction: Option[Point2D] = pair._1 match {
            case '^' => Some(new Point2D(0, -1))
            case 'v' => Some(new Point2D(0, 1))
            case '<' => Some(new Point2D(-1, 0))
            case '>' => Some(new Point2D(1, 0))
            case _ => None
          }
          if (direction.isDefined) blizzards = (pair._2, direction.get) :: blizzards // blizzards += pair._2 -> List(direction.get)
        }
      }

      blizzardsCache += 0 -> blizzards

      new MazeState(originalStart, originalGoal, 0, null)
    }
  }
}
