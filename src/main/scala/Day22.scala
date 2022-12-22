import Day22.Direction.Direction
import Day22.Rotation.DirectionChange

object Day22 extends IDay {
  type Instruction = Either[Int, DirectionChange]
  type WrapAround = (Position, Position)

  override def execute(input: String): (Int, Int) = {
    // val input = "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5"
    val instructions: Iterable[Instruction] = readInstructions(input)
    (part1(Board.read(input, cubeWrap = false), instructions), part2(Board.read(input, cubeWrap = true), instructions))
  }

  def part1(board: Board, instructions: Iterable[Instruction]): Int = {
    board.boardWalk(new Position(board.start, Direction.Right), instructions).value
  }

  def part2(board: Board, instructions: Iterable[Instruction]): Int = {
    board.boardWalk(new Position(board.start, Direction.Right), instructions).value
  }

  def negMod(a: Long, divisor: Long): Int = {
    val x = a % divisor
    (if (x >= 0) x else x + divisor).toInt
  }

  def readInstructions(input: String): Iterable[Instruction] = {
    val pattern = "(?m)^([LR\\d]+)$".r
    val line: String = pattern.findFirstMatchIn(input) match {
      case Some(m) => m.group(1)
    }

    var result: List[Instruction] = List()
    var currentDigit: String = ""

    def checkCurrentDigit(): Unit = if (currentDigit != "") {
      result = Left(currentDigit.toInt) :: result
      currentDigit = ""
    }

    for (char <- line) char match {
      case 'L' =>
        checkCurrentDigit()
        result = Right(Rotation.AntiClockwise) :: result
      case 'R' =>
        checkCurrentDigit()
        result = Right(Rotation.Clockwise) :: result
      case digit => currentDigit = currentDigit + digit
    }
    checkCurrentDigit()
    result.reverse
  }

  class Board(val walls: Set[Point2D], val wrapAroundArray: Array[WrapAround], val start: Point2D) {
    def isWall(point: Point2D): Boolean = walls.contains(point)

    def getWrapAround(position: Position): Position = {
      wrapAroundArray.find({
        wrapAround => wrapAround._1.point == position.point && wrapAround._1.facing == position.facing
      }) match {
        case Some(wrapAround) => wrapAround._2
        case None => position
      }
    }

    def nextPosition(position: Position, instruction: Instruction): Position = instruction match {
      case Left(distance: Int) =>
        if (distance == 0) position
        else {
          val newPosition: Position = getWrapAround(position.nextInDirection())
          if (isWall(newPosition.point)) position
          else nextPosition(newPosition, Left(distance - 1))
        }
      case Right(change: DirectionChange) => new Position(position.point, Direction.addChange(position.facing, change))
    }

    def boardWalk(startPosition: Position, instructions: Iterable[Instruction]): Position = {
      var position: Position = startPosition
      for (instruction <- instructions) {
        position = nextPosition(position, instruction)
      }
      position
    }
  }

  object Board {
    def read(input: String, cubeWrap: Boolean): Board = {
      val pattern = "(?m)^( *[.#]+)$".r
      val lines: Array[String] = pattern.findAllMatchIn(input).map({ m =>
        m.group(1)
      }).toArray

      val indexedLines: Array[Array[(Char, Point2D)]] = lines
        .map(_.zipWithIndex)
        .zipWithIndex
        .map(row => row._1.map(col => (col._1, new Point2D(col._2, row._2))).toArray)


      var start: Option[Point2D] = None
      var walls: Set[Point2D] = Set()
      var boardMaxSize: Point2D = new Point2D(0, 0)

      // find overall board size and all walls
      for (row <- indexedLines) {
        for (element <- row) {
          if (element._2.x > boardMaxSize.x) boardMaxSize = new Point2D(element._2.x, boardMaxSize.y)
          if (element._2.y > boardMaxSize.y) boardMaxSize = new Point2D(boardMaxSize.x, element._2.y)
          if (element._1 == '#') walls += element._2
          else if (start.isEmpty && element._1 == '.') start = Some(element._2)
        }
      }

      val wrapAroundArray: Array[WrapAround] = if (!cubeWrap) readWrapAroundArrayPart1(indexedLines, boardMaxSize)
      else readWrapAroundArrayPart2(indexedLines, boardMaxSize)

      new Board(walls, wrapAroundArray, start.get)
    }

    def readWrapAroundArrayPart1(indexedLines: Array[Array[(Char, Point2D)]], boardMaxSize: Point2D): Array[WrapAround] = {
      var wrapAroundPoints: Set[Point2D] = Set()

      def pointExists(point: Point2D): Boolean = {
        if (point.y < 0 || point.y >= indexedLines.length) false
        else {
          val row: Array[(Char, Point2D)] = indexedLines(point.y)
          point.x >= 0 && point.x < row.length && row(point.x)._1 != ' '
        }
      }

      // find wrap around points
      for (rowNum <- -1 to boardMaxSize.y + 1) {
        for (colNum <- -1 to boardMaxSize.x + 1) {
          val point: Point2D = new Point2D(colNum, rowNum)
          if (!pointExists(point) && Point2D.directions.exists(dir => pointExists(dir + point))) wrapAroundPoints += point
        }
      }

      // for each wrap around point, find wrap destination
      var wrapAroundList: List[WrapAround] = List()
      for (wrapAroundPoint <- wrapAroundPoints) {
        for (direction <- Direction.values) {
          val dirComponents: Point2D = Direction.components(direction)
          val neighbour: Point2D = wrapAroundPoint + dirComponents
          if (pointExists(neighbour)) {
            val other: Option[Point2D] = wrapAroundPoints.find({ other =>
              val difference: Point2D = other - wrapAroundPoint
              (difference.x == 0 || difference.y == 0) && difference.dotProduct(dirComponents) >= 1
            })
            other match {
              case Some(point) =>
                val start: Position = new Position(wrapAroundPoint, Direction.opposite(direction))
                val end: Position = new Position(point - dirComponents, Direction.opposite(direction))
                wrapAroundList = (start, end) :: wrapAroundList
              case None => sys.error("could not find matching wrap around point")
            }
          }
        }
      }

      wrapAroundList.toArray
    }

    def readWrapAroundArrayPart2(indexedLines: Array[Array[(Char, Point2D)]], boardMaxSize: Point2D): Array[WrapAround] = {
      val faceSize: Int =
        if ((boardMaxSize.x + 1) / 4 == (boardMaxSize.y + 1) / 3) (boardMaxSize.x + 1) / 4
        else if ((boardMaxSize.x + 1) / 3 == (boardMaxSize.y + 1) / 4) (boardMaxSize.x + 1) / 3
        else ???

      def getRegions: Map[Point2D, Int] = {
        var result: Map[Point2D, Int] = Map()
        var i: Int = 0
        for (x <- 0 until 4) {
          for (y <- 0 until 4) {
            if (indexedLines.exists(_.exists(_._2 == (x * faceSize, y * faceSize)))) {
              result += new Point2D(x, y) -> i
              i += 1
            }
          }
        }
        result
      }

      val regionMap: Map[Point2D, Int] = getRegions

      def regionOf(point: Point2D): Int = regionMap(point / faceSize)

      def edgeBetween(inner: Point2D, other: Point2D): (Point2D, Point2D) = {
        val dir = other - inner
        if (dir == new Point2D(1, 0)) {
          (new Point2D(faceSize * other.x - 1, faceSize * inner.y), new Point2D(faceSize * other.x - 1, faceSize * (inner.y + 1) - 1))
        } else if (dir == new Point2D(-1, 0)) {
          (new Point2D(faceSize * inner.x, faceSize * inner.y), new Point2D(faceSize * inner.x, faceSize * (inner.y + 1) - 1))
        } else if (dir == new Point2D(0, 1)) {
          (new Point2D(faceSize * inner.x, faceSize * other.y - 1), new Point2D(faceSize * (inner.x + 1) - 1, faceSize * other.y - 1))
        } else if (dir == new Point2D(0, -1)) {
          (new Point2D(faceSize * inner.x, faceSize * inner.y), new Point2D(faceSize * (inner.x + 1) - 1, faceSize * inner.y))
        } else ???
      }

      def regionConnection(regionPointA: Point2D, regionPointB: Point2D) = {
        val difference: Point2D = regionPointB - regionPointA
        if (difference.x.abs == 1 && difference.y.abs == 1) {
          val possibilityA: Point2D = new Point2D(regionPointA.x, regionPointA.y + difference.y)
          val possibilityB: Point2D = new Point2D(regionPointA.x + difference.x, regionPointA.y)
          val between = if (!regionMap.contains(possibilityA)) possibilityA else if (!regionMap.contains(possibilityB)) possibilityB else ???
          val edgeBetweenToA = edgeBetween(between, regionPointA)
          val edgeAToBetween = edgeBetween(regionPointA, between)
          val edgeBetweenToB = edgeBetween(between, regionPointB)
          val edgeBToBetween = edgeBetween(regionPointB, between)
        }
      }

      var wrapAroundList: List[WrapAround] = List()

      def addWrapAround(start: Position, end: Position): Unit = {
        def offByOne(position: Position): Position = {
          new Position(position.point - new Point2D(1, 1), position.facing)
        }
        wrapAroundList = (offByOne(start), offByOne(end)) :: wrapAroundList
      }




      val edges: List[((Point2D, Point2D, Direction), (Point2D, Point2D, Direction))] = List(
        ((new Point2D(1, 100), new Point2D(50, 100), Direction.Up), (new Point2D(51, 51), new Point2D(51, 100), Direction.Right)), // 1
        ((new Point2D(50, 51), new Point2D(50, 100), Direction.Left), (new Point2D(1, 101), new Point2D(50, 101), Direction.Down)), // 1
        ((new Point2D(101, 100), new Point2D(101, 51), Direction.Right), (new Point2D(150, 50), new Point2D(101, 50), Direction.Up)), // 2
        ((new Point2D(150, 51), new Point2D(101, 51), Direction.Down), (new Point2D(100, 100), new Point2D(100, 51), Direction.Left)), // 2
        ((new Point2D(51, 151), new Point2D(51, 200), Direction.Right), (new Point2D(51, 150), new Point2D(100, 150), Direction.Up)), // 3
        ((new Point2D(51, 151), new Point2D(100, 151), Direction.Down), (new Point2D(50, 151), new Point2D(50, 200), Direction.Left)), // 3
        ((new Point2D(101, 101), new Point2D(101, 150), Direction.Right), (new Point2D(150, 50), new Point2D(150, 1), Direction.Left)), // 4
        ((new Point2D(151, 50), new Point2D(151, 1), Direction.Right), (new Point2D(100, 101), new Point2D(100, 150), Direction.Left)), // 4
        ((new Point2D(0, 101), new Point2D(0, 150), Direction.Left), (new Point2D(51, 50), new Point2D(51, 1), Direction.Right)), // 5
        ((new Point2D(50, 50), new Point2D(50, 1), Direction.Left), (new Point2D(1, 101), new Point2D(1, 150), Direction.Right)), // 5
        ((new Point2D(0, 151), new Point2D(0, 200), Direction.Left), (new Point2D(51, 1), new Point2D(100, 1), Direction.Down)), // 6
        ((new Point2D(51, 0), new Point2D(100, 0), Direction.Up), (new Point2D(1, 151), new Point2D(1, 200), Direction.Right)), // 6
        ((new Point2D(50, 201), new Point2D(1, 201), Direction.Down), (new Point2D(150, 1), new Point2D(101, 1), Direction.Down)), // 7
        ((new Point2D(150, 0), new Point2D(101, 0), Direction.Up), (new Point2D(50, 200), new Point2D(1, 200), Direction.Up)) // 7
      )

      edges.foreach(pair => processEdge(pair._1, pair._2))

      def reverseEdge(from: (Point2D, Point2D, Direction), to: (Point2D, Point2D, Direction)): ((Point2D, Point2D, Direction), (Point2D, Point2D, Direction)) = {
        ???
      }

      def processEdge(from: (Point2D, Point2D, Direction), to: (Point2D, Point2D, Direction)): Unit = {
        val fromDir: Point2D = new Point2D(from._2.x.compareTo(from._1.x), from._2.y.compareTo(from._1.y))
        val toDir: Point2D = new Point2D(to._2.x.compareTo(to._1.x), to._2.y.compareTo(to._1.y))

        if (from._1.x != from._2.x && from._1.y != from._2.y)
          sys.error("broken from")
        if (to._1.x != to._2.x && to._1.y != to._2.y)
          sys.error("broken to")

        for (i <- 0 until faceSize) {
          val currFrom: Point2D = from._1 + fromDir * i
          val currTo: Point2D = to._1 + toDir * i

          if (i == faceSize - 1 && currFrom != from._2)
            sys.error("did not reach from end")
          if (i == faceSize - 1 && currTo != to._2)
            sys.error("did not reach to end")

          addWrapAround(new Position(currFrom, from._3), new Position(currTo, to._3))
        }
      }
      wrapAroundList.toArray
    }
  }

  class Position(val point: Point2D, val facing: Direction) {
    def nextInDirection(): Position = new Position(point + Direction.components(facing), facing)

    def value: Int = (point.y + 1) * 1000 + (point.x + 1) * 4 + facing.id

    override def toString: String = s"Position($point, $facing)"
  }

  object Rotation extends Enumeration {
    type DirectionChange = Value
    val AntiClockwise, Clockwise = Value

    def value(directionChange: DirectionChange): Int = directionChange match {
      case AntiClockwise => -1
      case Clockwise => 1
    }
  }

  object Direction extends Enumeration {
    type Direction = Value
    val Right, Down, Left, Up = Value

    def addChange(direction: Direction, directionChange: DirectionChange): Direction = {
      negMod(direction.id + Rotation.value(directionChange), values.size) match {
        case 0 => Right
        case 1 => Down
        case 2 => Left
        case 3 => Up
      }
    }

    def components(direction: Direction): Point2D = direction match {
      case Right => new Point2D(1, 0)
      case Down => new Point2D(0, 1)
      case Left => new Point2D(-1, 0)
      case Up => new Point2D(0, -1)
    }

    def opposite(direction: Direction): Direction = direction match {
      case Right => Left
      case Down => Up
      case Left => Right
      case Up => Down
    }
  }
}
