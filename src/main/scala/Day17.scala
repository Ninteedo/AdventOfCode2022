import Day17.Jet.Jet

object Day17 extends IDay {
  type Rock = List[Point2D]
  type PrunePoint = (Rock, Int, Int) // (ground formation, rock index, jet index)
  type MemoPoint = (PrunePoint, (Int, Int, Int)) // (prune point, (iterMaxRockHeight, iterRocksDropped, pruneFloor))

  override def execute(input: String): (Any, Any) = {

    val rocksInput = "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##"

    val jets = readJets(input.strip())
    val rocks = readRocks(rocksInput)

    (part1(jets, rocks), part2(jets, rocks))
  }

  def readRocks(rocksInput: String): Array[Rock] = {
    def readRock(str: String): Rock = {
      var result: List[Point2D] = List()
      Helper.readLines(str, _.zipWithIndex.toArray).zipWithIndex
        .foreach({ p: (Array[(Char, Int)], Int) =>
          p._1.foreach({ q: (Char, Int) =>
            q._1 match {
              case '#' => result = new Point2D(q._2, p._2) :: result
              case '.' =>
            }
          })
        })
      result
    }

    def yMirror(rock: Rock): Rock = {
      val yMax = rock.map(_.y).max
      rock.map(point => new Point2D(point.x, yMax - point.y))
    }

    rocksInput.split("(\\r?\\n){2}").map(readRock).map(yMirror)
  }

  object Jet extends Enumeration {
    type Jet = Value
    val Left, Right = Value
  }

  def readJets(str: String): Array[Jet] = str.map({
    case '>' => Jet.Right
    case '<' => Jet.Left
  }).toArray

  def simulateFallingBlocks(jets: Array[Jet], rocks: Array[Rock], rockCount: Long): Long = {
    val CHAMBER_WIDTH: Int = 7
    val SPAWN_FROM_LEFT_DIST: Int = 2
    val SPAWN_FROM_BOTTOM_DIST: Int = 3

    var rockIndex: Int = 0
    var jetIndex: Int = 0

    var iterRocksDropped: Int = 0
    var iterMaxRockHeight: Int = 0
    var overallRocksDropped: Long = 0
    var overallMaxRockHeight: Long = 0

    var pruneFloor: Int = 0
    var rockFormation: Rock = List()

    var startPrunePoint: PrunePoint = (List(), 0, 0)
    var memory: Map[PrunePoint, MemoPoint] = Map()

    def checkCollision(move: Point2D, cond: Point2D => Boolean, yAxis: Boolean, minOnAxis: Boolean)(rock: Rock): Boolean = {
      def oneOnAxis(pair: (Int, Rock)): Point2D = {
        val targets: List[Int] = pair._2.map(p => if (yAxis) p.y else p.x)
        val target: Int = if (minOnAxis) targets.min else targets.max
        pair._2.find(p => (if (yAxis) p.y else p.x) == target).get
      }

      val afterMove: Rock = rock
        .groupBy(p => if (yAxis) p.x else p.y)
        .toList
        .map(oneOnAxis)
        .map(p => new Point2D(p.x + move.x, p.y + move.y))

      afterMove.exists(cond) || afterMove.exists(rockFormation.contains(_))
    }

    def checkLeftCollision: Rock => Boolean =
      checkCollision(new Point2D(-1, 0), _.x < 0, yAxis = false, minOnAxis = true)

    def checkRightCollision: Rock => Boolean =
      checkCollision(new Point2D(1, 0), _.x >= CHAMBER_WIDTH, yAxis = false, minOnAxis = false)

    def checkDownCollision: Rock => Boolean =
      checkCollision(new Point2D(0, -1), _.y < 0, yAxis = true, minOnAxis = true)

    def moveRockJet(rock: Rock, jet: Jet): Rock = jet match {
      case Jet.Left => if (checkLeftCollision(rock)) rock else rock.map(_ + new Point2D(-1, 0))
      case Jet.Right => if (checkRightCollision(rock)) rock else rock.map(_ + new Point2D(1, 0))
    }

    def moveRockDown(rock: Rock): Option[Rock] =
      if (checkDownCollision(rock)) {
        settleRock(rock)
        None
      } else
        Some(rock.map(_ - new Point2D(0, 1)))

    def settleRock(rock: Rock): Unit = {
      iterRocksDropped += 1
      rockFormation = rock ++ rockFormation
      iterMaxRockHeight = Math.max(rock.map(_.y).max + 1, iterMaxRockHeight)
      val pruneRow: Option[Int] = rock.map(_.y).toSet.filter(checkCompleteRow).maxOption
      if (pruneRow.isDefined) pruneBelow(pruneRow.get)
    }

    def pruneBelow(row: Int): Unit = {
      pruneFloor = iterMaxRockHeight - row

      rockFormation = rowToGroundFormation(row)
      val endPrunePoint: PrunePoint = (rockFormation, rockIndex, jetIndex)
      memory += (startPrunePoint -> (endPrunePoint, (iterMaxRockHeight, iterRocksDropped, pruneFloor)))

      checkMemory(endPrunePoint) match {
        case ((memFormation, memRockIndex, memJetIndex), _) =>
          iterMaxRockHeight = pruneFloor
          iterRocksDropped = 0

          rockFormation = memFormation
          rockIndex = memRockIndex
          jetIndex = memJetIndex
          startPrunePoint = (memFormation, memRockIndex, memJetIndex)
      }
    }

    def checkMemory(endPrunePoint: PrunePoint): MemoPoint = {
      var curr: Option[MemoPoint] = memory.get(endPrunePoint)
      var prev: MemoPoint = memory(startPrunePoint)
      var cycleFound: Boolean = false
      while (needMoreRocks(prev._2._2) && curr.isDefined) {
        prev = curr.get
        prev match {
          case (prunePoint: PrunePoint, (memMaxRockHeight, memRocksDropped, memPruneFloor)) =>
            if (prunePoint == startPrunePoint) {
              cycleFound = true
              memoLoopHandler(startPrunePoint)
            } else {
              iterMaxRockHeight += memMaxRockHeight - pruneFloor
              iterRocksDropped += memRocksDropped
              curr = memory.get(prunePoint)
            }
            pruneFloor = memPruneFloor
        }
      }

      if (!cycleFound) {
        overallMaxRockHeight += iterMaxRockHeight - pruneFloor
        overallRocksDropped += iterRocksDropped
      }

      prev
    }

    def memoLoopHandler(start: PrunePoint): Unit = {
      var curr: MemoPoint = memory(start)
      var prev: PrunePoint = null
      var cycleIterSize: (Int, Int) = (0, 0)

      while (prev != start) curr match {
        case (prunePoint, (memMaxRockHeight, memRocksDropped, memPruneFloor)) => cycleIterSize match {
          case (totalHeight, totalRocks) =>
            prev = prunePoint
            cycleIterSize = (totalHeight + memMaxRockHeight - memPruneFloor, totalRocks + memRocksDropped)
            curr = memory(prev)
        }
      }

      cycleIterSize match {
        case (totalHeight, totalRocks) =>
          val iterCount: Long = (rockCount - overallRocksDropped) / totalRocks
          overallMaxRockHeight += iterCount * totalHeight
          overallRocksDropped += iterCount * totalRocks
      }
    }

    def rowToGroundFormation(row: Int): Rock =
      rockFormation.filter(_.y >= row).map(point => new Point2D(point.x, point.y - row))

    def checkCompleteRow(row: Int): Boolean =
      rockFormation.filter(point => point.y == row || point.y == row - 1).map(_.x).toSet.size == CHAMBER_WIDTH

    def spawnRock(rockType: Rock): Rock =
      rockType.map(_ + new Point2D(SPAWN_FROM_LEFT_DIST, SPAWN_FROM_BOTTOM_DIST + iterMaxRockHeight))

    def nextRock(): Rock = {
      val result: Rock = rocks(rockIndex)
      rockIndex = (rockIndex + 1) % rocks.length
      result
    }

    def nextJet(): Jet = {
      val result: Jet = jets(jetIndex)
      jetIndex = (jetIndex + 1) % jets.length
      result
    }

    def needMoreRocks(extra: Int): Boolean = (overallRocksDropped + iterRocksDropped + extra) < rockCount

    while (needMoreRocks(0)) {
      var rock: Option[Rock] = Some(spawnRock(nextRock()))
      while (rock.isDefined) {
        rock = moveRockDown(moveRockJet(rock.get, nextJet()))
      }
    }

    overallMaxRockHeight + iterMaxRockHeight
  }

  def part1(jets: Array[Jet], rocks: Array[Rock]): Long = simulateFallingBlocks(jets, rocks, 2022)

  def part2(jets: Array[Jet], rocks: Array[Rock]): Long = simulateFallingBlocks(jets, rocks, 1000000000000L)
}
