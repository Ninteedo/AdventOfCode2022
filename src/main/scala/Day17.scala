import Day17.Jet.Jet

object Day17 extends IDay {
  type Rock = Array[Point]
  type Point = (Int, Int) // (y, x)

  val CHAMBER_WIDTH: Int = 7
  val SPAWN_FROM_LEFT_DIST: Int = 2
  val SPAWN_FROM_BOTTOM_DIST: Int = 3

  override def execute(input: String): (Any, Any) = {
    val rocksInput = "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##"

    val jets = readJets(input.strip())
    val rocks = readRocks(rocksInput)

    (part1(jets, rocks), part2(jets, rocks))
  }

  def readRocks(rocksInput: String): Array[Rock] = {
    def readRock(str: String): Rock = {
      var result: List[Point] = List()
      Helper.readLines(str, _.zipWithIndex.toArray).zipWithIndex
        .foreach({ p: (Array[(Char, Int)], Int) =>
          p._1.foreach({ q: (Char, Int) =>
            q._1 match {
              case '#' => result = (p._2, q._2) :: result
              case '.' =>
            }
          })
        })
      result.toArray
    }

    def yMirror(rock: Rock): Rock = {
      val yMax = rock.map(_._1).max
      rock.map(point => (yMax - point._1, point._2))
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

  def part1(jets: Array[Jet], rocks: Array[Rock]): Int = {
    val rockCount: Int = 2022

    var rockIndex: Int = 0
    var jetIndex: Int = 0

    var maxRockHeight: Int = 0
    var pruneFloor: Int = 0
    var rockFormation: List[Point] = List()

    def checkCollision(move: Point, cond: Point => Boolean, yAxis: Boolean, minOnAxis: Boolean)(rock: Rock): Boolean = {
      def oneOnAxis(pair: (Int, Rock)): Point = {
        val targets: Array[Int] = pair._2.map(p => if (yAxis) p._1 else p._2)
        val target: Int = if (minOnAxis) targets.min else targets.max
        pair._2.find(p => (if (yAxis) p._1 else p._2) == target).get
      }

      val afterMove: Rock = rock
        .groupBy(p => if (yAxis) p._2 else p._1)
        .toArray
        .map(oneOnAxis)
        .map(p => (p._1 + move._1, p._2 + move._2))

      afterMove.exists(cond) || afterMove.exists(rockFormation.contains(_))
    }

    def checkLeftCollision: Rock => Boolean =
      checkCollision((0, -1), _._2 < 0, yAxis = false, minOnAxis = true)

    def checkRightCollision: Rock => Boolean =
      checkCollision((0, 1), _._2 >= CHAMBER_WIDTH, yAxis = false, minOnAxis = false)

    def checkDownCollision: Rock => Boolean =
      checkCollision((-1, 0), _._1 < pruneFloor, yAxis = true, minOnAxis = true)

    def moveRockJet(rock: Rock, jet: Jet): Rock = jet match {
      case Jet.Left => if (checkLeftCollision(rock)) rock else rock.map(point => (point._1, point._2 - 1))
      case Jet.Right => if (checkRightCollision(rock)) rock else rock.map(point => (point._1, point._2 + 1))
    }

    def moveRockDown(rock: Rock): Option[Rock] =
      if (checkDownCollision(rock)) {
        settleRock(rock)
        None
      } else
        Some(rock.map(point => (point._1 - 1, point._2)))

    def settleRock(rock: Rock): Unit = {
      rockFormation = rock.toList ++ rockFormation
      rock.map(_._1).toSet.filter(checkCompleteRow).foreach(pruneBelow)
      maxRockHeight = Math.max(rock.map(_._1).max + 1, maxRockHeight)
    }

    def pruneBelow(row: Int): Unit = {
      rockFormation = rockFormation.filter(_._1 >= row - 1)
      pruneFloor = row - 1
    }

    def checkCompleteRow(row: Int): Boolean =
      rockFormation.filter(point => point._1 == row || point._1 == row - 1).map(_._2).toSet.size == CHAMBER_WIDTH

    def spawnRock(rockType: Rock): Rock =
      rockType.map(point => (point._1 + SPAWN_FROM_BOTTOM_DIST + maxRockHeight, point._2 + SPAWN_FROM_LEFT_DIST))

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

    (1 to rockCount).foreach({ n =>
      var rock: Option[Rock] = Some(spawnRock(nextRock()))
      while (rock.isDefined) rock = moveRockDown(moveRockJet(rock.get, nextJet()))
    })

    println("")
    maxRockHeight
  }

  type GroundFormation = List[Point]
  type PrunePoint = (GroundFormation, Int, Int) // (ground formation, rock index, jet index)
  type MemoPoint = (PrunePoint, (Int, Int, Int)) // (prune point, (iterMaxRockHeight, iterRocksDropped, pruneFloor))

  def part2(jets: Array[Jet], rocks: Array[Rock]): Long = {
    val rockCount: Long = 1000000000000L

    var rockIndex: Int = 0
    var jetIndex: Int = 0

    var iterRocksDropped: Int = 0
    var iterMaxRockHeight: Int = 0
    var overallRocksDropped: Long = 0
    var overallMaxRockHeight: Long = 0

    var pruneFloor: Int = 0
    var rockFormation: List[Point] = List()

    var startPrunePoint: PrunePoint = (List(), 0, 0)
    var memory: Map[PrunePoint, MemoPoint] = Map()

    def checkCollision(move: Point, cond: Point => Boolean, yAxis: Boolean, minOnAxis: Boolean)(rock: Rock): Boolean = {
      def oneOnAxis(pair: (Int, Rock)): Point = {
        val targets: Array[Int] = pair._2.map(p => if (yAxis) p._1 else p._2)
        val target: Int = if (minOnAxis) targets.min else targets.max
        pair._2.find(p => (if (yAxis) p._1 else p._2) == target).get
      }

      val afterMove: Rock = rock
        .groupBy(p => if (yAxis) p._2 else p._1)
        .toArray
        .map(oneOnAxis)
        .map(p => (p._1 + move._1, p._2 + move._2))

      afterMove.exists(cond) || afterMove.exists(rockFormation.contains(_))
    }

    def checkLeftCollision: Rock => Boolean =
      checkCollision((0, -1), _._2 < 0, yAxis = false, minOnAxis = true)

    def checkRightCollision: Rock => Boolean =
      checkCollision((0, 1), _._2 >= CHAMBER_WIDTH, yAxis = false, minOnAxis = false)

    def checkDownCollision: Rock => Boolean =
      checkCollision((-1, 0), _._1 < 0, yAxis = true, minOnAxis = true)

    def moveRockJet(rock: Rock, jet: Jet): Rock = jet match {
      case Jet.Left => if (checkLeftCollision(rock)) rock else rock.map(point => (point._1, point._2 - 1))
      case Jet.Right => if (checkRightCollision(rock)) rock else rock.map(point => (point._1, point._2 + 1))
    }

    def moveRockDown(rock: Rock): Option[Rock] =
      if (checkDownCollision(rock)) {
        settleRock(rock)
        None
      } else
        Some(rock.map(point => (point._1 - 1, point._2)))

    def settleRock(rock: Rock): Unit = {
      iterRocksDropped += 1
      rockFormation = rock.toList ++ rockFormation
      iterMaxRockHeight = Math.max(rock.map(_._1).max + 1, iterMaxRockHeight)
      val pruneRow: Option[Int] = rock.map(_._1).toSet.filter(checkCompleteRow).maxOption
      if (pruneRow.isDefined) pruneBelow(pruneRow.get)
    }

    def pruneBelow(row: Int): Unit = {
      val endPrunePoint: PrunePoint = (rowToGroundFormation(row), rockIndex, jetIndex)
      rockFormation = endPrunePoint._1

      pruneFloor = iterMaxRockHeight - row

      memory = memory + (startPrunePoint -> (endPrunePoint, (iterMaxRockHeight, iterRocksDropped, pruneFloor)))

      var curr: Option[MemoPoint] = memory.get(endPrunePoint) // Some(endPrunePoint, (iterMaxRockHeight, rocksDroppedIter))

      var prev: MemoPoint = memory(startPrunePoint)
      var skip: Boolean = false
      while ((overallRocksDropped + iterRocksDropped + prev._2._2 < rockCount) && curr.isDefined) {
        prev = curr.get

        if (prev._1 == startPrunePoint) {
          skip = true
          memoLoopHandler(startPrunePoint)
        } else {
          iterMaxRockHeight += prev._2._1 - pruneFloor
          iterRocksDropped += prev._2._2
          pruneFloor = prev._2._3
          curr = memory.get(prev._1)
        }
      }
      pruneFloor = prev._2._3

      if (!skip) {
        overallMaxRockHeight += iterMaxRockHeight - pruneFloor
        overallRocksDropped += iterRocksDropped
      }

      iterMaxRockHeight = pruneFloor
      iterRocksDropped = 0

      rockFormation = prev._1._1
      rockIndex = prev._1._2
      jetIndex = prev._1._3
      startPrunePoint = prev._1
    }

    def memoLoopHandler(start: PrunePoint): Unit = {
      var bigLoopPoint: MemoPoint = memory(start)
      var prev: PrunePoint = null
      var bigLoopIterSize: (Int, Int) = (0, 0)

      while (prev != start) {
        prev = bigLoopPoint._1
        bigLoopIterSize = (bigLoopIterSize._1 + bigLoopPoint._2._1 - bigLoopPoint._2._3, bigLoopIterSize._2 + bigLoopPoint._2._2)
        bigLoopPoint = memory(bigLoopPoint._1)
      }

      val iterCount: Long = (rockCount - overallRocksDropped) / bigLoopIterSize._2
      overallMaxRockHeight += iterCount * bigLoopIterSize._1
      overallRocksDropped += iterCount * bigLoopIterSize._2

      println("I'm looping!: " + bigLoopIterSize)
    }

    def rowToGroundFormation(row: Int): GroundFormation = {
      rockFormation.filter(_._1 >= row).map(point => (point._1 - row, point._2))
    }

    def checkCompleteRow(row: Int): Boolean =
      rockFormation.filter(point => point._1 == row || point._1 == row - 1).map(_._2).toSet.size == CHAMBER_WIDTH

    def spawnRock(rockType: Rock): Rock =
      rockType.map(point => (point._1 + SPAWN_FROM_BOTTOM_DIST + iterMaxRockHeight, point._2 + SPAWN_FROM_LEFT_DIST))

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

    while (overallRocksDropped + iterRocksDropped < rockCount) {
      var rock: Option[Rock] = Some(spawnRock(nextRock()))
      while (rock.isDefined) {
        rock = moveRockDown(moveRockJet(rock.get, nextJet()))
      }
    }

    overallMaxRockHeight + iterMaxRockHeight
  }
}

// 1528323699442
