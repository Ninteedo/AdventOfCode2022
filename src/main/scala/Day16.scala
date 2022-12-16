import scala.collection.mutable

object Day16 extends IDay {
  type Valve = String
  type ValveInfo = (Int, List[Valve])
  type ValveMap = Map[Valve, ValveInfo]
  type ValveAPSP = (Array[Array[Int]], Map[Valve, Int])

  override def execute(input: String): (Any, Any) = {
    // val input = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
    val valves: ValveMap = readValveInputs(input)
    (part1(valves), part2(valves))
  }

  def readValveInputs(input: String): ValveMap = {
    var result: ValveMap = Map()
    val pattern = "Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? ([\\w, ]+)".r

    def readLine(line: String): Unit = line match {
      case pattern(valveName, flowRate, otherValves) =>
        result = result + (valveName -> (flowRate.toInt, otherValves.split(", ").toList))
      case _ => sys.error("input pattern mismatch: " + line)
    }

    Helper.readLines(input, readLine).toList
    result
  }

  def getPressureRate(valves: ValveMap, openValves: Set[Valve]): Int = openValves.map(valves(_)._1).sum

  def boolToInt(b: Boolean): Int = if (b) 1 else 0

  def part1(valves: ValveMap): Int = {
    val start: TimeNode = new TimeNode(valves, "AA", Set(), 30, 0)
    val frontier: mutable.PriorityQueue[TimeNode] = mutable.PriorityQueue(start)(Ordering.by(_.orderingValue()))

    while (frontier.nonEmpty) {
      val node: TimeNode = frontier.dequeue()

      if (node.timeUp)
        return node.getTotalPressure

      if (node.allValuedValvesOpen)
        return node.pressureProjection

      if (!node.locationAlreadyOpen && !node.worthlessValve) frontier.enqueue(node.openValve())
      node.movesToNextValves().foreach(frontier.enqueue(_))
    }

    sys.error("frontier empty")
  }

  def generateConnectionDistances(valves: ValveMap): ValveAPSP = {
    var indexMapping: Map[Valve, Int] = Map()
    valves.toList.map(_._1).zipWithIndex.foreach({ p: (Valve, Int) => indexMapping = indexMapping + (p._1 -> p._2) })

    def valveIndex(valve: Valve): Int = indexMapping(valve)

    val indexedValves: List[(Valve, Int)] = valves
      .toList
      .map(_._1)
      .sortBy(p => valveIndex(p))
      .zipWithIndex

    var result: Array[Array[Int]] = indexedValves.map({ a =>
      indexedValves.map({ b =>
        if (a == b) 0
        else if (valves(a._1)._2.contains(b._1)) 1
        else valves.size * 2
      }).toArray
    }).toArray
    indexedValves.foreach({ k =>
      result = indexedValves.map({ i: (Valve, Int) =>
        indexedValves.map({ j: (Valve, Int) =>
          if (result(i._2)(j._2) > result(i._2)(k._2) + result(k._2)(j._2))
            result(i._2)(k._2) + result(k._2)(j._2)
          else
            result(i._2)(j._2)
        }).toArray
      }).toArray
    })

    (result, indexMapping)
  }

  def getConDist(valveAPSP: ValveAPSP)(a: Valve, b: Valve): Int = valveAPSP._1(valveAPSP._2(a))(valveAPSP._2(b))

  def part2(valves: ValveMap): Any = {
    val connectionDistances = generateConnectionDistances(valves)

    val closedValves: Set[Valve] = valves.filter(_._2._1 != 0).keySet

    val minutes = 26

    val startPos = new DelayedMove("AA", 0)
    val start: TimeNode2 = new TimeNode2(valves, startPos, startPos, Set(), closedValves, minutes, 0,
      null, connectionDistances)
    val frontier: mutable.PriorityQueue[TimeNode2] = mutable.PriorityQueue(start)(Ordering.by(_.orderingValue))

    def printRoute(last: TimeNode2): String = {
      var route: List[String] = List()
      var prev = last
      while (prev != null) {
        route = prev.toString :: route
        prev = prev.getParent
      }
      route.mkString("\n")
    }

    var minVisited = start.orderingValue

    while (frontier.nonEmpty) {
      val node: TimeNode2 = frontier.dequeue()

      minVisited = Math.min(minVisited, node.orderingValue)

      if (node.timeUp) {
        return node.getTotalPressure + "\n" + printRoute(node) + "\n" + minVisited
      }

      // println(node.getTime)

      val moves = node.possibleMoves()
      moves
        .foreach(frontier.enqueue(_))
    }

    sys.error("frontier empty")
  }

  class TimeNode(valves: ValveMap, location: Valve, openValves: Set[Valve], time: Int, totalPressure: Int) {
    def openValve(): TimeNode =
      new TimeNode(valves, location, openValves + location, time - 1, totalPressure + getPressureRate(valves, openValves))

    def movesToNextValves(): Iterable[TimeNode] =
      valves(location)._2.map(new TimeNode(valves, _, openValves, time - 1, totalPressure + getPressureRate(valves, openValves)))

    def partialOrderingValue: Int = {
      if (!locationAlreadyOpen) valves(location)._1 * (time - 1) else 0
    }

    def maxExtraPressure: Int = {
      val test = valves.filter(p => !openValves.contains(p._1)).values.toList.map(_._1).sorted.reverse.zipWithIndex
      test.map(p => p._1 * (time - (1 + p._2) * 3)).sum
    }

    def pressureProjection: Int = totalPressure + getPressureRate(valves, openValves) * time

    def orderingValue(): Int = {
      totalPressure + (getPressureRate(valves, openValves) * (time)) + maxExtraPressure
    }

    def locationAlreadyOpen: Boolean = openValves.contains(location)

    def worthlessValve: Boolean = valves(location)._1 == 0

    def timeUp: Boolean = time == 0

    def getTotalPressure: Int = totalPressure

    def getTime: Int = time

    def allValuedValvesOpen: Boolean = valves.keys.filter(valves(_)._1 != 0).forall(openValves.contains)
  }

  class DelayedMove(target: Valve, dist: Int) {
    val eqTarget: Valve = target
    val eqDist: Int = dist

    def getTarget: Valve = target

    def getDist: Int = if (dist > 0) dist else 0

    def passTime(): DelayedMove = new DelayedMove(target, if (getDist > 0) getDist - 1 else 0)

    def atTarget: Boolean = dist <= 0

    def totalFlow(remainingTime: Int, flowRate: Int): Int = {
      val t = remainingTime - getDist
      flowRate * (if (t > 0) t else 0)
    }

    override def toString: Valve = "(" + target + ":" + dist + ")"

    def canEqual(other: Any): Boolean = other.isInstanceOf[DelayedMove]

    override def equals(other: Any): Boolean = other match {
      case that: DelayedMove =>
        (that canEqual this) &&
          eqTarget == that.eqTarget &&
          eqDist == that.eqDist
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(eqTarget, eqDist)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  class TimeNode2(valves: ValveMap, location1: DelayedMove, location2: DelayedMove,
                  openValves: Set[Valve], closedValves: Set[Valve], time: Int, totalPressure: Int, parent: TimeNode2,
                  connectionDistances: => ValveAPSP) {
    def openValve(loc1: DelayedMove, loc2: DelayedMove, newOpenValves: Set[Valve], newClosedValves: Set[Valve]): TimeNode2 =
      new TimeNode2(valves, loc1, loc2, newOpenValves, newClosedValves, time - 1, nextPressure, this, connectionDistances)

    val eqTime: Int = time
    val eqOpenValves: Set[Valve] = openValves
    val eqTotalPressure: Int = totalPressure
    val eqLoc1: DelayedMove = location1
    val eqLoc2: DelayedMove = location2

    def openValve1(loc2: DelayedMove): TimeNode2 =
      openValve(location1, loc2, openValves + location1.getTarget, closedValves - location1.getTarget)

    def openValve2(loc1: DelayedMove): TimeNode2 =
      openValve(loc1, location2, openValves + location2.getTarget, closedValves - location2.getTarget)

    def openBothValves(): TimeNode2 =
      openValve(location1, location2, (openValves + location1.getTarget) + location2.getTarget,
        (closedValves - location1.getTarget) - location2.getTarget)

    def possibleMoves(): Iterable[TimeNode2] = {
      if (canOpenValve1 && canOpenValve2) List(openBothValves())
      else if (canOpenValve1) continueOrNewTargets(location2).map(openValve1)
      else if (canOpenValve2) continueOrNewTargets(location1).map(openValve2)
      else {
        val targets2 = continueOrNewTargets(location2)
        continueOrNewTargets(location1)
          .flatMap(l1 => targets2
            .filter(_.getTarget != l1.getTarget)
            .map({ l2 =>
              new TimeNode2(valves, l1, l2, openValves, closedValves, time - 1, nextPressure,
                this, connectionDistances)
            })
          )
      } // .filter(!isMirror(_))
    }

    def isMirror(other: TimeNode2): Boolean = {
      eqTime == other.eqTime && eqOpenValves == other.eqOpenValves && eqTotalPressure == other.eqTotalPressure &&
        eqLoc1.equals(other.eqLoc2) && eqLoc2.equals(other.eqLoc1)
    }

    def continueOrNewTargets(loc: DelayedMove): List[DelayedMove] = {
      def valveNotTargeted(valve: Valve): Boolean = location1.getTarget != valve && location2.getTarget != valve

      val next = loc.passTime()

      if (!loc.atTarget) List(loc.passTime())
      // else if (!next.atTarget) List()
      else if (closedValves.size == 1 && !valveNotTargeted(closedValves.head)) List(next)
      else if (closedValves.isEmpty) List(next)
      else
        closedValves
          .toList
          .filter(valveNotTargeted)
          .map(v => delayedMove(loc.getTarget, v))
          .filter(_.getDist < time)
    }

    def nextPressure: Int = totalPressure + getPressureRate(valves, openValves)

    def delayedMove(start: Valve, target: Valve): DelayedMove = new DelayedMove(target, connDist(start, target))

    def maxExtraPressure: Int = {
      def timeToReach(target: Valve): Int = Math.min(
        connDist(location1.getTarget, target) + location1.getDist -
          boolToInt(location1.getTarget == target || openValves.contains(location1.getTarget)),
        connDist(location2.getTarget, target) + location2.getDist -
          boolToInt(location2.getTarget == target || openValves.contains(location2.getTarget))
      )

      closedValves
        .map(v => (v, valves(v)))
        .toList
        .map({ p: (Valve, ValveInfo) => (p._1, p._2._1) })
        .map({ p: (Valve, Int) =>
          val move = new DelayedMove(p._1, timeToReach(p._1))
          val totalFlow = move.totalFlow(time, p._2)
          totalFlow
        })
        .sum
    }

    def pressureProjection: Int = totalPressure + getPressureRate(valves, openValves) * time

    val orderingValue: Int = calculateOrderingValue()

    def calculateOrderingValue(): Int = {
      val res = pressureProjection + maxExtraPressure + time
      // if (parent != null && res >= parent.orderingValue)
         // sys.error("too big")
      res
    }

    def canOpenValve(loc: DelayedMove): Boolean =
      loc.atTarget && !openValves.contains(loc.getTarget) && valves(loc.getTarget)._1 != 0

    def canOpenValve1: Boolean = canOpenValve(location1)

    def canOpenValve2: Boolean = canOpenValve(location2)

    def timeUp: Boolean = time == 0

    def getTotalPressure: Int = totalPressure

    def getTime: Int = time

    def getParent: TimeNode2 = parent

    def allValuedValvesOpen: Boolean = valves.keys.filter(valves(_)._1 != 0).forall(openValves.contains)

    def connDist(a: Valve, b: Valve): Int = getConDist(connectionDistances)(a, b) - 1

    override def toString: Valve = location1 + "/" + location2 + ", time=" + time +
      ", flow=" + getPressureRate(valves, openValves) + ", curr=" + totalPressure +
      ", maxExtraPressure=" + maxExtraPressure +
      ", value=" + orderingValue + ", open=[" + openValves + "]"
  }

}

// 1935, 1934, 1915, 1923, 1912, 1942
