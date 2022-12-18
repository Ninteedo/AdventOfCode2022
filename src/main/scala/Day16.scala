import scala.collection.mutable

object Day16 extends IDay {
  type Valve = String
  type ValveInfo = (Int, List[Valve])
  type ValveMap = Map[Valve, ValveInfo]
  type ValveAPSP = (Array[Array[Int]], Map[Valve, Int])

  override def execute(input: String): (Any, Any) = {
    val valves: ValveMap = readValveInputs(input)
    val connDists = generateConnectionDistances(valves)
    val closedValves: Set[Valve] = valves.filter(_._2._1 != 0).keySet
    val startPos = new DelayedMove("AA", 0)
    (part1(valves, connDists, closedValves, startPos), part2(valves, connDists, closedValves, startPos))
  }

  def readValveInputs(input: String): ValveMap = {
    var result: ValveMap = Map()
    val pattern = "Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? ([\\w, ]+)".r

    def readLine(line: String): Unit = line match {
      case pattern(valveName, flowRate, otherValves) =>
        result += (valveName -> (flowRate.toInt, otherValves.split(", ").toList))
      case _ => sys.error("input pattern mismatch: " + line)
    }

    Helper.readLines(input, readLine).toList
    result
  }

  def part1(valves: ValveMap, connDists: => ValveAPSP, closedValves: Set[Valve], startPos: DelayedMove): Int = {
    val start: TimeNode1 = new TimeNode1(valves, startPos, Set(), closedValves, 30, 0, connDists)
    bestPressureSearch(start)
  }

  def part2(valves: ValveMap, connDists: => ValveAPSP, closedValves: Set[Valve], startPos: DelayedMove): Int = {
    val start: TimeNode2 = new TimeNode2(valves, startPos, startPos, Set(), closedValves, 26, 0, connDists)
    bestPressureSearch(start)
  }

  def bestPressureSearch[T <: TimeNode[T]](start: T): Int = {
    val frontier: mutable.PriorityQueue[T] = mutable.PriorityQueue(start)(Ordering.by(_.orderingValue))

    while (frontier.nonEmpty) {
      val node: T = frontier.dequeue()
      if (node.timeUp) return node.getTotalPressure
      node.possibleMoves.foreach(frontier.enqueue(_))
    }

    sys.error("frontier empty")
  }

  def generateConnectionDistances(valves: ValveMap): ValveAPSP = {
    var indexMapping: Map[Valve, Int] = Map()
    valves.toList.map(_._1).zipWithIndex.foreach({ p: (Valve, Int) => indexMapping += (p._1 -> p._2) })

    val indexedValves: List[(Valve, Int)] = valves
      .toList
      .map(_._1)
      .sortBy(indexMapping)
      .zipWithIndex

    // initial
    var result: Array[Array[Int]] = indexedValves.map({ a =>
      indexedValves.map({ b =>
        if (a == b) 0
        else if (valves(a._1)._2.contains(b._1)) 1
        else valves.size * 2
      }).toArray
    }).toArray
    // all-pairs shortest paths
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


  class DelayedMove(val target: Valve, val dist: Int) {
    def passTime(): DelayedMove = new DelayedMove(target, if (dist > 0) dist - 1 else 0)

    def atTarget: Boolean = dist <= 0
  }


  abstract class TimeNode[T <: TimeNode[T]](val valves: ValveMap, val openValves: Set[Valve],
                                            val closedValves: Set[Valve], val time: Int, val totalPressure: Int,
                                            connDists: => ValveAPSP) {
    val orderingValue: Int = calculateOrderingValue()

    def calculateOrderingValue(): Int = totalPressure + pressureRate * time + maxExtraPressure + time

    def possibleMoves: Iterable[T]

    def pressureRate: Int = openValves.toSeq.map(valves(_)._1).sum

    def nextPressure: Int = totalPressure + pressureRate

    def maxExtraPressure: Int = {
      def maxFromValve(valve: Valve): Int = Math.max(0, valves(valve)._1 * (time - timeToReach(valve) - 2))

      closedValves.toList.map(maxFromValve).sum
    }

    def connDist(a: Valve, b: Valve): Int = connDists._1(connDists._2(a))(connDists._2(b)) - 1

    def delayedMove(start: Valve, target: Valve): DelayedMove = new DelayedMove(target, connDist(start, target))

    def canOpenValve(loc: DelayedMove): Boolean =
      loc.atTarget && closedValves.contains(loc.target) && valves(loc.target)._1 != 0

    def timeToReach(target: Valve): Int

    def timeUp: Boolean = time == 0

    def getTotalPressure: Int = totalPressure
  }

  class TimeNode1(valves: ValveMap, location: DelayedMove, openValves: Set[Valve], closedValves: Set[Valve],
                  time: Int, totalPressure: Int, connDists: => ValveAPSP)
    extends TimeNode[TimeNode1](valves, openValves, closedValves, time, totalPressure, connDists) {

    def openValve(): TimeNode1 =
      new TimeNode1(valves, location.passTime(), openValves + location.target, closedValves - location.target,
        time - 1, nextPressure, connDists)

    def newTarget(loc: DelayedMove): TimeNode1 = new TimeNode1(valves, loc, openValves, closedValves,
      time - 1, nextPressure, connDists)

    def continueOrNewTargets: List[DelayedMove] = {
      if (!location.atTarget || closedValves.isEmpty) List(location.passTime())
      else closedValves.toList.map(delayedMove(location.target, _))
    }

    def timeToReach(target: Valve): Int = connDist(location.target, target) + location.dist

    def possibleMoves: Iterable[TimeNode1] = {
      if (canOpenValve(location)) List(openValve())
      else continueOrNewTargets.map(newTarget)
    }
  }

  class TimeNode2(valves: ValveMap, location1: DelayedMove, location2: DelayedMove, openValves: Set[Valve],
                  closedValves: Set[Valve], time: Int, totalPressure: Int, connDists: => ValveAPSP)
    extends TimeNode[TimeNode2](valves, openValves, closedValves, time, totalPressure, connDists) {

    def openValve(loc1: DelayedMove, loc2: DelayedMove, open: Set[Valve], closed: Set[Valve]): TimeNode2 =
      new TimeNode2(valves, loc1, loc2, open, closed, time - 1, nextPressure, connDists)

    def openValve1(loc2: DelayedMove): TimeNode2 =
      openValve(location1, loc2, openValves + location1.target, closedValves - location1.target)

    def openValve2(loc1: DelayedMove): TimeNode2 =
      openValve(loc1, location2, openValves + location2.target, closedValves - location2.target)

    def openBothValves(): TimeNode2 =
      openValve(location1, location2, (openValves + location1.target) + location2.target,
        (closedValves - location1.target) - location2.target)

    def bothNewTargets(loc1: DelayedMove, loc2: DelayedMove): TimeNode2 = {
      new TimeNode2(valves, loc1, loc2, openValves, closedValves, time - 1, nextPressure, connDists)
    }

    def possibleMoves: Iterable[TimeNode2] = {
      if (canOpenValve(location1) && canOpenValve(location2)) List(openBothValves())
      else if (canOpenValve(location1)) continueOrNewTargets(location2).map(openValve1)
      else if (canOpenValve(location2)) continueOrNewTargets(location1).map(openValve2)
      else {
        val targets2 = continueOrNewTargets(location2)
        continueOrNewTargets(location1).flatMap(l1 => targets2.map(bothNewTargets(l1, _)))
      }
    }

    def continueOrNewTargets(loc: DelayedMove): List[DelayedMove] = {
      val next = loc.passTime()

      if (!loc.atTarget || closedValves.isEmpty) List(next)
      else closedValves.toList.map(delayedMove(loc.target, _))
    }

    def timeToReach(target: Valve): Int = Math.min(
      connDist(location1.target, target) + location1.dist,
      connDist(location2.target, target) + location2.dist
    )
  }

}
