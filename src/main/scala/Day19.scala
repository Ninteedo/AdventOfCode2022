import Day19.Resource.{Clay, Geode, Obsidian, Ore, Resource}

import scala.collection.mutable

object Day19 extends IDay {
  type ResourceMap = Map[Resource, Int]

  override def execute(input: String): (Any, Any) = {
    val input = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
    val blueprints: List[Blueprint] = Helper.readLines(input, Blueprint.read).toList
    (part1(blueprints), part2(blueprints))
  }

  def part1(blueprints: List[Blueprint]): Int = {
    blueprints.map(b => b.num * b.bestGeodeCount(24)).sum
  }

  def part2(blueprints: List[Blueprint]): Int = {
    blueprints.take(3).map(b => b.bestGeodeCount(32)).sum
  }

  def printPath(end: TimeNode): String = {
    var result: List[String] = List()
    var curr: TimeNode = end
    while (curr != null) {
      result = curr.toString :: result
      curr = curr.parent
    }
    result.mkString("\n")
  }

  class Blueprint(val num: Int, val robots: Map[Resource, Robot]) {
    def bestGeodeCount(minutes: Int): Int = {
      def calculateMaxCosts: ResourceMap = {
        var result: ResourceMap = Resource.resourceMap
        Resource.values.foreach({ robotResource: Resource =>
          val maxCost: Int = Resource.values.map(robots(_).cost(robotResource)).max
          result += (robotResource -> maxCost)
        })
        result + (Geode -> Int.MaxValue)
      }

      val start: TimeNode = new TimeNode(this, minutes, Resource.resourceMap + (Ore -> 1),
        Resource.resourceMap, calculateMaxCosts, null)
      val frontier: mutable.PriorityQueue[TimeNode] = mutable.PriorityQueue(start)(Ordering.by(_.orderingValue))

      while (frontier.nonEmpty) {
        val node: TimeNode = frontier.dequeue()
        if (node.timeUp)
          return node.geodeCount // + "\n" + printPath(node) + "\n"
        node.possibilities.foreach(frontier.enqueue(_))
      }

      sys.error("frontier empty")
    }
  }

  object Blueprint {
    def read(line: String): Blueprint = {
      def readRobot(resource: String): Robot = {
        val pattern = (".*".r + resource + " robot costs ([\\w\\s]+)\\..*".r).r
        val cost: String = line match {
          case pattern(capture) => capture
        }
        Robot.read(resource, cost)
      }

      val numPattern = "Blueprint (\\d+):.*".r
      val num: Int = line match {
        case numPattern(num) => num.toInt
      }

      val resources = List("ore", "clay", "obsidian", "geode")
      var robots: Map[Resource, Robot] = Map()
      resources.foreach(resource => robots += Resource.read(resource) -> readRobot(resource))

      new Blueprint(num, robots)
    }
  }

  class Robot(val collects: Resource, val cost: ResourceMap) {

  }

  object Robot {
    def read(collects: String, cost: String): Robot = {
      val orePattern = "(\\d+) ore".r
      val oreClayPattern = "(\\d+) ore and (\\d+) clay".r
      val oreObsidianPattern = "(\\d+) ore and (\\d+) obsidian".r

      val collectionResource: Resource = collects match {
        case "ore" => Ore
        case "clay" => Clay
        case "obsidian" => Obsidian
        case "geode" => Geode
      }

      cost match {
        case orePattern(ore) =>
          new Robot(collectionResource, Resource.resourceMap + (Ore -> ore.toInt))
        case oreClayPattern(ore, clay) =>
          new Robot(collectionResource, Resource.resourceMap + (Ore -> ore.toInt) + (Clay -> clay.toInt))
        case oreObsidianPattern(ore, obsidian) =>
          new Robot(collectionResource, Resource.resourceMap + (Ore -> ore.toInt) + (Obsidian -> obsidian.toInt))
      }
    }
  }

  class TimeNode(val blueprint: Blueprint, val time: Int, val robots: ResourceMap, val resources: ResourceMap,
                 maxCosts: => ResourceMap, val parent: TimeNode) {
    def nextResources(newRobot: Option[Resource]): ResourceMap = {
      var newMap: ResourceMap = resources
      Resource.values.foreach(resource =>
        newMap += resource -> (resources.getOrElse(resource, 0) + robots.getOrElse(resource, 0))
      )
      if (newRobot.isDefined) {
        Resource.values.foreach({ resource =>
          newMap += resource -> (newMap.getOrElse(resource, 0) - blueprint.robots(newRobot.get).cost.getOrElse(resource, 0))
        })
      }
      newMap
    }

    def nextRobots(newRobot: Resource): ResourceMap = {
      robots + (newRobot -> (robots.getOrElse(newRobot, 0) + 1))
    }

    def canBuild(newRobot: Resource): Boolean = {
      Resource.values.forall({ resource =>
        robotCost(newRobot, resource) <= stockpile(resource)
      })
    }

    def possibilities: List[TimeNode] = {
      var result: List[TimeNode] = List()
      Resource.values.foreach({ resource =>
        if (canBuild(resource)) result =
          new TimeNode(blueprint, time - 1, nextRobots(resource), nextResources(Some(resource)), maxCosts, this) :: result
      })
      result = result.filter(_.idlenessPenalty == 0).filter(_.excessiveRobotsPenalty == 0).filter(_.orderingValue > 0)
      new TimeNode(blueprint, time - 1, robots, nextResources(None), maxCosts, this) :: result
    }

    def geodeCount: Int = stockpile(Geode)

    def geodeProjection: Int = geodeCount + robotCount(Geode) * time

    def maxExtraGeodes: Int = {
      def gainCap(gain: Int, offset: Int): Int = ((gain * (gain + 1)) - ((gain - offset) * (gain - offset + 1))) / 2

      def inverseSum(sum: Int): Int = {
        if (sum < 0) sys.error("invalid argument for inverse sum")
        if (sum == 0) 0
        else if (sum == 1) 1
        else if (sum <= 3) 2
        else if (sum <= 6) 3
        else if (sum <= 10) 4
        else if (sum <= 15) 5
        else if (sum <= 21) 6
        else sys.error("inverse sum argument too big")
      }

      def minTimeToMeetCost(robot: Resource, cost: Resource): Int = {
        val target: Int = robotCost(robot, cost)
        var bots: Int = robotCount(cost)
        var currStockpile = stockpile(cost)
        var t: Int = 0

        while (currStockpile < target) {
          t += 1
          currStockpile += bots
          bots += 1
        }

        t
      }

      def maxRateAtTimeSimple(resource: Resource, offset: Int): Int = {
        Math.min(
          maxResourceCost(resource),
          time - offset + robotCount(resource)
        )
      }



      // val maxOreRate: Int = robotCount(Ore) + maxRateAtTime(Ore, 0)

      val timeToFirstClayBot: Int = if (robotCount(Clay) > 0) 0 else minTimeToMeetCost(Clay, Ore)

      val timeToFirstObsidianBot: Int = if (robotCount(Obsidian) > 0) 0 else
        timeToFirstClayBot + Math.max(minTimeToMeetCost(Obsidian, Clay), minTimeToMeetCost(Obsidian, Ore))

      val minTimeToFirstGeodeBot: Int = if (robotCount(Geode) > 0) 0 else
        timeToFirstObsidianBot + Math.max(minTimeToMeetCost(Geode, Obsidian), minTimeToMeetCost(Geode, Ore))

      val maxGeodeRate = maxRateAtTimeSimple(Geode, minTimeToFirstGeodeBot) - robotCount(Geode) // Math.max(0, time - minTimeToFirstGeodeBot)

      (maxGeodeRate * (maxGeodeRate + 1)) / 2
    }

    def excessiveRobotsPenalty: Int = {
      Resource.values.map({robotResource =>
        if (robotResource == Geode) 0
        else Math.max(0, robotCount(robotResource) - maxResourceCost(robotResource))
      }).sum
    }

    def idlenessPenalty: Int = {
      Resource.values.map({ robotResource =>
        if (robotResource == Geode || robotCount(robotResource) == 0) 1
        else Math.max(0, stockpile(robotResource) - maxResourceCost(robotResource))
      }).product
    }

    def maxExtraGeodesOld: Int = {
      def rateCap(rate: Int, offset: Int): Int = Math.min(rate, time - offset)

      def calcMaxOreRate(): Int = {
        val oreBotCost: Int = robotCost(Ore, Ore)

        var passed: Int = -stockpile(Ore) / robotCount(Ore)
        var futureBotCount: Int = robotCount(Ore)
        while (futureBotCount < oreBotCost) {
          passed += oreBotCost / futureBotCount
          futureBotCount += 1
        }
        time + futureBotCount - passed - 1
      }

      val maxOreRate: Int = calcMaxOreRate()

      def calcMaxClayRate(): Int = {
        val clayBotCost: Int = robotCost(Clay, Ore)

        var passed: Int = -stockpile(Ore) / robotCount(Ore)
        var futureBotCount: Int = robotCount(Ore)
        while (futureBotCount < clayBotCost) {
          passed += clayBotCost / futureBotCount
          futureBotCount += 1
        }
        time + futureBotCount - passed - 1
      }


      val maxClayRate: Int = calcMaxClayRate()

      val timeToFirstClay: Int =
        if (robotCount(Clay) > 0) 0 else (robotCost(Clay, Ore) - stockpile(Ore)) / robotCount(Ore)

      // val maxClayRate: Int = stockpile(Clay) + rateCap((maxOreRate * time) / robotCost(Clay, Ore), timeToFirstClay)

      val timeToFirstObsidian: Int =
        if (robotCount(Obsidian) > 0)
          0
        else if (robotCount(Clay) > 0) Math.max(
          (robotCost(Obsidian, Clay)) / maxClayRate,
          (robotCost(Obsidian, Ore)) / maxOreRate
        )
        else
          timeToFirstClay + (robotCost(Obsidian, Ore)) / maxOreRate

      val maxObsidianRate: Int = if (robotCount(Obsidian) > 0) time * (time + 1) / 2
      else {
        val t = Math.max(maxClayRate / robotCost(Obsidian, Clay), maxOreRate / robotCost(Obsidian, Ore))
        (t * (t + 1)) / 2
      }
      /*val maxObsidianRate: Int = stockpile(Obsidian) + rateCap(Math.min(
        (maxOreRate * time) / robotCost(Obsidian, Ore),
        (maxClayRate * time) / robotCost(Obsidian, Clay)),
        timeToFirstObsidian
      )*/

      val timeToFirstGeode: Int = {
        if (robotCount(Geode) > 0)
          0
        else if (robotCount(Obsidian) > 0) Math.max(
          (robotCost(Geode, Obsidian)) / maxObsidianRate,
          (robotCost(Geode, Ore)) / maxOreRate
        )
        else
          timeToFirstObsidian + robotCost(Geode, Obsidian) + (robotCost(Geode, Ore)) / maxOreRate
      }

      val maxGeodeRate: Int = robotCount(Geode) + rateCap(Math.min(
        (maxOreRate) / robotCost(Geode, Ore),
        (maxObsidianRate) / robotCost(Geode, Obsidian)),
        0 // timeToFirstGeode
      )

      Math.max(0, maxGeodeRate * (time - 1))
    }

    val maxExtra: Int = maxExtraGeodes
    val orderingValue: Int = calcOrderingValue

    def calcOrderingValue: Int = {
      val result: Int = maxExtra + geodeProjection // + geodeProjection // - (excessiveRobotsPenalty * excessiveRobotsPenalty) - idlenessPenalty
      if (parent != null && result > parent.orderingValue)
        sys.error("invalid ordering value")
      if (false && parent != null && maxExtra < parent.maxExtra && robots == parent.robots)
        println("max extra changing when it shouldn't")
      result
    }

    def timeUp: Boolean = time == 0

    def robotCost(robot: Resource, resource: Resource): Int = blueprint.robots(robot).cost(resource)

    def robotCount(robot: Resource): Int = robots(robot)

    def stockpile(resource: Resource): Int = resources(resource)

    def maxResourceCost(resource: Resource): Int = maxCosts(resource)

    override def toString: String = "time=" + time + ", order=" + orderingValue + ", maxExtra=" + maxExtraGeodes +
      ", robots=" + robots + ", resources=" + resources
  }

  object Resource extends Enumeration {
    type Resource = Value
    val Ore, Clay, Obsidian, Geode = Value

    def read(str: String): Resource = str.toLowerCase match {
      case "ore" => Ore
      case "clay" => Clay
      case "obsidian" => Obsidian
      case "geode" => Geode
    }

    def resourceMap: ResourceMap = {
      var result: ResourceMap = Map()
      values.foreach(resource => result += resource -> 0)
      result
    }
  }
}
