import Day19.Resource.{Clay, Geode, Obsidian, Ore, Resource}

object Day19 extends IDay {
  type ResourceMap = Map[Resource, Int]

  override def execute(input: String): (Int, Int) = {
    val blueprints: List[Blueprint] = Helper.readLines(input, Blueprint.read).toList
    (part1(blueprints), part2(blueprints))
  }

  def part1(blueprints: List[Blueprint]): Int = {
    blueprints.map(b => b.num * b.bestGeodeCount(24)).sum
  }

  def part2(blueprints: List[Blueprint]): Int = {
    blueprints.take(3).map(b => b.bestGeodeCount(32)).product
  }

  class Blueprint(val num: Int, val robots: Map[Resource, ResourceMap]) {
    def bestGeodeCount(minutes: Int): Int = {
      def calculateMaxCosts: ResourceMap = {
        var result: ResourceMap = Resource.resourceMap
        Resource.values.foreach({ robotResource: Resource =>
          val maxCost: Int = Resource.values.map(robots(_)(robotResource)).max
          result += (robotResource -> maxCost)
        })
        result + (Geode -> Int.MaxValue) // no geode costs, so set to infinite
      }

      val start: TimeNode = new TimeNode(this, minutes, Resource.resourceMap + (Ore -> 1),
        Resource.resourceMap, calculateMaxCosts, null)

      start.bestFirstSearch() match {
        case Some(node: TimeNode) => node.getResult
        case None => 0 // default if no geodes can be mined
      }
    }
  }

  object Blueprint {
    def read(line: String): Blueprint = {
      def readRobot(resource: String): ResourceMap = {
        val pattern = (".*".r + resource + " robot costs ([\\w\\s]+)\\..*".r).r
        val cost: String = line match {
          case pattern(capture) => capture
        }

        val orePattern = "(\\d+) ore".r
        val oreClayPattern = "(\\d+) ore and (\\d+) clay".r
        val oreObsidianPattern = "(\\d+) ore and (\\d+) obsidian".r

        cost match {
          case orePattern(ore) =>
            Resource.resourceMap + (Ore -> ore.toInt)
          case oreClayPattern(ore, clay) =>
            Resource.resourceMap + (Ore -> ore.toInt) + (Clay -> clay.toInt)
          case oreObsidianPattern(ore, obsidian) =>
            Resource.resourceMap + (Ore -> ore.toInt) + (Obsidian -> obsidian.toInt)
        }
      }

      val numPattern = "Blueprint (\\d+):.*".r
      val num: Int = line match {
        case numPattern(num) => num.toInt
      }

      val resources = List("ore", "clay", "obsidian", "geode")
      var robots: Map[Resource, ResourceMap] = Map()
      resources.foreach(resource => robots += Resource.read(resource) -> readRobot(resource))

      new Blueprint(num, robots)
    }
  }

  class TimeNode(val blueprint: Blueprint, val time: Int, val robots: ResourceMap, val resources: ResourceMap,
                 maxCosts: => ResourceMap, val parent: TimeNode) extends SearchNode[TimeNode] {
    def nextResources(newRobot: Option[Resource]): ResourceMap = {
      var newMap: ResourceMap = resources
      Resource.values.foreach(resource =>
        newMap += resource -> (stockpile(resource) + robotCount(resource))
      )
      if (newRobot.isDefined) {
        Resource.values.foreach({ resource =>
          newMap += resource -> (newMap.getOrElse(resource, 0) - robotCost(newRobot.get, resource))
        })
      }
      newMap
    }

    def nextRobots(newRobot: Resource): ResourceMap = robots + (newRobot -> (robotCount(newRobot) + 1))

    def canBuild(newRobot: Resource): Boolean = Resource.values.forall({ resource =>
      robotCost(newRobot, resource) <= stockpile(resource)
    })

    override def descendents(): List[TimeNode] = {
      var result: List[TimeNode] = List(new TimeNode(blueprint, time - 1, robots, nextResources(None), maxCosts, this))
      Resource.values.foreach({ resource =>
        if (canBuild(resource) && maxResourceCost(resource) > robotCount(resource)) {
          val newTimeNode: TimeNode = new TimeNode(blueprint, time - 1, nextRobots(resource), nextResources(Some(resource)), maxCosts, this)
          if (newTimeNode.orderingValue > 0) result = newTimeNode :: result
        }
      })
      result
    }

    def geodeProjection: Int = stockpile(Geode) + robotCount(Geode) * time

    def maxExtraGeodes: Int = {
      // estimates maximum geodes possible by running a quick and optimistic simulation

      val obsidianTarget: Int = robotCost(Geode, Obsidian)
      val clayTarget: Int = robotCost(Obsidian, Clay)
      val oreTarget: Int = robotCost(Clay, Ore)

      var oreBots: Int = robotCount(Ore)
      var clayBots: Int = robotCount(Clay)
      var obsidianBots: Int = robotCount(Obsidian)
      var geodeBots: Int = 0

      var ore: Int = stockpile(Ore)
      var clay: Int = stockpile(Clay)
      var obsidian: Int = stockpile(Obsidian)
      var geodes: Int = 0

      (0 until time).foreach({ _ =>
        ore += oreBots
        clay += clayBots
        obsidian += obsidianBots
        geodes += geodeBots

        oreBots += 1
        if (ore >= oreTarget) {
          clayBots += 1
          ore -= oreTarget
        }
        if (clay >= clayTarget) {
          obsidianBots += 1
          clay -= clayTarget
        }
        if (obsidian >= obsidianTarget) {
          geodeBots += 1
          obsidian -= obsidianTarget
        }
      })
      geodes
    }

    override def calculateOrderingValue(): Int = maxExtraGeodes + geodeProjection

    def robotCost(robot: Resource, resource: Resource): Int = blueprint.robots(robot)(resource)

    def robotCount(robot: Resource): Int = robots(robot)

    def stockpile(resource: Resource): Int = resources(resource)

    def maxResourceCost(resource: Resource): Int = maxCosts(resource)

    override def getParent: TimeNode = parent

    override def atGoal: Boolean = time == 0

    override def getResult: Int = stockpile(Geode)

    override def toString: String = {
      s"TimeNode(time=$time, order=$orderingValue, maxExtra=$maxExtraGeodes, robots=$robots, resources=$resources)"
    }
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
