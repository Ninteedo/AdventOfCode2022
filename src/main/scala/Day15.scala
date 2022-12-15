object Day15 extends IDay {
  type Point = (Int, Int)
  type BeaconSensor = (Point, Point)

  override def execute(input: String): (Int, Long) = {
    val sensorBeaconPairs: Iterable[BeaconSensor] = Helper.readLines(input, readLine)
    (part1(sensorBeaconPairs), part2(sensorBeaconPairs))
  }

  def readLine(line: String): BeaconSensor = {
    val pattern = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    line match {
      case pattern(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
      case _ => sys.error("pattern mismatch on input: " + line)
    }
  }

  def distance(a: Point, b: Point): Int = (a._1 - b._1).abs + (a._2 - b._2).abs

  def addPoints(a: Point, b: Point): Point = (a._1 + b._1, a._2 + b._2)

  def inBeaconRange(point: Point, sensorBeacon: BeaconSensor): Boolean =
    distance(point, sensorBeacon._1) <= distance(sensorBeacon._1, sensorBeacon._2)

  def isBeacon(point: Point, sensorBeacons: Iterable[BeaconSensor]): Boolean =
    sensorBeacons.exists(_._2 == point)

  def remainingBeaconWidth(point: Point, sensorBeacon: BeaconSensor): Int = {
    val pureWidth = distance(sensorBeacon._1, sensorBeacon._2) - (point._2 - sensorBeacon._1._2).abs
    val xDiff = (sensorBeacon._1._1 - pureWidth) - point._1
    pureWidth * 2 + xDiff + 1
  }

  def beaconXMinMax(sensorBeacon: BeaconSensor, y: Int): (Int, Int) = {
    val pureWidth = distance(sensorBeacon._1, sensorBeacon._2) - (y - sensorBeacon._1._2).abs
    (sensorBeacon._1._1 - pureWidth, sensorBeacon._1._1 + pureWidth)
  }

  def whichBeaconInRange(point: Point, sensorBeacons: Iterable[BeaconSensor]): Option[BeaconSensor] =
    sensorBeacons.find(inBeaconRange(point, _))

  def part1(sensorBeaconPairs: Iterable[BeaconSensor]): Int = {
    val row = 2000000
    val xs = sensorBeaconPairs.map(_._1._1) ++ sensorBeaconPairs.map(_._2._1)
    val minX = xs.min
    val maxX = xs.max
    var count = 0
    var visitedZero = false
    List((-1, 0), (1, 0)).foreach({ dir =>
      var last: Boolean = true
      var curr: Point = if (!visitedZero) {
        visitedZero = true
        (0, row)
      }
      else
        addPoints((0, row), dir)
      while (last || (curr._1 >= minX && curr._1 <= maxX)) {
        last = sensorBeaconPairs.exists(inBeaconRange(curr, _))
        if (last && !isBeacon(curr, sensorBeaconPairs)) count += 1
        curr = addPoints(curr, dir)
      }
    })
    count
  }

  def part2(sensorBeaconPairs: Iterable[BeaconSensor]): Long = {
    val yMax = 4000000

    def tuningFrequency(point: Point): Long =
      point._1.toLong * yMax + point._2

    var curr: Point = (0, 0)
    (0 to yMax).foreach({ _ =>
      val beaconMinMaxes = sensorBeaconPairs.map(beaconXMinMax(_, curr._2))
      val lineMinX = beaconMinMaxes.map(_._1).min
      val lineMaxX = beaconMinMaxes.map(_._2).max

      curr = (lineMinX, curr._2)
      while (curr._1 <= lineMaxX) whichBeaconInRange(curr, sensorBeaconPairs) match {
        case Some(beaconSensor) => curr = addPoints(curr, (remainingBeaconWidth(curr, beaconSensor), 0))
        case None => return tuningFrequency(curr)
      }
      curr = addPoints((0, curr._2), (0, 1))
    })

    sys.error("could not find empty point")
  }
}
