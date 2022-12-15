object Day15 extends IDay {
  type Point = (Int, Int)
  type SensorBeacon = (Point, Point)

  override def execute(input: String): (Int, Long) = {
    val sensorBeaconPairs: Iterable[SensorBeacon] = Helper.readLines(input, readLine)
    (part1(sensorBeaconPairs), part2(sensorBeaconPairs))
  }

  def readLine(line: String): SensorBeacon = {
    val pattern = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    line match {
      case pattern(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
      case _ => sys.error("pattern mismatch on input: " + line)
    }
  }

  def overlap(a: Range, b: Range): Range = Range.inclusive(
    if (a.start < b.start) b.start else a.start,
    if (a.end < b.end) b.end else a.end
  )

  def distance(a: Point, b: Point): Int = (a._1 - b._1).abs + (a._2 - b._2).abs

  def addPoints(a: Point, b: Point): Point = (a._1 + b._1, a._2 + b._2)

  def whichBeaconInRange(sensorBeacons: Iterable[SensorBeacon])(point: Point): Option[SensorBeacon] =
    sensorBeacons.find(beacon => distance(point, beacon._1) <= distance(beacon._1, beacon._2))

  def remainingBeaconWidth(point: Point, sensorBeacon: SensorBeacon): Int = {
    val pureWidth = distance(sensorBeacon._1, sensorBeacon._2) - (point._2 - sensorBeacon._1._2).abs
    val xDiff = (sensorBeacon._1._1 - pureWidth) - point._1
    pureWidth * 2 + xDiff + 1
  }

  def rowCoverageMinMax(sensorBeacons: Iterable[SensorBeacon])(row: Int): (Int, Int) = {
    def beaconXMinMax(sensorBeacon: SensorBeacon): (Int, Int) = {
      val pureWidth = distance(sensorBeacon._1, sensorBeacon._2) - (row - sensorBeacon._1._2).abs
      (sensorBeacon._1._1 - pureWidth, sensorBeacon._1._1 + pureWidth)
    }

    val beaconMinMaxes = sensorBeacons.map(beaconXMinMax)
    (beaconMinMaxes.map(_._1).min, beaconMinMaxes.map(_._2).max)
  }

  def part1(sensorBeaconPairs: Iterable[SensorBeacon]): Int = {
    val row = 2000000
    val (lineMinX, lineMaxX) = rowCoverageMinMax(sensorBeaconPairs)(row)

    var count = 0
    var curr: Point = (lineMinX, row)

    while (curr._1 <= lineMaxX) whichBeaconInRange(sensorBeaconPairs)(curr) match {
      case Some(beaconSensor) =>
        val width = remainingBeaconWidth(curr, beaconSensor)
        count += width
        curr = addPoints(curr, (width, 0))
      case None => curr = addPoints(curr, (1, 0))
    }
    val beaconCount = sensorBeaconPairs.map(_._2).filter(_._2 == row).toSet.size
    count - beaconCount
  }

  type AltPoint = Point
  type AltSensorBound = (AltPoint, AltPoint)
  type Gap = (Int, Range)

  def part2(sensorBeaconPairs: Iterable[SensorBeacon]): Long = {
    val yMax = 4000000

    def tuningFrequency(point: Point): Long = point._1.toLong * yMax + point._2

    def toAltAxis(point: Point): AltPoint = (point._1 - point._2, point._1 + point._2)

    def fromAltAxis(altPoint: AltPoint): Point = ((altPoint._1 + altPoint._2) / 2, (-altPoint._1 + altPoint._2) / 2)

    def altSensorBound(sensorBeacon: SensorBeacon): AltSensorBound = {
      val r = distance(sensorBeacon._1, sensorBeacon._2)
      val center = toAltAxis(sensorBeacon._1)
      (addPoints(center, (-r, -r)), addPoints(center, (r, r)))
    }

    def inAltBound(point: AltPoint)(altSensorBound: AltSensorBound): Boolean = {
      altSensorBound._1._1 <= point._1 && point._1 <= altSensorBound._2._1 &&
        altSensorBound._1._2 <= point._2 && point._2 <= altSensorBound._2._2
    }

    val bounds = sensorBeaconPairs.map(altSensorBound)

    var xGaps: List[Gap] = List()
    var yGaps: List[Gap] = List()

    def gapsPairCond(pairX: (Int, Range), pairY: (Int, Range)): Boolean =
      pairY._2.contains(pairX._1) && pairX._2.contains(pairY._1) &&
        !bounds.exists(inAltBound((pairX._1, pairY._1)))

    def checkXGap(a: AltSensorBound, b: AltSensorBound): Unit = {
      if (a._2._1 + 2 == b._1._1)
        xGaps = (a._2._1 + 1, overlap(Range.inclusive(a._1._2, a._2._2), Range.inclusive(b._1._2, b._2._2))) :: xGaps
    }

    def checkYGap(a: AltSensorBound, b: AltSensorBound): Unit = {
      if (a._2._2 + 2 == b._1._2)
        yGaps = (a._2._2 + 1, overlap(Range.inclusive(a._1._1, a._2._1), Range.inclusive(b._1._1, b._2._1))) :: yGaps
    }

    bounds.foreach(a => bounds.filter(a != _).foreach(b => {
      checkXGap(a, b)
      checkXGap(b, a)
      checkYGap(a, b)
      checkYGap(b, a)
    }))

    val lonelyGap: ((Int, Range), (Int, Range)) = xGaps
      .flatMap(x => yGaps.map((x, _)))
      .find(p => gapsPairCond(p._1, p._2))
      .get

    tuningFrequency(fromAltAxis((lonelyGap._1._1, lonelyGap._2._1)))
  }

}
