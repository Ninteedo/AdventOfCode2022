object Day15 extends IDay {
  type SensorBeacon = (Point2D, Point2D)

  override def execute(input: String): (Int, Long) = {
    val sensorBeaconPairs: Iterable[SensorBeacon] = Helper.readLines(input, readLine)
    (part1(sensorBeaconPairs), part2(sensorBeaconPairs))
  }

  def readLine(line: String): SensorBeacon = {
    val pattern = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    line match {
      case pattern(x1, y1, x2, y2) => (new Point2D(x1.toInt, y1.toInt), new Point2D(x2.toInt, y2.toInt))
      case _ => sys.error("pattern mismatch on input: " + line)
    }
  }

  def overlap(a: Range, b: Range): Range = Range.inclusive(
    if (a.start < b.start) b.start else a.start,
    if (a.end < b.end) b.end else a.end
  )

  def whichBeaconInRange(sensorBeacons: Iterable[SensorBeacon])(point: Point2D): Option[SensorBeacon] =
    sensorBeacons.find(sbPair => point.mannDist(sbPair._1) <= sbPair._1.mannDist(sbPair._2))

  def part1(sensorBeaconPairs: Iterable[SensorBeacon]): Int = {
    val row = 2000000

    def pureWidth(y: Int, sensorBeacon: SensorBeacon): Int =
      sensorBeacon._1.mannDist(sensorBeacon._2) - (y - sensorBeacon._1.y).abs

    def beaconXMinMax(sensorBeacon: SensorBeacon): (Int, Int) = {
      val width = pureWidth(row, sensorBeacon)
      (sensorBeacon._1.x - width, sensorBeacon._1.x + width)
    }

    val beaconMinMaxes = sensorBeaconPairs.map(beaconXMinMax)
    val (lineMinX, lineMaxX) = (beaconMinMaxes.map(_._1).min, beaconMinMaxes.map(_._2).max)

    val beaconCount = sensorBeaconPairs.map(_._2).filter(_.y == row).toSet.size

    var coveredCount = 0
    var curr: Point2D = new Point2D(lineMinX, row)

    while (curr.x <= lineMaxX) whichBeaconInRange(sensorBeaconPairs)(curr) match {
      case Some(sensorBeacon) =>
        val width = pureWidth(curr.y, sensorBeacon) + sensorBeacon._1.x - curr.x + 1
        coveredCount += width
        curr += Point2D.xVec(width)
      case None => curr += Point2D.xVec(1)
    }

    coveredCount - beaconCount
  }

  type AltPoint = Point2D
  type AltSensorBound = (AltPoint, AltPoint)
  type Gap = (Int, Range)

  def part2(sensorBeaconPairs: Iterable[SensorBeacon]): Long = {
    val yMax = 4000000

    def tuningFrequency(point: Point2D): Long = point.x.toLong * yMax + point.y

    def toAltAxis(point: Point2D): AltPoint = new Point2D(point.x - point.y, point.x + point.y)

    def fromAltAxis(altPoint: AltPoint): Point2D = new Point2D((altPoint.x + altPoint.y) / 2, (-altPoint.x + altPoint.y) / 2)

    def altSensorBound(sensorBeacon: SensorBeacon): AltSensorBound = {
      val r = sensorBeacon._1.mannDist(sensorBeacon._2)
      val center = toAltAxis(sensorBeacon._1)
      (center + new AltPoint(-r, -r), center + new AltPoint(r, r))
    }

    def inAltBound(point: AltPoint)(altSensorBound: AltSensorBound): Boolean =
      point.inArea(altSensorBound._1, altSensorBound._2)

    val bounds = sensorBeaconPairs.map(altSensorBound)

    var xGaps: List[Gap] = List()
    var yGaps: List[Gap] = List()

    def gapsPairCond(pairX: (Int, Range), pairY: (Int, Range)): Boolean =
      pairY._2.contains(pairX._1) && pairX._2.contains(pairY._1) &&
        !bounds.exists(inAltBound(new AltPoint(pairX._1, pairY._1)))

    def checkXGap(a: AltSensorBound, b: AltSensorBound): Unit = {
      if (a._2.x + 2 == b._1.x)
        xGaps = (a._2.x + 1, overlap(Range.inclusive(a._1.y, a._2.y), Range.inclusive(b._1.y, b._2.y))) :: xGaps
    }

    def checkYGap(a: AltSensorBound, b: AltSensorBound): Unit = {
      if (a._2.y + 2 == b._1.y)
        yGaps = (a._2.y + 1, overlap(Range.inclusive(a._1.x, a._2.x), Range.inclusive(b._1.x, b._2.x))) :: yGaps
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

    tuningFrequency(fromAltAxis(new AltPoint(lonelyGap._1._1, lonelyGap._2._1)))
  }

}
