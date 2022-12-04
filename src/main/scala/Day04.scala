import scala.util.matching.Regex

object Day04 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val pattern: Regex = """(\d+)-(\d+),(\d+)-(\d+)""".r
    val pairs: Iterable[(Range, Range)] = Helper.mapAllMatches(pattern, input, {
      case pattern(aStart, aEnd, bStart, bEnd) =>
        (Range.inclusive(aStart.toInt, aEnd.toInt), Range.inclusive(bStart.toInt, bEnd.toInt))
    })
    (part1(pairs), part2(pairs))
  }

  def part1(pairs: Iterable[(Range, Range)]): Int = {
    def checkAllOverlap(p: (Range, Range)): Boolean = p._1.forall(p._2.contains) || p._2.forall(p._1.contains)

    pairs.count(checkAllOverlap)
  }

  def part2(pairs: Iterable[(Range, Range)]): Int = {
    def checkAnyOverlap(p: (Range, Range)): Boolean = p._1.exists(p._2.contains) || p._2.exists(p._1.contains)

    pairs.count(checkAnyOverlap)
  }
}
