import scala.io.Source

object TestRunner {
  val dayRunners: Array[IDay] = Array(
      Day00, Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12,
      Day13, Day14, Day15, Day16, Day17, Day18, Day19)

  def main(args: Array[String]): Unit = {
    val n: Int = args(0).toInt
    println(getDayResult(n))
  }

  def dayString(n: Int): String = if (n < 10) s"0$n" else s"$n"

  def getDayResult(n: Int): String = {
    val dayRunner: IDay = dayRunners(n)
    val source: Source = Source.fromFile(s"input/${dayString(n)}.txt")
    val input: String = try source.mkString finally source.close()
    val startTime = System.currentTimeMillis()
    val result = dayRunner.execute(input)
    val executionTime = System.currentTimeMillis() - startTime
    s"Day $n: (Part 1: ${result._1}, Part 2: ${result._2}) ${executionTime}ms"
  }
}
