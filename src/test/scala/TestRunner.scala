import scala.io.Source

object TestRunner {
  val dayRunners: List[IDay] =
    List(Day00, Day01)  // to be extended

  def main(args: Array[String]): Unit = {
    val n: Int = args(0).toInt
    println(getDayResult(n))
  }

  def dayString(n: Int): String = if (n < 10) s"0$n" else s"$n"

  def getDayResult(n: Int): String = {
    val dayRunner: IDay = dayRunners(n)
    val source: Source = Source.fromFile(s"input/${dayString(n)}.txt")
    val input: String = try source.mkString finally source.close()
    val result = dayRunner.execute(input)
    s"Day $n: (Part 1: ${result._1}, Part 2: ${result._2})"
  }
}
