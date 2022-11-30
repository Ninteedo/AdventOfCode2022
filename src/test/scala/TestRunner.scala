import scala.io.Source

object TestRunner {
  val dayRunners: List[IDay] =
    List(Day00)  // to be extended

  def main(args: Array[String]): Unit = {
    val n: Int = args(0).toInt
    println(getDayResult(n))
  }

  def dayString(n: Int): String = if (n < 10) "0" + n.toString else n.toString

  def getDayResult(n: Int): String = {
    val dayRunner: IDay = dayRunners(n)
    val source: Source = Source.fromFile("input/" + dayString(n) + ".txt")
    val input: String = try source.mkString finally source.close()
    val result = dayRunner.execute(input)
    "Day " + n + ": (Part 1: " + result._1.toString + ", Part 2: " + result._2.toString + ")"
  }
}
