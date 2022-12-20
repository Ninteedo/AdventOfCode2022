object TestRunnerAll {
  def main(args: Array[String]): Unit = {
    val startTime: Long = System.currentTimeMillis()
    LazyList.from(0)
      .take(TestRunner.dayRunners.length)
      .foreach{n: Int => TestRunner.main(Array(n.toString))}
    val totalTime: Long = System.currentTimeMillis() - startTime
    println(s"Total execution time: ${totalTime}ms")
  }
}
