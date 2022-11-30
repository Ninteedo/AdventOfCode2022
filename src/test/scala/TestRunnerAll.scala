object TestRunnerAll {
  def main(args: Array[String]): Unit = {
    LazyList.from(0)
      .take(TestRunner.dayRunners.size)
      .foreach{n: Int => TestRunner.main(Array(n.toString))}
  }
}
