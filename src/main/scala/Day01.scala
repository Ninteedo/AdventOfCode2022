object Day01 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val elfGroups: Array[String] = input.split("(\r?\n){2}")
    val elfCalorieCounts: Iterable[Int] = elfGroups.map(Helper.readLinesInt(_).sum)
    (part1(elfCalorieCounts), part2(elfCalorieCounts))
  }

  def part1(xs: Iterable[Int]): Int = xs.max

  def part2(xs: Iterable[Int]): Int = xs.toSeq.sorted.takeRight(3).sum
}
