object Day03 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val rucksacks: Iterable[String] = Helper.readLines(input, identity)
    (part1(rucksacks), part2(rucksacks))
  }

  def part1(rucksacks: Iterable[String]): Int = {
    def findReappearingItem(rucksack: String): Char = {
      val n: Int = rucksack.length
      val (first, second) = (rucksack.substring(0, n / 2), rucksack.substring(n / 2, n))
      reappears(first, second).head
    }

    rucksacks.map(findReappearingItem).map(itemPriority).sum
  }

  def part2(rucksacks: Iterable[String]): Int = {
    def findBadge(rucksackGroup: Iterable[String]): Char =
      rucksackGroup.tail.fold(rucksackGroup.head)(reappears).head

    val groups: Iterable[Iterable[String]] =
      rucksacks.zipWithIndex.groupBy{ pair: (String, Int) => pair._2 / 3 }.values.map(_.map(_._1))
    groups.map(findBadge).map(itemPriority).sum
  }

  def reappears(a: String, b: String): String = a.filter(b.contains(_))

  def itemPriority(item: Char): Int = if (item.isLower) item - 'a' + 1 else item - 'A' + 27
}
