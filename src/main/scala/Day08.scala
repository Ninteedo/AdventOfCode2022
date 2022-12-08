object Day08 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val heights: Seq[Seq[Int]] = Helper.readLines(input, _.split("").map(_.toInt).toSeq).toSeq
    (part1(heights), part2(heights))
  }

  def mapOverTrees[A, B](heights: Seq[Seq[Int]], coords: (Int, Int),
                         f: (Iterable[Int], Boolean, Int) => A,
                         g: Iterable[A] => B): B = {
    def validCoordinates(c: (Int, Int)): Boolean =
      c._1 >= 0 && c._1 < heights.head.length && c._2 >= 0 && c._2 < heights.length

    def mapInDirection(xDir: Int, yDir: Int): A = {
      val xAxis = xDir != 0
      val start = if (xAxis) coords._1 else coords._2
      val dir = if (xAxis) xDir else yDir
      val static = if (xAxis) coords._2 else coords._1
      val list = LazyList.from(start + dir, dir)
        .takeWhile({ c => if (xAxis) validCoordinates(c, static) else validCoordinates(static, c) })
      f(list, xAxis, static)
    }

    val dirs = LazyList((1, 0), (-1, 0), (0, 1), (0, -1))
    g(dirs.map(dir => mapInDirection(dir._1, dir._2)))
  }

  def shorterTree(heights: Seq[Seq[Int]], coords: (Int, Int), xAxis: Boolean, static: Int)(current: Int): Boolean =
    (if (xAxis) heights(static)(current) else heights(current)(static)) < heights(coords._2)(coords._1)

  def part1(heights: Seq[Seq[Int]]): Int = {
    def isVisible(heights: Seq[Seq[Int]], coords: (Int, Int)): Boolean = {
      def allShorter(list: Iterable[Int], xAxis: Boolean, static: Int): Boolean =
        list.forall(shorterTree(heights, coords, xAxis, static))

      mapOverTrees(heights, coords, allShorter, { list: Iterable[Boolean] => list.exists(identity) })
    }

    Range(0, heights.head.length)
      .map(x => Range(0, heights.length)
        .map(y => isVisible(heights, (x, y))).count(identity)).sum
  }

  def part2(heights: Seq[Seq[Int]]): Int = {
    def scenicScore(heights: Seq[Seq[Int]], coords: (Int, Int)): Int = {
      def visibleAndShorterCount(list: Iterable[Int], xAxis: Boolean, static: Int): Int =
        list.takeWhile(shorterTree(heights, coords, xAxis, static)).size

      def lastVisible(list: Iterable[Int], xAxis: Boolean, static: Int): Boolean =
        list.dropWhile(shorterTree(heights, coords, xAxis, static)).nonEmpty

      val l: Iterable[(Int, Int)] = mapOverTrees[Int, Iterable[Int]](heights, coords, visibleAndShorterCount, identity)
        .zip(mapOverTrees[Boolean, Iterable[Int]](heights, coords, lastVisible, _.map(if (_) 1 else 0)))
      l.map({ p => p._1 + p._2 }).product
    }

    Range(0, heights.head.length)
      .map(x => Range(0, heights.length)
        .map(y => scenicScore(heights, (x, y))).max).max
  }
}
