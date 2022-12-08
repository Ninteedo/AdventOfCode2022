object Day08 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val heights: Array[Array[Int]] = Helper.readLines(input, _.split("").map(_.toInt).toArray).toArray
    (part1(heights), part2(heights))
  }

  def mapOverTrees[A, B](heights: Array[Array[Int]], coords: (Int, Int),
                         f: (LazyList[Int], Boolean, Int) => A, g: LazyList[A] => B): B = {
    def validCoordinates(c: (Int, Int)): Boolean =
      c._1 >= 0 && c._1 < heights.head.length && c._2 >= 0 && c._2 < heights.length

    def mapInDirection(xDir: Int, yDir: Int): A = {
      val xAxis = xDir != 0
      val dir = if (xAxis) xDir else yDir
      val (start, static) = if (xAxis) coords else coords.swap
      val list = LazyList.from(start + dir, dir).takeWhile({ curr =>
        if (xAxis) validCoordinates(curr, static) else validCoordinates(static, curr) })
      f(list, xAxis, static)
    }

    val dirs = LazyList((1, 0), (-1, 0), (0, 1), (0, -1))
    g(dirs.map(dir => mapInDirection(dir._1, dir._2)))
  }

  def shorterTree(heights: Array[Array[Int]], coords: (Int, Int), xAxis: Boolean, static: Int)(current: Int): Boolean =
    (if (xAxis) heights(static)(current) else heights(current)(static)) < heights(coords._2)(coords._1)

  def part1(heights: Array[Array[Int]]): Int = {
    def isVisible(coords: (Int, Int)): Boolean = {
      def allShorter(list: LazyList[Int], xAxis: Boolean, static: Int): Boolean =
        list.forall(shorterTree(heights, coords, xAxis, static))

      mapOverTrees(heights, coords, allShorter, (_: LazyList[Boolean]).exists(identity))
    }

    Range(0, heights.head.length).map(x =>
      Range(0, heights.length).map(y =>
        isVisible((x, y))).count(identity)).sum
  }

  def part2(heights: Array[Array[Int]]): Int = {
    def scenicScore(coords: (Int, Int)): Int = {
      def visibleAndShorterCount(list: LazyList[Int], xAxis: Boolean, static: Int): Int =
        list.takeWhile(shorterTree(heights, coords, xAxis, static)).size

      def lastVisible(list: LazyList[Int], xAxis: Boolean, static: Int): Boolean =
        list.dropWhile(shorterTree(heights, coords, xAxis, static)).nonEmpty

      def lazyProduct(xs: LazyList[Int]): Int =
        if (xs.isEmpty) 1 else if (xs.head == 0) 0 else xs.head * lazyProduct(xs.tail)

      val l: LazyList[(Int, Int)] = mapOverTrees[Int, LazyList[Int]](heights, coords, visibleAndShorterCount, identity)
        .zip(mapOverTrees[Boolean, LazyList[Int]](heights, coords, lastVisible, _.map(if (_) 1 else 0)))
      lazyProduct(l.map(p => p._1 + p._2))
    }

    Range(0, heights.head.length).map(x =>
      Range(0, heights.length).map(y =>
        scenicScore((x, y))).max).max
  }
}
