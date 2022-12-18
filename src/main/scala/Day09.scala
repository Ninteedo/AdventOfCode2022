object Day09 extends IDay {
  type Move = (Char, Int)

  override def execute(input: String): (Int, Int) = {
    val moves: Iterable[Move] =
      Helper.mapAllMatches("(\\w) (\\d+)".r, input, { m => (m.group(1).head, m.group(2).toInt) })
    (part1(moves), part2(moves))
  }

  def nextHeadPos(dir: Char, head: Point2D): Point2D = head + (dir match {
    case 'R' => Point2D.xVec(1)
    case 'L' => Point2D.xVec(-1)
    case 'U' => Point2D.yVec(1)
    case 'D' => Point2D.yVec(-1)
  })

  def updateTailPos(headPos: Point2D, tailPos: Point2D): Point2D = {
    def altRound(x: Double): Int = if (x > 0) x.ceil.toInt else x.floor.toInt

    if ((headPos.x - tailPos.x).abs <= 1 && (headPos.y - tailPos.y).abs <= 1) tailPos
    else tailPos + new Point2D(altRound((headPos.x - tailPos.x) / 2.0), altRound((headPos.y - tailPos.y) / 2.0))
  }

  def processMove(dir: Char, rope: List[Point2D]): List[Point2D] = {
    var result: List[Point2D] = List(nextHeadPos(dir, rope.head))
    (1 until rope.size).foreach(i => result = updateTailPos(result.head, rope(i)) :: result)
    result.reverse
  }

  def countTailPositions(moves: Iterable[Move], ropeLength: Int): Int = {
    var visited: List[List[Point2D]] = List(List.fill(ropeLength)(Point2D.zero))
    moves.foreach(move =>
      (0 until move._2).foreach(_ =>
        visited = processMove(move._1, visited.head) :: visited
      )
    )
    visited.map(_.last).toSet.size
  }

  def part1(moves: Iterable[Move]): Int = countTailPositions(moves, 2)

  def part2(moves: Iterable[Move]): Int = countTailPositions(moves, 10)
}
