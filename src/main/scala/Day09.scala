object Day09 extends IDay {
  type Point = (Int, Int)
  type Move = (Char, Int)

  override def execute(input: String): (Int, Int) = {
    val moves: Iterable[Move] =
      Helper.mapAllMatches("(\\w) (\\d+)".r, input, { m => (m.group(1).toCharArray.head, m.group(2).toInt) })
    (part1(moves), part2(moves))
  }

  def nextHeadPos(dir: Char, head: Point): Point = dir match {
    case 'R' => (head._1 + 1, head._2)
    case 'L' => (head._1 - 1, head._2)
    case 'U' => (head._1, head._2 + 1)
    case 'D' => (head._1, head._2 - 1)
  }

  def updateTailPos(headPos: Point, tailPos: Point): Point = {
    def altRound(x: Double): Int = if (x > 0) x.ceil.toInt else x.floor.toInt

    if ((headPos._1 - tailPos._1).abs <= 1 && (headPos._2 - tailPos._2).abs <= 1) tailPos
    else
      (tailPos._1 + altRound((headPos._1 - tailPos._1) / 2.0),
        tailPos._2 + altRound((headPos._2 - tailPos._2) / 2.0))
  }

  def processMove(dir: Char, rope: List[Point]): List[Point] = {
    var result: List[Point] = List(nextHeadPos(dir, rope.head))
    (1 until rope.size).foreach(i => result = updateTailPos(result.head, rope(i)) :: result)
    result.reverse
  }

  def countTailPositions(moves: Iterable[Move], ropeLength: Int): Int = {
    var visited = List((0 until ropeLength).map(_ => (0, 0)).toList)
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
