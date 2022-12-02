object Day02 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val moves: Iterable[(String, String)] = Helper.readLines(input, Helper.splitPair(_, " "))
    (part1(moves), part2(moves))
  }

  def part1(moves: Iterable[(String, String)]): Int = {
    def getMoveScore(move: (String, String)): Int = {
      val opp: Int = symbolToVal(move._1)
      val your: Int = symbolToVal(move._2)
      val outcome: Int = your - opp match {
        case 0 => 3
        case 1 => 6
        case -2 => 6
        case _ => 0
      }
      your + outcome
    }

    moves.map(getMoveScore).sum
  }

  def part2(moves: Iterable[(String, String)]): Int = {
    def getMoveScore(move: (String, String)): Int = {
      val opp: Int = symbolToVal(move._1)
      val outcome: Int = symbolToVal(move._2)
      val your: Int = ((opp + outcome) % 3) + 1
      your + 3 * (outcome - 1)
    }

    moves.map(getMoveScore).sum
  }

  def symbolToVal(symbol: String): Int = symbol match {
    case "A" => 1
    case "B" => 2
    case "C" => 3
    case "X" => 1
    case "Y" => 2
    case "Z" => 3
  }
}
