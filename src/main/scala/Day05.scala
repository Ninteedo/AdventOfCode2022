import scala.collection.mutable

object Day05 extends IDay {
  override def execute(input: String): (String, String) = {
    val lines: Iterable[String] = Helper.readLines(input, identity)
    val stacks: Seq[mutable.Stack[Char]] = readStacks(lines.takeWhile(_ != "").dropRight(1))
    val moves: Iterable[(Int, Int, Int)] = lines.dropWhile(_ != "").drop(1).map(readMove)

    (part1(stacks, moves), part2(stacks, moves))
  }

  def readStacks(stackStrings: Iterable[String]): Seq[mutable.Stack[Char]] = {
    var stacks: List[mutable.Stack[Char]] = List()
    val pattern = "\\[(\\w)] ?".r
    stackStrings.toSeq.reverse.foreach(stackStr => {
      0.to(stackStr.length / 4).foreach { i: Int =>
        if (stacks.length <= i) stacks = stacks ++ List(mutable.Stack())
        stackStr.slice(4 * i, 4 * (i + 1)) match {
          case pattern(c) => stacks(i).push(c.charAt(0))
          case _ =>
        }
      }
    })
    stacks
  }

  def readMove(moveStr: String): (Int, Int, Int) = {
    val pattern = "move (\\d+) from (\\d) to (\\d)".r
    moveStr match {
      case pattern(n, s1, s2) => (n.toInt, s1.toInt, s2.toInt)
    }
  }

  def craneOperation(initialStacks: Seq[mutable.Stack[Char]], moves: Iterable[(Int, Int, Int)],
                     processMove: Seq[mutable.Stack[Char]] => ((Int, Int, Int)) => Unit): String = {
    val stacks = initialStacks.map(_.clone)
    moves.foreach(processMove(stacks))
    stacks.map(_.pop).mkString
  }

  def part1(initialStacks: Seq[mutable.Stack[Char]], moves: Iterable[(Int, Int, Int)]): String = {
    def processMove(stacks: Seq[mutable.Stack[Char]])(move: (Int, Int, Int)): Unit = {
      1.to(move._1).foreach(_ => stacks(move._3 - 1).push(stacks(move._2 - 1).pop))
    }

    craneOperation(initialStacks, moves, processMove)
  }

  def part2(initialStacks: Seq[mutable.Stack[Char]], moves: Iterable[(Int, Int, Int)]): String = {
    def processMove(stacks: Seq[mutable.Stack[Char]])(move: (Int, Int, Int)): Unit = {
      stacks(move._2 - 1).take(move._1).reverse.foreach { c: Char =>
        stacks(move._3 - 1).push(c)
        stacks(move._2 - 1).pop
      }
    }

    craneOperation(initialStacks, moves, processMove)
  }
}
