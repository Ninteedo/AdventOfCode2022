object Day10 extends IDay {
  abstract class Command
  case class Noop() extends Command
  case class AddX(x: Int) extends Command

  override def execute(input: String): (Int, String) = {
    def readCommand(line: String): Command = {
      val noopPattern = "noop".r
      val addXPattern = "addx (-?\\d+)".r
      line match {
        case noopPattern() => Noop()
        case addXPattern(x) => AddX(x.toInt)
      }
    }

    val commands: Iterable[Command] = Helper.readLines(input, readCommand).toList
    (part1(commands), part2(commands))
  }

  def mapOverCommands[A](commands: Iterable[Command], f: Int => Int => A): Iterable[A] = {
    var cycle = 1
    var currX = 1
    var res: List[A] = List()

    def processCmd(cmd: Command): Unit = {
      def incrCycle(): Unit = {
        res = f(cycle)(currX) :: res
        cycle += 1
      }

      cmd match {
        case AddX(x) => incrCycle(); incrCycle(); currX += x
        case Noop() => incrCycle()
      }
    }

    commands.foreach(processCmd)
    res.reverse
  }

  def part1(commands: Iterable[Command]): Int = {
    def signalStrength(cycle: Int)(x: Int): Int =
      if ((cycle - 20) % 40 == 0) cycle * x
      else 0

    mapOverCommands(commands, signalStrength).sum
  }

  def part2(commands: Iterable[Command]): String = {
    def renderChar(cycle: Int)(x: Int): Char =
      if (cycle % 40 == 0) '\n'
      else if ((((cycle - 1) % 40) - x).abs <= 1) '#'
      else '.'

    "\n" + mapOverCommands(commands, renderChar).mkString
  }
}
