import scala.collection.mutable

object Day11 extends IDay {
  override def execute(input: String): (Long, Long) = {
    def readMonkey(str: String): Monkey = {
      val startingItemsPattern = "\\s+Starting items: ([\\d, ]+)".r
      val operationPattern = "\\s+Operation: new = old ([+*]) (old|\\d+)".r
      val testPattern = "\\s+Test: divisible by (\\d+)".r
      val destPattern = "\\s+If (false|true): throw to monkey (\\d+)".r

      var startingItems: Iterable[Long] = List()
      var operation: Long => Long = identity
      var (testDiv, trueDest, falseDest) = (0, 0, 0)

      Helper.readLines(str, identity).foreach {
        case startingItemsPattern(items: String) => startingItems = items.split(", ").map(_.toLong)
        case operationPattern(op, other) => operation =
          if (op == "+") _ + other.toInt
          else if (other != "old") _ * other.toInt
          else { x: Long => x * x }
        case testPattern(divisor) => testDiv = divisor.toInt
        case destPattern(destName, n) => if (destName == "true") trueDest = n.toInt else falseDest = n.toInt
        case _ =>
      }
      new Monkey(startingItems, operation, testDiv, trueDest, falseDest)
    }

    val monkeyPattern = "Monkey \\d+:([^M]+)((\\r?\\n){2}|(\\r?\\n$)|$)".r
    val monkeys: Array[Monkey] = Helper.mapAllMatches(monkeyPattern, input, { m => readMonkey(m.group(1)) }).toArray

    (part1(monkeys), part2(monkeys))
  }


  class Monkey(val startItems: Iterable[Long], val operation: Long => Long,
               val testDiv: Int, val trueDest: Int, val falseDest: Int) {
    val items: mutable.Queue[Long] = mutable.Queue()
    items.enqueueAll(startItems)
    var inspectCount: Long = 0

    def inspect(divideByThree: Boolean, bigDiv: Long): (Long, Int) = {
      inspectCount += 1
      var worryLevel: Long = operation(items.dequeue()) % bigDiv
      if (divideByThree) worryLevel = (worryLevel / 3.0).floor.toInt
      (worryLevel, if (worryLevel % testDiv == 0) trueDest else falseDest)
    }

    def reset(): Unit = {
      inspectCount = 0
      items.clear()
      items.enqueueAll(startItems)
    }
  }


  def activeMonkeysProduct(monkeys: Array[Monkey], rounds: Int, divideByThree: Boolean, bigDiv: Long): Long = {
    def monkeyTurn(monkey: Monkey): Unit = while (monkey.items.nonEmpty) {
      val (worryLevel, destMonkey) = monkey.inspect(divideByThree, bigDiv)
      monkeys(destMonkey).items.enqueue(worryLevel)
    }

    monkeys.foreach(_.reset())
    (0 until rounds).foreach(_ => monkeys.foreach(monkeyTurn))
    val inspectionCounts = monkeys.map(_.inspectCount).toList
    inspectionCounts.sorted.takeRight(2).product
  }

  def part1(monkeys: Array[Monkey]): Long =
    activeMonkeysProduct(monkeys, 20, divideByThree = true, Long.MaxValue)

  def part2(monkeys: Array[Monkey]): Long =
    activeMonkeysProduct(monkeys, 10000, divideByThree = false, monkeys.map(_.testDiv.toLong).product)
}
