object Day25 extends IDay {
  type Snafu = List[Int]

  override def execute(input: String): (String, String) = {
    val snafuNumbers: Iterable[Snafu] = Helper.readLines(input, readSnafuNumber)
    (part1(snafuNumbers), part2())
  }

  def part1(snafuNumbers: Iterable[Snafu]): String =
    snafuToString(decimalToSnafu(snafuNumbers.map(snafuToDecimal).sum))

  def part2(): String = "FREE"

  def readSnafuNumber(line: String): Snafu = {
    line.map({
      case '=' => -2
      case '-' => -1
      case '0' => 0
      case '1' => 1
      case '2' => 2
    }).toList
  }

  def snafuToString(snafu: Snafu): String = snafu.map({
    case -2 => '='
    case -1 => '-'
    case 0 => '0'
    case 1 => '1'
    case 2 => '2'
  }).mkString

  def expBase5(x: Int): Long = x match {
    case 0 => 1
    case x => expBase5(x - 1) * 5
  }

  def snafuToDecimal(snafu: Snafu): Long = {
    val n: Int = snafu.length
    var exp = expBase5(n)

    snafu.map({ pair =>
      exp /= 5
      pair * exp
    }).sum
  }

  def decimalToSnafu(number: Long): Snafu = {
    var remaining: Long = number
    val n: Int = LazyList.from(0).find(2 * expBase5(_) >= number).get + 1
    var decimalBase5: List[Int] = List()

    for (x <- (0 until n).reverse) {
      val exp: Long = expBase5(x)
      val digitValue: Int = (remaining / exp).toInt
      remaining = remaining % exp
      decimalBase5 = digitValue :: decimalBase5
    }
    decimalBase5 = decimalBase5.reverse

    var result: Snafu = decimalBase5
    while (result.exists(_ > 2)) {
      var updated: Snafu = Nil
      for (digit <- result) {
        val currHead = if (updated.isEmpty) 0 else updated.head
        val currTail = if (updated.length <= 1) List(0) else updated.tail

        updated = if (digit > 2) digit - 5 :: currHead + 1 :: currTail
        else digit :: updated
      }
      result = updated.reverse.dropWhile(_ == 0)
    }

    result
  }
}
