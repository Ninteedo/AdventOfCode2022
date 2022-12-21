object Day21 extends IDay {
  val ROOT_NAME: String = "root"
  val HUMAN_NAME: String = "humn"

  override def execute(input: String): (Long, Long) = {
    (part1(Monkey.readMonkeyList(input, withHuman = false)), part2(Monkey.readMonkeyList(input, withHuman = true)))
  }

  def part1(rootMonkey: Monkey): Long = rootMonkey.getResult

  def part2(rootMonkey: Monkey): Long = {
    val originalRootMonkey: OpMonkey = rootMonkey match {
      case monkey: OpMonkey => monkey
    }
    var root: RootMonkey = if (originalRootMonkey.left.dependsOnHuman)
      RootMonkey(originalRootMonkey.left, originalRootMonkey.right)
    else
      RootMonkey(originalRootMonkey.right, originalRootMonkey.left)

    while (!root.left.isHuman) {
      val (newLeft, newRight) = root.left match {
        case monkey: OpMonkey => monkey.simplifyHumanBranch(root.right)
      }
      root = RootMonkey(newLeft, newRight)
    }
    root.right.getResult
  }

  abstract class Monkey(val name: String) {
    def getResult: Long

    def dependsOnHuman: Boolean

    def isHuman: Boolean = false

    def toEquationString: String

    def simplify: Monkey
  }

  case class NumberMonkey(override val name: String, number: Long) extends Monkey(name) {
    override def getResult: Long = number

    override def dependsOnHuman: Boolean = false

    override def toEquationString: String = s"$number"

    override def simplify: Monkey = this
  }

  case class RootMonkey(left: Monkey, right: Monkey) extends Monkey(ROOT_NAME) {
    override def getResult: Long = sys.error("should not evaluate root monkey")

    override def dependsOnHuman: Boolean = true

    override def toEquationString: String = s"(${left.toEquationString} === ${right.toEquationString})"

    override def simplify: Monkey = RootMonkey(left.simplify, right.simplify)
  }

  case class HumanMonkey() extends Monkey(HUMAN_NAME) {
    override def getResult: Long = sys.error("human value is unknown")

    override def dependsOnHuman: Boolean = true

    override def isHuman: Boolean = true

    override def toEquationString: String = "x"

    override def simplify: Monkey = this
  }

  abstract class OpMonkey(override val name: String, val left: Monkey, val right: Monkey) extends Monkey(name) {
    override def getResult: Long = {
      getOp(left.getResult, right.getResult)
    }

    override def dependsOnHuman: Boolean = left.dependsOnHuman || right.dependsOnHuman

    def getOp: (Long, Long) => Long = this match {
      case _: AddMonkey => (l, r) => l + r
      case _: SubMonkey => (l, r) => l - r
      case _: MulMonkey => (l, r) => l * r
      case _: DivMonkey => (l, r) => l / r
    }

    def simplifyHumanBranch(otherBranch: Monkey): (Monkey, Monkey) = (left.dependsOnHuman, this) match {
      case (true, AddMonkey(name, left, right)) => (left, SubMonkey(name + "-", otherBranch, right))
      case (false, AddMonkey(name, left, right)) => (right, SubMonkey(name + "-", otherBranch, left))
      case (true, SubMonkey(name, left, right)) => (left, AddMonkey(name + "+", otherBranch, right))
      case (false, SubMonkey(name, left, right)) => (right, SubMonkey(name + "-", left, otherBranch))
      case (true, MulMonkey(name, left, right)) => (left, DivMonkey(name + "/", otherBranch, right))
      case (false, MulMonkey(name, left, right)) => (right, DivMonkey(name + "/", otherBranch, left))
      case (true, DivMonkey(name, left, right)) => (left, MulMonkey(name + "*", right, otherBranch))
      case (false, DivMonkey(name, left, right)) => (right, DivMonkey(name + "/", left, otherBranch))
    }

    override def simplify: Monkey = {
      if (!dependsOnHuman) NumberMonkey(name, getResult)
      else this match {
        case AddMonkey(name, left, right) => AddMonkey(name, left.simplify, right.simplify)
        case SubMonkey(name, left, right) => SubMonkey(name, left.simplify, right.simplify)
        case MulMonkey(name, left, right) => MulMonkey(name, left.simplify, right.simplify)
        case DivMonkey(name, left, right) => DivMonkey(name, left.simplify, right.simplify)
      }
    }

    override def toEquationString: String = s"(${left.toEquationString} $opChar ${right.toEquationString})"

    def opChar: Char = this match {
      case _: AddMonkey => '+'
      case _: SubMonkey => '-'
      case _: MulMonkey => '*'
      case _: DivMonkey => '/'
    }
  }

  case class AddMonkey(override val name: String, override val left: Monkey, override val right: Monkey)
    extends OpMonkey(name, left, right)

  case class SubMonkey(override val name: String, override val left: Monkey, override val right: Monkey)
    extends OpMonkey(name, left, right)

  case class MulMonkey(override val name: String, override val left: Monkey, override val right: Monkey)
    extends OpMonkey(name, left, right)

  case class DivMonkey(override val name: String, override val left: Monkey, override val right: Monkey)
    extends OpMonkey(name, left, right)


  object Monkey {
    def readMonkeyList(input: String, withHuman: Boolean): Monkey = {
      var lines: Map[String, String] = Map()
      input.split("\\r?\\n").foreach(str => lines += str.substring(0, 4) -> str)

      def readOpMonkey(name: String, left: Monkey, right: Monkey, op: Char): OpMonkey = op match {
        case '+' => AddMonkey(name, left, right)
        case '-' => SubMonkey(name, left, right)
        case '*' => MulMonkey(name, left, right)
        case '/' => DivMonkey(name, left, right)
      }

      def getMonkey(name: String): Monkey = {
        if (withHuman && name == HUMAN_NAME) return HumanMonkey()

        val numberPattern = "(\\w{4}): (\\d+)".r
        val opPattern = "(\\w{4}): (\\w{4}) ([+\\-*/]) (\\w{4})".r

        lines(name) match {
          case numberPattern(name, number) => NumberMonkey(name, number.toInt)
          case opPattern(name, left, op, right) => readOpMonkey(name, getMonkey(left), getMonkey(right), op.head)
        }
      }

      getMonkey("root").simplify
    }
  }
}
