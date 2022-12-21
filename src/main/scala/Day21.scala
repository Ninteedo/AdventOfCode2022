object Day21 extends IDay {
  type Monkeys = Iterable[Monkey]

  override def execute(input: String): (Any, Any) = {
    // val input = "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"
    val monkeys: Monkeys = Helper.readLines(input, Monkey.read)
    (part1(monkeys), incomplete) // part2(monkeys))
  }

  val ROOT_NAME: String = "root"
  val HUMAN_NAME: String = "humn"

  def part1(monkeys: Monkeys): Long = {
    monkeys.find(_.name == ROOT_NAME).get.getResult(monkeys)
  }

  def part2(monkeys: Monkeys): Long = {
    val originalRootMonkey: OpMonkey = monkeys.find(_.name == ROOT_NAME).get.asInstanceOf[OpMonkey]
    val humanMonkey: HumanMonkey = HumanMonkey()
    val rootMonkey: RootMonkey = RootMonkey(originalRootMonkey.left, originalRootMonkey.right)
    val newMonkeys: Monkeys = humanMonkey :: monkeys.filter(monkey => monkey.name != ROOT_NAME && monkey.name != HUMAN_NAME).toList

    val leftEquation: Long => Long = rootMonkey.findLeft(newMonkeys).getHumanEquation(newMonkeys)
    val rightEquation: Long => Long = rootMonkey.findRight(newMonkeys).getHumanEquation(newMonkeys)

    var x: Long = 0
    while (leftEquation(x) != rightEquation(x)) x += 1
    x
  }

  abstract class Monkey(val name: String) {
    def getResult(monkeys: Monkeys): Long

    def getConstraint(monkeys: Monkeys): Constraint

    def getHumanEquation(monkeys: Monkeys): Long => Long
  }

  case class NumberMonkey(override val name: String, number: Long) extends Monkey(name) {
    override def getResult(monkeys: Monkeys): Long = number

    override def getConstraint(monkeys: Monkeys): Constraint = ???

    override def getHumanEquation(monkeys: Monkeys): Long => Long = _ => number
  }

  case class OpMonkey(override val name: String, left: String, right: String, op: (Long, Long) => Long) extends Monkey(name) {
    var result: Option[Long] = None

    var humanEquation: Option[Long => Long] = None

    override def getResult(monkeys: Monkeys): Long = {
      if (result.isEmpty) result = Some(op(findLeft(monkeys).getResult(monkeys), findRight(monkeys).getResult(monkeys)))
      result.get
    }

    def findLeft(monkeys: Monkeys): Monkey = monkeys.find(_.name == left).get

    def findRight(monkeys: Monkeys): Monkey = monkeys.find(_.name == right).get

    override def getConstraint(monkeys: Monkeys): Constraint = ???

    override def getHumanEquation(monkeys: Monkeys): Long => Long = {
      if (humanEquation.isEmpty)
        humanEquation = Some({
          x => repurposeOp(findLeft(monkeys).getHumanEquation(monkeys), findRight(monkeys).getHumanEquation(monkeys))(x)
        })
      humanEquation.get
    }

    def repurposeOp(leftOp: Long => Long, rightOp: Long => Long): Long => Long = {
       x => op(leftOp(x), rightOp(x))
    }
  }

  case class RootMonkey(left: String, right: String) extends Monkey(ROOT_NAME) {
    override def getResult(monkeys: Monkeys): Long = ???

    override def getConstraint(monkeys: Monkeys): Constraint = ???

    override def getHumanEquation(monkeys: Monkeys): Long => Long = ???

    def findLeft(monkeys: Monkeys): Monkey = monkeys.find(_.name == left).get

    def findRight(monkeys: Monkeys): Monkey = monkeys.find(_.name == right).get
  }

  case class HumanMonkey() extends Monkey(HUMAN_NAME) {
    override def getResult(monkeys: Monkeys): Long = ???

    override def getConstraint(monkeys: Monkeys): Constraint = ???

    override def getHumanEquation(monkeys: Monkeys): Long => Long = x => x
  }

  object Monkey {
    def read(line: String): Monkey = {
      val numberPattern = "(\\w{4}): (\\d+)".r
      val opPattern = "(\\w{4}): (\\w{4}) ([+\\-*/]) (\\w{4})".r

      def readOp(op: Char): (Long, Long) => Long = op match {
        case '+' => (l, r) => l + r
        case '-' => (l, r) => l - r
        case '*' => (l, r) => l * r
        case '/' => (l, r) => l / r
      }

      line match {
        case numberPattern(name, number) => NumberMonkey(name, number.toInt)
        case opPattern(name, left, op, right) => OpMonkey(name, left, right, readOp(op.head))
      }
    }
  }

  abstract class Constraint() {
    def isSatisfied(monkeys: Monkeys, number: Long): Boolean
  }

  case class EqConstraint(left: Monkey, right: Monkey) extends Constraint {
    override def isSatisfied(monkeys: Monkeys, number: Long): Boolean = {
      left.getResult(monkeys) == right.getResult(monkeys)
    }
  }

  case class NoneConstraint() extends Constraint {
    override def isSatisfied(monkeys: Monkeys, number: Long): Boolean = true
  }

  case class OpConstraint(left: Constraint, right: Constraint, op: (Long, Long) => Long) extends Constraint {
    override def isSatisfied(monkeys: Monkeys, number: Long): Boolean =
      left.isSatisfied(monkeys, number) && right.isSatisfied(monkeys, number)
  }

  abstract class Expr {
    var dependsOnHumanResult: Option[Boolean] = None
    var evalResult: Option[Long] = None

    def eval(humanValue: Long): Long = {
      if (dependsOnHuman) evalOperation(humanValue)
      else {
        if (evalResult.isEmpty) evalResult = Some(evalOperation(humanValue))
        evalResult.get
      }
    }

    def dependsOnHuman: Boolean = {
      if (dependsOnHumanResult.isEmpty) dependsOnHumanResult = Some(checkDependsOnHuman)
      dependsOnHumanResult.get
    }

    def evalOperation(humanValue: Long): Long

    def reverse: Expr

    def checkDependsOnHuman: Boolean
  }

  case class Num(n: Long) extends Expr {
    override def evalOperation(humanValue: Long): Long = n

    override def reverse: Expr = Num(n)

    override def checkDependsOnHuman: Boolean = false
  }

  case class Add(l: Expr, r: Expr) extends Expr {
    override def evalOperation(humanValue: Long): Long = l.evalOperation(humanValue) + r.evalOperation(humanValue)

    override def reverse: Expr = Sub(l, r)

    override def checkDependsOnHuman: Boolean = l.dependsOnHuman || r.dependsOnHuman
  }

  case class Sub(l: Expr, r: Expr) extends Expr {
    override def evalOperation(humanValue: Long): Long = l.evalOperation(humanValue) - r.evalOperation(humanValue)

    override def reverse: Expr = Add(l, r)

    override def checkDependsOnHuman: Boolean = l.dependsOnHuman || r.dependsOnHuman
  }

  case class Mult(l: Expr, r: Expr) extends Expr {
    override def evalOperation(humanValue: Long): Long = l.evalOperation(humanValue) * r.evalOperation(humanValue)

    override def reverse: Expr = Div(l, r)

    override def checkDependsOnHuman: Boolean = l.dependsOnHuman || r.dependsOnHuman
  }

  case class Div(l: Expr, r: Expr) extends Expr {
    override def evalOperation(humanValue: Long): Long = l.evalOperation(humanValue) / r.evalOperation(humanValue)

    override def reverse: Expr = Mult(l, r)

    override def checkDependsOnHuman: Boolean = l.dependsOnHuman || r.dependsOnHuman
  }

  case class HumanVar() extends Expr {
    override def evalOperation(humanValue: Long): Long = humanValue

    override def reverse: Expr = HumanVar()

    override def checkDependsOnHuman: Boolean = true
  }

  case class MonkeyVar(name: String) extends Expr {
    override def evalOperation(humanValue: Long): Long = ???

    override def reverse: Expr = MonkeyVar(name)

    override def checkDependsOnHuman: Boolean = false
  }
}
