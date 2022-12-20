import scala.collection.mutable

trait TimeSearchNode[T <: TimeSearchNode[T]] {
  val orderingValue: Int = {
    val result: Int = calculateOrderingValue()
    if (getParent != null && result > getParent.orderingValue)
      sys.error(s"invalid child ordering value $result, parent has ${getParent.orderingValue}\n${pathString()}")
    result
  }

  def calculateOrderingValue(): Int

  def descendents(): Iterable[T]

  def timeUp: Boolean = getTime == 0

  def getTime: Int

  def getParent: T

  def getResult: Int

  def bestFirstSearch(): Option[TimeSearchNode[T]] = {
    val frontier: mutable.PriorityQueue[TimeSearchNode[T]] = mutable.PriorityQueue(this)(Ordering.by(_.orderingValue))

    while (frontier.nonEmpty) {
      val node: TimeSearchNode[T] = frontier.dequeue()
      if (node.timeUp) return Some(node)
      node.descendents().foreach(frontier.enqueue(_))
    }

    None
  }

  def pathString(): String = {
    var result: List[String] = List()
    var curr: TimeSearchNode[T] = this
    while (curr != null) {
      result = curr.toString :: result
      curr = curr.getParent
    }
    result.mkString("\n")
  }

  override def toString: String = {
    s"time=$getTime, order=$orderingValue, result=$getResult"
  }
}
