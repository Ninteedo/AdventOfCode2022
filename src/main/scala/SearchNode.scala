import scala.collection.mutable

trait SearchNode[T <: SearchNode[T]] {
  val orderingValue: Int = {
    val result: Int = calculateOrderingValue()
    if (getParent != null && result > getParent.orderingValue)
      sys.error(s"invalid child ordering value $result, parent has ${getParent.orderingValue}\n${pathString()}")
    result
  }

  def calculateOrderingValue(): Int

  def descendents(): Iterable[T]

  def atGoal: Boolean

  def getParent: T

  def getResult: Int

  def isDuplicateOf(other: SearchNode[T]): Boolean = false

  def filterDuplicates: Boolean = false

  def bestFirstSearch(): Option[SearchNode[T]] = {
    val frontier: mutable.PriorityQueue[SearchNode[T]] = mutable.PriorityQueue(this)(Ordering.by(_.orderingValue))

    while (frontier.nonEmpty) {
      val node: SearchNode[T] = frontier.dequeue()
      if (node.atGoal) return Some(node)

      var nodesToAdd = node.descendents()
      if (filterDuplicates) nodesToAdd = nodesToAdd.filter(newNode => frontier.forall(!newNode.isDuplicateOf(_)))
      nodesToAdd.foreach(frontier.enqueue(_))
    }

    None
  }

  def pathString(): String = {
    var result: List[String] = List()
    var curr: SearchNode[T] = this
    while (curr != null) {
      result = curr.toString :: result
      curr = curr.getParent
    }
    result.mkString("\n")
  }

  override def toString: String = {
    s"TimeNode(order=$orderingValue, result=$getResult)"
  }
}
