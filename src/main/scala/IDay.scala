trait IDay {
  def execute(input: String): (Any, Any)

  val incomplete: String = "INCOMPLETE"
}
