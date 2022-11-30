trait IDay {
  def execute(input: String): (String, String)

  val incomplete: String = "INCOMPLETE"
}
