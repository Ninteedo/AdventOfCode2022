object Day00 extends IDay {
  override def execute(input: String): (String, String) = {
    val result: Seq[String] = Helper.readLines(input, identity)
    (result.head, result(1))
  }
}
