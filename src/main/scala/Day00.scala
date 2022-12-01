object Day00 extends IDay {
  override def execute(input: String): (String, String) = {
    val result: Iterable[String] = Helper.readLines(input, identity)
    (result.head, result.tail.head)
  }
}
