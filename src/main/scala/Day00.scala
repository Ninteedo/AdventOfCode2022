import scala.util.matching.Regex

object Day00 extends IDay {
  override def execute(input: String): (String, String) = {
    val pattern: Regex = "(\\w+)\\r?\\n".r
    val result: List[String] = Helper.mapAllMatches(pattern, input, { m: Regex.Match => m.group(1)})
    (result.head, result(1))
  }
}
