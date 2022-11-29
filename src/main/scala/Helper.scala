import scala.util.matching.Regex

object Helper {
  def mapAllMatches[A](pattern: Regex, input: String, f: Regex.Match => A): List[A] = {
    pattern.findAllMatchIn(input).map{patternMatch => f(patternMatch)}.toList
  }
}
