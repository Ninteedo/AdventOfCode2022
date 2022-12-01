import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.matching.Regex

object Helper {
  def mapAllMatches[A](pattern: Regex, input: String, f: Regex.Match => A): Iterable[A] = {
    pattern.findAllMatchIn(input).map{patternMatch => f(patternMatch)}.toSeq
  }

  def readLines[A](input: String, f: String => A): Iterable[A] = {
    input.lines().toScala(LazyList).map(f)
  }

  def readLinesInt(input: String): Iterable[Int] = {
    readLines(input, _.toInt)
  }

  def splitPair(line: String, delimiter: String): (String, String) = {
    val pattern: Regex = ("(.+)" + delimiter + "(.+)").r
    pattern.findFirstMatchIn(line) match {
      case Some(x) => (x.group(1), x.group(2))
      case None => sys.error(s"could not split $line with $delimiter")
    }
  }
}
