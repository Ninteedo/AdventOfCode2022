import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.matching.Regex

object Helper {
  def mapAllMatches[A](pattern: Regex, input: String, f: Regex.Match => A): Seq[A] = {
    pattern.findAllMatchIn(input).map{patternMatch => f(patternMatch)}.toSeq
  }

  def readLines[A](input: String, f: String => A): Seq[A] = {
    input.lines().toScala(LazyList).map(f)
  }

  def readLinesInt(input: String): Seq[Int] = {
    readLines(input, _.toInt)
  }

  def splitPair(line: String, delimiter: String): (String, String) = {
    val pattern: Regex = ("(.+)" + delimiter + "(.+)").r
    pattern.findFirstMatchIn(line) match {
      case Some(x) => (x.group(1), x.group(2))
      case None => sys.error("could not split " + line + " with " + delimiter)
    }
  }
}
