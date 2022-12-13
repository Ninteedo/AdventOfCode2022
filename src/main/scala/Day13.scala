object Day13 extends IDay {

  abstract class Packet
  case class PacketValue(n: Int) extends Packet
  case class SubPacket(sub: Seq[Packet]) extends Packet

  override def execute(input: String): (Any, Any) = {
    def readPacket(str: String): Packet = {
      if (str.matches("\\d+")) PacketValue(str.toInt)
      else if (str == "") SubPacket(Seq())
      else SubPacket(indentedSplit(str.stripPrefix("[").stripSuffix("]")).map(readPacket))
    }

    val pairs: Iterable[(String, String)] =
      input.split("(\\r?\\n){2}").map(str => (str.split("\\r?\\n").head, str.split("\\r?\\n").last))
    val packetPairs: Iterable[(Packet, Packet)] = pairs.map(p => (readPacket(p._1), readPacket(p._2)))
    val dividers: List[Packet] = List("[[2]]", "[[6]]").map(readPacket)

    (part1(packetPairs), part2(packetPairs.flatMap(p => List(p._1, p._2)).toList, dividers))
  }

  def indentedSplit(str: String): Seq[String] = {
    var depth = 0
    var split: String = ""
    var splits: Seq[String] = Seq()
    str.foreach({
      case '[' => depth += 1; split = split ++ "["
      case ']' => depth -= 1; split = split ++ "]"
      case ',' =>
        if (depth != 0) split = split ++ "," else {
          splits = splits ++ List(split)
          split = ""
        }
      case c => split = split ++ c.toString
    })
    splits ++ Seq(split)
  }

  def validPacketPair(a: Packet, b: Packet): Boolean = {
    def recursivePacketCheck(p: Packet, q: Packet): Option[Boolean] = p match {
      case PacketValue(a) => q match {
        case PacketValue(b) => if (a == b) None else Some(a < b)
        case SubPacket(sub2) => recursivePacketCheck(SubPacket(Seq(PacketValue(a))), SubPacket(sub2))
      }
      case SubPacket(sub1) => q match {
        case PacketValue(b) => recursivePacketCheck(SubPacket(sub1), SubPacket(Seq(PacketValue(b))))
        case SubPacket(sub2) =>
          val xs = sub1.zip(sub2).map(p => recursivePacketCheck(p._1, p._2)).filter(_.isDefined)
          if (xs.nonEmpty) xs.head
          else if (sub1.length != sub2.length) Some(sub1.length < sub2.length)
          else None
      }
    }

    recursivePacketCheck(a, b).get
  }


  def part1(pairs: Iterable[(Packet, Packet)]): Int =
    pairs
      .zip(LazyList.from(1))
      .filter(p => validPacketPair(p._1._1, p._1._2))
      .map(_._2).sum

  def part2(packets: List[Packet], dividers: List[Packet]): Int = {
    (packets ++ dividers)
      .sortWith(validPacketPair)
      .zip(LazyList.from(1))
      .filter(p => dividers.contains(p._1))
      .map(_._2)
      .product
  }
}
