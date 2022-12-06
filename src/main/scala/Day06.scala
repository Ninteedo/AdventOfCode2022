import scala.collection.mutable

object Day06 extends IDay {
  override def execute(input: String): (Int, Int) = {
    val dataStream: String = Helper.readLines(input, identity).head
    (part1(dataStream), part2(dataStream))
  }

  def part1(dataStream: String): Int = uniqueSequenceIndex(dataStream, 4)

  def part2(dataStream: String): Int = uniqueSequenceIndex(dataStream, 14)

  def uniqueSequenceIndex(dataStream: String, n: Int): Int = {
    val queue = mutable.Queue[Char]()

    def uniqueQueue(): Boolean = queue.forall(c => queue.count(_ == c) == 1)

    dataStream.to(LazyList).map { c: Char =>
      if (queue.size < n) {
        queue.enqueue(c)
        false
      } else {
        queue.dequeue()
        queue.enqueue(c)
        uniqueQueue()
      }
    }.zipWithIndex
      .filter(_._1)
      .head._2 + 1
  }
}
