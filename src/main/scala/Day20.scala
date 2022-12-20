object Day20 extends IDay {
  override def execute(input: String): (Any, Any) = {
    val sequence: Array[Long] = Helper.readLinesInt(input).map(_.toLong).toArray
    (part1(sequence), part2(sequence))
  }

  def mixSequence(originalSequence: Array[Long], repeats: Int): Array[Long] = {
    val n: Int = originalSequence.length
    var changes: Array[Long] = Array.fill(originalSequence.length)(0)

    def negMod(a: Long, divisor: Long): Int = {
      val x = a % divisor
      (if (x >= 0) x else x + divisor).toInt
    }

    def negModN(index: Long): Int = {
      negMod(index, n)
    }

    def currentIndex(originalIndex: Int): Int = negModN(originalIndex + changes(originalIndex))

    def reconstructSequence: Array[Long] = {
      originalSequence
        .zipWithIndex
        .zip(changes)
        .sortBy(pair => negModN(pair._1._2 + pair._2))
        .map(_._1._1)
    }

    def processMove(pair: (Long, Int)): Unit = {
      val sequenceValue: Long = pair._1
      val originalIndex: Int = pair._2

      val startIndex: Int = currentIndex(originalIndex)
      val targetIndex: Int = negModN(startIndex + sequenceValue)

      def determineNewChanges: Array[Long] = {
        val allMove: Long = -sequenceValue / (n - 1)
        val someMove: Long = allMove + (if (sequenceValue <= 0) 1 else -1)
        val extraMoveCount: Int = negMod(sequenceValue.abs, n - 1)
        if (sequenceValue >= 0) {
          val rightExtra: Int = Math.min(n - startIndex - 1, extraMoveCount)
          val leftExtra: Int = Math.max(0, extraMoveCount - rightExtra)
          Array.fill(leftExtra)(someMove) ++ Array.fill(startIndex - leftExtra)(allMove) ++ Array(sequenceValue) ++
            Array.fill(rightExtra)(someMove) ++ Array.fill(n - startIndex - rightExtra - 1)(allMove)
        } else {
          val leftExtra: Int = Math.min(startIndex, extraMoveCount)
          val rightExtra: Int = Math.max(0, extraMoveCount - leftExtra)
          Array.fill(startIndex - leftExtra)(allMove) ++ Array.fill(leftExtra)(someMove) ++ Array(sequenceValue) ++
            Array.fill(n - startIndex - rightExtra - 1)(allMove) ++ Array.fill(rightExtra)(someMove)
        }
      }

      val newChanges: Array[Long] = determineNewChanges

      if (newChanges.sum != 0)
        sys.error("new changes sum nonzero, actual sum " + newChanges.sum)
      if (newChanges.length != n)
        sys.error("new changes incorrect length")

      def newChangesUnshuffleValue(index: Int): Long = {
        newChanges(negModN(currentIndex(index)))
      }

      val unshuffledNewChanges: Array[Long] = (0 until n).map(newChangesUnshuffleValue).toArray

      changes = changes.zip(unshuffledNewChanges).map(p => p._1 + p._2)
    }

    (0 until repeats).foreach({n =>
      originalSequence.zipWithIndex.foreach(processMove)
    })
    reconstructSequence
  }

  def part1(originalSequence: Array[Long]): Long = {
    val sequence: Array[Long] = mixSequence(originalSequence, 1)
    if (sequence.length != originalSequence.length)
      sys.error("mismatched sequence length")
    val zeroIndex: Int = sequence.zipWithIndex.find(_._1 == 0).get._2
    (1 to 3).map(n => sequence((1000 * n + zeroIndex) % sequence.length)).sum
  }

  def part2(originalSequence: Array[Long]): Long = {
    val decryptionKey: Long = 811589153
    val sequence: Array[Long] = mixSequence(originalSequence.map(_ * decryptionKey), 10)

    val zeroIndex: Int = sequence.zipWithIndex.find(_._1 == 0).get._2
    (1 to 3).map(n => sequence((1000 * n + zeroIndex) % sequence.length)).sum
  }

}
