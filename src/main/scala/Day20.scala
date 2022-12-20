object Day20 extends IDay {
  override def execute(input: String): (Long, Long) = {
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

    def negModN(index: Long): Int = negMod(index, n)

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

      def determineNewChanges: ChangeStruct = {
        val allMove: Long = -sequenceValue / (n - 1)
        val someMove: Long = allMove + (if (sequenceValue <= 0) 1 else -1)
        val extraMoveCount: Int = negMod(sequenceValue.abs, n - 1)
        new ChangeStruct(sequenceValue, allMove, someMove, extraMoveCount, startIndex, n)
      }

      val newChanges: ChangeStruct = determineNewChanges

      def newChangesOriginalValue(index: Int): Long = newChanges.getAtIndex(negModN(currentIndex(index)))

      changes = changes.zip((0 until n).map(newChangesOriginalValue)).map(p => p._1 + p._2)
    }

    (0 until repeats).foreach(_ => originalSequence.zipWithIndex.foreach(processMove))
    reconstructSequence
  }

  def groveCoordinates(sequence: Array[Long]): Long = {
    val zeroIndex: Int = sequence.zipWithIndex.find(_._1 == 0).get._2
    (1 to 3).map(n => sequence((1000 * n + zeroIndex) % sequence.length)).sum
  }

  def part1(originalSequence: Array[Long]): Long = groveCoordinates(mixSequence(originalSequence, 1))

  def part2(originalSequence: Array[Long]): Long = {
    val decryptionKey: Long = 811589153
    val decryptSequence: Array[Long] = originalSequence.map(_ * decryptionKey)
    groveCoordinates(mixSequence(decryptSequence, 10))
  }


  class ChangeStruct(val sequenceValue: Long, val allMove: Long, val someMove: Long, val extraMoveCount: Int,
                     val startIndex: Int, val n: Int) {
    def getAtIndex(i: Int): Long = if (sequenceValue >= 0) getAtIndexPositive(i) else getAtIndexNegative(i)

    def getAtIndexPositive(i: Int): Long = {
      val rightExtra: Int = Math.min(n - startIndex - 1, extraMoveCount)
      val leftExtra: Int = Math.max(0, extraMoveCount - rightExtra)

      if (i < leftExtra) someMove
      else if (i < startIndex) allMove
      else if (i == startIndex) sequenceValue
      else if (i > startIndex + rightExtra) allMove
      else someMove
    }

    def getAtIndexNegative(i: Int): Long = {
      val leftExtra: Int = Math.min(startIndex, extraMoveCount)
      val rightExtra: Int = Math.max(0, extraMoveCount - leftExtra)

      if (i < startIndex - leftExtra) allMove
      else if (i < startIndex) someMove
      else if (i == startIndex) sequenceValue
      else if (i >= n - rightExtra) someMove
      else allMove
    }
  }
}
