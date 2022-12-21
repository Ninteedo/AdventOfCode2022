object Day20 extends IDay {
  override def execute(input: String): (Long, Long) = {
    val sequence: Array[Long] = Helper.readLinesInt(input).map(_.toLong).toArray
    (part1(sequence), part2(sequence))
  }

  def negMod(a: Long, divisor: Long): Int = {
    val x = a % divisor
    (if (x >= 0) x else x + divisor).toInt
  }

  def mixSequence(originalSequence: Array[Long], repeats: Int): Array[Long] = {
    val n: Int = originalSequence.length
    val indexedOriginal: Array[(Long, Int)] = originalSequence.zipWithIndex
    var changes: Array[(Int, Int)] = Array.fill(originalSequence.length)(0).zipWithIndex

    def negModN(index: Long): Int = negMod(index, n)

    def currentIndex(originalIndex: Int): Int = negModN(originalIndex + changes(originalIndex)._1)

    def processMove(pair: (Long, Int)): Unit = {
      val sequenceValue: Long = pair._1
      val originalIndex: Int = pair._2

      val startIndex: Int = currentIndex(originalIndex)
      val newChanges: ChangeStruct = new ChangeStruct(sequenceValue, startIndex, n)

      def newChangesOriginalValue(index: Int): Int = newChanges.getAtIndex(currentIndex(index))

      changes = changes.map(pair => (pair._1 + newChangesOriginalValue(pair._2), pair._2))
    }

    (0 until repeats).foreach(_ => indexedOriginal.foreach(processMove))

    indexedOriginal
      .zip(changes)
      .sortBy(pair => negModN(pair._1._2 + pair._2._1))
      .map(_._1._1)
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


  class ChangeStruct(val sequenceValue: Long, val startIndex: Int, val n: Int) {
    val allZero: Boolean = sequenceValue == 0
    val positive: Boolean = sequenceValue > 0
    val sequenceValueModN: Int = (sequenceValue % n).toInt

    val allMove: Int = ((-sequenceValue / (n - 1)) % n).toInt
    val someMove: Int = ((-sequenceValue / (n - 1) + (if (positive) -1 else 1)) % n).toInt
    val extraMoveCount: Int = negMod(sequenceValue.abs, n - 1)

    val (leftExtra, rightExtra): (Int, Int) = getSideExtras

    def getAtIndex(i: Int): Int = if (allZero) 0 else if (positive) getAtIndexPositive(i) else getAtIndexNegative(i)

    def getAtIndexPositive(i: Int): Int = {
      if (i < leftExtra) someMove
      else if (i < startIndex) allMove
      else if (i == startIndex) sequenceValueModN
      else if (i > startIndex + rightExtra) allMove
      else someMove
    }

    def getAtIndexNegative(i: Int): Int = {
      if (i < startIndex - leftExtra) allMove
      else if (i < startIndex) someMove
      else if (i == startIndex) sequenceValueModN
      else if (i >= n - rightExtra) someMove
      else allMove
    }

    def getSideExtras: (Int, Int) = if (allZero) (0, 0)
    else if (positive) {
      val right: Int = Math.min(n - startIndex - 1, extraMoveCount)
      val left: Int = Math.max(0, extraMoveCount - right)
      (left, right)
    } else {
      val left: Int = Math.min(startIndex, extraMoveCount)
      val right: Int = Math.max(0, extraMoveCount - left)
      (left, right)
    }
  }
}
