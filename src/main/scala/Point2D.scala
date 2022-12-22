class Point2D(val x: Int, val y: Int) {
  def +(other: Point2D) = new Point2D(x + other.x, y + other.y)

  def -(other: Point2D) = new Point2D(x - other.x, y - other.y)

  def *(other: Point2D) = new Point2D(x * other.x, y * other.y)

  def *(factor: Int) = new Point2D(x * factor, y * factor)

  def /(divisor: Int) = new Point2D(x / divisor, y / divisor)

  def dotProduct(other: Point2D): Int = x * other.x + y * other.y

  def mannDist(other: Point2D): Int = (x - other.x).abs + (y - other.y).abs

  def inArea(topLeft: Point2D, bottomRight: Point2D): Boolean =
    topLeft.x <= x && x <= bottomRight.x && topLeft.y <= y && y <= bottomRight.y

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point2D]

  override def equals(other: Any): Boolean = other match {
    case that: Point2D =>
        x == that.x &&
        y == that.y
    case _ => false
  }

  override def hashCode(): Int = {
    x * 31 + y
  }

  override def toString = s"Point2D($x, $y)"
}

object Point2D {
  val zero: Point2D = new Point2D(0, 0)

  val directions: LazyList[Point2D] = LazyList(xVec(1), xVec(-1), yVec(1), yVec(-1))

  def xVec(x: Int): Point2D = new Point2D(x, 0)

  def yVec(y: Int): Point2D = new Point2D(0, y)
}
