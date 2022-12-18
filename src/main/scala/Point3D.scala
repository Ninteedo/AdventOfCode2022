class Point3D(val x: Int, val y: Int, val z: Int) {
  def +(other: Point3D) = new Point3D(x + other.x, y + other.y, z + other.z)

  def -(other: Point3D) = new Point3D(x - other.x, y - other.y, z - other.z)

  def *(factor: Int) = new Point3D(x * factor, y * factor, z * factor)

  def /(divisor: Int) = new Point3D(x / divisor, y / divisor, z / divisor)

  def mannDist(other: Point3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  def inVolume(vertex1: Point3D, vertex2: Point3D): Boolean =
    vertex1.x <= x && x <= vertex2.x && vertex1.y <= y && y <= vertex2.y && vertex1.z <= z && z <= vertex2.z

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point3D]

  override def equals(other: Any): Boolean = other match {
    case that: Point3D =>
        x == that.x &&
        y == that.y &&
        z == that.z
    case _ => false
  }

  override def hashCode(): Int = {
    x * 961 + y * 31 + z
  }

  override def toString = s"Point3D($x, $y, $z)"
}

object Point3D {
  val zero: Point3D = new Point3D(0, 0, 0)

  val directions: LazyList[Point3D] = LazyList(xVec(1), xVec(-1), yVec(1), yVec(-1), zVec(1), zVec(-1))

  def xVec(x: Int): Point3D = new Point3D(x, 0, 0)

  def yVec(y: Int): Point3D = new Point3D(0, y, 0)

  def zVec(z: Int): Point3D = new Point3D(0, 0, z)
}
