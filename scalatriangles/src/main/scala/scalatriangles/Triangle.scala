package scalatriangles

/** Factory for [[scalatriangles.Triangle]] objects. */
object Triangle {
  /**
    * Checks if the given edges form a valid triangle.
    *
    * @param a the length of first edge of the triangle
    * @param b the length of second edge of the triangle
    * @param c the length of third edge of the triangle
    * @return whether the edges can form a valid triangle
    */
  private def valid_triangle(a: Double, b: Double, c: Double) = {
    if ((a + b) > c && (b + c) > a && (a + c) > b) true
    else false
  }
}

/** A generic triangle shape.
  *
  * @param a the length of first edge of the triangle
  * @param b the length of second edge of the triangle
  * @param c the length of third edge of the triangle
  */
case class Triangle(a: Double, b: Double, c: Double) extends Polygon {

  val edgeType: EdgeType =
    if (a == b && b == c) Equilateral
    else if ((a == b) || (b == c) || (a == c)) Isosceles
    else Scalene

  if (!Triangle.valid_triangle(a, b, c)) throw new IllegalArgumentException("Edges do not form a valid triangle")
}
