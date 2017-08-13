package scalatriangles

/** Factory for [[scalatriangles.Triangle]] objects. */
object Triangle {

  /** Creates an appropriate [[scalatriangles.Triangle]] object based on the edge lengths provided.
    *
    * @param a the length of first edge of the triangle
    * @param b the length of second edge of the triangle
    * @param c the length of third edge of the triangle
    * @return a new instance of [[scalatriangles.Triangle.ScaleneTriangle]], [[scalatriangles.Triangle.IsoscelesTriangle]] or [[scalatriangles.Triangle.EquilateralTriangle]]
    */
  def apply(a: Double, b: Double, c: Double): Triangle = {
    if (valid_triangle(a, b, c)) {
      if (a == b && b == c)
        new EquilateralTriangle(a)
      else if ((a == b) || (b == c) || (a == c)) {
        new IsoscelesTriangle(a, b, c)
      }
      else
        new ScaleneTriangle(a, b, c)
    }
    else
      throw new IllegalArgumentException("Edges do not form a valid triangle")
  }

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

  /** A triangle with all edges of the same length.
    *
    * @param a the edge length
    */
  class EquilateralTriangle private[Triangle](a: Double) extends Triangle(a, a, a)

  /** A triangle with two edges of the same length and one of different length.
    *
    * @param a the length of first edge of the triangle
    * @param b the length of second edge of the triangle
    * @param c the length of third edge of the triangle
    */
  class IsoscelesTriangle private[Triangle](a: Double, b: Double, c: Double) extends Triangle(a, b, c)

  /** A triangle with all edges of different length.
    *
    * @param a the length of first edge of the triangle
    * @param b the length of second edge of the triangle
    * @param c the length of third edge of the triangle
    */
  class ScaleneTriangle private[Triangle](a: Double, b: Double, c: Double) extends Triangle(a, b, c)

}

/** A generic triangle.
  *
  * @param a the length of first edge of the triangle
  * @param b the length of second edge of the triangle
  * @param c the length of third edge of the triangle
  */
sealed abstract case class Triangle(a: Double, b: Double, c: Double) extends Polygon
