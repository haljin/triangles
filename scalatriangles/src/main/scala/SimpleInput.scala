import scala.io.StdIn.readFloat
import scalatriangles._

/** An application for inputting of triangles via cmd line.
  */
object SimpleInput extends App {

  /**
    * Reads the edge length from cmd line.
    * @return a [[Some]] containing the length if the input was correct or [[None]] if not.
    */
  private def read_edge(): Option[Double] = {
    try Some(readFloat())
    catch {
      case _: NumberFormatException => None
    }
  }

  /**
    * Get the edge from the cmd line.
    * @param prompt prompt for the user
    * @return the length of the triangle edge
    */
  private def get_edge(prompt: String): Double = {
    print(prompt)
    read_edge() match {
      case Some(num) => num
      case None => get_edge(prompt)
    }
  }

  println("Please provide the lengths of the three edges of the triangle.")
  val a = get_edge("First edge:")
  val b = get_edge("Second edge:")
  val c = get_edge("Third edge:")

  try
      Triangle(a, b, c).edgeType match {
      case Equilateral => println("Your triangle is equilateral")
      case Isosceles => println("Your triangle is isosceles")
      case Scalene => println("Your triangle is scalene")
    }
  catch {
    case _: IllegalArgumentException => println("That is not a valid triangle")
  }

}
