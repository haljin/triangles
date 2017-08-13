import scala.io.StdIn.readFloat
import scalatriangles._
import scalatriangles.Triangle._

object SimpleInput extends App {

  private def read_edge(): Option[Double] = {
    try Some(readFloat())
    catch {
      case _: NumberFormatException => None
    }
  }

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
    Triangle(a, b, c) match {
      case _: EquilateralTriangle => println("Your triangle is equilateral")
      case _: IsoscelesTriangle => println("Your triangle is isosceles")
      case _: ScaleneTriangle => println("Your triangle is scalene")
    }
  catch {
    case _: IllegalArgumentException => println("That is not a valid triangle")
  }

}
