package scalatriangles


import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.prop._
import org.scalatest.PropSpec

class TriangleProperty extends PropSpec with PropertyChecks {
  private val validEdge = Gen.choose(1, 50) // choose is necessary, otherwise the random spread is too large to create real triangles

  private val triangleEdges = for {
    e1 <- validEdge
    e2 <- validEdge
    e3 <- validEdge
  } yield (e1, e2, e3)

  private val triangle = triangleEdges suchThat { case (a, b, c) => (a + b) > c && (a + c) > b && (b + c) > a }


  property("validTriangle") {
    forAll(triangle) {
      case (a, b, c) => Triangle(a.toDouble, b.toDouble, c.toDouble)
        true
    }
  }

  property("equilateralTriangle") {
    forAll(validEdge) {
      (edge: Int) =>
        val floatEdge = edge.toDouble
        val t = Triangle(floatEdge, floatEdge, floatEdge)
        t match {
          case _: Triangle.EquilateralTriangle => true
          case _ => false
        }
    }
  }


  property("isoscelesTriangle") {
    forAll(validEdge, validEdge) {
      (edge: Int, base: Int) =>
        (2 * edge > base && edge + base > edge && base != edge) ==> {
          Triangle(base.toDouble, edge.toDouble, edge.toDouble) match {
            case _: Triangle.IsoscelesTriangle => true
            case _ => false
          }
        }
    }
  }


  property("scaleneTriangle") {
    forAll(triangle) {
      case (a, b, c) =>
        (a != b && a != c && b != c) ==> {
          Triangle(a.toDouble, b.toDouble, c.toDouble) match {
            case _: Triangle.ScaleneTriangle => true
            case _ => false
          }
        }
    }
  }

}
