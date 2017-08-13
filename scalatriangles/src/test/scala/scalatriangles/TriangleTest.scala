package scalatriangles

/**
  * Created by haljin on 8/12/17.
  */

import org.scalatest.FunSuite


class TriangleTest extends FunSuite {
  test("Simple triangle") {
    val t = Triangle(4, 2, 3)
    assert(t.a == 4)
    assert(t.b == 2)
    assert(t.c == 3)
  }

  test("Triangle validity") {
    Triangle(3, 4, 5)
    Triangle(2.3, 2.7, 4.0)
    assertThrows[IllegalArgumentException] {
      Triangle(1, 1, 2)
    }
    assertThrows[IllegalArgumentException] {
      Triangle(0, 1, 3)
    }
    assertThrows[IllegalArgumentException] {
      Triangle(-2, -4, 1)
    }
  }

  test("Triangle types") {
    assert(Triangle(1, 1, 1) match {
      case _: Triangle.EquilateralTriangle => true
      case _ => fail
    })
    assert(Triangle(3, 3, 4) match {
      case _: Triangle.IsoscelesTriangle => true
      case _ => fail
    })
    assert(Triangle(3, 4, 3) match {
      case _: Triangle.IsoscelesTriangle => true
      case _ => fail
    })
    assert(Triangle(4, 3, 3) match {
      case _: Triangle.IsoscelesTriangle => true
      case _ => fail
    })
    assert(Triangle(3, 4, 5) match {
      case _: Triangle.ScaleneTriangle => true
      case _ => fail
    })
  }
}
