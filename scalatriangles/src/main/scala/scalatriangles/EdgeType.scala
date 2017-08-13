package scalatriangles


/** The enumeration base for the edge types of the triangles.
  */
sealed abstract class EdgeType {}

/** A triangle with all edges of different lengths */
case object Scalene extends EdgeType
/** A triangle with two edges of the same length */
case object Isosceles extends EdgeType
/** A triangle with all edges of the same length */
case object Equilateral extends EdgeType
