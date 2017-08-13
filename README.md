# triangles

Two simple solutions to determining the type of the triangle, based on the lengths of its edges.

## Erlang 

Erlang solutions saves as a baseline.
* `triangle` module allows for creation of new triangle structures and can be extended with more API for manipulating triangles. This is standard for creating data structures and modules operating on them in Erlang
* the triangle type can be determined using `triangle` module API as an atom
* eUnit unit tests for validity
* `simple_input` is made for simplicity sake with a try/catch block - in normal operations a process that attempts to create an invalid triangle should crash with an exception in order to fail early and identify the source of invalid data easily

## Scala

Scala solutions serves as a learning opportunity
* Simple solution given the simple problem specification
* `Triangle` class inherits from `Polygon` allowing for other shapes to be added later
* The edge type of the triangle is calculated upon object creation for efficiency
* `Triangle` is a case class allowing for pattern matching
* `EdgeType` is a sealed class as no other edge types exist in geometry
* Experimenting with ScalaCheck to do Erlang-like performance-based testing

Alternatively, an older commit contains implementation where each triangle type is a different class that inherits from `Triangle` allowing for defining of different behavior for each triangle type. This could be further enhanced with `EdgeType` becoming a trait and its sub-traits being mixed with different triangle types.


