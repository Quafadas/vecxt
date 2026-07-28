package vecxt

case class VectorDimensionMismatch(givenDimension: Int, requiredDimension: Int)
    extends Exception(
      s"Expected Vector dimensions to match. First dimension was : $requiredDimension, second was : $givenDimension ."
    )

protected[vecxt] object dimCheckLen:
  inline def apply[A](a: Array[A], b: Int) =
    if a.length != b then throw VectorDimensionMismatch(a.length, b)
end dimCheckLen

protected[vecxt] object indexCheck:
  inline def apply[A](a: Array[Double], idx: Int) =
    if !(idx < a.length && idx >= 0) then
      throw java.lang.IndexOutOfBoundsException(s"Array of length : ${a.length} cannot be indexed at $idx")
end indexCheck

protected[vecxt] object dimCheck:
  inline def apply[A](a: Array[Double], b: scala.scalajs.js.Array[A]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply[A](a: Array[A], b: Array[Boolean]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Double], b: Array[Double]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Int], b: Array[Int]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Double], b: Array[Boolean]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Int], b: Array[Boolean]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Float], b: Array[Float]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Float], b: Array[Boolean]) =
    if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

end dimCheck
