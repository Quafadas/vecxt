package vecxt

import vecxt.BoundsCheck.BoundsCheck

protected[vecxt] object dimCheckLen:
  inline def apply[A](a: Array[A], b: Int)(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b then throw VectorDimensionMismatch(a.length, b)
end dimCheckLen

protected[vecxt] object dimCheck:
  inline def apply[A, B](a: Array[A], b: Array[B])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply[A](a: Array[A], b: Array[Boolean])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply[A](a: Array[A], b: Array[A])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)

  inline def apply(a: Array[Double], b: Array[Double])(using inline doCheck: BoundsCheck) =
    inline if doCheck then if a.length != b.length then throw VectorDimensionMismatch(a.length, b.length)
end dimCheck

case class VectorDimensionMismatch(givenDimension: Int, requiredDimension: Int)
    extends Exception(
      s"Expected Vector dimensions to match. First dimension was : $requiredDimension, second was : $givenDimension ."
    )
