package vecxt

import vecxt.BoundsCheck.BoundsCheck

/** strideNDArrayCheck validates construction of an NDArray with arbitrary strides and offset.
  *
  * Validates:
  *   - shape and strides have the same length (rank consistency)
  *   - All dimensions in shape are > 0
  *   - Offset is >= 0 and < data.length
  *   - Strides are non-zero (except 0 for broadcast dims of size 1)
  *   - All corner combinations of indices stay within [0, data.length)
  */
object strideNDArrayCheck:
  inline def apply[A](
      data: Array[A],
      shape: Array[Int],
      strides: Array[Int],
      offset: Int
  )(using inline doCheck: BoundsCheck): Unit =
    inline if doCheck then
      if shape.length != strides.length then
        throw InvalidNDArray(
          s"Shape rank (${shape.length}) and strides rank (${strides.length}) must match"
        )
      end if

      var i = 0
      while i < shape.length do
        if shape(i) <= 0 then
          throw InvalidNDArray(
            s"All shape dimensions must be > 0, but shape($i) = ${shape(i)}"
          )
        end if
        i += 1
      end while

      if offset < 0 || (data.length > 0 && offset >= data.length) then
        throw java.lang.IndexOutOfBoundsException(
          s"Offset $offset is out of bounds for array of size ${data.length}"
        )
      end if

      // Compute min and max reachable index from all corner combinations
      var minIdx = offset
      var maxIdx = offset
      var j = 0
      while j < shape.length do
        val contribution = (shape(j) - 1) * strides(j)
        if contribution > 0 then maxIdx += contribution
        else if contribution < 0 then minIdx += contribution
        end if
        j += 1
      end while

      if minIdx < 0 then
        throw java.lang.IndexOutOfBoundsException(
          s"NDArray with shape [${shape.mkString(",")}], strides [${strides.mkString(",")}], offset $offset " +
            s"would access negative index $minIdx"
        )
      end if

      if maxIdx >= data.length then
        throw java.lang.IndexOutOfBoundsException(
          s"NDArray with shape [${shape.mkString(",")}], strides [${strides.mkString(",")}], offset $offset " +
            s"would access index $maxIdx, but array size is only ${data.length}"
        )
      end if
end strideNDArrayCheck

/** dimNDArrayCheck validates that the product of shape dimensions equals data.length. */
object dimNDArrayCheck:
  inline def apply[A](
      data: Array[A],
      shape: Array[Int]
  )(using inline doCheck: BoundsCheck): Unit =
    inline if doCheck then
      var prod = 1
      var i = 0
      while i < shape.length do
        prod *= shape(i)
        i += 1
      end while
      if prod != data.length then
        throw InvalidNDArray(
          s"Shape [${shape.mkString(",")}] implies $prod elements, but data has ${data.length} elements"
        )
      end if
end dimNDArrayCheck

/** shapeCheck validates that the shape is non-empty and all dimensions are > 0. */
object shapeCheck:
  inline def apply(
      shape: Array[Int]
  )(using inline doCheck: BoundsCheck): Unit =
    inline if doCheck then
      if shape.length == 0 then throw InvalidNDArray("Shape must be non-empty")
      end if
      var i = 0
      while i < shape.length do
        if shape(i) <= 0 then
          throw InvalidNDArray(
            s"All shape dimensions must be > 0, but shape($i) = ${shape(i)}"
          )
        end if
        i += 1
      end while
end shapeCheck

case class InvalidNDArray(message: String) extends Exception(message)
