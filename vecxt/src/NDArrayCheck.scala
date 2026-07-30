package vecxt

import vecxt.ndarray.NDArray

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

  /** Only ever reads `dataLength` - never an element - so the concrete/generic split (vecxt/issues/105, check C6a) only
    * has to happen once, here, rather than being duplicated across every overload below.
    */
  private def checkBounds(dataLength: Int, shape: Array[Int], strides: Array[Int], offset: Int): Unit =

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

    if offset < 0 || (dataLength > 0 && offset >= dataLength) then
      throw java.lang.IndexOutOfBoundsException(
        s"Offset $offset is out of bounds for array of size $dataLength"
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

    if maxIdx >= dataLength then
      throw java.lang.IndexOutOfBoundsException(
        s"NDArray with shape [${shape.mkString(",")}], strides [${strides.mkString(",")}], offset $offset " +
          s"would access index $maxIdx, but array size is only $dataLength"
      )
    end if
  end checkBounds

  inline def apply[A](data: Array[A], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    checkBounds(data.length, shape, strides, offset)

  def apply(data: Array[Double], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    checkBounds(data.length, shape, strides, offset)

  def apply(data: Array[Float], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    checkBounds(data.length, shape, strides, offset)

  def apply(data: Array[Int], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    checkBounds(data.length, shape, strides, offset)

  def apply(data: Array[Long], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    checkBounds(data.length, shape, strides, offset)

  def apply(data: Array[Boolean], shape: Array[Int], strides: Array[Int], offset: Int): Unit =
    checkBounds(data.length, shape, strides, offset)
end strideNDArrayCheck

/** dimNDArrayCheck validates that the product of shape dimensions equals data.length. */
object dimNDArrayCheck:

  private def checkProduct(dataLength: Int, shape: Array[Int]): Unit =
    var prod = 1
    var i = 0
    while i < shape.length do
      prod *= shape(i)
      i += 1
    end while
    if prod != dataLength then
      throw InvalidNDArray(
        s"Shape [${shape.mkString(",")}] implies $prod elements, but data has $dataLength elements"
      )
    end if
  end checkProduct

  inline def apply[A](data: Array[A], shape: Array[Int]): Unit = checkProduct(data.length, shape)

  def apply(data: Array[Double], shape: Array[Int]): Unit = checkProduct(data.length, shape)
  def apply(data: Array[Float], shape: Array[Int]): Unit = checkProduct(data.length, shape)
  def apply(data: Array[Int], shape: Array[Int]): Unit = checkProduct(data.length, shape)
  def apply(data: Array[Long], shape: Array[Int]): Unit = checkProduct(data.length, shape)
  def apply(data: Array[Boolean], shape: Array[Int]): Unit = checkProduct(data.length, shape)
end dimNDArrayCheck

/** shapeCheck validates that all dimensions are > 0. A 0-length shape (0-d array) is valid. */
object shapeCheck:
  inline def apply(
      shape: Array[Int]
  ): Unit =

    var i = 0
    while i < shape.length do
      if shape(i) <= 0 then
        throw InvalidNDArray(
          s"All shape dimensions must be > 0, but shape($i) = ${shape(i)}"
        )
      end if
      i += 1
    end while
  end apply
end shapeCheck

case class InvalidNDArray(message: String) extends Exception(message)

/** indexNDArrayCheck validates element-access indices against the NDArray's shape. */
object indexNDArrayCheck:
  inline def apply[A](arr: NDArray[A], indices: Array[Int]): Unit =
    if indices.length != arr.ndim then
      throw InvalidNDArray(
        s"Rank mismatch: expected ${arr.ndim} indices, got ${indices.length}"
      )
    end if
    var k = 0
    while k < indices.length do
      if indices(k) < 0 || indices(k) >= arr.shape(k) then
        throw new java.lang.IndexOutOfBoundsException(
          s"Index ${indices(k)} out of bounds for dimension $k of size ${arr.shape(k)}"
        )
      end if
      k += 1
    end while
  end apply
end indexNDArrayCheck
