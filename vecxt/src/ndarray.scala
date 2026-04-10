package vecxt

import scala.annotation.publicInBinary

import vecxt.BoundsCheck.BoundsCheck

object ndarray:

  class NDArray[A] @publicInBinary() private[ndarray] (
      val data: Array[A],
      val shape: Array[Int],
      val strides: Array[Int],
      val offset: Int
  ):

    lazy val ndim: Int = shape.length

    lazy val numel: Int =
      var prod = 1
      var i = 0
      while i < shape.length do
        prod *= shape(i)
        i += 1
      end while
      prod
    end numel

    lazy val isContiguous: Boolean = isColMajor || isRowMajor

    lazy val isColMajor: Boolean =
      // Column-major (F-order): strides = [1, shape(0), shape(0)*shape(1), ...]
      // and data.length == numel and offset == 0
      if offset != 0 then false
      else if shape.length == 0 then true
      else
        var expected = 1
        var result = true
        var i = 0
        while i < strides.length do
          if strides(i) != expected then result = false
          end if
          expected *= shape(i)
          i += 1
        end while
        result && data.length == numel

    lazy val isRowMajor: Boolean =
      // Row-major (C-order): strides = [..., shape(n-1), 1]
      if offset != 0 then false
      else if shape.length == 0 then true
      else
        var expected = 1
        var result = true
        var i = strides.length - 1
        while i >= 0 do
          if strides(i) != expected then result = false
          end if
          expected *= shape(i)
          i -= 1
        end while
        result && data.length == numel

    /** True if this is a 0-dimensional (scalar) NDArray. */
    lazy val isScalar: Boolean = shape.length == 0

    lazy val layout: String =
      s"ndim: $ndim, shape: [${shape.mkString(",")}], strides: [${strides.mkString(",")}], offset: $offset, data length: ${data.length}"

  end NDArray

  object NDArray:

    // Primary constructor — full control
    inline def apply[A](
        data: Array[A],
        shape: Array[Int],
        strides: Array[Int],
        offset: Int = 0
    )(using inline boundsCheck: BoundsCheck): NDArray[A] =
      strideNDArrayCheck(data, shape, strides, offset)
      new NDArray(data, shape, strides, offset)
    end apply

    // Convenience: column-major from data + shape
    inline def apply[A](
        data: Array[A],
        shape: Array[Int]
    )(using inline boundsCheck: BoundsCheck): NDArray[A] =
      dimNDArrayCheck(data, shape)
      new NDArray(data, shape, colMajorStrides(shape), 0)
    end apply

    // 1D from flat array
    inline def fromArray[A](
        data: Array[A]
    )(using inline boundsCheck: BoundsCheck): NDArray[A] =
      new NDArray(data, Array(data.length), Array(1), 0)

    /** Create a 0-dimensional (scalar) NDArray holding a single value. */
    inline def scalar[A](value: A)(using ct: scala.reflect.ClassTag[A]): NDArray[A] =
      new NDArray(Array(value), Array.emptyIntArray, Array.emptyIntArray, 0)

    inline def zeros[A](
        shape: Array[Int]
    )(using inline boundsCheck: BoundsCheck, oz: OneAndZero[A], ct: scala.reflect.ClassTag[A]): NDArray[A] =
      shapeCheck(shape)
      val n = shapeProduct(shape)
      val data = Array.fill[A](n)(oz.zero)
      new NDArray(data, shape.clone(), colMajorStrides(shape), 0)
    end zeros

    inline def ones[A](
        shape: Array[Int]
    )(using inline boundsCheck: BoundsCheck, oz: OneAndZero[A], ct: scala.reflect.ClassTag[A]): NDArray[A] =
      shapeCheck(shape)
      val n = shapeProduct(shape)
      val data = Array.fill[A](n)(oz.one)
      new NDArray(data, shape.clone(), colMajorStrides(shape), 0)
    end ones

    inline def fill[A](
        shape: Array[Int],
        value: A
    )(using inline boundsCheck: BoundsCheck, ct: scala.reflect.ClassTag[A]): NDArray[A] =
      shapeCheck(shape)
      val n = shapeProduct(shape)
      val data = Array.fill[A](n)(value)
      new NDArray(data, shape.clone(), colMajorStrides(shape), 0)
    end fill

  end NDArray

  extension [A](arr: NDArray[A]) inline def shapeArray: Array[Int] = arr.shape
  end extension

  /** Package-private factory — creates NDArray without bounds checking. Used by operations that have already validated
    * invariants (slice, transpose, etc.).
    */
  private[vecxt] inline def mkNDArray[A](
      data: Array[A],
      shape: Array[Int],
      strides: Array[Int],
      offset: Int
  ): NDArray[A] = new NDArray(data, shape, strides, offset)

  // Compute column-major strides for a given shape
  private[vecxt] inline def colMajorStrides(shape: Array[Int]): Array[Int] =
    val strides = new Array[Int](shape.length)
    if shape.length > 0 then
      strides(0) = 1
      var i = 1
      while i < shape.length do
        strides(i) = strides(i - 1) * shape(i - 1)
        i += 1
      end while
    end if
    strides
  end colMajorStrides

  // Product of shape elements
  private[vecxt] inline def shapeProduct(shape: Array[Int]): Int =
    var prod = 1
    var i = 0
    while i < shape.length do
      prod *= shape(i)
      i += 1
    end while
    prod
  end shapeProduct

end ndarray
