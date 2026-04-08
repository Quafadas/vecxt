package vecxt

import vecxt.BooleanArrays.*
import vecxt.broadcast.*
import vecxt.ndarray.*

object NDArrayBooleanOps:

  // ── General-case iteration kernels ──────────────────────────────────────

  private[NDArrayBooleanOps] def binaryLogicalGeneral(
      a: NDArray[Boolean],
      b: NDArray[Boolean],
      f: (Boolean, Boolean) => Boolean
  ): NDArray[Boolean] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Boolean](n)
    val cumProd = colMajorStrides(a.shape)
    var j = 0
    while j < n do
      var posA = a.offset
      var posB = b.offset
      var k = 0
      while k < ndim do
        val coord = (j / cumProd(k)) % a.shape(k)
        posA += coord * a.strides(k)
        posB += coord * b.strides(k)
        k += 1
      end while
      out(j) = f(a.data(posA), b.data(posB))
      j += 1
    end while
    mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
  end binaryLogicalGeneral

  private[NDArrayBooleanOps] def unaryLogicalGeneral(a: NDArray[Boolean]): NDArray[Boolean] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Boolean](n)
    val cumProd = colMajorStrides(a.shape)
    var j = 0
    while j < n do
      var posA = a.offset
      var k = 0
      while k < ndim do
        val coord = (j / cumProd(k)) % a.shape(k)
        posA += coord * a.strides(k)
        k += 1
      end while
      out(j) = !a.data(posA)
      j += 1
    end while
    mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
  end unaryLogicalGeneral

  /** Boolean axis reduce — same structure as NDArrayReductions.reduceAxis */
  private[NDArrayBooleanOps] inline def reduceAxisBool(
      a: NDArray[Boolean],
      axis: Int,
      inline initial: Boolean,
      inline f: (Boolean, Boolean) => Boolean
  ): NDArray[Boolean] =
    val outShape = removeAxis(a.shape, axis)
    val outStrides = colMajorStrides(outShape)
    val outN = shapeProduct(outShape)
    val out = Array.fill(outN)(initial)

    val n = a.numel
    val ndim = a.ndim
    val inCumProd = colMajorStrides(a.shape)

    var j = 0
    while j < n do
      var posIn = a.offset
      var posOut = 0
      var outDim = 0
      var k = 0
      while k < ndim do
        val coord = (j / inCumProd(k)) % a.shape(k)
        posIn += coord * a.strides(k)
        if k != axis then
          posOut += coord * outStrides(outDim)
          outDim += 1
        end if
        k += 1
      end while
      out(posOut) = f(out(posOut), a.data(posIn))
      j += 1
    end while

    mkNDArray(out, outShape, colMajorStrides(outShape), 0)
  end reduceAxisBool

  /** countTrue axis reduce — accumulates Int */
  private[NDArrayBooleanOps] inline def countTrueAxis(
      a: NDArray[Boolean],
      axis: Int
  ): NDArray[Int] =
    val outShape = removeAxis(a.shape, axis)
    val outStrides = colMajorStrides(outShape)
    val outN = shapeProduct(outShape)
    val out = new Array[Int](outN)

    val n = a.numel
    val ndim = a.ndim
    val inCumProd = colMajorStrides(a.shape)

    var j = 0
    while j < n do
      var posIn = a.offset
      var posOut = 0
      var outDim = 0
      var k = 0
      while k < ndim do
        val coord = (j / inCumProd(k)) % a.shape(k)
        posIn += coord * a.strides(k)
        if k != axis then
          posOut += coord * outStrides(outDim)
          outDim += 1
        end if
        k += 1
      end while
      if a.data(posIn) then out(posOut) += 1
      end if
      j += 1
    end while

    mkNDArray(out, outShape, colMajorStrides(outShape), 0)
  end countTrueAxis

  private[NDArrayBooleanOps] inline def removeAxis(shape: Array[Int], axis: Int): Array[Int] =
    val result = new Array[Int](shape.length - 1)
    var i = 0
    var j = 0
    while i < shape.length do
      if i != axis then
        result(j) = shape(i)
        j += 1
      end if
      i += 1
    end while
    result
  end removeAxis

  // ── Flat-array loop helpers (col-major fast path) ────────────────────────

  private inline def flatBinaryLogical(
      aData: Array[Boolean],
      bData: Array[Boolean],
      inline f: (Boolean, Boolean) => Boolean
  ): Array[Boolean] =
    val n = aData.length
    val out = new Array[Boolean](n)
    var i = 0
    while i < n do
      out(i) = f(aData(i), bData(i))
      i += 1
    end while
    out
  end flatBinaryLogical

  // ── Extension methods on NDArray[Boolean] ─────────────────────────────────

  extension (a: NDArray[Boolean])

    // ── Logical binary ops (same shape required) ──────────────────────────

    /** Element-wise logical AND. Operands must have the same shape. */
    inline def &&(b: NDArray[Boolean]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Logical && requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryLogical(a.data, b.data, _ && _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryLogicalGeneral(a, b, _ && _)
      end if
    end &&

    /** Element-wise logical OR. Operands must have the same shape. */
    inline def ||(b: NDArray[Boolean]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Logical || requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryLogical(a.data, b.data, _ || _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryLogicalGeneral(a, b, _ || _)
      end if
    end ||

    // ── Unary ops ──────────────────────────────────────────────────────────

    /** Element-wise logical NOT. Returns a new NDArray[Boolean]. */
    inline def not: NDArray[Boolean] =
      if a.isColMajor then mkNDArray(a.data.not, a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryLogicalGeneral(a)

    /** In-place element-wise logical NOT. `a` must be contiguous. */
    inline def `not!`: Unit =
      if !a.isContiguous then throw InvalidNDArray("In-place ops require a contiguous NDArray")
      end if
      a.data.`not!`
    end `not!`

    // ── Full reductions ────────────────────────────────────────────────────

    /** Returns true if any element is true. */
    inline def any: Boolean =
      if a.isContiguous then a.data.any
      else
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var j = 0
        var result = false
        while j < n && !result do
          var pos = a.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % a.shape(k)
            pos += coord * a.strides(k)
            k += 1
          end while
          if a.data(pos) then result = true
          end if
          j += 1
        end while
        result

    /** Returns true if all elements are true. */
    inline def all: Boolean =
      if a.isContiguous then a.data.allTrue
      else
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var j = 0
        var result = true
        while j < n && result do
          var pos = a.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % a.shape(k)
            pos += coord * a.strides(k)
            k += 1
          end while
          if !a.data(pos) then result = false
          end if
          j += 1
        end while
        result

    /** Count of true elements. */
    inline def countTrue: Int =
      if a.isContiguous then a.data.trues
      else
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var acc = 0
        var j = 0
        while j < n do
          var pos = a.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % a.shape(k)
            pos += coord * a.strides(k)
            k += 1
          end while
          if a.data(pos) then acc += 1
          end if
          j += 1
        end while
        acc

    // ── Axis reductions ────────────────────────────────────────────────────

    /** Returns NDArray[Boolean] where each element is true if any element along `axis` is true. */
    inline def any(axis: Int): NDArray[Boolean] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxisBool(a, axis, false, _ || _)
    end any

    /** Returns NDArray[Boolean] where each element is true if all elements along `axis` are true. */
    inline def all(axis: Int): NDArray[Boolean] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxisBool(a, axis, true, _ && _)
    end all

    /** Count true values along `axis`. Returns NDArray[Int]. */
    inline def countTrue(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      countTrueAxis(a, axis)
    end countTrue

  end extension

end NDArrayBooleanOps
