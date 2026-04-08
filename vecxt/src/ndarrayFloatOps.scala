package vecxt

import scala.annotation.targetName

import vecxt.broadcast.*
import vecxt.ndarray.*

object NDArrayFloatOps:

  // ── General-case iteration kernels ──────────────────────────────────────

  private[NDArrayFloatOps] def binaryOpGeneral(
      a: NDArray[Float],
      b: NDArray[Float],
      f: (Float, Float) => Float
  ): NDArray[Float] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Float](n)
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
  end binaryOpGeneral

  private[NDArrayFloatOps] def unaryOpGeneral(a: NDArray[Float], f: Float => Float): NDArray[Float] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Float](n)
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
      out(j) = f(a.data(posA))
      j += 1
    end while
    mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
  end unaryOpGeneral

  private[NDArrayFloatOps] def compareGeneral(
      a: NDArray[Float],
      b: NDArray[Float],
      f: (Float, Float) => Boolean
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
  end compareGeneral

  private[NDArrayFloatOps] def compareScalarGeneral(
      a: NDArray[Float],
      s: Float,
      f: (Float, Float) => Boolean
  ): NDArray[Boolean] =
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
      out(j) = f(a.data(posA), s)
      j += 1
    end while
    mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
  end compareScalarGeneral

  private[NDArrayFloatOps] def binaryOpInPlaceGeneral(
      a: NDArray[Float],
      b: NDArray[Float],
      f: (Float, Float) => Float
  ): Unit =
    val n = a.numel
    val ndim = a.ndim
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
      a.data(posA) = f(a.data(posA), b.data(posB))
      j += 1
    end while
  end binaryOpInPlaceGeneral

  // ── Flat-array loop helpers (col-major fast path) ────────────────────────

  private inline def flatBinaryOp(
      aData: Array[Float],
      bData: Array[Float],
      inline f: (Float, Float) => Float
  ): Array[Float] =
    val n = aData.length
    val out = new Array[Float](n)
    var i = 0
    while i < n do
      out(i) = f(aData(i), bData(i))
      i += 1
    end while
    out
  end flatBinaryOp

  private inline def flatUnaryOp(data: Array[Float], inline f: Float => Float): Array[Float] =
    val n = data.length
    val out = new Array[Float](n)
    var i = 0
    while i < n do
      out(i) = f(data(i))
      i += 1
    end while
    out
  end flatUnaryOp

  private inline def flatBinaryCompare(
      aData: Array[Float],
      bData: Array[Float],
      inline f: (Float, Float) => Boolean
  ): Array[Boolean] =
    val n = aData.length
    val out = new Array[Boolean](n)
    var i = 0
    while i < n do
      out(i) = f(aData(i), bData(i))
      i += 1
    end while
    out
  end flatBinaryCompare

  private inline def flatScalarCompare(
      data: Array[Float],
      s: Float,
      inline f: (Float, Float) => Boolean
  ): Array[Boolean] =
    val n = data.length
    val out = new Array[Boolean](n)
    var i = 0
    while i < n do
      out(i) = f(data(i), s)
      i += 1
    end while
    out
  end flatScalarCompare

  // ── Extension methods on NDArray[Float] ─────────────────────────────────

  extension (a: NDArray[Float])

    // ── Binary ops (same shape required) ──────────────────────────────────

    /** Element-wise addition. Operands must have the same shape. */
    inline def +(b: NDArray[Float]): NDArray[Float] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Binary op + requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryOp(a.data, b.data, _ + _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryOpGeneral(a, b, _ + _)
      end if
    end +

    /** Element-wise subtraction. Operands must have the same shape. */
    inline def -(b: NDArray[Float]): NDArray[Float] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Binary op - requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryOp(a.data, b.data, _ - _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryOpGeneral(a, b, _ - _)
      end if
    end -

    /** Element-wise multiplication. Operands must have the same shape. */
    inline def *(b: NDArray[Float]): NDArray[Float] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Binary op * requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryOp(a.data, b.data, _ * _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryOpGeneral(a, b, _ * _)
      end if
    end *

    /** Element-wise division. Operands must have the same shape. */
    inline def /(b: NDArray[Float]): NDArray[Float] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Binary op / requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryOp(a.data, b.data, _ / _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryOpGeneral(a, b, _ / _)
      end if
    end /

    // ── Scalar binary ops ──────────────────────────────────────────────────

    /** Add scalar `s` to every element. */
    inline def +(s: Float): NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ + s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ + s)

    /** Subtract scalar `s` from every element. */
    inline def -(s: Float): NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ - s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ - s)

    /** Multiply every element by scalar `s`. */
    inline def *(s: Float): NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ * s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ * s)

    /** Divide every element by scalar `s`. */
    inline def /(s: Float): NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ / s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ / s)

    // ── Unary ops ──────────────────────────────────────────────────────────

    /** Element-wise negation. */
    inline def neg: NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => -x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => -x)

    /** Element-wise absolute value. */
    inline def abs: NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => Math.abs(x.toDouble).toFloat), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => Math.abs(x.toDouble).toFloat)

    /** Element-wise natural exponential. */
    inline def exp: NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => Math.exp(x.toDouble).toFloat), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => Math.exp(x.toDouble).toFloat)

    /** Element-wise natural logarithm. */
    inline def log: NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => Math.log(x.toDouble).toFloat), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => Math.log(x.toDouble).toFloat)

    /** Element-wise square root. */
    inline def sqrt: NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => Math.sqrt(x.toDouble).toFloat), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => Math.sqrt(x.toDouble).toFloat)

    /** Element-wise hyperbolic tangent. */
    inline def tanh: NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => Math.tanh(x.toDouble).toFloat), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => Math.tanh(x.toDouble).toFloat)

    /** Element-wise sigmoid: `1 / (1 + exp(-x))`. */
    inline def sigmoid: NDArray[Float] =
      val sig = (x: Float) => (1.0 / (1.0 + Math.exp(-x.toDouble))).toFloat
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, sig), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, sig)
      end if
    end sigmoid

    // ── In-place binary ops ────────────────────────────────────────────────

    /** In-place element-wise addition. `a` must be contiguous; operands must have the same shape. */
    inline def +=(b: NDArray[Float]): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"In-place += requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        val n = a.numel
        var i = 0
        while i < n do
          a.data(i) += b.data(i)
          i += 1
        end while
      else binaryOpInPlaceGeneral(a, b, _ + _)
      end if
    end +=

    /** In-place element-wise subtraction. `a` must be contiguous; operands must have the same shape. */
    inline def -=(b: NDArray[Float]): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"In-place -= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        val n = a.numel
        var i = 0
        while i < n do
          a.data(i) -= b.data(i)
          i += 1
        end while
      else binaryOpInPlaceGeneral(a, b, _ - _)
      end if
    end -=

    /** In-place element-wise multiplication. `a` must be contiguous; operands must have the same shape. */
    inline def *=(b: NDArray[Float]): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"In-place *= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        val n = a.numel
        var i = 0
        while i < n do
          a.data(i) *= b.data(i)
          i += 1
        end while
      else binaryOpInPlaceGeneral(a, b, _ * _)
      end if
    end *=

    /** In-place element-wise division. `a` must be contiguous; operands must have the same shape. */
    inline def /=(b: NDArray[Float]): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"In-place /= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        val n = a.numel
        var i = 0
        while i < n do
          a.data(i) /= b.data(i)
          i += 1
        end while
      else binaryOpInPlaceGeneral(a, b, _ / _)
      end if
    end /=

    // ── In-place scalar ops ────────────────────────────────────────────────

    /** Add scalar `s` to every element in place. `a` must be contiguous. */
    inline def +=(s: Float): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) += s
        i += 1
      end while
    end +=

    /** Subtract scalar `s` from every element in place. `a` must be contiguous. */
    inline def -=(s: Float): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) -= s
        i += 1
      end while
    end -=

    /** Multiply every element by scalar `s` in place. `a` must be contiguous. */
    inline def *=(s: Float): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) *= s
        i += 1
      end while
    end *=

    /** Divide every element by scalar `s` in place. `a` must be contiguous. */
    inline def /=(s: Float): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) /= s
        i += 1
      end while
    end /=

    // ── Comparison ops (array vs array) ───────────────────────────────────

    /** Element-wise greater-than. Operands must have the same shape. */
    inline def >(b: NDArray[Float]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Comparison > requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryCompare(a.data, b.data, _ > _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareGeneral(a, b, _ > _)
      end if
    end >

    /** Element-wise less-than. Operands must have the same shape. */
    inline def <(b: NDArray[Float]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Comparison < requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryCompare(a.data, b.data, _ < _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareGeneral(a, b, _ < _)
      end if
    end <

    /** Element-wise greater-than-or-equal. Operands must have the same shape. */
    inline def >=(b: NDArray[Float]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Comparison >= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryCompare(a.data, b.data, _ >= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareGeneral(a, b, _ >= _)
      end if
    end >=

    /** Element-wise less-than-or-equal. Operands must have the same shape. */
    inline def <=(b: NDArray[Float]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Comparison <= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryCompare(a.data, b.data, _ <= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareGeneral(a, b, _ <= _)
      end if
    end <=

    /** Element-wise equality. Operands must have the same shape. */
    inline def =:=(b: NDArray[Float]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Comparison =:= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryCompare(a.data, b.data, _ == _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareGeneral(a, b, _ == _)
      end if
    end =:=

    /** Element-wise inequality. Operands must have the same shape. */
    inline def !:=(b: NDArray[Float]): NDArray[Boolean] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Comparison !:= requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryCompare(a.data, b.data, _ != _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareGeneral(a, b, _ != _)
      end if
    end !:=

    // ── Comparison ops (array vs scalar) ──────────────────────────────────

    /** Element-wise greater-than scalar. */
    inline def >(s: Float): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ > _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ > _)

    /** Element-wise less-than scalar. */
    inline def <(s: Float): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ < _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ < _)

    /** Element-wise greater-than-or-equal scalar. */
    inline def >=(s: Float): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ >= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ >= _)

    /** Element-wise less-than-or-equal scalar. */
    inline def <=(s: Float): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ <= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ <= _)

    /** Element-wise equality with scalar. */
    inline def =:=(s: Float): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ == _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ == _)

    /** Element-wise inequality with scalar. */
    inline def !:=(s: Float): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ != _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ != _)

  end extension

  // ── Left-scalar ops (`scalar op ndarray`) ─────────────────────────────────

  extension (s: Float)

    /** Scalar + NDArray[Float]: equivalent to `arr + s`. */
    @targetName("floatPlusNDArray")
    inline def +(a: NDArray[Float]): NDArray[Float] = a + s

    /** Scalar - NDArray[Float]: `s - arr(i)` for each element. */
    @targetName("floatMinusNDArray")
    inline def -(a: NDArray[Float]): NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => s - x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => s - x)

    /** Scalar * NDArray[Float]: equivalent to `arr * s`. */
    @targetName("floatTimesNDArray")
    inline def *(a: NDArray[Float]): NDArray[Float] = a * s

    /** Scalar / NDArray[Float]: `s / arr(i)` for each element. */
    @targetName("floatDivNDArray")
    inline def /(a: NDArray[Float]): NDArray[Float] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => s / x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => s / x)

  end extension

end NDArrayFloatOps
