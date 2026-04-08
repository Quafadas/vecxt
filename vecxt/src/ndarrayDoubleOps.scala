package vecxt

import vecxt.broadcast.*
import vecxt.ndarray.*

object NDArrayDoubleOps:

  // ── General-case iteration kernels ──────────────────────────────────────

  /** Binary op kernel for arrays with arbitrary strides (same shape).
    *
    * Iterates all `a.numel` elements in column-major order, computing each element's physical position in `a.data` and
    * `b.data` using their respective strides. Handles broadcast views (stride-0) correctly. Produces a fresh
    * column-major result.
    */
  private[NDArrayDoubleOps] def binaryOpGeneral(
      a: NDArray[Double],
      b: NDArray[Double],
      f: (Double, Double) => Double
  ): NDArray[Double] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Double](n)
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

  /** Unary op kernel for non-column-major arrays. Produces a fresh column-major result. */
  private[NDArrayDoubleOps] def unaryOpGeneral(a: NDArray[Double], f: Double => Double): NDArray[Double] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Double](n)
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

  /** Binary comparison kernel for arrays with arbitrary strides (same shape). */
  private[NDArrayDoubleOps] def compareGeneral(
      a: NDArray[Double],
      b: NDArray[Double],
      f: (Double, Double) => Boolean
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

  /** Scalar comparison kernel for non-column-major arrays. */
  private[NDArrayDoubleOps] def compareScalarGeneral(
      a: NDArray[Double],
      s: Double,
      f: (Double, Double) => Boolean
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

  /** In-place binary op kernel: mutates `a.data` via its strides, reads `b` via its strides.
    *
    * `a` must be contiguous (checked by caller). Iterates in column-major coordinate order.
    */
  private[NDArrayDoubleOps] def binaryOpInPlaceGeneral(
      a: NDArray[Double],
      b: NDArray[Double],
      f: (Double, Double) => Double
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
      aData: Array[Double],
      bData: Array[Double],
      inline f: (Double, Double) => Double
  ): Array[Double] =
    val n = aData.length
    val out = new Array[Double](n)
    var i = 0
    while i < n do
      out(i) = f(aData(i), bData(i))
      i += 1
    end while
    out
  end flatBinaryOp

  private inline def flatUnaryOp(data: Array[Double], inline f: Double => Double): Array[Double] =
    val n = data.length
    val out = new Array[Double](n)
    var i = 0
    while i < n do
      out(i) = f(data(i))
      i += 1
    end while
    out
  end flatUnaryOp

  private inline def flatBinaryCompare(
      aData: Array[Double],
      bData: Array[Double],
      inline f: (Double, Double) => Boolean
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
      data: Array[Double],
      s: Double,
      inline f: (Double, Double) => Boolean
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

  // ── Extension methods on NDArray[Double] ─────────────────────────────────

  extension (a: NDArray[Double])

    // ── Binary ops (same shape required) ──────────────────────────────────

    /** Element-wise addition. Operands must have the same shape; use `broadcastTo` or `broadcastPair` first if needed.
      */
    inline def +(b: NDArray[Double]): NDArray[Double] =
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
    inline def -(b: NDArray[Double]): NDArray[Double] =
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

    /** Element-wise multiplication (Hadamard product). Operands must have the same shape. */
    inline def *(b: NDArray[Double]): NDArray[Double] =
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
    inline def /(b: NDArray[Double]): NDArray[Double] =
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
    inline def +(s: Double): NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ + s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ + s)

    /** Subtract scalar `s` from every element. */
    inline def -(s: Double): NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ - s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ - s)

    /** Multiply every element by scalar `s`. */
    inline def *(s: Double): NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ * s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ * s)

    /** Divide every element by scalar `s`. */
    inline def /(s: Double): NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ / s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ / s)

    // ── Unary ops ──────────────────────────────────────────────────────────

    /** Element-wise negation. */
    inline def neg: NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => -x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => -x)

    /** Element-wise absolute value. */
    inline def abs: NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, Math.abs), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, Math.abs)

    /** Element-wise natural exponential. */
    inline def exp: NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, Math.exp), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, Math.exp)

    /** Element-wise natural logarithm. */
    inline def log: NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, Math.log), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, Math.log)

    /** Element-wise square root. */
    inline def sqrt: NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, Math.sqrt), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, Math.sqrt)

    /** Element-wise hyperbolic tangent. */
    inline def tanh: NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, Math.tanh), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, Math.tanh)

    /** Element-wise sigmoid: `1 / (1 + exp(-x))`. */
    inline def sigmoid: NDArray[Double] =
      val sig = (x: Double) => 1.0 / (1.0 + Math.exp(-x))
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, sig), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, sig)
      end if
    end sigmoid

    // ── In-place binary ops ────────────────────────────────────────────────

    /** In-place element-wise addition. `a` must be contiguous; operands must have the same shape. */
    inline def +=(b: NDArray[Double]): Unit =
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
    inline def -=(b: NDArray[Double]): Unit =
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
    inline def *=(b: NDArray[Double]): Unit =
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
    inline def /=(b: NDArray[Double]): Unit =
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
    inline def +=(s: Double): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) += s
        i += 1
      end while
    end +=

    /** Subtract scalar `s` from every element in place. `a` must be contiguous. */
    inline def -=(s: Double): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) -= s
        i += 1
      end while
    end -=

    /** Multiply every element by scalar `s` in place. `a` must be contiguous. */
    inline def *=(s: Double): Unit =
      if !a.isContiguous then throw new UnsupportedOperationException("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) *= s
        i += 1
      end while
    end *=

    /** Divide every element by scalar `s` in place. `a` must be contiguous. */
    inline def /=(s: Double): Unit =
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
    inline def >(b: NDArray[Double]): NDArray[Boolean] =
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
    inline def <(b: NDArray[Double]): NDArray[Boolean] =
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
    inline def >=(b: NDArray[Double]): NDArray[Boolean] =
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
    inline def <=(b: NDArray[Double]): NDArray[Boolean] =
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
    inline def =:=(b: NDArray[Double]): NDArray[Boolean] =
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
    inline def !:=(b: NDArray[Double]): NDArray[Boolean] =
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
    inline def >(s: Double): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ > _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ > _)

    /** Element-wise less-than scalar. */
    inline def <(s: Double): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ < _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ < _)

    /** Element-wise greater-than-or-equal scalar. */
    inline def >=(s: Double): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ >= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ >= _)

    /** Element-wise less-than-or-equal scalar. */
    inline def <=(s: Double): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ <= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ <= _)

    /** Element-wise equality with scalar. */
    inline def =:=(s: Double): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ == _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ == _)

    /** Element-wise inequality with scalar. */
    inline def !:=(s: Double): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ != _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ != _)

  end extension

  // ── Left-scalar ops (`scalar op ndarray`) ─────────────────────────────────

  extension (s: Double)

    /** Scalar + NDArray[Double]: equivalent to `arr + s`. */
    inline def +(a: NDArray[Double]): NDArray[Double] = a + s

    /** Scalar - NDArray[Double]: `s - arr(i)` for each element. */
    inline def -(a: NDArray[Double]): NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => s - x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => s - x)

    /** Scalar * NDArray[Double]: equivalent to `arr * s`. */
    inline def *(a: NDArray[Double]): NDArray[Double] = a * s

    /** Scalar / NDArray[Double]: `s / arr(i)` for each element. */
    inline def /(a: NDArray[Double]): NDArray[Double] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => s / x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => s / x)

  end extension

end NDArrayDoubleOps
