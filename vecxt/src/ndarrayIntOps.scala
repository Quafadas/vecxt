package vecxt

import scala.annotation.targetName

import vecxt.broadcast.*
import vecxt.ndarray.*

object NDArrayIntOps:

  // ── General-case iteration kernels ──────────────────────────────────────

  private[NDArrayIntOps] def binaryOpGeneral(
      a: NDArray[Int],
      b: NDArray[Int],
      f: (Int, Int) => Int
  ): NDArray[Int] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Int](n)
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

  private[NDArrayIntOps] def unaryOpGeneral(a: NDArray[Int], f: Int => Int): NDArray[Int] =
    val n = a.numel
    val ndim = a.ndim
    val out = new Array[Int](n)
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

  private[NDArrayIntOps] def compareGeneral(
      a: NDArray[Int],
      b: NDArray[Int],
      f: (Int, Int) => Boolean
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

  private[NDArrayIntOps] def compareScalarGeneral(
      a: NDArray[Int],
      s: Int,
      f: (Int, Int) => Boolean
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

  private[NDArrayIntOps] def binaryOpInPlaceGeneral(
      a: NDArray[Int],
      b: NDArray[Int],
      f: (Int, Int) => Int
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
      aData: Array[Int],
      bData: Array[Int],
      inline f: (Int, Int) => Int
  ): Array[Int] =
    val n = aData.length
    val out = new Array[Int](n)
    var i = 0
    while i < n do
      out(i) = f(aData(i), bData(i))
      i += 1
    end while
    out
  end flatBinaryOp

  private inline def flatUnaryOp(data: Array[Int], inline f: Int => Int): Array[Int] =
    val n = data.length
    val out = new Array[Int](n)
    var i = 0
    while i < n do
      out(i) = f(data(i))
      i += 1
    end while
    out
  end flatUnaryOp

  private inline def flatBinaryCompare(
      aData: Array[Int],
      bData: Array[Int],
      inline f: (Int, Int) => Boolean
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
      data: Array[Int],
      s: Int,
      inline f: (Int, Int) => Boolean
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

  // ── Extension methods on NDArray[Int] ─────────────────────────────────────

  extension (a: NDArray[Int])

    // ── Binary ops (same shape required) ──────────────────────────────────

    /** Element-wise addition. Operands must have the same shape. */
    @targetName("ndIntAdd")
    inline def +(b: NDArray[Int]): NDArray[Int] =
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
    @targetName("ndIntSub")
    inline def -(b: NDArray[Int]): NDArray[Int] =
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
    @targetName("ndIntMul")
    inline def *(b: NDArray[Int]): NDArray[Int] =
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

    /** Element-wise integer division. Operands must have the same shape. */
    @targetName("ndIntDiv")
    inline def /(b: NDArray[Int]): NDArray[Int] =
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

    /** Element-wise modulo. Operands must have the same shape. */
    @targetName("ndIntMod")
    inline def %(b: NDArray[Int]): NDArray[Int] =
      if !sameShape(a.shape, b.shape) then
        throw ShapeMismatchException(
          s"Binary op % requires same shape: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]. " +
            "Use broadcastTo or broadcastPair to align shapes first."
        )
      end if
      if a.isColMajor && b.isColMajor then
        mkNDArray(flatBinaryOp(a.data, b.data, _ % _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else binaryOpGeneral(a, b, _ % _)
      end if
    end %

    // ── Scalar binary ops ──────────────────────────────────────────────────

    /** Add scalar `s` to every element. */
    inline def +(s: Int): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ + s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ + s)

    /** Subtract scalar `s` from every element. */
    inline def -(s: Int): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ - s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ - s)

    /** Multiply every element by scalar `s`. */
    inline def *(s: Int): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ * s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ * s)

    /** Divide every element by scalar `s` (integer division). */
    inline def /(s: Int): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ / s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ / s)

    /** Modulo every element by scalar `s`. */
    inline def %(s: Int): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, _ % s), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, _ % s)

    // ── Unary ops ──────────────────────────────────────────────────────────

    /** Element-wise negation. */
    @targetName("ndIntNeg")
    inline def neg: NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => -x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => -x)

    /** Element-wise absolute value. */
    @targetName("ndIntAbs")
    inline def abs: NDArray[Int] =
      if a.isColMajor then
        mkNDArray(flatUnaryOp(a.data, x => if x < 0 then -x else x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => if x < 0 then -x else x)

    // ── In-place binary ops ────────────────────────────────────────────────

    /** In-place element-wise addition. `a` must be contiguous; operands must have the same shape. */
    @targetName("ndIntAddAssign")
    inline def +=(b: NDArray[Int]): Unit =
      if !a.isColMajor then throw InvalidNDArray("In-place ops require a contiguous NDArray")
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
    @targetName("ndIntSubAssign")
    inline def -=(b: NDArray[Int]): Unit =
      if !a.isColMajor then throw InvalidNDArray("In-place ops require a contiguous NDArray")
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
    @targetName("ndIntMulAssign")
    inline def *=(b: NDArray[Int]): Unit =
      if !a.isColMajor then throw InvalidNDArray("In-place ops require a contiguous NDArray")
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

    // ── In-place scalar ops ────────────────────────────────────────────────

    /** Add scalar `s` to every element in place. `a` must be contiguous. */
    inline def +=(s: Int): Unit =
      if !a.isContiguous then throw InvalidNDArray("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) += s
        i += 1
      end while
    end +=

    /** Subtract scalar `s` from every element in place. `a` must be contiguous. */
    inline def -=(s: Int): Unit =
      if !a.isContiguous then throw InvalidNDArray("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) -= s
        i += 1
      end while
    end -=

    /** Multiply every element by scalar `s` in place. `a` must be contiguous. */
    inline def *=(s: Int): Unit =
      if !a.isContiguous then throw InvalidNDArray("In-place ops require a contiguous NDArray")
      end if
      var i = 0
      while i < a.data.length do
        a.data(i) *= s
        i += 1
      end while
    end *=

    // ── Comparison ops (array vs array) ───────────────────────────────────

    /** Element-wise greater-than. Operands must have the same shape. */
    @targetName("ndIntGt")
    inline def >(b: NDArray[Int]): NDArray[Boolean] =
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
    @targetName("ndIntLt")
    inline def <(b: NDArray[Int]): NDArray[Boolean] =
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
    @targetName("ndIntGe")
    inline def >=(b: NDArray[Int]): NDArray[Boolean] =
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
    @targetName("ndIntLe")
    inline def <=(b: NDArray[Int]): NDArray[Boolean] =
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
    @targetName("ndIntEq")
    inline def =:=(b: NDArray[Int]): NDArray[Boolean] =
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
    @targetName("ndIntNe")
    inline def !:=(b: NDArray[Int]): NDArray[Boolean] =
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
    inline def >(s: Int): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ > _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ > _)

    /** Element-wise less-than scalar. */
    inline def <(s: Int): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ < _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ < _)

    /** Element-wise greater-than-or-equal scalar. */
    inline def >=(s: Int): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ >= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ >= _)

    /** Element-wise less-than-or-equal scalar. */
    inline def <=(s: Int): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ <= _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ <= _)

    /** Element-wise equality with scalar. */
    inline def =:=(s: Int): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ == _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ == _)

    /** Element-wise inequality with scalar. */
    inline def !:=(s: Int): NDArray[Boolean] =
      if a.isColMajor then mkNDArray(flatScalarCompare(a.data, s, _ != _), a.shape.clone(), colMajorStrides(a.shape), 0)
      else compareScalarGeneral(a, s, _ != _)

  end extension

  // ── Left-scalar ops (`scalar op ndarray`) ─────────────────────────────────

  extension (s: Int)

    /** Scalar + NDArray[Int]: equivalent to `arr + s`. */
    @targetName("intPlusNDArray")
    inline def +(a: NDArray[Int]): NDArray[Int] = a + s

    /** Scalar - NDArray[Int]: `s - arr(i)` for each element. */
    @targetName("intMinusNDArray")
    inline def -(a: NDArray[Int]): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => s - x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => s - x)

    /** Scalar * NDArray[Int]: equivalent to `arr * s`. */
    @targetName("intTimesNDArray")
    inline def *(a: NDArray[Int]): NDArray[Int] = a * s

    /** Scalar / NDArray[Int]: `s / arr(i)` for each element. */
    @targetName("intDivNDArray")
    inline def /(a: NDArray[Int]): NDArray[Int] =
      if a.isColMajor then mkNDArray(flatUnaryOp(a.data, x => s / x), a.shape.clone(), colMajorStrides(a.shape), 0)
      else unaryOpGeneral(a, x => s / x)

  end extension

end NDArrayIntOps
