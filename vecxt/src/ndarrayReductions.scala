package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.broadcast.ShapeMismatchException
import vecxt.doublearrays.*
import vecxt.matrix.*
import vecxt.ndarray.*

/** Private helper object for delegating to Array[Double] and Matrix[Double] extension methods.
  *
  * This is defined as a top-level private object (not nested inside NDArrayReductions) so that NDArrayReductions'
  * extension methods are NOT in scope here. This prevents name-shadowing when calling, e.g., `arrays.dot` on
  * `Array[Double]`.
  */
private object NDArrayReductionHelpers:
  import vecxt.doublearrays.*
  import vecxt.DoubleMatrix.*

  inline def mean(d: Array[Double]): Double = d.mean
  inline def variance(d: Array[Double]): Double = d.variance
  inline def norm(d: Array[Double]): Double = d.norm
  inline def argmax(d: Array[Double]): Int = d.argmax
  inline def argmin(d: Array[Double]): Int = d.argmin
  inline def dot(d1: Array[Double], d2: Array[Double]): Double =
    d1.dot(d2)(using BoundsCheck.DoBoundsCheck.no)
  inline def matmul(m1: Matrix[Double], m2: Matrix[Double]): Matrix[Double] =
    m1.matmul(m2)(using BoundsCheck.DoBoundsCheck.no)

end NDArrayReductionHelpers

object NDArrayReductions:

  // ── Private helper functions ──────────────────────────────────────────────

  /** Remove axis `k` from a shape array, producing a shape with one fewer dimension. */
  private[NDArrayReductions] inline def removeAxis(shape: Array[Int], axis: Int): Array[Int] =
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

  /** General reduce kernel: iterates all elements of `a` in column-major coordinate order. */
  private[NDArrayReductions] inline def reduceGeneral(
      a: NDArray[Double],
      inline initial: Double,
      inline f: (Double, Double) => Double
  ): Double =
    val n = a.numel
    val ndim = a.ndim
    val cumProd = colMajorStrides(a.shape)
    var acc = initial
    var j = 0
    while j < n do
      var pos = a.offset
      var k = 0
      while k < ndim do
        val coord = (j / cumProd(k)) % a.shape(k)
        pos += coord * a.strides(k)
        k += 1
      end while
      acc = f(acc, a.data(pos))
      j += 1
    end while
    acc
  end reduceGeneral

  /** Axis reduction kernel: reduces `a` along `axis` using `f` with `initial` accumulator.
    *
    * Output shape = `a.shape` with dimension `axis` removed. Output is always col-major.
    */
  private[NDArrayReductions] inline def reduceAxis(
      a: NDArray[Double],
      axis: Int,
      inline initial: Double,
      inline f: (Double, Double) => Double
  ): NDArray[Double] =
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
  end reduceAxis

  /** Axis arg-reduction kernel: returns the index along `axis` of the extremum at each output position.
    *
    * `initial` should be `Double.NegativeInfinity` for argmax, `Double.PositiveInfinity` for argmin. `compare(newVal,
    * bestSoFar)` should return `true` when `newVal` is a better candidate.
    *
    * Output is `NDArray[Int]` of integral indices.
    */
  private[NDArrayReductions] inline def argReduceAxis(
      a: NDArray[Double],
      axis: Int,
      inline initial: Double,
      inline compare: (Double, Double) => Boolean
  ): NDArray[Int] =
    val outShape = removeAxis(a.shape, axis)
    val outStrides = colMajorStrides(outShape)
    val outN = shapeProduct(outShape)
    val bestVals = Array.fill(outN)(initial)
    val outIdx = new Array[Int](outN)

    val n = a.numel
    val ndim = a.ndim
    val inCumProd = colMajorStrides(a.shape)

    var j = 0
    while j < n do
      var posIn = a.offset
      var posOut = 0
      var outDim = 0
      var axisCoord = 0
      var k = 0
      while k < ndim do
        val coord = (j / inCumProd(k)) % a.shape(k)
        posIn += coord * a.strides(k)
        if k == axis then axisCoord = coord
        else
          posOut += coord * outStrides(outDim)
          outDim += 1
        end if
        k += 1
      end while
      val v = a.data(posIn)
      if compare(v, bestVals(posOut)) then
        bestVals(posOut) = v
        outIdx(posOut) = axisCoord
      end if
      j += 1
    end while

    mkNDArray(outIdx, outShape, colMajorStrides(outShape), 0)
  end argReduceAxis

  // ── Extension methods on NDArray[Double] ─────────────────────────────────

  extension (a: NDArray[Double])

    // ── Full reductions ────────────────────────────────────────────────────

    /** Sum of all elements. */
    inline def sum: Double =
      if a.isContiguous then a.data.sum
      else reduceGeneral(a, 0.0, _ + _)

    /** Arithmetic mean of all elements. */
    inline def mean: Double =
      if a.isContiguous then NDArrayReductionHelpers.mean(a.data)
      else reduceGeneral(a, 0.0, _ + _) / a.numel

    /** Minimum element. */
    inline def min: Double =
      if a.isContiguous then a.data.minSIMD
      else reduceGeneral(a, Double.PositiveInfinity, (acc, x) => if x < acc then x else acc)

    /** Maximum element. */
    inline def max: Double =
      if a.isContiguous then a.data.maxSIMD
      else reduceGeneral(a, Double.NegativeInfinity, (acc, x) => if x > acc then x else acc)

    /** Product of all elements. */
    inline def product: Double =
      if a.isContiguous then a.data.product
      else reduceGeneral(a, 1.0, _ * _)

    /** Population variance. */
    inline def variance: Double =
      if a.isContiguous then NDArrayReductionHelpers.variance(a.data)
      else
        val m = a.mean
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var acc = 0.0
        var j = 0
        while j < n do
          var pos = a.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % a.shape(k)
            pos += coord * a.strides(k)
            k += 1
          end while
          val d = a.data(pos) - m
          acc += d * d
          j += 1
        end while
        acc / n

    /** L2 (Euclidean) norm: √(Σ xᵢ²). */
    inline def norm: Double =
      if a.isContiguous then NDArrayReductionHelpers.norm(a.data)
      else Math.sqrt(reduceGeneral(a, 0.0, (acc, x) => acc + x * x))

    /** Index of the maximum element (flat, col-major order). */
    inline def argmax: Int =
      if a.isColMajor then NDArrayReductionHelpers.argmax(a.data)
      else
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var bestIdx = 0
        var bestVal = Double.NegativeInfinity
        var j = 0
        while j < n do
          var pos = a.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % a.shape(k)
            pos += coord * a.strides(k)
            k += 1
          end while
          val v = a.data(pos)
          if v > bestVal then
            bestVal = v
            bestIdx = j
          end if
          j += 1
        end while
        bestIdx

    /** Index of the minimum element (flat, col-major order). */
    inline def argmin: Int =
      if a.isColMajor then NDArrayReductionHelpers.argmin(a.data)
      else
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var bestIdx = 0
        var bestVal = Double.PositiveInfinity
        var j = 0
        while j < n do
          var pos = a.offset
          var k = 0
          while k < ndim do
            val coord = (j / cumProd(k)) % a.shape(k)
            pos += coord * a.strides(k)
            k += 1
          end while
          val v = a.data(pos)
          if v < bestVal then
            bestVal = v
            bestIdx = j
          end if
          j += 1
        end while
        bestIdx

    // ── Axis reductions ────────────────────────────────────────────────────

    /** Sum along axis `axis`. Result has one fewer dimension. */
    inline def sum(axis: Int): NDArray[Double] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, 0.0, _ + _)
    end sum

    /** Mean along axis `axis`. */
    inline def mean(axis: Int): NDArray[Double] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      val result = reduceAxis(a, axis, 0.0, _ + _)
      val n = a.shape(axis).toDouble
      var i = 0
      while i < result.data.length do
        result.data(i) /= n
        i += 1
      end while
      result
    end mean

    /** Min along axis `axis`. */
    inline def min(axis: Int): NDArray[Double] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, Double.PositiveInfinity, (acc, x) => if x < acc then x else acc)
    end min

    /** Max along axis `axis`. */
    inline def max(axis: Int): NDArray[Double] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, Double.NegativeInfinity, (acc, x) => if x > acc then x else acc)
    end max

    /** Product along axis `axis`. */
    inline def product(axis: Int): NDArray[Double] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, 1.0, _ * _)
    end product

    /** Argmax along axis `axis`. Returns NDArray[Int] of indices. */
    inline def argmax(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      argReduceAxis(a, axis, Double.NegativeInfinity, _ > _)
    end argmax

    /** Argmin along axis `axis`. Returns NDArray[Int] of indices. */
    inline def argmin(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      argReduceAxis(a, axis, Double.PositiveInfinity, _ < _)
    end argmin

    // ── Linear algebra ─────────────────────────────────────────────────────

    /** Dot product of two 1-D NDArrays. */
    inline def dot(b: NDArray[Double])(using inline bc: BoundsCheck): Double =
      inline if bc then
        if a.ndim != 1 then throw InvalidNDArray(s"dot requires 1-D arrays, got ndim=${a.ndim}")
        end if
        if b.ndim != 1 then throw InvalidNDArray(s"dot requires 1-D arrays, got ndim=${b.ndim}")
        end if
        if a.shape(0) != b.shape(0) then
          throw ShapeMismatchException(s"dot: length mismatch: ${a.shape(0)} vs ${b.shape(0)}")
        end if
      end if
      if a.isColMajor && b.isColMajor then NDArrayReductionHelpers.dot(a.data, b.data)
      else
        var acc = 0.0
        var i = 0
        while i < a.shape(0) do
          acc += a.data(a.offset + i * a.strides(0)) * b.data(b.offset + i * b.strides(0))
          i += 1
        end while
        acc
      end if
    end dot

    /** Matrix multiply two 2-D NDArrays. Result shape: [a.shape(0), b.shape(1)]. */
    inline def matmul(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] =
      inline if bc then
        if a.ndim != 2 then throw InvalidNDArray(s"matmul requires 2-D arrays, got ndim=${a.ndim}")
        end if
        if b.ndim != 2 then throw InvalidNDArray(s"matmul requires 2-D arrays, got ndim=${b.ndim}")
        end if
        if a.shape(1) != b.shape(0) then
          throw ShapeMismatchException(
            s"matmul: inner dimension mismatch: ${a.shape(1)} vs ${b.shape(0)}"
          )
        end if
      end if
      val aRows = a.shape(0)
      val aCols = a.shape(1)
      val bCols = b.shape(1)
      val matA = Matrix[Double](a.data, aRows, aCols, a.strides(0), a.strides(1), a.offset)(using
        BoundsCheck.DoBoundsCheck.no
      )
      val matB = Matrix[Double](b.data, b.shape(0), bCols, b.strides(0), b.strides(1), b.offset)(using
        BoundsCheck.DoBoundsCheck.no
      )
      val result = NDArrayReductionHelpers.matmul(matA, matB)
      val outShape = Array(result.rows, result.cols)
      mkNDArray(result.raw, outShape, colMajorStrides(outShape), 0)
    end matmul

    /** Alias for matmul. */
    inline def @@(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = a.matmul(b)

  end extension

end NDArrayReductions
