package vecxt

import vecxt.ndarray.*

object NDArrayFloatReductions:

  // ── Private helper functions ──────────────────────────────────────────────

  /** Remove axis `k` from a shape array, producing a shape with one fewer dimension. */
  private[NDArrayFloatReductions] inline def removeAxis(shape: Array[Int], axis: Int): Array[Int] =
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
  private[NDArrayFloatReductions] inline def reduceGeneral(
      a: NDArray[Float],
      inline initial: Float,
      inline f: (Float, Float) => Float
  ): Float =
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

  /** Axis reduction kernel: reduces `a` along `axis` using `f` with `initial` accumulator. */
  private[NDArrayFloatReductions] inline def reduceAxis(
      a: NDArray[Float],
      axis: Int,
      inline initial: Float,
      inline f: (Float, Float) => Float
  ): NDArray[Float] =
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

  /** Axis arg-reduction kernel: returns the index along `axis` of the extremum at each output position. */
  private[NDArrayFloatReductions] inline def argReduceAxis(
      a: NDArray[Float],
      axis: Int,
      inline initial: Float,
      inline compare: (Float, Float) => Boolean
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

  // ── Extension methods on NDArray[Float] ─────────────────────────────────

  extension (a: NDArray[Float])

    // ── Full reductions ────────────────────────────────────────────────────

    /** Sum of all elements. */
    inline def sum: Float =
      if a.isContiguous then
        var acc = 0.0f
        var i = 0
        while i < a.data.length do
          acc += a.data(i)
          i += 1
        end while
        acc
      else reduceGeneral(a, 0.0f, _ + _)

    /** Arithmetic mean of all elements. */
    inline def mean: Float =
      a.sum / a.numel.toFloat

    /** Minimum element. */
    inline def min: Float =
      if a.isContiguous then
        var acc = Float.PositiveInfinity
        var i = 0
        while i < a.data.length do
          if a.data(i) < acc then acc = a.data(i)
          i += 1
        end while
        acc
      else reduceGeneral(a, Float.PositiveInfinity, (acc, x) => if x < acc then x else acc)

    /** Maximum element. */
    inline def max: Float =
      if a.isContiguous then
        var acc = Float.NegativeInfinity
        var i = 0
        while i < a.data.length do
          if a.data(i) > acc then acc = a.data(i)
          i += 1
        end while
        acc
      else reduceGeneral(a, Float.NegativeInfinity, (acc, x) => if x > acc then x else acc)

    /** Product of all elements. */
    inline def product: Float =
      if a.isContiguous then
        var acc = 1.0f
        var i = 0
        while i < a.data.length do
          acc *= a.data(i)
          i += 1
        end while
        acc
      else reduceGeneral(a, 1.0f, _ * _)

    /** Population variance. */
    inline def variance: Float =
      if a.isContiguous then
        val m = a.mean
        val n = a.numel
        var acc = 0.0f
        var i = 0
        while i < a.data.length do
          val d = a.data(i) - m
          acc += d * d
          i += 1
        end while
        acc / n.toFloat
      else
        val m = a.mean
        val n = a.numel
        val ndim = a.ndim
        val cumProd = colMajorStrides(a.shape)
        var acc = 0.0f
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
        acc / n.toFloat

    /** L2 (Euclidean) norm: √(Σ xᵢ²). */
    inline def norm: Float =
      Math.sqrt(reduceGeneral(a, 0.0f, (acc, x) => acc + x * x).toDouble).toFloat

    /** Index of the maximum element (flat, col-major order). */
    inline def argmax: Int =
      val n = a.numel
      val ndim = a.ndim
      if a.isColMajor then
        var bestIdx = 0
        var bestVal = Float.NegativeInfinity
        var i = 0
        while i < a.data.length do
          if a.data(i) > bestVal then
            bestVal = a.data(i)
            bestIdx = i
          end if
          i += 1
        end while
        bestIdx
      else
        val cumProd = colMajorStrides(a.shape)
        var bestIdx = 0
        var bestVal = Float.NegativeInfinity
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
      val n = a.numel
      val ndim = a.ndim
      if a.isColMajor then
        var bestIdx = 0
        var bestVal = Float.PositiveInfinity
        var i = 0
        while i < a.data.length do
          if a.data(i) < bestVal then
            bestVal = a.data(i)
            bestIdx = i
          end if
          i += 1
        end while
        bestIdx
      else
        val cumProd = colMajorStrides(a.shape)
        var bestIdx = 0
        var bestVal = Float.PositiveInfinity
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
    inline def sum(axis: Int): NDArray[Float] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, 0.0f, _ + _)
    end sum

    /** Mean along axis `axis`. */
    inline def mean(axis: Int): NDArray[Float] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      val result = reduceAxis(a, axis, 0.0f, _ + _)
      val n = a.shape(axis).toFloat
      var i = 0
      while i < result.data.length do
        result.data(i) /= n
        i += 1
      end while
      result
    end mean

    /** Min along axis `axis`. */
    inline def min(axis: Int): NDArray[Float] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, Float.PositiveInfinity, (acc, x) => if x < acc then x else acc)
    end min

    /** Max along axis `axis`. */
    inline def max(axis: Int): NDArray[Float] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, Float.NegativeInfinity, (acc, x) => if x > acc then x else acc)
    end max

    /** Product along axis `axis`. */
    inline def product(axis: Int): NDArray[Float] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, 1.0f, _ * _)
    end product

    /** Argmax along axis `axis`. Returns NDArray[Int] of indices. */
    inline def argmax(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      argReduceAxis(a, axis, Float.NegativeInfinity, _ > _)
    end argmax

    /** Argmin along axis `axis`. Returns NDArray[Int] of indices. */
    inline def argmin(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      argReduceAxis(a, axis, Float.PositiveInfinity, _ < _)
    end argmin

  end extension

end NDArrayFloatReductions
