package vecxt

import vecxt.ndarray.*

private object NDArrayIntReductionHelpers:
  // Placeholder — Int reductions use inline flat loops directly
end NDArrayIntReductionHelpers

object NDArrayIntReductions:

  // ── Private helper functions ──────────────────────────────────────────────

  /** Remove axis `k` from a shape array, producing a shape with one fewer dimension. */
  private[NDArrayIntReductions] inline def removeAxis(shape: Array[Int], axis: Int): Array[Int] =
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
  private[NDArrayIntReductions] inline def reduceGeneral(
      a: NDArray[Int],
      inline initial: Int,
      inline f: (Int, Int) => Int
  ): Int =
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
  private[NDArrayIntReductions] inline def reduceAxis(
      a: NDArray[Int],
      axis: Int,
      inline initial: Int,
      inline f: (Int, Int) => Int
  ): NDArray[Int] =
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
  private[NDArrayIntReductions] inline def argReduceAxis(
      a: NDArray[Int],
      axis: Int,
      inline initial: Int,
      inline compare: (Int, Int) => Boolean
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

  // ── Extension methods on NDArray[Int] ─────────────────────────────────────

  extension (a: NDArray[Int])

    // ── Full reductions ────────────────────────────────────────────────────

    /** Sum of all elements. */
    inline def sum: Int =
      if a.isContiguous then
        var acc = 0
        var i = 0
        while i < a.data.length do
          acc += a.data(i)
          i += 1
        end while
        acc
      else reduceGeneral(a, 0, _ + _)

    /** Arithmetic mean of all elements. Returns Double since integer mean is fractional. */
    inline def mean: Double =
      a.sum.toDouble / a.numel

    /** Minimum element. */
    inline def min: Int =
      if a.isContiguous then
        var acc = Int.MaxValue
        var i = 0
        while i < a.data.length do
          if a.data(i) < acc then acc = a.data(i)
          end if
          i += 1
        end while
        acc
      else reduceGeneral(a, Int.MaxValue, (acc, x) => if x < acc then x else acc)

    /** Maximum element. */
    inline def max: Int =
      if a.isContiguous then
        var acc = Int.MinValue
        var i = 0
        while i < a.data.length do
          if a.data(i) > acc then acc = a.data(i)
          end if
          i += 1
        end while
        acc
      else reduceGeneral(a, Int.MinValue, (acc, x) => if x > acc then x else acc)

    /** Product of all elements. */
    inline def product: Int =
      if a.isContiguous then
        var acc = 1
        var i = 0
        while i < a.data.length do
          acc *= a.data(i)
          i += 1
        end while
        acc
      else reduceGeneral(a, 1, _ * _)

    /** Index of the maximum element (flat, col-major order). */
    inline def argmax: Int =
      val n = a.numel
      val ndim = a.ndim
      if a.isColMajor then
        var bestIdx = 0
        var bestVal = Int.MinValue
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
        var bestVal = Int.MinValue
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
      end if
    end argmax

    /** Index of the minimum element (flat, col-major order). */
    inline def argmin: Int =
      val n = a.numel
      val ndim = a.ndim
      if a.isColMajor then
        var bestIdx = 0
        var bestVal = Int.MaxValue
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
        var bestVal = Int.MaxValue
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
      end if
    end argmin

    // ── Axis reductions ────────────────────────────────────────────────────

    /** Sum along axis `axis`. Result has one fewer dimension. */
    inline def sum(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, 0, _ + _)
    end sum

    /** Mean along axis `axis`. Returns NDArray[Double] since integer mean is fractional. */
    inline def mean(axis: Int): NDArray[Double] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      val sumResult = reduceAxis(a, axis, 0, _ + _)
      val n = a.shape(axis).toDouble
      val outN = sumResult.numel
      val out = new Array[Double](outN)
      var i = 0
      while i < outN do
        out(i) = sumResult.data(i).toDouble / n
        i += 1
      end while
      mkNDArray(out, sumResult.shape.clone(), colMajorStrides(sumResult.shape), 0)
    end mean

    /** Min along axis `axis`. */
    inline def min(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, Int.MaxValue, (acc, x) => if x < acc then x else acc)
    end min

    /** Max along axis `axis`. */
    inline def max(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, Int.MinValue, (acc, x) => if x > acc then x else acc)
    end max

    /** Product along axis `axis`. */
    inline def product(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      reduceAxis(a, axis, 1, _ * _)
    end product

    /** Argmax along axis `axis`. Returns NDArray[Int] of indices. */
    inline def argmax(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      argReduceAxis(a, axis, Int.MinValue, _ > _)
    end argmax

    /** Argmin along axis `axis`. Returns NDArray[Int] of indices. */
    inline def argmin(axis: Int): NDArray[Int] =
      if axis < 0 || axis >= a.ndim then throw InvalidNDArray(s"Axis $axis out of range [0, ${a.ndim})")
      end if
      argReduceAxis(a, axis, Int.MaxValue, _ < _)
    end argmin

  end extension

end NDArrayIntReductions
