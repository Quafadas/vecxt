package vecxt

import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.typedarray.Float32Array

import vecxt.BooleanArrays.trues

import vecxt.matrix.Matrix

object floatarrays:

  extension (f: Float)
    def /(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f / arr(i)
        i = i + 1
      end while
      out
    end /

    def +(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f + arr(i)
        i = i + 1
      end while
      out
    end +

    def -(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f - arr(i)
        i = i + 1
      end while
      out
    end -

    def *(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f * arr(i)
        i = i + 1
      end while
      out
    end *

  end extension

  extension (vec: Array[Float])

    def clampMin(min: Float): Array[Float] =
      val n = vec.length
      val res = Array.ofDim[Float](n)

      var i = 0
      while i < n do
        res(i) = math.max(vec(i), min)
        i += 1
      end while
      res
    end clampMin

    def `clampMin!`(min: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = math.max(vec(i), min)
        i += 1
      end while
    end `clampMin!`

    def maxClamp(max: Float): Array[Float] = clampMax(max)

    def minClamp(min: Float): Array[Float] = clampMin(min)

    def clampMax(max: Float): Array[Float] =
      val n = vec.length
      val res = Array.ofDim[Float](n)

      var i = 0
      while i < n do
        res(i) = math.min(vec(i), max)
        i += 1
      end while
      res
    end clampMax

    def `clampMax!`(max: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = math.min(vec(i), max)
        i += 1
      end while
    end `clampMax!`

    def clamp(min: Float, max: Float): Array[Float] =
      val n = vec.length
      val res = Array.ofDim[Float](n)

      var i = 0
      while i < n do
        res(i) = math.min(math.max(vec(i), min), max)
        i += 1
      end while
      res
    end clamp

    def `clamp!`(min: Float, max: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = math.min(math.max(vec(i), min), max)
        i += 1
      end while
    end `clamp!`

    def argmax: Int =
      val n = vec.length
      if n == 0 then -1
      else
        var maxIdx = 0
        var maxVal = vec(0)
        var i = 1
        while i < n do
          if vec(i) > maxVal then
            maxVal = vec(i)
            maxIdx = i
          end if
          i += 1
        end while
        maxIdx
      end if
    end argmax

    def argmin: Int =
      val n = vec.length
      if n == 0 then -1
      else
        var minIdx = 0
        var minVal = vec(0)
        var i = 1
        while i < n do
          if vec(i) < minVal then
            minVal = vec(i)
            minIdx = i
          end if
          i += 1
        end while
        minIdx
      end if
    end argmin

    def minSIMD: Float =
      var i = 0
      var acc = Float.PositiveInfinity
      while i < vec.length do
        val v = vec(i)
        if v < acc then acc = v
        end if
        i += 1
      end while
      acc
    end minSIMD

    def maxSIMD: Float =
      var i = 0
      var acc = Float.NegativeInfinity
      while i < vec.length do
        val v = vec(i)
        if v > acc then acc = v
        end if
        i += 1
      end while
      acc
    end maxSIMD

    def min: Float = minSIMD

    def max: Float = maxSIMD

    def sumSIMD: Float =
      var sum = 0.0
      var i = 0
      while i < vec.length do
        sum += vec(i)
        i += 1
      end while
      sum.toFloat
    end sumSIMD

    def sum: Float = sumSIMD

    def productSIMD: Float =
      var prod = 1.0
      var i = 0
      while i < vec.length do
        prod *= vec(i)
        i += 1
      end while
      prod.toFloat
    end productSIMD

    // inline def product: Float = productSIMD

    def `**!`(power: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.pow(vec(i).toDouble, power.toDouble).toFloat
        i += 1
      end while
    end `**!`

    def **(power: Float): Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.pow(vec(i).toDouble, power.toDouble).toFloat
        i += 1
      end while
      newVec
    end **

    def `fma!`(multiply: Float, add: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = (vec(i) * multiply + add)
        i += 1
      end while
    end `fma!`

    def fma(multiply: Float, add: Float): Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = (vec(i) * multiply + add)
        i += 1
      end while
      newVec
    end fma

    private inline def applyUnaryOp(inline op: Double => Double): Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = op(vec(i).toDouble).toFloat
        i += 1
      end while
      newVec
    end applyUnaryOp

    private inline def applyUnaryOpInPlace(inline op: Double => Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = op(vec(i).toDouble).toFloat
        i += 1
      end while
    end applyUnaryOpInPlace

    def abs: Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.abs(vec(i))
        i += 1
      end while
      newVec
    end abs

    def `abs!`: Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.abs(vec(i))
        i += 1
      end while
    end `abs!`

    def exp: Array[Float] =
      applyUnaryOp(Math.exp)

    def `exp!`: Unit =
      applyUnaryOpInPlace(Math.exp)

    def expm1: Array[Float] =
      applyUnaryOp(Math.expm1)

    def `expm1!`: Unit =
      applyUnaryOpInPlace(Math.expm1)

    def log: Array[Float] =
      applyUnaryOp(Math.log)

    def `log!`: Unit =
      applyUnaryOpInPlace(Math.log)

    def log10: Array[Float] =
      applyUnaryOp(Math.log10)

    def `log10!`: Unit =
      applyUnaryOpInPlace(Math.log10)

    def log1p: Array[Float] =
      applyUnaryOp(Math.log1p)

    def `log1p!`: Unit =
      applyUnaryOpInPlace(Math.log1p)

    def sqrt: Array[Float] =
      applyUnaryOp(Math.sqrt)

    def `sqrt!`: Unit =
      applyUnaryOpInPlace(Math.sqrt)

    def cbrt: Array[Float] =
      applyUnaryOp(Math.cbrt)

    def `cbrt!`: Unit =
      applyUnaryOpInPlace(Math.cbrt)

    def sin: Array[Float] =
      applyUnaryOp(Math.sin)

    def `sin!`: Unit =
      applyUnaryOpInPlace(Math.sin)

    def sinh: Array[Float] =
      applyUnaryOp(Math.sinh)

    def `sinh!`: Unit =
      applyUnaryOpInPlace(Math.sinh)

    def cos: Array[Float] =
      applyUnaryOp(Math.cos)

    def `cos!`: Unit =
      applyUnaryOpInPlace(Math.cos)

    def cosh: Array[Float] =
      applyUnaryOp(Math.cosh)

    def `cosh!`: Unit =
      applyUnaryOpInPlace(Math.cosh)

    def tan: Array[Float] =
      applyUnaryOp(Math.tan)

    def `tan!`: Unit =
      applyUnaryOpInPlace(Math.tan)

    def tanh: Array[Float] =
      applyUnaryOp(Math.tanh)

    def `tanh!`: Unit =
      applyUnaryOpInPlace(Math.tanh)

    def asin: Array[Float] =
      applyUnaryOp(Math.asin)

    def `asin!`: Unit =
      applyUnaryOpInPlace(Math.asin)

    def acos: Array[Float] =
      applyUnaryOp(Math.acos)

    def `acos!`: Unit =
      applyUnaryOpInPlace(Math.acos)

    def atan: Array[Float] =
      applyUnaryOp(Math.atan)

    def `atan!`: Unit =
      applyUnaryOpInPlace(Math.atan)

    def unary_- : Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = -vec(i)
        i += 1
      end while
      newVec
    end unary_-

    def `-!`: Unit =
      var i = 0
      while i < vec.length do
        vec(i) = -vec(i)
        i += 1
      end while
    end `-!`

    def /(d: Array[Float]): Array[Float] =
      dimCheck(vec, d)
      val n = vec.length
      val res = Array.ofDim[Float](n)
      var i = 0
      while i < n do
        res(i) = vec(i) / d(i)
        i += 1
      end while
      res
    end /

    def /:/(d: Array[Float]): Array[Float] =
      dimCheck(vec, d)
      val n = vec.length
      val res = Array.ofDim[Float](n)
      var i = 0
      while i < n do
        res(i) = vec(i) / d(i)
        i += 1
      end while
      res
    end /:/

    def /=(d: Array[Float]): Unit =
      dimCheck(vec, d)
      val n = vec.length
      var i = 0
      while i < n do
        vec(i) = vec(i) / d(i)
        i += 1
      end while
    end /=

    def *(d: Array[Float]): Array[Float] =
      dimCheck(vec, d)
      val out = new Array[Float](vec.length)
      var i = 0
      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *

    def *:*(d: Array[Float]): Array[Float] =
      dimCheck(vec, d)
      val out = new Array[Float](vec.length)
      var i = 0
      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *:*

    def *=(d: Array[Float]): Unit =
      dimCheck(vec, d)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) * d(i)
        i = i + 1
      end while
    end *=

    // inline def *(d: Float): Array[Float] =
    //   val n = vec.length
    //   val res = Array.ofDim[Float](n)
    //   var i = 0
    //   while i < n do
    //     res(i) = vec(i) * d
    //     i += 1
    //   end while
    //   res
    // end *

    // inline def /(d: Float): Array[Float] =
    //   val n = vec.length
    //   val res = Array.ofDim[Float](n)
    //   var i = 0
    //   while i < n do
    //     res(i) = vec(i) / d
    //     i += 1
    //   end while
    //   res
    // end /

    def productExceptSelf: Array[Float] =
      val n = vec.length
      val left = Array.ofDim[Float](n)
      val right = Array.ofDim[Float](n)
      val result = Array.ofDim[Float](n)

      left(0) = 1.0f
      right(n - 1) = 1.0f

      var i = 1
      while i < n do
        left(i) = vec(i - 1) * left(i - 1)
        i += 1
      end while

      i = n - 2
      while i >= 0 do
        right(i) = vec(i + 1) * right(i + 1)
        i -= 1
      end while

      i = 0
      while i < n do
        result(i) = left(i) * right(i)
        i += 1
      end while

      result
    end productExceptSelf

    /** The formula for the logarithm of the sum of exponentials is:
      *
      * logSumExp(x) = log(sum(exp(x_i))) for i = 1 to n
      *
      * This is computed in a numerically stable way by subtracting the maximum value in the array before taking the
      * exponentials:
      *
      * logSumExp(x) = max(x) + log(sum(exp(x_i - max(x)))) for i = 1 to n
      */
    def logSumExp: Float =
      val maxVal = vec.max
      var sumExp = 0.0
      var i = 0
      while i < vec.length do
        sumExp += Math.exp((vec(i) - maxVal).toDouble)
        i += 1
      end while
      (maxVal + Math.log(sumExp)).toFloat
    end logSumExp

    def <(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    def <=(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    def >(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    def >=(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Float, Float) => Boolean,
        inline num: Float
    ): Array[Boolean] =
      val n = vec.length
      val idx = Array.fill(n)(false)

      var i = 0
      while i < n do
        if op(vec(i), num) then idx(i) = true
        end if
        i = i + 1
      end while
      idx
    end logicalIdx

    def outer(other: Array[Float])(using ClassTag[Float]): Matrix[Float] =
      val n = vec.length
      val m = other.length
      val out: Array[Float] = Array.ofDim[Float](n * m)

      var i = 0
      while i < n do
        var j = 0
        while j < m do
          out(j * n + i) = vec(i) * other(j)
          j = j + 1
        end while
        i = i + 1
      end while
      Matrix[Float](out, (n, m))
    end outer

    def cumsum: Array[Float] =
      val out = vec.clone()
      out.`cumsum!`
      out
    end cumsum

    def `cumsum!`: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    def increments: Array[Float] =
      val out = new Array[Float](vec.length)
      out(0) = vec(0)
      var i = 1
      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out
    end increments

  end extension

  extension (vec: Array[Float])

    def toFloat32 = Float32Array.from(js.Array(vec*))

    def apply(index: Array[Boolean]): Array[Float] =
      val truely = index.trues
      val newVec = Array.ofDim[Float](truely)
      var j = 0
      for i <- 0 until index.length do
        if index(i) then
          newVec(j) = vec(i)
          j += 1
      end for
      newVec
    end apply

    def mean: Float =
      (vec.sum / vec.length).toFloat
    end mean

    def variance: Float = variance(VarianceMode.Population)

    def variance(mode: VarianceMode): Float =
      meanAndVariance(mode).variance
    end variance

    def meanAndVariance: (mean: Float, variance: Float) =
      meanAndVariance(VarianceMode.Population)

    def meanAndVariance(mode: VarianceMode): (mean: Float, variance: Float) =
      var mean = 0.0
      var m2 = 0.0
      var i = 0
      while i < vec.length do
        val n = i + 1
        val delta = vec(i).toDouble - mean
        mean += delta / n
        val delta2 = vec(i).toDouble - mean
        m2 += delta * delta2
        i += 1
      end while
      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      (mean.toFloat, (m2 / denom).toFloat)
    end meanAndVariance

    def std: Float = std(VarianceMode.Population)

    def std(mode: VarianceMode): Float =
      Math.sqrt(vec.variance(mode).toDouble).toFloat

    def stdDev: Float = stdDev(VarianceMode.Population)

    def stdDev(mode: VarianceMode): Float = std(mode)

    def norm: Float = blas.snrm2(vec.length, vec.toFloat32, 1)

    def dot(v1: Array[Float]): Float =
      dimCheck(vec, v1)
      blas.sdot(vec.length, vec.toFloat32, 1, v1.toFloat32, 1)
    end dot

    def -(vec2: Array[Float]): Array[Float] =
      val out = vec.clone
      out -= vec2
      out
    end -

    def -=(vec2: Array[Float]): Unit =
      dimCheck(vec, vec2)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - vec2(i)
        i = i + 1
      end while
    end -=

    def +(vec2: Array[Float]): Array[Float] =
      val out = vec.clone
      out += vec2
      out
    end +

    def +=(vec2: Array[Float]): Unit =
      dimCheck(vec, vec2)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + vec2(i)
        i = i + 1
      end while
    end +=

    def +:+(d: Float): Array[Float] =
      val out = vec.clone
      out +:+= d
      out
    end +:+

    def +:+=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +:+=

    def -(d: Float): Array[Float] =
      val out = vec.clone()
      out -= d
      out
    end -

    def -=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    def +(d: Float): Array[Float] =
      val out = vec.clone()
      out += d
      out
    end +

    def +=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    def *=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) * d
        i = i + 1
      end while
    end *=

    def *(d: Float): Array[Float] =
      val out = vec.clone
      out *= d
      out
    end *

    def /=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) / d
        i = i + 1
      end while
    end /=

    def /(d: Float): Array[Float] =
      val out = vec.clone
      out /= d
      out
    end /

    def pearsonCorrelationCoefficient(thatVector: Array[Float]): Float =
      dimCheck(vec, thatVector)
      val n = vec.length
      var i = 0

      var sum_x = 0.0
      var sum_y = 0.0
      var sum_xy = 0.0
      var sum_x2 = 0.0
      var sum_y2 = 0.0

      while i < n do
        sum_x = sum_x + vec(i)
        sum_y = sum_y + thatVector(i)
        sum_xy = sum_xy + vec(i) * thatVector(i)
        sum_x2 = sum_x2 + vec(i) * vec(i)
        sum_y2 = sum_y2 + thatVector(i) * thatVector(i)
        i = i + 1
      end while
      ((n * sum_xy - (sum_x * sum_y)) / Math.sqrt(
        (sum_x2 * n - sum_x * sum_x) * (sum_y2 * n - sum_y * sum_y)
      )).toFloat
    end pearsonCorrelationCoefficient

    def spearmansRankCorrelation(thatVector: Array[Float]): Float =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    def corr(thatVector: Array[Float]): Float =
      pearsonCorrelationCoefficient(thatVector)

    def elementRanks: Array[Float] =
      val indexed: Array[(Float, Int)] = vec.zipWithIndex
      indexed.sortInPlace()(using Ordering.by(_._1))

      val ranks: Array[Float] = new Array[Float](vec.length)
      ranks(indexed.last._2) = vec.length.toFloat
      var currentValue: Float = indexed(0)._1
      var r0: Int = 0
      var rank: Int = 1
      while rank < vec.length do
        val temp: Float = indexed(rank)._1
        val end: Int =
          if temp != currentValue then rank
          else if rank == vec.length - 1 then rank + 1
          else -1
        if end > -1 then
          val avg: Float = ((1.0 + (end + r0)) / 2.0).toFloat
          var i: Int = r0;
          while i < end do
            ranks(indexed(i)._2) = avg
            i += 1
          end while
          r0 = rank
          currentValue = temp
        end if
        rank += 1
      end while
      ranks
    end elementRanks

    def covariance(thatVector: Array[Float]): Float =
      val μThis = vec.mean
      val μThat = thatVector.mean
      var cv: Double = 0
      var i: Int = 0
      while i < vec.length do
        cv += (vec(i) - μThis) * (thatVector(i) - μThat)
        i += 1
      end while
      (cv / (vec.length - 1)).toFloat
    end covariance

    inline def `zeroWhere!`(
        other: Array[Float],
        threshold: Float,
        inline op: ComparisonOp
    ): Unit =
      assert(vec.length == other.length)
      var i = 0
      while i < vec.length do
        val hit = inline op match
          case ComparisonOp.LE => other(i) <= threshold
          case ComparisonOp.LT => other(i) < threshold
          case ComparisonOp.GE => other(i) >= threshold
          case ComparisonOp.GT => other(i) > threshold
          case ComparisonOp.EQ => other(i) == threshold
          case ComparisonOp.NE => other(i) != threshold
        if hit then vec(i) = 0.0f
        end if
        i += 1
      end while
    end `zeroWhere!`

    inline def zeroWhere(
        other: Array[Float],
        threshold: Float,
        inline op: ComparisonOp
    ): Array[Float] =
      val out = vec.clone()
      out.`zeroWhere!`(other, threshold, op)
      out
    end zeroWhere

  end extension

  extension (vec: Array[Array[Double]])
    def horizontalSum: Array[Double] =
      val out = new Array[Double](vec.head.length)
      var i = 0
      while i < vec.head.length do
        var sum = 0.0
        var j = 0
        while j < vec.length do
          sum += vec(j)(i)
          // pprint.pprintln(s"j : $j i : $i vecij : ${vec(j)(i)}  out : ${out(i)} sum : $sum")
          j = j + 1
        end while
        out(i) = sum
        i = i + 1
      end while
      out
  end extension

end floatarrays
