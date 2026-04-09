package vecxt

import scala.reflect.ClassTag
import scala.scalanative.unsafe.*
import scala.util.chaining.*

import org.ekrich.blas.unsafe.*

import vecxt.BooleanArrays.trues
import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.Matrix

object floatarrays:
  // export vecxt.JsNativeFloatArrays.*


  extension (f: Float)
    inline def /(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f / arr(i)
        i = i + 1
      end while
      out
    end /

    inline def +(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f + arr(i)
        i = i + 1
      end while
      out
    end +

    inline def -(arr: Array[Float]) =
      val out = new Array[Float](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = f - arr(i)
        i = i + 1
      end while
      out
    end -

    inline def *(arr: Array[Float]) =
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

    inline def clampMin(min: Float): Array[Float] =
      val n = vec.length
      val res = Array.ofDim[Float](n)

      var i = 0
      while i < n do
        res(i) = math.max(vec(i), min)
        i += 1
      end while
      res
    end clampMin

    inline def `clampMin!`(min: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = math.max(vec(i), min)
        i += 1
      end while
    end `clampMin!`

    inline def maxClamp(max: Float): Array[Float] = clampMax(max)

    inline def minClamp(min: Float): Array[Float] = clampMin(min)

    inline def clampMax(max: Float): Array[Float] =
      val n = vec.length
      val res = Array.ofDim[Float](n)

      var i = 0
      while i < n do
        res(i) = math.min(vec(i), max)
        i += 1
      end while
      res
    end clampMax

    inline def `clampMax!`(max: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = math.min(vec(i), max)
        i += 1
      end while
    end `clampMax!`

    inline def clamp(min: Float, max: Float): Array[Float] =
      val n = vec.length
      val res = Array.ofDim[Float](n)

      var i = 0
      while i < n do
        res(i) = math.min(math.max(vec(i), min), max)
        i += 1
      end while
      res
    end clamp

    inline def `clamp!`(min: Float, max: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = math.min(math.max(vec(i), min), max)
        i += 1
      end while
    end `clamp!`

    inline def argmax: Int =
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

    inline def argmin: Int =
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

    inline def minSIMD: Float =
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

    inline def maxSIMD: Float =
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

    inline def min: Float = minSIMD

    inline def max: Float = maxSIMD

    inline def sumSIMD: Float =
      var sum = 0.0
      var i = 0
      while i < vec.length do
        sum += vec(i)
        i += 1
      end while
      sum.toFloat
    end sumSIMD

    inline def sum: Float = sumSIMD

    inline def productSIMD: Float =
      var prod = 1.0
      var i = 0
      while i < vec.length do
        prod *= vec(i)
        i += 1
      end while
      prod.toFloat
    end productSIMD

    // inline def product: Float = productSIMD

    inline def `**!`(power: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.pow(vec(i).toDouble, power.toDouble).toFloat
        i += 1
      end while
    end `**!`

    inline def **(power: Float): Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.pow(vec(i).toDouble, power.toDouble).toFloat
        i += 1
      end while
      newVec
    end **

    inline def `fma!`(multiply: Float, add: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = (vec(i) * multiply + add)
        i += 1
      end while
    end `fma!`

    inline def fma(multiply: Float, add: Float): Array[Float] =
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

    inline def abs: Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.abs(vec(i))
        i += 1
      end while
      newVec
    end abs

    inline def `abs!`: Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.abs(vec(i))
        i += 1
      end while
    end `abs!`

    inline def exp: Array[Float] =
      applyUnaryOp(Math.exp)

    inline def `exp!`: Unit =
      applyUnaryOpInPlace(Math.exp)

    inline def expm1: Array[Float] =
      applyUnaryOp(Math.expm1)

    inline def `expm1!`: Unit =
      applyUnaryOpInPlace(Math.expm1)

    inline def log: Array[Float] =
      applyUnaryOp(Math.log)

    inline def `log!`: Unit =
      applyUnaryOpInPlace(Math.log)

    inline def log10: Array[Float] =
      applyUnaryOp(Math.log10)

    inline def `log10!`: Unit =
      applyUnaryOpInPlace(Math.log10)

    inline def log1p: Array[Float] =
      applyUnaryOp(Math.log1p)

    inline def `log1p!`: Unit =
      applyUnaryOpInPlace(Math.log1p)

    inline def sqrt: Array[Float] =
      applyUnaryOp(Math.sqrt)

    inline def `sqrt!`: Unit =
      applyUnaryOpInPlace(Math.sqrt)

    inline def cbrt: Array[Float] =
      applyUnaryOp(Math.cbrt)

    inline def `cbrt!`: Unit =
      applyUnaryOpInPlace(Math.cbrt)

    inline def sin: Array[Float] =
      applyUnaryOp(Math.sin)

    inline def `sin!`: Unit =
      applyUnaryOpInPlace(Math.sin)

    inline def sinh: Array[Float] =
      applyUnaryOp(Math.sinh)

    inline def `sinh!`: Unit =
      applyUnaryOpInPlace(Math.sinh)

    inline def cos: Array[Float] =
      applyUnaryOp(Math.cos)

    inline def `cos!`: Unit =
      applyUnaryOpInPlace(Math.cos)

    inline def cosh: Array[Float] =
      applyUnaryOp(Math.cosh)

    inline def `cosh!`: Unit =
      applyUnaryOpInPlace(Math.cosh)

    inline def tan: Array[Float] =
      applyUnaryOp(Math.tan)

    inline def `tan!`: Unit =
      applyUnaryOpInPlace(Math.tan)

    inline def tanh: Array[Float] =
      applyUnaryOp(Math.tanh)

    inline def `tanh!`: Unit =
      applyUnaryOpInPlace(Math.tanh)

    inline def asin: Array[Float] =
      applyUnaryOp(Math.asin)

    inline def `asin!`: Unit =
      applyUnaryOpInPlace(Math.asin)

    inline def acos: Array[Float] =
      applyUnaryOp(Math.acos)

    inline def `acos!`: Unit =
      applyUnaryOpInPlace(Math.acos)

    inline def atan: Array[Float] =
      applyUnaryOp(Math.atan)

    inline def `atan!`: Unit =
      applyUnaryOpInPlace(Math.atan)

    inline def unary_- : Array[Float] =
      val newVec = Array.ofDim[Float](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = -vec(i)
        i += 1
      end while
      newVec
    end unary_-

    inline def `-!`: Unit =
      var i = 0
      while i < vec.length do
        vec(i) = -vec(i)
        i += 1
      end while
    end `-!`

    inline def /(d: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
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

    inline def /:/(d: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
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


    inline def /=(d: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, d)
      val n = vec.length
      var i = 0
      while i < n do
        vec(i) = vec(i) / d(i)
        i += 1
      end while
    end /=

    inline def *(d: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
      dimCheck(vec, d)
      val out = new Array[Float](vec.length)
      var i = 0
      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *

    inline def *:*(d: Array[Float])(using inline boundsCheck: BoundsCheck): Array[Float] =
      dimCheck(vec, d)
      val out = new Array[Float](vec.length)
      var i = 0
      while i < vec.length do
        out(i) = vec(i) * d(i)
        i = i + 1
      end while
      out
    end *:*

    inline def *=(d: Array[Float])(using inline boundsCheck: BoundsCheck): Unit =
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


    inline def productExceptSelf: Array[Float] =
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
    inline def logSumExp: Float =
      val maxVal = vec.max
      var sumExp = 0.0
      var i = 0
      while i < vec.length do
        sumExp += Math.exp((vec(i) - maxVal).toDouble)
        i += 1
      end while
      (maxVal + Math.log(sumExp)).toFloat
    end logSumExp

    inline def outer(other: Array[Float])(using ClassTag[Float]): Matrix[Float] =
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
      Matrix[Float](out, (n, m))(using BoundsCheck.DoBoundsCheck.no)
    end outer

    inline def <(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Float): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Float): Array[Boolean] =
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

    inline def cumsum: Array[Float] =
      val out = vec.clone()
      out.`cumsum!`
      out
    end cumsum

    inline def `cumsum!`: Unit =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    inline def increments: Array[Float] =
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

    inline def mean: Float =
      (vec.sum / vec.length).toFloat
    end mean

    inline def variance: Float = variance(VarianceMode.Population)

    inline def variance(mode: VarianceMode): Float =
      meanAndVariance(mode).variance
    end variance

    inline def meanAndVariance: (mean: Float, variance: Float) =
      meanAndVariance(VarianceMode.Population)

    inline def meanAndVariance(mode: VarianceMode): (mean: Float, variance: Float) =
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

    inline def std: Float = std(VarianceMode.Population)

    inline def std(mode: VarianceMode): Float =
      Math.sqrt(vec.variance(mode).toDouble).toFloat

    inline def stdDev: Float = stdDev(VarianceMode.Population)

    inline def stdDev(mode: VarianceMode): Float = std(mode)

    inline def norm: Float = blas.cblas_snrm2(vec.length, vec.at(0), 1)

    inline def dot(v1: Array[Float])(using inline boundsCheck: BoundsCheck): Float =
      dimCheck(vec, v1)
      blas.cblas_sdot(vec.length, vec.at(0), 1, v1.at(0), 1)
    end dot

    inline def -(vec2: Array[Float])(using inline boundsCheck: BoundsCheck.BoundsCheck): Array[Float] =
      vec.clone.tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Float])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.cblas_saxpy(vec.length, -1.0f, vec2.at(0), 1, vec.at(0), 1)
    end -=

    inline def +(vec2: Array[Float])(using inline boundsCheck: BoundsCheck.BoundsCheck): Array[Float] =
      vec.clone.tap(_ += vec2)
    end +

    inline def +=(vec2: Array[Float])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      blas.cblas_saxpy(vec.length, 1.0f, vec2.at(0), 1, vec.at(0), 1)
    end +=

    inline def +:+(d: Float): Array[Float] =
      vec.clone.tap(_ +:+= d)
    end +:+

    inline def +:+=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +:+=

    inline def -(d: Float): Array[Float] =
      vec.clone().tap(_ -= d)
    end -

    inline def -=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    inline def +(d: Float): Array[Float] =
      vec.clone().tap(_ += d)

    inline def +=(d: Float): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    inline def *=(d: Float): Unit =
      blas.cblas_sscal(vec.length, d, vec.at(0), 1)
    end *=

    inline def *(d: Float): Array[Float] =
      vec.clone.tap(_ *= d)
    end *

    inline def /=(d: Float): Unit =
      blas.cblas_sscal(vec.length, 1.0f / d, vec.at(0), 1)

    inline def /(d: Float): Array[Float] =
      vec.clone.tap(_ /= d)

    inline def pearsonCorrelationCoefficient(thatVector: Array[Float])(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Float =
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

    inline def spearmansRankCorrelation(thatVector: Array[Float])(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Float =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    inline def corr(thatVector: Array[Float])(using inline boundsCheck: BoundsCheck.BoundsCheck): Float =
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

  end extension

  extension (vec: Array[Array[Double]])
    inline def horizontalSum: Array[Double] =
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
