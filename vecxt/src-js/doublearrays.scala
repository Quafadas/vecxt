package vecxt

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.typedarray.Float64Array
import scala.util.chaining.*

import vecxt.BooleanArrays.*
import vecxt.BoundsCheck.BoundsCheck

import vecxt.matrix.Matrix

object arrayUtil:
  extension [A](d: Array[A]) def printArr: String = d.mkString("[", ",", "]")
  end extension

end arrayUtil

object doublearrays:

  @js.native
  trait JsArrayFacade extends js.Object:
    def fill[A](a: A): Unit = js.native
  end JsArrayFacade

  extension [A](v: js.Array[A]) inline def fill(a: A): Unit = v.asInstanceOf[JsArrayFacade].fill(a)
  end extension
  extension (vec: Array[Boolean])
    // inline def trues: Int =
    //   var sum = 0
    //   for i <- 0 until vec.length do if vec(i) then sum = sum + 1
    //   end for
    //   sum
    // end trues

    inline def &&(thatIdx: Array[Boolean]): Array[Boolean] =
      val result = Array.ofDim[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) && thatIdx(i)
      end for
      result
    end &&

    inline def ||(thatIdx: Array[Boolean]): Array[Boolean] =
      val result = Array.ofDim[Boolean](vec.length)
      for i <- 0 until vec.length do result(i) = vec(i) || thatIdx(i)
      end for
      result
    end ||
  end extension

  def linspace(a: Double, b: Double, length: Int = 100): Array[Double] =
    val increment = (b - a) / (length - 1)
    Array.tabulate[Double](length)(i => a + increment * i)
  end linspace

  extension (d: Double)
    inline def /(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d / arr(i)
        i = i + 1
      end while
      out
    end /

    inline def +(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d + arr(i)
        i = i + 1
      end while
      out
    end +

    inline def -(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d - arr(i)
        i = i + 1
      end while
      out
    end -

    inline def *(arr: Array[Double]) =
      val out = new Array[Double](arr.length)
      var i = 0

      while i < arr.length do
        out(i) = d * arr(i)
        i = i + 1
      end while
      out
    end *

  end extension

  extension (vec: Array[Double])

    inline def clampMin(min: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.max(vec(i), min)
        i += 1
      end while
      res
    end clampMin

    inline def `clampMin!`(min: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.max(vec(i), min)
        i += 1
      end while
    end `clampMin!`

    inline def maxClamp(max: Double): Array[Double] = clampMax(max)

    inline def minClamp(min: Double): Array[Double] = clampMin(min)

    inline def clampMax(max: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.min(vec(i), max)
        i += 1
      end while
      res
    end clampMax

    inline def `clampMax!`(max: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.min(vec(i), max)
        i += 1
      end while
    end `clampMax!`

    inline def clamp(min: Double, max: Double): Array[Double] =
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = Math.min(Math.max(vec(i), min), max)
        i += 1
      end while
      res
    end clamp
    inline def `clamp!`(min: Double, max: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.min(Math.max(vec(i), min), max)
        i += 1
      end while
    end `clamp!`

    inline def argmax: Int =
      val n = vec.length
      if n == 0 then -1 // Handle empty array case
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
      if n == 0 then -1 // Handle empty array case
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

    inline def `**!`(power: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = Math.pow(vec(i), power)
        i += 1
      end while
    end `**!`

    inline def **(power: Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = Math.pow(vec(i), power)
        i += 1
      end while
      newVec
    end **

    inline def `fma!`(multiply: Double, add: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) * multiply + add
        i += 1
      end while
    end `fma!`

    inline def `fma`(multiply: Double, add: Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = vec(i) * multiply + add
        i += 1
      end while
      newVec
    end `fma`

    inline def exp: Array[Double] =
      applyUnaryOp(Math.exp)

    inline def `exp!`: Unit =
      applyUnaryOpInPlace(Math.exp)

    inline def log: Array[Double] =
      applyUnaryOp(Math.log)

    inline def `log!`: Unit =
      applyUnaryOpInPlace(Math.log)

    inline def sqrt: Array[Double] =
      applyUnaryOp(Math.sqrt)

    inline def `sqrt!`: Unit =
      applyUnaryOpInPlace(Math.sqrt)

    inline def cbrt: Array[Double] =
      applyUnaryOp(Math.cbrt)

    inline def `cbrt!`: Unit =
      applyUnaryOpInPlace(Math.cbrt)

    inline def sin: Array[Double] =
      applyUnaryOp(Math.sin)

    inline def `sin!`: Unit =
      applyUnaryOpInPlace(Math.sin)

    inline def cos: Array[Double] =
      applyUnaryOp(Math.cos)

    inline def `cos!`: Unit =
      applyUnaryOpInPlace(Math.cos)

    inline def tan: Array[Double] =
      applyUnaryOp(Math.tan)

    inline def `tan!`: Unit =
      applyUnaryOpInPlace(Math.tan)

    inline def asin: Array[Double] =
      applyUnaryOp(Math.asin)

    inline def `asin!`: Unit =
      applyUnaryOpInPlace(Math.asin)

    inline def - : Array[Double] =
      applyUnaryOp(-_)

    inline def `-!`: Unit =
      applyUnaryOpInPlace(-_)

    private inline def applyUnaryOp(inline op: Double => Double): Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = op(vec(i))
        i += 1
      end while
      newVec
    end applyUnaryOp

    private inline def applyUnaryOpInPlace(inline op: Double => Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = op(vec(i))
        i += 1
      end while
    end applyUnaryOpInPlace

    inline def /(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, d)
      val n = vec.length
      val res = Array.ofDim[Double](n)
      var i = 0
      while i < n do
        res(i) = vec(i) / d(i)
        i += 1
      end while
      res
    end /

    inline def productSIMD: Double = product

    inline def productExceptSelf: Array[Double] =
      val n = vec.length
      val left = Array.ofDim[Double](n)
      val right = Array.ofDim[Double](n)
      val result = Array.ofDim[Double](n)

      left(0) = 1.0
      right(n - 1) = 1.0

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
    inline def logSumExp: Double =
      val maxVal = vec.max
      val sumExp = vec.map(x => Math.exp(x - maxVal)).sum
      maxVal + Math.log(sumExp)
    end logSumExp

        inline def *(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] =
      dimCheck(vec, d)
      val n = vec.length
      val res = Array.ofDim[Double](n)

      var i = 0
      while i < n do
        res(i) = vec(i) * d(i)
        i += 1
      end while
      res
    end *

    inline def *=(d: Array[Double])(using inline boundsCheck: BoundsCheck): Unit =
      dimCheck(vec, d)
      val n = vec.length

      var i = 0
      while i < n do
        vec(i) = vec(i) * d(i)
        i += 1
      end while
    end *=

    inline def outer(other: Array[Double])(using ClassTag[Double]): Matrix[Double] =
      val n = vec.length
      val m = other.length
      val out: Array[Double] = Array.ofDim[Double](n * m)

      var i = 0
      while i < n do
        var j = 0
        while j < m do
          out(j * n + i) = vec(i) * other(j)
          j = j + 1
        end while
        i = i + 1
      end while
      Matrix[Double](out, (n, m))(using BoundsCheck.DoBoundsCheck.no)
    end outer


    inline def *:*(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] = vec.*(d)

    inline def *:*=(d: Array[Double])(using inline boundsCheck: BoundsCheck): Unit = vec.*=(d)

    inline def <(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a < b, num)

    inline def <=(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a <= b, num)

    inline def >(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a > b, num)

    inline def >=(num: Double): Array[Boolean] =
      logicalIdx((a, b) => a >= b, num)

    inline def logicalIdx(
        inline op: (Double, Double) => Boolean,
        inline num: Double
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

    def toFloat64 = Float64Array.from(js.Array(vec *))

    inline def apply(index: Array[Boolean])(using inline boundsCheck: BoundsCheck.BoundsCheck): Array[Double] =
      dimCheck(vec, index)
      val trues = index.trues
      val newVec = Array.ofDim[Double](trues)
      var j = 0
      for i <- 0 until index.length do
        // println(s"i: $i  || j: $j || ${index(i)} ${vec(i)} ")
        if index(i) then
          newVec(j) = vec(i)
          j = 1 + j
      end for
      newVec
    end apply

    inline def minSIMD: Double =
      var i = 0
      var acc = Double.PositiveInfinity
      while i < vec.length do
        val v = vec(i)
        if v < acc then acc = v
        end if
        i += 1
      end while
      acc
    end minSIMD

    inline def maxSIMD: Double =
      var i = 0
      var acc = Double.NegativeInfinity
      while i < vec.length do
        val v = vec(i)
        if v > acc then acc = v
        end if
        i += 1
      end while
      acc
    end maxSIMD

    def increments: Array[Double] =
      val out = Array.ofDim[Double](vec.length)
      out(0) = vec(0)
      var i = 1
      while i < vec.length do
        out(i) = vec(i) - vec(i - 1)
        i = i + 1
      end while
      out
    end increments

    inline def stdDev: Double = stdDev(VarianceMode.Population)

    inline def stdDev(mode: VarianceMode): Double = std(mode)

    inline def std: Double = std(VarianceMode.Population)

    inline def std(mode: VarianceMode): Double =
      Math.sqrt(vec.variance(mode))

    inline def mean: Double = vec.sumSIMD / vec.length

    inline def sum: Double =
      var sum = 0.0
      var i = 0;
      while i < vec.length do
        sum = sum + vec(i)
        i = i + 1
      end while
      sum
    end sum

    inline def sumSIMD: Double = sum

    inline def product: Double =
      var sum = 1.0
      var i = 0;
      while i < vec.length do
        sum *= vec(i)
        i = i + 1
      end while
      sum
    end product

    inline def variance: Double = variance(VarianceMode.Population)

    def variance(mode: VarianceMode): Double =
      meanAndVariance(mode).variance
    end variance

    inline def meanAndVariance: (mean: Double, variance: Double) =
      meanAndVariance(VarianceMode.Population)

    inline def meanAndVariance(mode: VarianceMode): (mean: Double, variance: Double) =
      var mean = 0.0
      var m2 = 0.0
      var i = 0
      while i < vec.length do
        val n = i + 1
        val delta = vec(i) - mean
        mean += delta / n
        val delta2 = vec(i) - mean
        m2 += delta * delta2
        i += 1
      end while

      val denom = mode match
        case VarianceMode.Population => vec.length.toDouble
        case VarianceMode.Sample     => (vec.length - 1).toDouble

      (mean, m2 / denom)
    end meanAndVariance

    inline def unary_- : Array[Double] =
      val newVec = Array.ofDim[Double](vec.length)
      var i = 0
      while i < vec.length do
        newVec(i) = -vec(i)
        i += 1
      end while
      newVec
    end unary_-

    inline def pearsonCorrelationCoefficient(thatVector: Array[Double])(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Double =
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
      (n * sum_xy - (sum_x * sum_y)) / Math.sqrt(
        (sum_x2 * n - sum_x * sum_x) * (sum_y2 * n - sum_y * sum_y)
      )
    end pearsonCorrelationCoefficient

    inline def spearmansRankCorrelation(thatVector: Array[Double])(using
        inline boundsCheck: BoundsCheck.BoundsCheck
    ): Double =
      dimCheck(vec, thatVector)
      val theseRanks = vec.elementRanks
      val thoseRanks = thatVector.elementRanks
      theseRanks.pearsonCorrelationCoefficient(thoseRanks)
    end spearmansRankCorrelation

    // An alias - pearson is the most commonly requested type of correlation
    inline def corr(thatVector: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Double =
      pearsonCorrelationCoefficient(thatVector)

    def elementRanks: Array[Double] =
      val indexed1 = vec.zipWithIndex
      val indexed = indexed1.toArray.sorted(using Ordering.by(_._1))

      val ranks: Array[Double] = new Array(vec.length) // faster than zeros.
      ranks(indexed.last._2) = vec.length
      var currentValue: Double = indexed(0)._1
      var r0: Int = 0
      var rank: Int = 1
      while rank < vec.length do
        val temp: Double = indexed(rank)._1
        val end: Int =
          if temp != currentValue then rank
          else if rank == vec.length - 1 then rank + 1
          else -1
        if end > -1 then
          val avg: Double = (1.0 + (end + r0)) / 2.0
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

    inline def `cumsum!` =
      var i = 1
      while i < vec.length do
        vec(i) = vec(i - 1) + vec(i)
        i = i + 1
      end while
    end `cumsum!`

    inline def cumsum: Array[Double] =
      val out = vec.clone()
      out.`cumsum!`
      out
    end cumsum

    inline def dot(v1: Array[Double])(using inline boundsCheck: BoundsCheck): Double =
      dimCheck(vec, v1)

      var product = 0.0
      var i = 0;
      while i < vec.length do
        product = product + vec(i) * v1(i)
        i = i + 1
      end while
      product
    end dot

    inline def norm: Double =
      Math.sqrt(vec.dot(vec)(using vecxt.BoundsCheck.DoBoundsCheck.no))
    end norm

    inline def +(d: Double): Array[Double] =
      vec.clone().tap(_ += d)

    inline def +=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +=

    inline def -(d: Double): Array[Double] =
      vec.clone().tap(_ -= d)
    end -

    inline def -=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - d
        i = i + 1
      end while
    end -=

    inline def -(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Array[Double] =
      dimCheck(vec, vec2)
      vec.clone().tap(_ -= vec2)
    end -

    inline def -=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) - vec2(i)
        i = i + 1
      end while
    end -=

    inline def *=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) * d
        i = i + 1
      end while
    end *=

    inline def +(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Array[Double] =
      dimCheck(vec, vec2)
      vec.clone().tap(_ += vec2)
    end +

    inline def +:+(d: Double) =
      vec.clone().tap(_ +:+= d)
    end +:+

    inline def +:+=(d: Double): Unit =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + d
        i = i + 1
      end while
    end +:+=

    inline def +=(vec2: Array[Double])(using inline boundsCheck: BoundsCheck.BoundsCheck): Unit =
      dimCheck(vec, vec2)
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) + vec2(i)
        i = i + 1
      end while
    end +=

    inline def add(d: Array[Double])(using inline boundsCheck: BoundsCheck): Array[Double] = vec + d
    inline def multInPlace(d: Double): Unit = vec *= d

    inline def *(d: Double): Array[Double] =
      vec.clone().tap(_ *= d)
    end *

    inline def /=(d: Double): Array[Double] =
      var i = 0
      while i < vec.length do
        vec(i) = vec(i) / d
        i = i + 1
      end while
      vec
    end /=

    inline def /(d: Double): Array[Double] =
      vec.clone().tap(_ /= d)
    end /

    def covariance(thatVector: Array[Double]): Double =
      val μThis = vec.mean
      val μThat = thatVector.mean
      var cv: Double = 0
      var i: Int = 0;
      while i < vec.length do
        cv += (vec(i) - μThis) * (thatVector(i) - μThat)
        i += 1
      end while
      cv / (vec.length - 1)
    end covariance

    def maxElement: Double = vec.max
    // val t = js.Math.max( vec.toArray: _* )
  end extension

end doublearrays
